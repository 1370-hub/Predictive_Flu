library(textreadr)

survey <- read_document(file="survey.txt")

a <- length(survey)/6 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- survey[i*b+z-b]
  }#closing z loop
}#closing i loop
############################################
#wash gender column
############################################
my_txt <- my_df$V1
#View(my_txt)
library(dplyr)
library(tidytext)
mydf <- data_frame(line=1:a, text=my_txt)
print(mydf)
data(stop_words)
gender_nostop <- mydf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)  #here's where we remove tokens
  
m<-as.matrix(gender_nostop$word)
m[m=='mail']<-'male'
m[m=='email']<-'female'
m[m!='male'&m!='female']<-NA
gender_nostop$word<-m
gender_nostop<-gender_nostop[complete.cases(gender_nostop[,]),]
colnames(gender_nostop)<-c('person','gender')

my_df['person']<-c(1:39)

my_gender_df<-gender_nostop%>%
  right_join(my_df, by = c("person"))

library(dplyr)
library(stringr)
library(tidytext)


########################################
###wash age column
########################################
my_txt_age <- my_df$V2
#View(my_txt_age)

library(tidyr)
my_txt_age<-extract_numeric(my_txt_age)
#View(my_txt_age)
my_txt_age[my_txt_age>65]<-NA
my_txt_age[my_txt_age<18]<-NA
mydf_age <- data_frame(line=1:a, text=my_txt_age)
colnames(mydf_age)<-c('person','age')

mydf_final<-mydf_age%>%
  right_join(my_gender_df, by = c("person"))%>%
  select(-c(V1,V2))

####################################################
### Now use mydf_final to do analyse
####################################################

#choose one column as example:V5

#frenquency calculate
mydf_analyse <- mydf_final %>%
  unnest_tokens(word, V6) %>%
  anti_join(stop_words)
#write.csv(mydf_analyse, file='C:/Users/xuhao/Desktop/case_Q6.csv')

nrc<-get_sentiments("nrc")
unique(nrc$sentiment)

nrcpositive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive") #what is your sentiment

#inner joining the emma book and the surprise sentiments
mydf_analyse %>%
  inner_join(nrcpositive) %>%
  count(word, sort=T)

########################################################
##### Comparing different sentiment libraries on chosen column
########################################################


afinn <- mydf_analyse %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(gender) %>% #using integer division to define larger sections of text
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")
library(tidyr)
bing_and_nrc1 <- bind_rows(
  mydf_analyse%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  mydf_analyse %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, gender, sentiment) %>% #two genders
  spread(sentiment, n, fill=0) %>% #change the column structure reduce row numbers
  mutate(sentiment = positive-negative)

library(ggplot2)
bind_rows(afinn, bing_and_nrc1) %>%
  ggplot(aes(gender, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##############################################################
######## Most common positive and negative words #############
##############################################################

bing_counts <- mydf_analyse %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
###############################################################
#word cloud
######################################################3
library(wordcloud)
data("stop_words")
mydf_analyse%>%
  count(word, sort = T)%>%
  with(wordcloud(word, n, max.words = 100))

###################################################
#### Adding positive and negative sentiments ######
###################################################
#install.packages(("reshape2"))
library(reshape2)
mydf_analyse %>%
  count(word,sort = T)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100)



############################################
#correlation
##############################################
frequency <- mydf_analyse%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(gender, word) %>%
  group_by(gender) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(gender, proportion) %>%
  gather(gender, proportion, `female`)

#let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`male`, 
                      color = abs(`male`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~gender, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "male", x=NULL)

############################################
####positive or negative
####
mydf_analyse %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=25,
                   scale=c(0.5,0.5),
                   fixed.asp= T,
                   title.size = 1)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$gender == "female",],
         ~proportion + `male`)



############################################
#TF - IDF
##########################
library(readr)
case_Q3 <- mydf_final%>%
  select(-c(5:7))
colnames(case_Q3)<-c("person","age","gender","text")
case_Q4 <- mydf_final%>%
  select(-c(4,6,7))
colnames(case_Q4)<-c("person","age","gender","text")
case_Q5 <- mydf_final%>%
  select(-c(4,5,7))
colnames(case_Q5)<-c("person","age","gender","text")
case_Q6 <- mydf_final%>%
  select(-c(4,5,6))
colnames(case_Q6)<-c("person","age","gender","text")

question_word<-bind_rows(mutate(case_Q3,question='Q3'),
                         mutate(case_Q4,question='Q4'),
                         mutate(case_Q5,question='Q5'),
                         mutate(case_Q6,question='Q6'),)

question_new <- question_word %>%
  unnest_tokens(word,text)%>%
  count(question, word, sort=TRUE) %>%
  ungroup()%>%
  bind_tf_idf(word,question,n)




question_new%>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()

#############################################################################
library(reader)
data("AssociatedPress", package = "topicmodels")
AssociatedPress
q<-bind_rows(mutate(case_Q4,question="Q4"),
             mutate(case_Q5,question="Q5"))

tidy_flu <- q %>%
  unnest_tokens(word,text)%>%
  group_by(gender) %>%
  anti_join(stop_words) %>%
  count(word)%>%
  cast_dtm(gender, word, n)

#####################################################
### Running LDA per documnet
######################################################
library(topicmodels)
flu_lda <- LDA(tidy_flu,k=2, control = list(seed=123))

flu_gamma <- tidy(flu_lda, matrix="gamma")
 

##########################################################
### Running LDA per token
##########################################################
library(tidytext)
flu_topics <- tidy(flu_lda, matrix="beta")
flu_topics
library(ggplot2)
library(dplyr)

top_terms <- flu_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
####-------------------------------##Naive bayes
corp_frame<-unite(mydf_final, text, c(V3,V4,V5,V6), sep = " ", remove = TRUE, na.rm = FALSE)

lexi_naive<-data.frame(test=c(1,1,1,1),word=c('healthy','vitamin', 'vaccination','vegetable'))

df_naive<- corp_frame%>%
  unnest_tokens(word,text)%>%
  anti_join(stop_words)%>%
  left_join(lexi_naive, by="word")%>%
  group_by(person)

df_naive$test[is.na(df_naive$test)]<-0

library(quanteda)
library(RColorBrewer)
library(ggplot2)

naive_opinion<- Corpus(VectorSource(mydf_analyse)) 
inspect(naive_opinion)


msg.dfm_f <- dfm(corpus(corp_frame), tolower = TRUE) #generating document 
msg.dfm_f <- dfm_trim(msg.dfm_f, min_termfreq  = 2, min_docfreq = 1)
msg.dfm_f <- dfm_weight(msg.dfm_f)
msg.dfm_f
head(msg.dfm_f)
#let's split the docs into training and testing data
df_train<-msg.dfm_f[1:20,]
df_test<-msg.dfm_f[20:39,]
df_value<-df_naive%>%
  group_by(person)%>%
  summarise(sum(test))

df_value$`sum(test)`[df_value$`sum(test)`>0]<-1
#building the Naive Bayes model:
NB_classifier_f <- textmodel_nb(df_train, df_value$`sum(test)`[1:20])
NB_classifier_f
summary(NB_classifier_f)

# predicting the testing data

pred <- predict(NB_classifier_f, df_test)
pred
