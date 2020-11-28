

library(shiny)
library(plotly)
library(rhandsontable)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  library(readxl)
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(stringr)
  library(tidytext)
  library(reader)
  
  mydf <- read_csv("mydf_final.xls")
  
  
  output$sentiment <- renderPlotly({
  mydf_analyse<-mydf%>%
    unnest_tokens(word,text)%>%
    filter(question==input$Question)
  
  afinn <- mydf_analyse %>%
    inner_join(get_sentiments("afinn"))%>%
    group_by(gender) %>% #using integer division to define larger sections of text
    summarise(sentiment=sum(value)) %>%
    mutate(method="AFINN")

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
    
     
  
    z <- bind_rows(afinn, bing_and_nrc1) %>%
      ggplot(aes(gender, sentiment, fill=method))+
      geom_col(show.legend=FALSE)+
      facet_wrap(~method, ncol =1, scales= "free_y")
    z
  })
  
  output$corr <- renderPrint({
    mydf_corr1<-mydf%>%
      unnest_tokens(word,text)%>%
      anti_join(stop_words)%>%
      filter(question==input$corr1)
    
    mydf_corr2<-mydf%>%
      unnest_tokens(word,text)%>%
      anti_join(stop_words)%>%
      filter(question==input$corr2)
    
    corr_bind<-bind_rows(mydf_corr1,mydf_corr2)
    
    frequency <- corr_bind%>%
      mutate(word=str_extract(word, "[a-z']+")) %>%
      count(question, word) %>%
      group_by(question) %>%
      mutate(proportion = n/sum(n))%>%
      select(-n) %>%
      spread(question, proportion) %>%
      gather(question, proportion, input$corr1)%>%
      arrange(desc(proportion))
    
    frequency
  })
  
  output$word_cloud <- renderPlot({
    mydf_analyse <- mydf %>%
      unnest_tokens(word, text) 
    
    ###################################################
    #### Adding positive and negative sentiments ######
    ###################################################
    library(wordcloud)
    library(reshape2)
    cloud<-mydf_analyse %>%
      inner_join(get_sentiments("nrc")) %>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors = c("grey20", "gray80"),
                       max.words=25,
                       scale=c(0.5,0.5),
                       fixed.asp= T,
                       title.size = 1)
    
    cloud
  })
  
  output$tfdif2 <- renderPlotly({
    #bigram each question
    questiontoken <- mydf%>%
      unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
      separate(bigram, c("word1", "word2"), sep = " ")%>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      group_by(question) %>%
      count(word1, word2, sort = TRUE)
    
    
    #tfidf eqch question
    questiontf_idf <-questiontoken%>%
      unite(bigram, word1, word2, sep = " ")%>%
      count(question, bigram) %>%
      bind_tf_idf(bigram, question, n) %>%
      arrange(desc(tf_idf))%>%
      mutate(word=factor(bigram, levels=rev(unique(bigram))))%>%
      top_n(15) %>%
      ungroup %>%
      ggplot(aes(word, tf_idf, fill=question))+
      geom_col(show.legend=FALSE)+
      labs(x=NULL, y="tf-idf")+
      facet_wrap(~question, ncol=2, scales="free")+
      coord_flip()
    
    questiontf_idf
  })
  
  output$plot_afinn_1 <- renderPlotly({
    if(input$afinn_1=="all"){
      mydf_afinn_1<-mydf%>%
        unnest_tokens(word,text)%>%
        group_by(question)
    }
    else{
      mydf_afinn_1<-mydf%>%
        filter(gender==input$afinn_1)%>%
        unnest_tokens(word,text)%>%
        group_by(question)
    }
    
    afinn_1 <- mydf_afinn_1 %>%
      inner_join(get_sentiments("afinn"))%>%
      summarise(sentiment=sum(value)) %>%
      mutate(gender=input$afinn_1)
    
    
    plot_afinn_1<-afinn_1 %>%
      ggplot(aes(question, sentiment,fill=gender))+
      geom_col(show.legend=FALSE)
    
    plot_afinn_1
  })
  
  output$plot_afinn_2 <- renderPlotly({
    if(input$afinn_2=="all"){
      mydf_afinn_2<-mydf%>%
        unnest_tokens(word,text)%>%
        group_by(question)
    }
    else{
    mydf_afinn_2<-mydf%>%
      filter(gender==input$afinn_2)%>%
      unnest_tokens(word,text)%>%
      group_by(question)
  }
    
    afinn_2 <- mydf_afinn_2 %>%
      inner_join(get_sentiments("afinn"))%>%
      summarise(sentiment=sum(value)) %>%
      mutate(gender=input$afinn_2)
    
    
    plot_afinn_2<-afinn_2 %>%
      ggplot(aes(question, sentiment,fill=gender))+
      geom_col(show.legend=FALSE)
    
    plot_afinn_2
  })
  
  
  output$model <- renderPrint({
    mydf_final<-mydf%>%
      spread(question,text)
    
    corp_frame<-unite(mydf_final, text, c(Q3,Q4,Q5,Q6), sep = " ", remove = TRUE, na.rm = FALSE)
    txt<-c(input$word1,input$word2,input$word3,input$word4)
    lexi_naive<-data.frame(test=c(1,1,1,1),word=txt)
    
    df_naive<- corp_frame%>%
      unnest_tokens(word,text)%>%
      anti_join(stop_words)%>%
      left_join(lexi_naive, by="word")%>%
      group_by(person)
    
    df_naive$test[is.na(df_naive$test)]<-0
    
    library(quanteda)
    library(RColorBrewer)
    library(ggplot2)

    msg.dfm_f <- dfm(corpus(corp_frame), tolower = TRUE) #generating document 
    msg.dfm_f <- dfm_trim(msg.dfm_f, min_termfreq  = 2, min_docfreq = 1)
    msg.dfm_f <- dfm_weight(msg.dfm_f)

    #let's split the docs into training and testing data
    df_train<-msg.dfm_f[1:20,]
    df_test<-msg.dfm_f[20:39,]
    df_value<-df_naive%>%
      group_by(person)%>%
      summarise(sum(test))
    
    df_value$`sum(test)`[df_value$`sum(test)`>0]<-1
    #building the Naive Bayes model:
    NB_classifier_f <- textmodel_nb(df_train, df_value$`sum(test)`[1:20])

    summary(NB_classifier_f)
    
    # predicting the testing data
    
    pred <- predict(NB_classifier_f, df_test)
    pred
    print( summary(NB_classifier_f))
    print(pred)
  })
  
  
})

