#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(rhandsontable)
# Define UI for application that draws a histogram
shinyUI(
  navbarPage("R plot", 
             # multi-page user-interface that includes a navigation bar.
             tabPanel("Sentiments",
                      sidebarPanel(
                        selectInput("Question",
                                    label = "Sentiments question choices",
                                    choices = list("Q3","Q4","Q5","Q6"),
                                    selected = "Q3"),
                        selectInput("corr1",
                                    label = "Correlation 1",
                                    choices = list("Q3","Q4","Q5","Q6"),
                                    selected = "Q3"),
                        selectInput("corr2",
                                    label = "Correlation 2",
                                    choices = list("Q3","Q4","Q5","Q6"),
                                    selected = "Q4")
                      ),
                      mainPanel(plotlyOutput(outputId = "sentiment"),
                                verbatimTextOutput(outputId = "corr"))
             ),
             tabPanel("Word Cloud",
                      sidebarPanel(
                        selectInput("cloud",
                                    label = "Select your word cloud Question",
                                    choices = list("Q3","Q4","Q5","Q6"),
                                    selected = "Q4")
                      ),
                      mainPanel(plotOutput(outputId = "word_cloud")
                      
             ) 
  ),
  tabPanel("TF-IDF",
           mainPanel(
             plotlyOutput(outputId = "tfdif2")
             ) ),
  tabPanel("Afinn",
           sidebarPanel(
             selectInput("afinn_1",
                         label = "select what you need",
                         choices = list("male","female","all"),
                         selected = "male"),
             selectInput("afinn_2",
                         label = "select what you need",
                         choices = list("male","female","all"),
                         selected = "female")
           ),
           mainPanel(
             plotlyOutput(outputId = "plot_afinn_1"),
             plotlyOutput(outputId = "plot_afinn_2")
           ) 
  ),
  tabPanel("Prediction Model",
           sidebarPanel(
             textInput("word1",
                       label = "Type in your words!",
                       value = "healthy"),
             textInput("word2",
                       label = "Type in your words!",
                       value = "healthy"),
             textInput("word3",
                       label = "Type in your words!",
                       value = "healthy"),
             textInput("word4",
                       label = "Type in your words!",
                       value = "healthy")
             ),
           mainPanel(verbatimTextOutput(outputId = "model")
                     
           ) 
  )
  
  
))
