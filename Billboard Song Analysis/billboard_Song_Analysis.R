#Installing Packages
library(shiny)
library(data.table)
library(gdata)
library(lubridate)
library(stringr)
library(ggplot2)
library(rlang)
library(leaflet)
library(tidyverse)
library(plotly)
library(sqldf)
library(ggthemes)
library(tm)
library(dplyr)
library(dbplyr)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(treemap)
library(tidytext)

#list.of.packages <- c("data.table","date","gdata","lubridate","stringr","shiny","ggplot2","rlang",
#                      "leaflet","tidyverse","plotly","sqldf","ggthemes","stringr","tm","dplyr","wordcloud",
#                      "RColorBrewer","SnowballC","treemap","tidytext")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages,dependencies = TRUE)

#Loading Packages
#invisible(lapply(list.of.packages, require, character.only = TRUE))

#Loading Dataset
df <- fread("billboard_lyrics_1964-2015.csv")

#Grouping by Artist 
df_rank <- sqldf("select Rank,Artist,count(Artist) as ArtCount from df group by Rank,Artist
                 order by count(Artist) desc")

#Filtering only required columns
df.lyrics <- df %>% select("Year","Lyrics")

#Cleaning Lyrics
df.lyrics$cleartxt <- tm::removeNumbers(df.lyrics$Lyrics) #Removing Numbers
df.lyrics$cleartxt <- tm::removePunctuation(df.lyrics$cleartxt) #Removing Punctuation
df.lyrics$cleartxt <- tm::removeWords(x = df.lyrics$cleartxt, stopwords("english"))
df.lyrics$cleartxt <- tm::removeWords(x = df.lyrics$cleartxt, stopwords(kind = "SMART"))

#Collapsing the Lyrics text by year
df.lyrics.year <- df.lyrics %>%
  group_by(Year) %>%
  summarise(cleartxt=paste(cleartxt,collapse=''))

#Bucketing Years based on a 5 year period
df.lyrics.year$Year_bucket <- cut(df.lyrics.year$Year, seq(1950,2015,5))

#Collapsing the Lyrics text by Bucket Period
df.lyrics.year.bucket <- df.lyrics.year %>%
  group_by(Year_bucket) %>%
  summarise(cleartxt=paste(cleartxt,collapse=''))

#Function to create the word Frequency dataframe
text_to_freq_count <- function(df_word,yr_filter) {
  temp <- df_word[df_word$Year_bucket==yr_filter,] %>% select(cleartxt)
  corpus <- Corpus(VectorSource(temp))
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(d)
}

#Filtering only required columns
df.song <- df %>% select("Year","Song")

#Cleaning Lyrics
df.song$cleartxt <- tm::removeNumbers(df.song$Song) #Removing Numbers
df.song$cleartxt <- tm::removePunctuation(df.song$cleartxt) #Removing Punctuation
df.song$cleartxt <- tm::removeWords(x = df.song$cleartxt, stopwords("english"))
df.song$cleartxt <- tm::removeWords(x = df.song$cleartxt, c(stopwords(kind = "SMART"),"aint","youve"))
df.song$cleartxt <- gsub("[^0-9A-Za-z///' ]", "", df.song$cleartxt)


#Collapsing the Lyrics text by year
df.song.year <- df.song %>%
  group_by(Year) %>%
  summarise(cleartxt=paste(cleartxt,collapse=''))

#Bucketing Years based on a 5 year period
df.song.year$Year_bucket <- cut(df.song.year$Year, seq(1950,2015,5))

#Collapsing the Lyrics text by Bucket Period
df.song.year.bucket <- df.song.year %>%
  group_by(Year_bucket) %>%
  summarise(cleartxt=paste(cleartxt,collapse=''))

#Function to create the word Frequency based on Year
text_to_freq_count_year <- function(df_word,yr_filter) {
  temp <- df_word[df_word$Year==yr_filter,] %>% select(cleartxt)
  corpus <- Corpus(VectorSource(temp))
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  d$word <-  sapply(d$word, as.character)
  return(d)
}



#UI
ui <- fluidPage(
  
  titlePanel(strong(h2("Billboard Song Analysis"),style = "font-family: 'Arial', 'Helvetica', 'sans-serif'")),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Song Lyrics",br(), 
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = "Year",
                                         label = "Select Year Bucket:",
                                         choices=unique(df.lyrics.year.bucket$Year_bucket)),
                             hr(),
                             sliderInput("max",
                                         "Maximum Number of Words:",
                                         min = 1,  max = 300,  value = 50),
                             hr(),
                             numericInput("minfreq", label = "Minimum Frequency of the Word:", value = 1), width = 4),
                           mainPanel(
                             h4(strong("Most Frequently used words used in Song Lyrics"),align="center"),
                             plotOutput(outputId = "WordCloud")
                           )
                         )
                ),
                tabPanel("Song Title", br(), 
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = "Year2",
                                         label = "Select Year Bucket:",
                                         choices=unique(df.song.year.bucket$Year_bucket)),
                             hr(),
                             numericInput("countofwords", label = "Top N words:", value = 10)
                           ),
                           mainPanel(
                             h4(strong("Most Frequently used words used in Song Title"),align="center") ,
                             plotOutput(outputId = "TreeMap"),width = 8
                           )
                         )),
                tabPanel("Sentiment Analysis",br(), 
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = "Year3",
                                         label = "Select Year:",
                                         choices=unique(df$Year))
                             ,
                             sliderInput("Nsenti", label = "Top N words", min=5,max=20,value=5),width = 3),
                           mainPanel(
                             h4(strong("Lyrics Sentiment Analysis"),align="center"),
                             plotOutput(outputId = "SentimentChart")
                           )
                         ))
    )
  )
  
)

#Server
server <- function(input, output) {
  
  output$BarChart <- renderPlot({ggplot(data=df_rank[df_rank$Rank==input$rank,], 
                                        aes(x=Artist, y=ArtCount)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=ArtCount), vjust=1.6, color="white", size=3.5)+
      theme_minimal() })
  
  output$WordCloud <- renderPlot({wordcloud(words = text_to_freq_count(df.lyrics.year.bucket,input$Year)$word, 
                                            freq = text_to_freq_count(df.lyrics.year.bucket,input$Year)$freq, min.freq = input$minfreq,
                                            max.words=input$max, random.order=FALSE, rot.per=0.35, 
                                            colors=brewer.pal(8, "Dark2"))  })
  
  output$TreeMap <- renderPlot({ treemap(head(text_to_freq_count(df.song.year.bucket,input$Year2),input$countofwords),index = c("word"),vSize ="freq",
                                         title="") })
  
  output$SentimentChart <- renderPlot({ text_to_freq_count_year(df.lyrics.year,input$Year3) %>% 
      inner_join(get_sentiments("bing"),by='word') %>%
      group_by(sentiment) %>%
      top_n(input$Nsenti,freq) %>%
      ungroup() %>%
      mutate(word = reorder(word, freq)) %>%
      ggplot(aes(word, freq, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "",
           x = NULL) +
      coord_flip() })
  
}


#R Shiny Application
shinyApp(ui, server)

