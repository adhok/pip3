library(shiny)
library(tidyverse)
library(tidytext)
library(widyr)

library(stringr)
library(networkD3)
library(data.table)

inaug <- fread("inaug_speeches.csv",header=T,stringsAsFactors = F)
important_words <- inaug %>%
  unnest_tokens(word,text) %>%
  filter(str_detect(word,"^[a-z]+$")) %>%
  group_by(Name,word)%>%
  summarise(n=n()) %>%
  bind_tf_idf(word,Name,n) %>%
  group_by(Name) %>%
  arrange(desc(tf_idf)) %>%
  top_n(5)
persons <- inaug %>% select(Name) %>% unique()

shinyUI(fluidPage(
  
  
  titlePanel("Visualization of Top correlated Terms and top 10 TF IDF values "),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(tags$style("#force{height:100vh !important;}")),
      tags$head(tags$style("#plot{height:50vh !important;}")),
      
      selectInput("var", 
                  label = "President's Name",
                  choices = dput(persons$Name),
                  selected = "George Washington"),
      plotOutput("plot")
      
      
    ),
    mainPanel(
      
      
      networkD3::forceNetworkOutput("force"),
      style = "border: 1px solid black;"
      
    )
  )
)
)

