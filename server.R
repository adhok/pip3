library(tidyverse)
library(tidytext)
library(widyr)

library(stringr)
library(shiny)
library(networkD3)

inaug <- read.csv("inaug_speeches.csv",header=T,stringsAsFactors = F)
important_words <- inaug %>%
  unnest_tokens(word,text) %>%
  filter(str_detect(word,"^[a-z]+$")) %>%
  group_by(Name,word)%>%
  summarise(n=n()) %>%
  bind_tf_idf(word,Name,n) %>%
  group_by(Name) %>%
  arrange(desc(tf_idf)) %>%
  top_n(10)

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 10,colour = "black",hjust=0.5),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=8),
    axis.title = element_text(size=5),
    axis.text = element_text(size=5),
    axis.title.x = element_text(hjust=1),
    axis.title.y = element_text(hjust=1),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "bold"),
    legend.text = element_text(colour = "black", face = "bold"),
    axis.text.x = element_text(vjust=-1,angle=90,size=10))
}

persons <- inaug %>% select(Name) %>% unique()


plot_network <- function(data,person){
  
  words_to_check <- important_words %>% filter(Name==person) %>% select(word)
  data_new_cor <- data %>%
    unnest_tokens(word,text) %>%
    filter(!word %in% stop_words$word)%>%
    pairwise_cor(word,X) %>%
    filter(item1 %in% words_to_check$word) %>%
    na.omit() %>%
    filter(correlation>0.8)
  
  item1 <- data_new_cor$item1
  item2 <- data_new_cor$item2
  n <- data_new_cor$correlation
  
  nodeFactors <- factor(sort(unique(c(item1, item2))))
  nodes <- data.frame(name = nodeFactors, group = 1)
  
  item1 <- match(item1, levels(nodeFactors)) - 1
  item2 <- match(item2, levels(nodeFactors)) - 1
  links <- data.frame(item1, item2, n)
  
  forceNetwork(Links = links, Nodes = nodes, Source = 'item1', 
               Target = 'item2', Value = 'n', NodeID = 'name', Group = 'group',fontSize = 15)
  
}


## for the progress bar
compute_data <- function(updateProgress = NULL) {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:10) {
    Sys.sleep(0.25)
    
    # Compute new row of data
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
    }
    
    # Add the new row of data
    dat <- rbind(dat, new_row)
  }
  
  dat
}


shinyServer(function(input, output) {
  
  
  
  output$force <- renderForceNetwork({
    #library(networkD3)
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "Crunching the numbers...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 5
      }
      progress$set(value = value, detail = detail)
    }
    
    # Compute the new data, and pass in the updateProgress function so
    # that it can update the progress indicator.
    compute_data(updateProgress)
    
    
    plot_network(inaug,input$var)
  })
  output$plot <- renderPlot({
    
    important_words %>%
      filter(Name==input$var) %>%
      ggplot(aes(x=reorder(word,tf_idf),y=tf_idf))+geom_bar(stat="identity")+
      labs(title="Top Terms By TF-IDF Value",x="Word",y="TF-IDF Value")+plotTheme()
    
    
    
    
  })
  
})

