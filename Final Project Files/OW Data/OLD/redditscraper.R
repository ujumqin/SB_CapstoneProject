library(XML)
library(xml2)
library(RCurl)
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
#library(httr)
library(purrr)


library(purrr)
# get all the links present on a particular url/page



extract_links <- function(url){
  url_f <- url
  #data <- readLines(con = url_f,)
  parsed_data <- read_html(url_f)
  data_from_page<- parsed_data %>%
    html_nodes(".comment,.bylink") %>% 
    html_attr(name = "href")
}

#Given a url get all the data 
extract_data= function(url){
  url_d<-url
  #data <- readLines(con = url_d,)
  parsed_data <- read_html(url_d)
  data_from_page<- parsed_data %>%
    html_nodes(".md,.usertext-body") %>% 
    html_text()
  data_from_page<- data_from_page[-c(1,2)]
}

#extract timestamp for comment
extract_time= function(url){
  url_t <- url
  parsed_data <- read_html(url_t)
  data_from_page<- parsed_data %>%
    
}

# Based on comments length decide which links to crawl and which one to not
Links_for_crawling= function(url,threshold){
  url_d<-url
  #data <- readLines(con = url_d,)
  parsed_data <- read_html(url_d)
  data_from_page<- parsed_data %>%
    html_nodes(".comment,.bylink") %>% 
    html_text()
  comments<-strsplit(data_from_page," ")
  len<-vector()
  for(i in 1:length(comments)){
    test<-as.integer(comments[[i]][1])
    if(!is.na(test)){
      len[i]<-as.integer(comments[[i]][1])
    }else{
      print("Came to else")
      len[i]<-0
    }
  }
  threshold1<-threshold
  link_index_to_crawl<-which(len>threshold1)
  
}
get_next_page_links= function(url){
  main_url<-url
  #f_data <- readLines(con = main_url,)
  parsed_data_f <- read_html(main_url)
  data_from_page_f<- parsed_data_f %>%
    html_nodes(".next-button") %>% 
    html_children()
  htmltxt <- paste(capture.output(data_from_page_f, file=NULL))
  next_link<-htmltxt[3]
  next_link<-strsplit(next_link,split ="\"")[[1]][2]
  
}


####test

url_d<-url
#data <- readLines(con = url_d,)
parsed_data <- read_html(url_d)
data_from_page<- parsed_data %>%
  html_nodes(".comment,.bylink") %>% 
  html_text()
comments<-strsplit(data_from_page," ")
len<-vector()
for(i in 1:length(comments)){
  test<-as.integer(comments[[i]][1])
  if(!is.na(test)){
    len[i]<-as.integer(comments[[i]][1])
  }else{
    print("Came to else")
    len[i]<-0
  }
}
threshold1<-threshold
link_index_to_crawl<-which(len>threshold1)


####test

#Modify Following two lines
subreddits<-c("Overwatch/top","CompetitiveOverwatch/top")
threshold_for_comment<-30
for(i in 1:length(subreddits)){
  reddit<-"https://www.reddit.com/r/"
  starting_url <-paste(reddit, subreddits[i],"/",sep = "")
  print(starting_url)
  logFile = paste("Reddit_comment_", i,".txt",sep = "")
  cat("Scrapping Subreddit:",subreddits[i])
  while(!is.na(starting_url)) {
    links <- extract_links(starting_url)
    links_index_to_crawl<-Links_for_crawling(starting_url,threshold_for_comment)
    for(j in links_index_to_crawl){
      link<-links[j]
      data<-extract_data(link)
      time<-extract_time(link)
      print("extra_data")
      cat("Scrapping Page:",j, links[j])
      cat(link, file=logFile, append=TRUE, sep = "\n")
      cat(data, file=logFile, append=TRUE, sep = "\n")
      
    } 
    next_l<-get_next_page_links(starting_url)
    starting_url<-next_l
    print("next_link")
    print(next_l)
  }
}

#need to extract date and user name
url_t <- url
parsed_data <- read_html("https://www.reddit.com/")

htime <- html_nodes(parsed_data, ".tagline")
htime
hovertime <- parsed_data %>%
  html_nodes(".tagline, .title") #%>%
  #html_text()
hovertime

all_parameters <- sapply(hovertime, xmlAttrs)

write.csv(hovertime, file ="hovertime.csv")

data_from_page<- parsed_data %>%
  html_nodes(".tagline") %>%
  #grep to grab time title
  #html_attr(".live-timestamp")
data_from_page



  data_from_page<- parsed_data %>%
  html_nodes(".md,.usertext-body") %>% 
  html_text()
data_from_page<- data_from_page[-c(1,2)]

#figure out how to remove duplicate entries
