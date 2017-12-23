library(rvest)
library(stringr)
library(tidyr)
library(RCurl)
library(XML)
library(dplyr)
library(plyr)
library(xml2)

#Access URL 

#Allows saving
ow.links <- getURL("https://us.battle.net/forums/en/overwatch/22813879/")

#Useful for scraping
ow.links2 <- read_html("https://us.battle.net/forums/en/overwatch/22813879/")
ow.links3 <- read_html("https://us.battle.net/forums/en/overwatch/22813879/?page=2")

#write file
#write.csv(url.request2, file = "OW_request.csv")
#write.csv(url.request.ow2, file = "OW_request2.csv")
#write.csv(url.request.ow, file = "TESST.csv")
write.csv(ow.links, file = "single.csv")

#scrapes links
links <- ow.links3 %>% html_nodes("a") %>% html_attr("href")
links
write.csv(links, file = "links2.csv")

#Makes links scrape a data frame
dflinks2 <- as.data.frame(links, row.names = NULL)

#test file write
write.csv(dflinks2, file = "dflinks2.csv")


#Find the links for the forums (since links are /forums)
keep<- grepl("^/forums", dflinks2$links)
keep <- as.data.frame(keep, row.names = NULL)
keep

#bind the answers to the new dataframe
dflinks3 <- cbind(dflinks2, keep)
dflinks3
str(dflinks3)
write.csv(dflinks3, file = "dflinks3_keep.csv")

#remove the rows of unneeded data

dfnew <- dflinks3[dflinks3$keep == "TRUE",]
dfnew 
write.csv(dfnew, file = "dflinks4_keep.csv")
