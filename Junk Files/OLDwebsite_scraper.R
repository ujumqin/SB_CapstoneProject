library(rvest)
library(stringr)
library(tidyr)
library(RCurl)
library(XML)
library(dplyr)
library(plyr)
library(xml2)

#Access URL 
#url.request2 <- getURL('https://us.battle.net/forums/en/overwatch/22813879/', ssl.verifypeer = FALSE)
#url.request.ow2 <- getURL('https://us.battle.net/forums/en/overwatch/22813879/', ssl.verifypeer = FALSE)
#ow.single.output <- getURL("https://us.battle.net/forums/en/overwatch/topic/20759286760")
url.request.ow <- read_html("https://us.battle.net/forums/en/overwatch/22813879/")
#ow.single <- read_html("https://us.battle.net/forums/en/overwatch/topic/20759286760")

#write file
#write.csv(url.request2, file = "OW_request.csv")
#write.csv(url.request.ow2, file = "OW_request2.csv")
#write.csv(url.request.ow, file = "TESST.csv")
write.csv(ow.single.output, file = "single.csv")

#scrapes title + text preview from html code
title_results <- url.request.ow %>% html_nodes(".ForumTopic-title")
topic_results <- url.request.ow %>% html_nodes(".ForumTopic--preview")

#prints
title_results
topic_results

#stores title into first_result and topic into first_topic
first_result <- title_results
first_result
first_topic <- topic_results
first_topic

#extracts the title
title <- xml_contents(title_results) %>% html_text(trim = TRUE)
title


#extracts preview
topic <- xml_contents(topic_results)[1] %>% html_text(trim = TRUE)
topic

records <- vector("list", length = length(title_results))

for (i in seq_along(title_results)) {
  title <- xml_contents(title_results)[i] %>% html_text(trim = TRUE)
  topic <- xml_contents(topic_results)[i] %>% html_text(trim = TRUE)
  records[[i]] <- data_frame(title = title, topic = topic)
}

df <- bind_rows(records)
glimpse(df)
df
write.csv(df, file = "OWBoards.csv")

#how do I extract posts in thread? See below. Use ".TopicPost-bodyContent" make sure to use html_nodeS
single_result <- ow.single %>% html_nodes(".TopicPost-bodyContent") 
single_result

text <- xml_contents(single_result) %>% html_text(trim = TRUE)
text

#print(span_characters)
#results_text <- results %>% html_node() %>% html_text(trim = FALSE)
#results_text

xmlToDataFrame(results)

df_ow <- data.frame(results, results_topic)

results

#checking url.request
class(url.request)
print(url.request)

#write file to parse manually
write.csv(url.request, file = "OW_request.csv")




