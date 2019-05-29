library('rvest')
library('tidyverse')
library('xmltools')
library('tidyr')
library('XML')

links = c("https://economictimes.indiatimes.com/",
"https://www.livemint.com/",
"https://www.business-standard.com/",
"https://www.bloombergquint.com/",
"https://www.thehindubusinessline.com/")

weblink <- links[1]

webpage <- read_html(weblink)  
results_html <- html_nodes(webpage, '.clearfix')
leadstory <- html_nodes(results_html, '.leadStoryLt')

lead_story = html_children(leadstory)[which(html_name(html_children(leadstory)) != 'div' | html_name(html_children(leadstory)) != 'time')]
headlines = do.call(rbind, html_text(lead_story))

extract_url <- function(x){
  url = unlist(strsplit(as.character(x), " "))[grepl("href=",unlist(strsplit(as.character(x), " ")))]
  return(unlist(strsplit(url, "\""))[2])
}

lead_stories <- as.data.frame(do.call(rbind, lapply(lead_story, extract_url)))
lead_stories$Headlines <- as.matrix(headlines)

lead_stories <- lead_stories[which(lead_stories$Headlines != ""),]

#Top Stories Bar
topstories <- html_nodes(results_html, '.leadStoryRt')
topstories_layer <- html_children(topstories)[length(html_children(topstories))]
topnewsbar <- html_nodes(topstories_layer, '.active')[2]
links <- lapply(html_nodes(topnewsbar, 'li'), extract_url)


topnewsbar <- html_text(html_nodes(topnewsbar, 'li'))
#topnewsbar <- topnewsbar[which(!grepl("[^\x01-\x7F]+", topnewsbar))]
exclude = c(which(lapply(links, is.null) == TRUE), which(grepl("[^\x01-\x7F]+", topnewsbar)))
topnews <- as.data.frame(do.call(rbind, links[-exclude]))
topnews$Headlines <- topnewsbar[-exclude]


library(openxlsx)
library(tidyRSS)

read_RSS <- function(link){
  return(data.frame(Title = tidyfeed(link)$item_title, 
                    Date = tidyfeed(link)$item_date_published, 
                    Link = tidyfeed(link)$item_link))
}

links <- list(
  #Economic Times India  
  c("https://economictimes.indiatimes.com/markets/rssfeeds/1977021501.cms", 
    'https://economictimes.indiatimes.com/rssfeedstopstories.cms',
    "https://economictimes.indiatimes.com/news/economy/rssfeeds/1373380680.cms",
    "https://economictimes.indiatimes.com/news/india-unlimited/rssfeeds/45228216.cms"), 
  #Livemint
  c("https://www.livemint.com/rss/economy_politics",
    "https://www.livemint.com/rss/companies",
    "https://www.livemint.com/rss/money",
    "https://www.livemint.com/rss/industry"),
  #Business Standard
  c("https://www.business-standard.com/rss/markets-106.rss",
    "https://www.business-standard.com/rss/economy-policy-102.rss",
    "https://www.business-standard.com/rss/finance-103.rss",
    "https://www.business-standard.com/rss/opinion-105.rss",
    "https://www.business-standard.com/rss/budget2018-110.rss",
    "https://www.business-standard.com/rss/news-cm-151.rss"),
  #Bloomberg Quint
  c("https://www.bloombergquint.com/stories.rss"),
  #Hindu Business Line
  c("https://www.thehindubusinessline.com/markets/feeder/default.rss",
    "https://www.thehindubusinessline.com/news/feeder/default.rss",
    "https://www.thehindubusinessline.com/economy/feeder/default.rss"))

names(links) <- c("Economic Times India", "Livemint", "Business Standard", 
                  "Bloomberg Quint", "Hindu Business Line")

write_data <- function(name){
  results <- as.data.frame(do.call(rbind, lapply(links[[name]], read_RSS)))
  results <- results[!duplicated(results$Link),]
  results <- results[rev(order(results$Date)),]
  results <- results[c('Date', 'Title', 'Link')]
  writeData(wb, name, results)
}

wb <- createWorkbook()
for (x in names(links)){
  addWorksheet(wb, x)
  write_data(x)
}
saveWorkbook(wb, "IndianNews.xlsx", overwrite = TRUE)
