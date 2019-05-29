library(openxlsx)
library(tidyRSS)
library(RDCOMClient)
library(kableExtra)
library(dplyr)

options(warn=-1)

read_RSS <- function(link){
  return(data.frame(Title = tidyfeed(link)$item_title, 
                    Date = tidyfeed(link)$item_date_published, 
                    Link = tidyfeed(link)$item_link))
}

links <- list(
  #Economic Times India  
  c("http://feeds.reuters.com/reuters/topNews", 
    'http://feeds.reuters.com/Reuters/PoliticsNews'), 
  #Livemint
  c("https://api.axios.com/feed/"))

names(links) <- c("Reuters", "Axios")

write_data <- function(name){
  results <- as.data.frame(do.call(rbind, lapply(links[[name]], read_RSS)))
  results <- results[!duplicated(results$Link),]
  results <- results[rev(order(results$Date)),]
  results$Date <- format(results$Date, "%d-%m-%Y %H:%M")
  results <- results[c('Date', 'Title', 'Link')]
  results <- results[unique(results$Title),]
  
  results$Title <- paste0("<a href='",results$Link,"'>",results$Title,"</a>")
  results$Link <- NULL
  row.names(results) <- as.character(1:nrow(results))
  
  return(as.data.frame(results))
}

for (x in names(links)){
  y <- knitr::kable(write_data(x), format="html", booktabs = T, escape = F, full_width = F, linesep = c('','','\\hline')) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    column_spec(c(2), border_left = TRUE) %>%
    column_spec(1, border_left = TRUE) %>%
    column_spec(2, border_right = TRUE) %>%
    row_spec(0, background = "#e0e0eb") %>%
    row_spec(dim(write_data(x))[1], background = "#e0e0eb")
  
  body <- paste0("<html>",
                 y,
                 "</html>")
  
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  #outMail[["To"]] = "myself@gmail.com"
  outMail[["Cc"]] = "myself@gmail.com"
  outMail[["subject"]] = paste0(x, " News Update ", as.character(format(Sys.time(),"%H:%M %p")), ' SGT')
  outMail[["HTMLbody"]] = body
  
  outMail$Send()
}
