library(openxlsx)
library(tidyRSS)
library(RDCOMClient)
library(kableExtra)
library(dplyr)

options(warn=-1)

################ NOTE ######################
# Blogs may not offer links to each individual post (Test 2)
##############################################

#Blogspot URLs
links <- list(
  #Test 1 
  'http://sgyounginvestment.blogspot.com/', 
  #Test 2
  "https://gregmankiw.blogspot.com/"
)

#Preferred Title/Name of Blog (Email Header)
names(links) <- c("SG Young Investment", "Gregman")



scrape_blogs <- function(website){
  weblink <- unlist(links[website])
  webpage <- read_html(weblink)   
  
  date <- html_nodes(webpage, '.date-header')
  header <- html_nodes(webpage, '.post-title')
  links <- tryCatch({html_nodes(webpage, '.post-title > a') %>% html_attr("href")}, error = function(e) NULL)
  
  #Date / Header
  date <- do.call(rbind, lapply(1:length(date), function(x) html_text(date[x])))
  header <- do.call(rbind, lapply(1:length(header), function(x) html_text(header[x])))
  
  #Cleaning String
  date <- gsub('\n', '', date)
  header <- gsub('\t', '', gsub('\n', '', header))
  
  result <- tryCatch({data.frame(Date = date, Title = header, Link = links)}, error = function(e) data.frame(Date = date, Title = header))
  
  if ('Link' %in% names(result)){
    result$Title <-paste0("<a href='",result$Link,"'>",result$Title,"</a>")
  }
  
  result$Link <- NULL
  row.names(result) <- as.character(1:nrow(result))
  
  return(as.data.frame(result))
}

for (x in names(links)){
  y <- knitr::kable(scrape_blogs(x), format="html", booktabs = T, escape = F, full_width = F, linesep = c('','','\\hline')) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    column_spec(c(2), border_left = TRUE) %>%
    column_spec(1, border_left = TRUE) %>%
    column_spec(2, border_right = TRUE) %>%
    row_spec(0, background = "#e0e0eb") %>%
    row_spec(dim(scrape_blogs(x))[1], background = "#e0e0eb")
  
  body <- paste0("<html>",
                 y,
                 "</html>")
  
  OutApp <- COMCreate("Outlook.Application")
  outMail = OutApp$CreateItem(0)
  outMail[["To"]] = "myself@gmail.com"
  #outMail[["Cc"]] = "myself@gmail.com"
  outMail[["subject"]] = paste0(x, " News Update ", as.character(format(Sys.time(),"%H:%M %p")), ' SGT')
  outMail[["HTMLbody"]] = body
  
  outMail$Send()
}
