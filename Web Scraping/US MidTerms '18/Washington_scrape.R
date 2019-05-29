library('rvest')
library('tidyverse')
library(xmltools)
library('tidyr')
library('htmlTable')
library(openxlsx)
library(RSelenium)
library(XML)
library(stringr)

setwd('US MidTerms '18/')

#States
States <- list("Alabama" = 'AL', "Alaska" = 'AK', "Arizona" = 'AZ', "Arkansas" = 'AR',
               "California" = 'CA', "Colorado" = 'CO', "Connecticut" = 'CT', "Delaware" = 'DE',
               "Florida" = 'FL', "Georgia" = 'GA', "Hawaii" = 'HI',
               "Idaho" = 'ID', "Illinois" = 'IL', "Indiana" = 'IN', "Iowa" = 'IA',
               "Kansas" = 'KS', "Kentucky" = 'KY', "Louisiana" = 'LA', "Maine" = 'ME',
               "Maryland" = 'MD', "Massachusetts" = 'MA', "Michigan" = 'MI', "Minnesota" = 'MN', #"Minnesota-(special)" = 'MN_S', "Mississippi-(special)" = 'MS_S',
               "Mississippi" = 'MS', "Missouri" = 'MO', "Montana" = 'MT', "Nebraska" = 'NE',
               "Nevada" = 'NV', "New-Hampshire" = 'NH', "New-Jersey" = 'NJ', "New-Mexico" = 'NM',
               "New-York" = 'NY', "North-Carolina" = 'NC', "North-Dakota" = 'ND', "Ohio" = 'OH',
               "Oklahoma" = 'OK', "Oregon" = 'OR', "Pennsylvania" = 'PA', "Rhode-Island" = 'RI',
               "South-Carolina" = 'SC', "South-Dakota" = 'SD', "Tennessee" = 'TN', "Texas" = 'TX',
               "Utah" = 'UT', "Vermont" = 'VT', "Virginia" = 'VA', "Washington" = 'WA',
               "West-Virginia" = 'WV', "Wisconsin" = 'WI', "Wyoming" = 'WY')

'%not in%' <- Negate('%in%')
url = 'https://www.washingtonpost.com/election-results/'

driver<- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]

senate <- function(index){
  print(index)
  weblink <- paste0(url, tolower(names(States)[index]))
  remDr$navigate(weblink)
  Sys.sleep(3)
  
  x <- remDr$findElements(using = 'css',  ".wpe-hsb")
  result <- unlist(strsplit(x[[1]]$getElementText()[[1]], split='\n'))
  reporting <- tryCatch({gsub("[^0-9]", "", result[1])},error=function(e) result[1]) 
  reporting <- tryCatch({str_replace_all(reporting, "[[:punct:]]", "")/100}, error = function(e) reporting)
  
  string_list <- lapply(result, function(x) unlist(strsplit(x, split=' ')))
  
  votes <- do.call(rbind, lapply(3:length(result)-1, function(x) unlist(strsplit(result[x], split=' '))[c(3,5,6)]))
  votes[,1] <- gsub('[()]','',votes[,1])
  votes <- votes[which(!is.na(votes[,2])),]
  votes <- votes[grepl('^[A-Za-z]+$', votes[,1]),]
  votes <- votes[which(votes[,1] %in% c('D', 'R')),]
  
  if ("D" %not in% votes){
    votes <- rbind(votes, c('D', 'Nil', '0'))
    votes[1,3] <- "100%"
  } else if ("R" %not in% votes) {
    votes <- rbind(votes, c('R', 'Nil', '0'))
    votes[1,3] <- "100%"
  }
  
  output <- c(votes[which(votes[,1] == 'D'),c(2,3)], votes[which(votes[,1] == 'R'),c(2,3)])
  output <- as.numeric(str_replace_all(output, "[[:punct:]]", ""))/100
  output <- c(States[index][[1]], output, reporting)
  return(output[,1:6])
}

scr_house <- function(index){
  print(index)
  weblink <- paste0(url, tolower(names(States)[index]))
  remDr$navigate(weblink)
  Sys.sleep(5)
  #tryCatch({senate(index)}, error=function(e) NULL)
  
  x <- remDr$findElements(using = 'css',  ".tiling-results-wrapper")
  tiling_result <- x[[1]]$findChildElements(using = 'css', ".tiling-result")
  end <- tail(x[[1]]$findChildElements(using = 'css', ".border-bottom-off"),1)
  
  scrape_table <- function(i, result){
    node <- result[[i]]$findChildElements(using='css', ".wpe-result")[[1]]$getElementText()
    string_list <- strsplit(node[[1]], split="[\n]")[[1]]
    
    votes <- do.call(rbind, lapply(2:length(string_list), function(x) tail(unlist(strsplit(string_list[x], split=' ')),3)))
    votes[,1] <- gsub('[()]','',votes[,1])
    if ("D" %not in% votes){
      votes <- rbind(votes, c('D', 'Nil', '0'))
      votes[1,3] <- "100%"
    } else if ("R" %not in% votes) {
      votes <- rbind(votes, c('R', 'Nil', '0'))
      votes[1,3] <- "100%"
    }
    
    votes <- votes[which(votes[,1] %in% c('D', 'R')),]
    result <- c(votes[which(votes[,1] == 'D'),3], votes[which(votes[,1] == 'R'),3])
    result <- as.numeric(str_replace_all(result, "[[:punct:]]", ""))/100
    y <- tryCatch({result[[i]]$findChildElements(using='css', ".wpe-progress")[[1]]$getElementText()},
                  error = function(e) 0)
    reporting <- tryCatch({gsub("[^0-9]", "", y[[i]]$getElementText()[[1]])},error=function(e) y) 
    reporting <- tryCatch({str_replace_all(reporting, "[[:punct:]]", "")/100}, error = function(e) reporting)
    result <- c(States[index][[1]], paste(States[index][[1]], i, sep='_'), result, reporting)
    return(result)
  }
  
  if (length(tiling_result) == 0){
    bottom <- scrape_table(1, end)
    district <- as.data.frame(t(bottom))
    district[,2] <- paste(States[index][[1]], 0, sep='_')
    print(district)
    colnames(district) = c('State', 'District', 'D_Votes', 'R_Votes', 'Reporting')
    #row.names(district) = 1
  } else {
    district <- do.call(rbind, lapply(1:length(tiling_result), function(x) tryCatch({scrape_table(x, tiling_result)},
                                                                                    error=function(e) 0)))
    print(district)
    bottom <- scrape_table(1, end)
    bottom[2] <- paste(States[index][[1]], (nrow(district)+1), sep= "_")
    
    district <- as.data.frame(rbind(district, bottom), row.names=F)
    print(district)
    colnames(district) = c('State', 'District', 'D_Votes', 'R_Votes', 'Reporting')
  }
  return(district[,1:5])
}



house <- as.data.frame(do.call(rbind, lapply(1:length(States), function(x) scr_house(x))))
house[is.na(house)] <- 0

write.csv(house, 'washington_house.csv')
driver[["server"]]$stop()

#driver<- rsDriver(browser=c("chrome"))
#remDr <- driver[["client"]]
#senate_data <- as.data.frame(do.call(rbind, lapply(1:length(States), function(x) tryCatch({senate(x)}, error = function(e) NULL))))
#colnames(senate_data) <- c('State', 'D_Votes', 'R_Votes', 'D_Pct', 'R_Pct', 'Reporting')

#write.csv(house, 'washington_senate.csv')



