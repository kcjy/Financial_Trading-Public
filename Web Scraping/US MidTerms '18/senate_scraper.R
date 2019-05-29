rm(list = ls())

library('rvest')
library('tidyverse')
library(xmltools)
library('tidyr')

#Specifying the url for desired website
#url <- 'https://www.nytimes.com/elections/results'
url <- 'https://www.nytimes.com/interactive/2018/11/06/us/elections/results'


#States"District-of-Columbia" = 0
States <- list("Alabama" = 'AL', "Alaska" = 'AK', "Arizona" = 'AZ', "Arkansas" = 'AR',
               "California" = 'CA', "Colorado" = 'CO', "Connecticut" = 'CT', "Delaware" = 'DE',
               "Florida" = 'FL', "Georgia" = 'GA', "Hawaii" = 'HI',
               "Idaho" = 'ID', "Illinois" = 'IL', "Indiana" = 'IN', "Iowa" = 'IA',
               "Kansas" = 'KS', "Kentucky" = 'KY', "Louisiana" = 'LA', "Maine" = 'ME',
               "Maryland" = 'MD', "Massachusetts" = 'MA', "Michigan" = 'MI', "Minnesota" = 'MN', 
               "Minnesota-(special)" = 'MN_S', "Mississippi-(special)" = 'MS_S',
               "Mississippi" = 'MS', "Missouri" = 'MO', "Montana" = 'MT', "Nebraska" = 'NE',
               "Nevada" = 'NV', "New-Hampshire" = 'NH', "New-Jersey" = 'NJ', "New-Mexico" = 'NM',
               "New-York" = 'NY', "North-Carolina" = 'NC', "North-Dakota" = 'ND', "Ohio" = 'OH',
               "Oklahoma" = 'OK', "Oregon" = 'OR', "Pennsylvania" = 'PA', "Rhode-Island" = 'RI',
               "South-Carolina" = 'SC', "South-Dakota" = 'SD', "Tennessee" = 'TN', "Texas" = 'TX',
               "Utah" = 'UT', "Vermont" = 'VT', "Virginia" = 'VA', "Washington" = 'WA',
               "West-Virginia" = 'WV', "Wisconsin" = 'WI', "Wyoming" = 'WY')

#Reading the HTML code from the website
precinct_accuracy <- function(page){
  accuracy_html <- html_nodes(page, '.g-precinct-count')
  accuracy <- str_replace_all(html_text(accuracy_html), "[^[:alnum:]]", "")
  accuracy <- as.numeric(unlist(regmatches(accuracy, gregexpr("[0-9]+", accuracy))))[1]/
    as.numeric(unlist(regmatches(accuracy, gregexpr("[0-9]+", accuracy))))[2]
  return(accuracy)
}

#Function to scrape the Senate table
scrape_sen_table <- function(x){
  if (x == "Minnesota-(special)"){
    state <- as.character(tolower("Minnesota"))
    weblink <- paste(c(url,state, 'elections.html'),collapse='-')
    webpage <- read_html(weblink)    
    
    results_html <- html_nodes(webpage, '.eln-senate')
    senators <- html_nodes(results_html, '.eln-results-table')[2]
  } else if (x == "Mississippi-(special)"){
    state <- as.character(tolower("Mississippi"))
    weblink <- paste(c(url,state, 'elections.html'),collapse='-')
    webpage <- read_html(weblink)  
    
    results_html <- html_nodes(webpage, '.eln-senate')
    senators <- html_nodes(results_html, '.eln-results-table')[2]
  } else {
    state <- as.character(tolower(x))
    weblink <- paste(c(url,state, 'elections.html'),collapse='-')
    webpage <- read_html(weblink)
    results_html <- html_nodes(webpage, '.eln-senate')
    if (length(results_html) == 0) {
      stop('No Senator Election Found')
    }
    senators <- html_nodes(results_html, '.eln-results-table')[1] #Main Table
  }
  
  #Reporting Accuracy
  #rep_acc <- precinct_accuracy(results_html)
  senators <- html_table(senators, fill=TRUE) %>% as.data.frame()
  cand_names <- lapply(senators$Candidate, function(y){unique(unlist(strsplit(y, 
                                                                                split=" "))[1])})
  cand_names <- lapply(cand_names, function(y){gsub("[^[:alnum:] ]", "", y)})

  party <- lapply(senators$Party, function(y){unique(unlist(strsplit(y, 
                                                                     split="[.\n]"))[1])})
  #party_index <- c('A','B')
  #names(party_index) = c(unlist(strsplit(party[[1]],split=" "))[1], unlist(strsplit(party[[2]],split=" "))[1])
  
  party <- lapply(party, function(x) {unique(unlist(strsplit(x, split=" "))[1])})
  names(party) <- cand_names
  #Remember to change this for 2018
  if (x == "Minnesota-(special)" | x == "Mississippi-(special)"){
    results_html <- html_nodes(results_html, '.eln-county-table')[2]
  } else {
    results_html <- html_nodes(results_html, '.eln-county-table')[1] #Main Table
  }
  senator_table <- na.omit(html_table(results_html, fill=TRUE) %>% as.data.frame())
  senator_table <- senator_table[1:(nrow(senator_table)-1),]
  senator_table[,1] <- sapply(senator_table[,1], 
                              function(z) paste(c(as.character(States[x]), z), collapse='_'))
  if (x == 'Vermont'){
    index <- c(party['Sanders'], party[2])
  } else {
    index <- party[which(party %in% c('Democrat', 'Republican'))]
  }
  senator_table <- tryCatch({senator_table[c(names(senator_table)[1], names(index), 
                                             names(senator_table)[dim(senator_table)[2]])]}, 
                            error=function(e) senator_table[,c(1:3,dim(senator_table)[2])])
  party_index <- c('A', 'B')
  names(party_index) <- c(as.character(data.frame(index)[,1]), as.character(data.frame(index)[,2]))
  
  
  names(senator_table) <- c('County', 'A', 'B', 'Reporting')
  #Formatting Table
  if (length(unique(index)) == 1){
    if (unlist(unique(index)) == 'Democrat'){ #If No Republicans exist
      senator_table$D_Votes <- as.numeric(gsub(',','',senator_table$A)) + as.numeric(gsub(',','',senator_table$B))
      senator_table$R_Votes <- 0
      senator_table$D_Pct <- 1
      senator_table$R_Pct <- 0
    } else if (unlist(unique(index)) == 'Republican'){ #If No Democrats Exist
      senator_table$R_Votes <- as.numeric(gsub(',','',senator_table$A)) + as.numeric(gsub(',','',senator_table$B))
      senator_table$D_Votes <- 0
      senator_table$R_Pct <- 1
      senator_table$D_Pct <- 0
    }
    
  } else if (x == 'Vermont'){
    senator_table$D_Votes <- as.numeric(gsub(',','',senator_table$A))
    senator_table$R_Votes <- as.numeric(gsub(',','',senator_table$B))
    senator_table$D_Pct <- as.numeric(gsub(',','',senator_table$D_Votes)) / (as.numeric(gsub(',','',senator_table$D_Votes)) + as.numeric(gsub(',','',senator_table$R_Votes)))
    senator_table$R_Pct <- as.numeric(gsub(',','',senator_table$R_Votes)) / (as.numeric(gsub(',','',senator_table$D_Votes)) + as.numeric(gsub(',','',senator_table$R_Votes)))
  } else {
    senator_table$D_Votes <- senator_table[,party_index['Democrat']]
    senator_table$R_Votes <- senator_table[,party_index['Republican']]
    senator_table$D_Pct <- as.numeric(gsub(',','',senator_table$D_Votes)) / (as.numeric(gsub(',','',senator_table$D_Votes)) + as.numeric(gsub(',','',senator_table$R_Votes)))
    senator_table$R_Pct <- as.numeric(gsub(',','',senator_table$R_Votes)) / (as.numeric(gsub(',','',senator_table$D_Votes)) + as.numeric(gsub(',','',senator_table$R_Votes)))
  }
  #senator_table$Reporting <- rep_acc
  senator_table$State <- as.character(States[x])
  senator_table[names(index)] <- NULL
  senator_table <- senator_table[c('State', 'County', 'D_Votes', 'R_Votes',
                                   'D_Pct', 'R_Pct', 'Reporting')]
  senator_table$Reporting[grepl('<', senator_table$Reporting)] = '0%'
  senator_table$Reporting[grepl('>', senator_table$Reporting)] = '100%'
  #senator_table$Reporting <- gsub('<', '', senator_table$Reporting)
  #senator_table$Reporting <- gsub('>', '', senator_table$Reporting)
  senator_table$Reporting[senator_table$Reporting == ""] = paste0(0,"%")
  closeAllConnections()
  message(paste(state,"done",Sys.time(),sep="-"))
  return(senator_table)
}

scrapeSen <- function (state) {
  return(tryCatch(scrape_sen_table(state), error=function(e) NULL))
}

#Results Table



senator <- as.data.frame(do.call(rbind, lapply(names(States), scrapeSen)))
senator[is.na(senator)] <- 0
senator[senator == 'NA'] <- 0
#Adjusting Reporting Pct
#baseline_pop <- read.csv('Population/senate_population.csv', row.names = 'County')
#if (length(row.names(baseline_pop[senator$County,1]) != length(senator$County))){
#  print("Error!")
#  print('Missing Counties from baseline population: ', senator$County[!which(senator$County %in% row.names(baseline_pop))])
#}

#senator$Reporting <- round((as.numeric(gsub(',', '', senator$D_Votes)) + as.numeric(gsub(',', '', senator$R_Votes))) / as.numeric(gsub(',','',baseline_pop[senator$County,1])),4)

write_csv(data.frame(senator), 'senator_race.csv')
rm(list = ls())
