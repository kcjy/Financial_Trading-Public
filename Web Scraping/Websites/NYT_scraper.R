#install.packages('rvest')
#install.packages('tidyverse')
library('rvest')
library('tidyverse')
library('xmltools')
library('tidyr')

#WITHOUT STATE in result csv

#Specifying the url for desired website
url <- 'https://www.nytimes.com/elections/results'

#States"District-of-Columbia" = 0
States <- list("Alabama" = 'AL', "Alaska" = 'AK', "Arizona" = 'AZ', "Arkansas" = 'AR',
               "California" = 'CA', "Colorado" = 'CO', "Connecticut" = 'CT', "Delaware" = 'DE',
               "Florida" = 'FL', "Georgia" = 'GA', "Hawaii" = 'HI',
               "Idaho" = 'ID', "Illinois" = 'IL', "Indiana" = 'IN', "Iowa" = 'IA',
               "Kansas" = 'LA', "Kentucky" = 'KY', "Louisiana" = 'LA', "Maine" = 'ME',
               "Maryland" = 'MD', "Massachusetts" = 'MA', "Michigan" = 'MI', "Minnesota" = 'MN',
               "Mississippi" = 'MS', "Missouri" = 'MO', "Montana" = 'MT', "Nebraska" = 'NE',
               "Nevada" = 'NV', "New-Hampshire" = 'NH', "New-Jersey" = 'NJ', "New-Mexico" = 'NM',
               "New-York" = 'NY', "North-Carolina" = 'NC', "North-Dakota" = 'ND', "Ohio" = 'OH',
               "Oklahoma" = 'OK', "Oregon" = 'OR', "Pennsylvania" = 'PA', "Rhode-Island" = 'RI',
               "South-Carolina" = 'SC', "South-Dakota" = 'SD', "Tennessee" = 'TN', "Texas" = 'TX',
               "Utah" = 'UT', "Vermont" = 'VT', "Virginia" = 'VA', "Washington" = 'WA',
               "West-Virginia" = 'WV', "Wisconsin" = 'WI', "Wyoming" = 'WY')

at_large =c('Alaska','Delaware','Montana','North-Dakota','South-Dakota','Vermont','Wyoming')
rest_of_states <- States[!(names(States) %in% at_large)]


#Reading the HTML code from the website
precinct_accuracy <- function(page){
  accuracy_html <- html_nodes(page, '.g-precinct-count')
  #accuracy_html <- html_table(accuracy_html, fill=TRUE) %>% as.data.frame()
  #accuracy <- unlist(strsplit(html_text(accuracy_html),","))
  accuracy <- str_replace_all(html_text(accuracy_html), "[^[:alnum:]]", "")
  #as.numeric(gsub(".*?([0-9]+).*", "\\1",accuracy))
  accuracy <- as.numeric(unlist(regmatches(accuracy, gregexpr("[0-9]+", accuracy))))[1]/
    as.numeric(unlist(regmatches(accuracy, gregexpr("[0-9]+", accuracy))))[2]
  return(accuracy)
}

scrape_sen_table <- function(x){
  state <- as.character(tolower(x))
  weblink <- paste(c(url,state),collapse='/')
  webpage <- read_html(weblink)   
  
  results_html <- html_nodes(webpage, '.eln-senate')
  if (length(results_html) == 0) {
    stop('No Senator Election Found')
  }
  
  senators <- html_nodes(results_html, '.eln-results-table')
  rep_acc <- precinct_accuracy(results_html)
  senators <- html_table(senators, fill=TRUE) %>% as.data.frame()
  cand_names <- lapply(senators$Candidate.1, function(y){unique(unlist(strsplit(y, 
                                                                  split="[* \n]"))[1])})
  party <- lapply(senators$Party, function(y){unique(unlist(strsplit(y, 
                                                            split="[.\n]"))[1])})
  #names(party) <- cand_names
  results_html <- html_nodes(results_html, '.eln-county-table')
  senator_table <- na.omit(html_table(results_html, fill=TRUE) %>% as.data.frame())
  senator_table[,1] <- sapply(senator_table[,1], 
                              function(z) paste(c(as.character(States[x]), z), collapse='_'))
  #Formatting Table
  names(senator_table) <- c('County', 'Votes_A', 'Votes_B')
  senator_table$Candidate_A <- as.character(cand_names[1])
  senator_table$Candidate_B <- as.character(cand_names[2])
  senator_table$Party_A <- as.character(party[1])
  senator_table$Party_B <- as.character(party[2])
  senator_table$Reporting <- rep_acc
  
  closeAllConnections()
  return(senator_table)
}

scrapeSen <- function (state) {
  return(tryCatch(scrape_sen_table(state), error=function(e) NULL))
}

#Results Table
senator <- as.data.frame(do.call(rbind, lapply(names(States), scrapeSen)))
senator$Index <- paste(senator$County, '_County', sep='')
senator$Votes_A <- as.numeric(gsub(",", "", senator$Votes_A))
senator$Votes_B <- as.numeric(gsub(",", "", senator$Votes_B))
senator$Voting_Pct_A <- round(senator$Votes_A/ (senator$Votes_A + senator$Votes_B),2)
senator$Voting_Pct_B <- round(senator$Votes_B/ (senator$Votes_A + senator$Votes_B),2)
senator <- senator[c('Index', 'County', 'Candidate_A', 'Party_A', 'Votes_A', 'Voting_Pct_A', 
                     'Candidate_B', 'Party_B', 'Votes_B', 'Voting_Pct_B', 'Reporting')]
senator$Party_A <- sapply(senator$Party_A, gsub, pattern = c("Republican",'Rep.'), replacement = "R", fixed = TRUE)
senator$Party_A <- sapply(senator$Party_A, gsub, pattern = c("Democrat",'Dem.'), replacement = "D", fixed = TRUE)
senator$Party_B <- sapply(senator$Party_B, gsub, pattern = c("Republican",'Rep.'), replacement = "R", fixed = TRUE)
senator$Party_B <- sapply(senator$Party_B, gsub, pattern = c("Democrat",'Dem.'), replacement = "D", fixed = TRUE)

write_csv(senator, 'senator_race.csv')

#Results Table for At-Large Districts
at_large_tables <- function(x){
  state <- as.character(tolower(x))
  weblink <- paste(c(url,state),collapse='/')
  webpage <- read_html(weblink) 
  results_html <- html_nodes(webpage, '.eln-house.eln-has-map')
  results_html <- Filter(function(x) !any(grepl("eln-runoff-group", x)), results_html)
  if (length(results_html) == 0) {
    stop('No House Election Found')
  }
  results_html <- html_nodes(results_html, '.eln-results-table')
  house_table <- html_table(results_html, fill=TRUE) %>% as.data.frame()
  
  State_Code <- as.character(States[x])
  Candidate_A <- unlist(strsplit(house_table$Candidate[1], split="[ \n]"))[1]
  Candidate_B <- unlist(strsplit(house_table$Candidate[2], split="[ \n]"))[1]
  Votes_A <- house_table$Votes[1]
  Votes_B <- house_table$Votes[2]
  Party_A <- unlist(strsplit(house_table$Party[1], split="[ \n]"))[1]
  Party_B <- unlist(strsplit(house_table$Party[2], split="[ \n]"))[1]
  Party_A <- gsub(Party_A, pattern = c("Democrat"), replacement = "D", fixed = TRUE)
  Party_A <- gsub(Party_A, pattern = c('Republican'), replacement = "R", fixed = TRUE)
  Party_B <- gsub(Party_B, pattern = c("Democrat"), replacement = "D", fixed = TRUE)
  Party_B <- gsub(Party_B, pattern = c('Republican'), replacement = "R", fixed = TRUE)

  Voting_Pct_A <- house_table$Pct.[1]
  Voting_Pct_B <- house_table$Pct.[2]
  Voting_Pct_A <- as.numeric(gsub("([0-9]+).*$", "\\1", Voting_Pct_A))/100
  Voting_Pct_B <- as.numeric(gsub("([0-9]+).*$", "\\1", Voting_Pct_B))/100
                             
  
  table <- data.frame(cbind(State_Code, Candidate_A, Candidate_B, Votes_A, Votes_B, 
                        Party_A, Party_B, Voting_Pct_A, Voting_Pct_B))
  table$State <- x
  table$District_Number <- 0
  table$Reporting <- paste(precinct_accuracy(webpage)*100,"%",sep="")
  closeAllConnections()
  return(table)
}


#House Representatives Table
compiled_housetable <- function(x){
  state <- as.character(tolower(x))
  weblink <- paste(c(url,state),collapse='/')
  webpage <- read_html(weblink) 
  results_html <- html_nodes(webpage, '.eln-house.eln-has-map')
  results_html <- Filter(function(x) !any(grepl("eln-runoff-group", x)), results_html)
  if (length(results_html) == 0) {
    stop('No House Election Found')
  }
  results_html <- html_nodes(results_html, '.eln-group-table')
  house_table <- html_table(results_html, fill=TRUE) %>% as.data.frame()
  house_table[,1] <- x
  house_table$State_Code <- as.character(States[x])
  house_table$District_number <- 1:nrow(house_table)
  
  return(house_table)
  closeAllConnections()
}

#########################COUNTY LEVEL#######################################################
#Individual US House Race Website
scrapeHouselinks <- function(houseline,row_number){
  state_name <- as.character(tolower(houseline$District...........Dist.))
  tryCatch({
    lead_candidate <- unlist(strsplit(houseline$Leader, split="[%*\n]"))[2]
    lead_candidate <- str_replace_all(lead_candidate, "[^[:alnum:]]", "-")
    
    if (unlist(strsplit(houseline$Var.3, split="[%\n]"))[2] == 'Uncontested'||
        grepl("Dem|Rep", houseline$Var.3) == FALSE){
      weblink <- paste(c(url, state_name),collapse='/')
      house_url <- c(weblink, 'house-district', as.character(row_number))
    } else {
      lag_candidate <- unlist(strsplit(houseline$Var.3, split="[%\n]"))[2]
      lag_candidate <- str_replace_all(lag_candidate, "[^[:alnum:]]", "")
      weblink <- paste(c(url, state_name),collapse='/')
      house_url <- c(weblink, 'house-district', as.character(row_number),  
                     gsub(" ", "-", tolower(lead_candidate)), gsub(" ", "-", tolower(lag_candidate)))
    }
    
    weblink <- paste(c(house_url),collapse='-')
    webpage <- read_html(weblink)
    by_county <- tryCatch({read_links(webpage, row_number, 'county')},error =function(e) NULL)
    by_district <- tryCatch({read_links(webpage, row_number, 'results')},error =function(e) NULL)
    return(list(by_county, by_district))
    
  }, error = function(http_error) {
    lead_candidate <- unlist(strsplit(houseline$Leader, split="[%*\n]"))[2]
    lead_candidate <- str_replace_all(lead_candidate, "[^[:alnum:]]", "")
    lag_candidate <- unlist(strsplit(houseline$Var.3, split="[%\n]"))[2]
    lag_candidate <- str_replace_all(lag_candidate, "[^[:alnum:]]", "")
    
    weblink <- paste(c(url, state_name),collapse='/')
    house_url <- c(weblink, 'house-district', as.character(row_number),  
                   gsub(" ", "-", tolower(lag_candidate)), gsub(" ", "-", tolower(lead_candidate)))
    weblink <- paste(c(house_url),collapse='-')
    webpage <- read_html(weblink)
    by_county <- tryCatch({read_links(webpage, row_number, 'county')},error =function(e) NULL)
    by_district <- tryCatch({read_links(webpage, row_number, 'results')},error =function(e) NULL)
    return(list(by_county, by_district))  
  })
}

read_links <- function(webpage, row_num, key){
  if (key == 'county'){
  results_html <- html_nodes(webpage, '.eln-county-table')
  county_table <- html_table(results_html, fill=TRUE) %>% as.data.frame()
  county_table$District <- row_num
  names(county_table) <- c('County', 'Votes_A', 'Votes_B', 'District_Number')
  return(county_table)
  } else if (key == 'results')
    results_html <- html_nodes(webpage, '.eln-results-table')
    county_table <- html_table(results_html, fill=TRUE) %>% as.data.frame()
    county_table <- as.data.frame(t(county_table$Votes[1:2]))
    county_table$District <- row_num
    names(county_table) <- c('Votes_A', 'Votes_B','District_Number')
    return(county_table)
}

allhousetables <- function(table){
  table_list <- lapply(1:nrow(table), function(row) tryCatch(scrapeHouselinks(table[row,], row), 
                       error = function(e) NULL))
  closeAllConnections()
  return(table_list)
}

scrape_housetable <- function(x){
  state <- as.character(tolower(x))
  weblink <- paste(c(url,state),collapse='/')
  webpage <- read_html(weblink) 
  results_html <- html_nodes(webpage, '.eln-house.eln-has-map')
  results_html <- Filter(function(x) !any(grepl("eln-runoff-group", x)), results_html)
  results_html <- html_nodes(results_html, '.eln-group-table')
  if (length(results_html) == 0) {
    stop(NULL)
  } else {
    house_table <- html_table(results_html, fill=TRUE) %>% as.data.frame()
    house_table[,1] <- x
    link_list <- allhousetables(house_table)
    county <- do.call(rbind,lapply(1:length(link_list), function(x) return(link_list[[x]][[1]])))
    votes <- do.call(rbind,lapply(1:length(link_list), function(x) return(link_list[[x]][[2]])))
    return(list(county, votes))
    }
  }

scrapeHouse <- function (state){
  tryCatch({
    data <- scrape_housetable(state)
    county <- as.data.frame(data[1][[1]])       #reduce(data[1][[1]], rbind))
    votes <- as.data.frame(data[[2]])                    #reduce(data[2][[1]], rbind))
    county$State <- state
    votes$State <- state
    county$State_Code <- as.character(States[state])
    votes$State_Code <- as.character(States[state])
    closeAllConnections()
    data <- list(county, votes)
  }, error=function(e){
    data = NULL
    closeAllConnections()
  })
  return(data)
}

###########Running the scrapeHouse function###########################################
compiledhouse <- as.data.frame(do.call(rbind, lapply(names(rest_of_states), 
                                                     function(x) tryCatch(compiled_housetable(x), 
                                                                          error = function(e) NULL))))
compiledhouse <- compiledhouse[,colSums(is.na(compiledhouse))<nrow(compiledhouse)]
names(compiledhouse) <- c('State', 'Leader', 'Second', 'Reporting', 'State_Code', 'District_Number')

cand_format <- function(candidate, key){
  if (key == 'percentage'){
    percentage <- as.numeric(gsub("([0-9]+).*$", "\\1", candidate))
    return(percentage/100)
  } else if (key == 'candidate'){
    w <- gsub('[0-9]+', '', candidate)
    str_list <- unique(unlist(strsplit(w, split="[*%\n]")))[unique(unlist(strsplit(w, split="[*%\n]"))) != ""]
    return(str_list[1])
  } else if (key == 'party'){
    w <- gsub('[0-9]+', '', candidate)
    str_list <- unique(unlist(strsplit(w, split="[*%\n]")))[unique(unlist(strsplit(w, split="[*%\n]"))) != ""]
    party <- str_list[2]
    party <- gsub(party, pattern = c("      Dem."), replacement = "D", fixed = TRUE)
    party <- gsub(party, pattern = c('      Rep.'), replacement = "R", fixed = TRUE)
    return(party)
  }
}

compiledhouse$Candidate_A <- sapply(compiledhouse$Leader, function(x) cand_format(x, 'candidate'))
compiledhouse$Voting_Pct_A <- sapply(compiledhouse$Leader, function(x) cand_format(x, 'percentage'))
compiledhouse$Party_A <- sapply(compiledhouse$Leader, function(x) cand_format(x, 'party'))
compiledhouse$Candidate_B <- sapply(compiledhouse$Second, function(x) cand_format(x, 'candidate'))
compiledhouse$Voting_Pct_B <- sapply(compiledhouse$Second, function(x) cand_format(x, 'percentage'))
compiledhouse$Party_B <- sapply(compiledhouse$Second, function(x) cand_format(x, 'party'))

compiledhouse$Leader <- NULL
compiledhouse$Second <- NULL

link_values <- lapply(names(rest_of_states), function(x) scrapeHouse(x))
district <- do.call(rbind, lapply(1:length(link_values), function(x) return(link_values[[x]][[1]])))
county <- do.call(rbind, lapply(1:length(link_values), function(x) return(link_values[[x]][[2]])))
atlarge_scrape <- do.call(rbind, lapply(at_large, function(x) at_large_tables(x)))

compiledhouse <- merge(compiledhouse,county,by=c("State", "State_Code", "District_Number"),all = TRUE)
compiledhouse <- merge(compiledhouse, atlarge_scrape, by = colnames(atlarge_scrape),all= TRUE)
compiledhouse$Index <- paste(compiledhouse$State_Code, compiledhouse$District_Number, sep='_')
compiledhouse <- compiledhouse[c('Index','State', 'State_Code', 'District_Number', 'Candidate_A', 'Votes_A', 'Voting_Pct_A',
                                 'Party_A', 'Candidate_B', 'Voting_Pct_B', 'Votes_B', 'Party_B', 'Reporting')]

write_csv(compiledhouse, 'house-race.csv')
#############################################################################
district$Index <- paste(district$State_Code, district$District_Number, sep='_')
district <- district[c('Index', 'State', 'State_Code', 'District_Number', 'County', 'Votes_A', 'Votes_B')]
write_csv(district, 'county-results.csv')
