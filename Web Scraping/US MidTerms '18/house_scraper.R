rm(list = ls())

library('rvest')
library('tidyverse')
library('xmltools')
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
               "Mississippi" = 'MS', "Missouri" = 'MO', "Montana" = 'MT', "Nebraska" = 'NE',
               "Nevada" = 'NV', "New-Hampshire" = 'NH', "New-Jersey" = 'NJ', "New-Mexico" = 'NM',
               "New-York" = 'NY', "North-Carolina" = 'NC', "North-Dakota" = 'ND', "Ohio" = 'OH',
               "Oklahoma" = 'OK', "Oregon" = 'OR', "Pennsylvania" = 'PA', "Rhode-Island" = 'RI',
               "South-Carolina" = 'SC', "South-Dakota" = 'SD', "Tennessee" = 'TN', "Texas" = 'TX',
               "Utah" = 'UT', "Vermont" = 'VT', "Virginia" = 'VA', "Washington" = 'WA',
               "West-Virginia" = 'WV', "Wisconsin" = 'WI', "Wyoming" = 'WY')

atlarge <- States[c('Alaska', 'Delaware', 'Montana', 'North-Dakota', 'South-Dakota', 'Vermont', 'Wyoming')] 
States <- States[!States %in% atlarge]
names(States) = tolower(names(States))

#Reading the HTML code from the website
precinct_accuracy <- function(page){
  accuracy_html <- html_nodes(page, '.g-precinct-count')
  accuracy <- str_replace_all(html_text(accuracy_html), "[^[:alnum:]]", "")
  accuracy <- as.numeric(unlist(regmatches(accuracy, gregexpr("[0-9]+", accuracy))))[1]/
    as.numeric(unlist(regmatches(accuracy, gregexpr("[0-9]+", accuracy))))[2]
  return(accuracy)
}

cand_format <- function(candidate, key){
  if (key == 'percentage'){
    percentage <- as.numeric(gsub("([0-9]+).*$", "\\1", candidate))/100
    return(percentage)
  } else if (key == 'candidate'){
    w <- gsub('[0-9]+', '', candidate)
    str_list <- unique(unlist(strsplit(w, split="[*%\n]")))[unique(unlist(strsplit(w, split="[*%\n]"))) != ""]
    return(str_list[1])
  } else if (key == 'party'){
    w <- gsub('[0-9]+', '', candidate)
    str_list <- unique(unlist(strsplit(w, split="[*%\n]")))[unique(unlist(strsplit(w, split="[*%\n]"))) != ""]
    return(str_list[2])
  } else if (key == 'Defective'){
    candidate <- paste(tail(strsplit(candidate,"")[[1]],4), collapse="")
    return(candidate)
  }
}

#House Representatives Table
compiled_housetable <- function(x){
  state <- as.character(tolower(x))
  weblink <- paste(c(url,state, 'elections.html'),collapse='-')
  webpage <- read_html(weblink) 
  results_html <- html_nodes(webpage, '.eln-house.eln-has-map')
  results_html <- Filter(function(x) !any(grepl("eln-runoff-group", x)), results_html)
  if (length(results_html) == 0) {
    stop('No House Election Found')
  }
  results_html <- html_nodes(results_html, '.eln-group-table')
  house_table <- html_table(results_html, fill=TRUE) %>% as.data.frame()
  
  house_table <- house_table[!is.na(as.numeric(house_table[,1])),]
  house_table$District_number <- as.numeric(house_table$District.Dist.)
  reporting <- house_table$Rpt.
  reporting[grepl('<', reporting)] = '0%'
  reporting[grepl('>', reporting)] = '100%'
  
  house_table[,1] <- as.character(States[x])
  
  house_table$Party_A <- sapply(house_table$Candidate, function(x) tail(unique(unlist(strsplit(x, split="[*% \n]"))),1))
  house_table$Party_A <- sapply(house_table$Party_A , function(x) tail(unique(unlist(strsplit(x, split=" "))),1))
  house_table$Party_A <- sapply(house_table$Party_A, function(x) cand_format(x, 'Defective'))
  
  house_table$A <- sapply(1:length(house_table$Candidate), 
                          function(x) tryCatch({cand_format(house_table$Candidate[x], 'percentage')},
                                                                            warning = function(w) 100))
  
  house_table$Party_B <- sapply(house_table$Var.3, function(x) tail(unique(unlist(strsplit(x, split="[*% \n]"))),1))
  house_table$Party_B <- sapply(house_table$Var.3, function(x) cand_format(x, 'Defective'))
  house_table$Party_B[which(house_table$Party_B == 'sted')] <- 'Uncontested'
  house_table$B <- sapply(1:length(house_table$Var.3), function(x) tryCatch({cand_format(house_table$Var.3[x], 'percentage')},
                            warning = function(w) 100))
  
  house_table[c('Candidate', 'Var.3', 'NA.', 'NA..1', 'Rpt.')] <- NULL
  house_table$A <- sapply(1:nrow(house_table), 
                          function(x) if (house_table[x,]$Party_B == 'Uncontested'){return(1)} else {return(house_table[x,]$A)})
  house_table$D_Votes <- sapply(1:nrow(house_table), 
                          function(x) if (house_table[x,]$Party_A == 'Dem.'){return(house_table[x,]$A)} 
                          else if (house_table[x,]$Party_B == 'Dem.'){return(house_table[x,]$B)}
                          else {return(0)})
  house_table$R_Votes <- sapply(1:nrow(house_table), 
                                function(x) if (house_table[x,]$Party_A == 'Rep.'){return(house_table[x,]$A)} 
                                else if (house_table[x,]$Party_B == 'Rep.'){return(house_table[x,]$B)}
                                else {return(0)})
  house_table[c('A', 'B', 'Party_A', 'Party_B')] <- NULL
  house_table[,1] <- States[tolower(x)]
  house_table$Reporting <- reporting
  house_table$Reporting <- gsub('<', '', house_table$Reporting)
  house_table$Reporting <- gsub('>', '', house_table$Reporting)
  house_table$Reporting[house_table$Reporting == ""] = paste0(0,"%")
  colnames(house_table) <- c('State', 'District', 'D_Votes', 'R_Votes', 'Reporting')
  house_table <- house_table[order(house_table$District),]
  message(paste(state,"done",Sys.time(),sep="-"))
  return(house_table)
  closeAllConnections()
}
#############################AT LARGE############################
scrape_atlarge <- function(x){
  state <- as.character(tolower(x))
  weblink <- paste(c(url,state, 'elections.html'),collapse='-')
  webpage <- read_html(weblink) 
  results_html <- html_nodes(webpage, '.eln-house')
  reporting <- paste0(round(precinct_accuracy(webpage),2)*100,"%")
  results_html <- html_nodes(results_html, '.eln-results-table')
  if (length(results_html) == 0) {
    stop(NULL)
  } else {
    house_table <- html_table(results_html, fill=TRUE) %>% as.data.frame()
    house_table <- house_table[1:2,]
    index <- c(house_table$Pct.[1], house_table$Pct.[2])
    names(index) <- c(tail(unique(unlist(strsplit(house_table$Party[1], split="[*% \n]"))),1), 
                      tail(unique(unlist(strsplit(house_table$Party[2], split="[*% \n]"))),1))
    D_votes <- cand_format(index['Dem.'],'percentage')
    R_votes <- cand_format(index['Rep.'],'percentage')
    if (is.na(R_votes)){R_votes <- 0}
    if(is.na(D_votes)){D_votes <- 0}
    data <- data.frame('District' = atlarge[x],'District_Number' = 0, 'D_Votes' = D_votes, 'R_Votes' = R_votes,
                       'Reporting'=reporting)
    colnames(data)[1] = 'State'
    return(data)
  }
}

###########Running the scrapeHouse function###########################################
compiledhouse <- as.data.frame(do.call(rbind, lapply(names(States), 
                                                     function(x) tryCatch(compiled_housetable(x), 
                                                                          error = function(e) NULL))))
#compiledhouse <- compiledhouse[,colSums(is.na(compiledhouse))<nrow(compiledhouse)]
names(compiledhouse) <- c('State', 'District_Number', 'D_Votes', 'R_Votes', 'Reporting')

atlarge_houses <- as.data.frame(do.call(rbind, lapply(names(atlarge), 
                                                      function(x) tryCatch(scrape_atlarge(x), 
                                                                         error = function(e) NULL))))

compiledhouse <- merge(compiledhouse,atlarge_houses,by=c('State','District_Number', 'D_Votes', 'R_Votes', 'Reporting'),all = TRUE)

compiledhouse$District <- paste(compiledhouse$State, compiledhouse$District_Number, sep='_')
compiledhouse[c('District_Number')] <- NULL
compiledhouse <- compiledhouse[c('State','District', 'D_Votes', 'R_Votes', 'Reporting')]
compiledhouse$Reporting[compiledhouse$Reporting == ''] = '100%'
compiledhouse[is.na(compiledhouse)] <- 0
compiledhouse[compiledhouse == 'NA'] <- 0

write_csv(compiledhouse, 'house-race.csv')

rm(list = ls())
#Output becomes County, D_votes, R_votes, D_pct, R_pct, Reporting
#Uncontested Party Percentage needs to be 100%
