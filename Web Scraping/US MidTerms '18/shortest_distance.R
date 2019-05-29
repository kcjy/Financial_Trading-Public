rm(list = ls())

library('rvest')
library('tidyverse')
library(xmltools)
library('tidyr')
library('htmlTable')
library(openxlsx)

#States
States <- list("Alabama" = 'AL', "Alaska" = 'AK', "Arizona" = 'AZ', "Arkansas" = 'AR',
               "California" = 'CA', "Colorado" = 'CO', "Connecticut" = 'CT', "Delaware" = 'DE',
               "Florida" = 'FL', "Georgia" = 'GA', "Hawaii" = 'HI',
               "Idaho" = 'ID', "Illinois" = 'IL', "Indiana" = 'IN', "Iowa" = 'IA',
               "Kansas" = 'KS', "Kentucky" = 'KY', "Louisiana" = 'LA', "Maine" = 'ME',
               "Maryland" = 'MD', "Massachusetts" = 'MA', "Michigan" = 'MI', "Minnesota" = 'MN', "Minnesota-(special)" = 'MN_S', "Mississippi-(special)" = 'MS_S',
               "Mississippi" = 'MS', "Missouri" = 'MO', "Montana" = 'MT', "Nebraska" = 'NE',
               "Nevada" = 'NV', "New-Hampshire" = 'NH', "New-Jersey" = 'NJ', "New-Mexico" = 'NM',
               "New-York" = 'NY', "North-Carolina" = 'NC', "North-Dakota" = 'ND', "Ohio" = 'OH',
               "Oklahoma" = 'OK', "Oregon" = 'OR', "Pennsylvania" = 'PA', "Rhode-Island" = 'RI',
               "South-Carolina" = 'SC', "South-Dakota" = 'SD', "Tennessee" = 'TN', "Texas" = 'TX',
               "Utah" = 'UT', "Vermont" = 'VT', "Virginia" = 'VA', "Washington" = 'WA',
               "West-Virginia" = 'WV', "Wisconsin" = 'WI', "Wyoming" = 'WY')

live_results <- read.csv('senator_race.csv')
estpopulation <- read.csv('Population/senate_population.csv')

baseline <- read.xlsx('baseline.csv', 
                      sheet = 'out', rowNames=TRUE)

baseline$Combined <- paste0(names(States)[which(States %in% row.names(baseline))], ' (', baseline$Incumbent, ')')

run <- function(){
#Modifying data values from string to numeric
live_results$State <- substr(live_results$County, 1, 2)
estpopulation $State <- substr(estpopulation $County, 1, 2)

#Convert format
live_results$D_Votes <- as.numeric(gsub(',', '', live_results$D_Votes))
live_results$R_Votes <- as.numeric(gsub(',', '', live_results$R_Votes))
estpopulation $Total <- as.numeric(gsub(',', '', estpopulation $Total))

live_results$Total_Votes <- live_results$D_Votes + live_results$R_Votes


#######################################################################
#Key Criteria for Democrats to win the Senate Race
total_seats = 35
D_target = 28
R_target = 9
##########################################################################
#Manual Overridde for declaration of States (D = 1, R = 0)
override_D <- row.names(baseline[which(baseline[,1] == 1),1,drop=FALSE])
override_R <- row.names(baseline[which(baseline[,1] == 0),1,drop=FALSE])

'%not in%' <- Negate('%in%')
live_results <- live_results[live_results$State %not in% union(override_D, override_R),]
######################################################################
#Total Registered Voters
reg_votes <- as.numeric(gsub(',','',estpopulation$Total[which(estpopulation$County %in% live_results$County)]))


comparison_table <- data.frame(State = live_results$State, County = live_results$County, Total_Votes = reg_votes, D_Votes = live_results$D_Votes,
                               R_Votes = live_results$R_Votes)
#comparison_table <- comparison_table[!is.na(comparison_table),]

#Historical Voter Turnout based on 2016 Registered Voters
historical_reg <- as.data.frame(do.call(rbind, lapply(unique(comparison_table$State), 
                                    function(x) sum(comparison_table[which(comparison_table$State == x),'Total_Votes'], na.rm= T))))
row.names(historical_reg) <- unique(comparison_table$State)

#All Total Votes Live by State
live_R <- as.data.frame(do.call(rbind, lapply(unique(comparison_table$State), 
                                                  function(x) sum(comparison_table[which(comparison_table$State == x),c('R_Votes')],  na.rm= T))))
row.names(live_R) <- unique(comparison_table$State)
live_R[is.na(live_R)] <- 0

#All Democrat Votes Live by State
live_D <- as.data.frame(do.call(rbind, lapply(unique(comparison_table$State), 
                                                  function(x) sum(comparison_table[which(comparison_table$State == x),c('D_Votes')],  na.rm= T))))
row.names(live_D) <- unique(comparison_table$State)
live_D[is.na(live_D)] <- 0

#Total Live Votes By State
live_total <- live_R + live_D

#Calculation of distance to 50% of estimated population by votes
D_difference <- (historical_reg * 0.5) - live_D #Vote Differential to win each State for D
row.names(D_difference) <- unique(comparison_table$State)
R_difference <- (historical_reg * 0.5) - live_R #Vote Differential to win each State for D
row.names(R_difference) <- unique(comparison_table$State)

Dresult <- as.data.frame(live_D[historical_reg - live_total > D_difference & D_difference > 0,])
row.names(Dresult) <- row.names(live_D)[historical_reg - live_total > D_difference & D_difference > 0] #Available counties to be contested 
Rresult <- as.data.frame(live_R[historical_reg - live_total > R_difference & R_difference > 0,])
row.names(Rresult) <- row.names(live_R)[historical_reg - live_total > R_difference & R_difference > 0]

#Based on assumption, which race has already closed since Live - 50% < 0
#R_confirmed_states <- row.names(R_difference)[R_difference < 0]
#D_confirmed_states <- row.names(D_difference)[D_difference < 0]

R_confirmed_states <- row.names(baseline)[which(baseline$Race == 1 & baseline$Party == 'R')]
D_confirmed_states <- row.names(baseline)[which(baseline$Race == 1 & baseline$Party == 'D')]

R_confirmed_states <- unique(c(R_confirmed_states, override_R))
D_confirmed_states <- unique(c(D_confirmed_states, override_D))

#Check that all states won are unique
if (length(unique(union(D_confirmed_states, R_confirmed_states))) != (length(D_confirmed_states) + length(R_confirmed_states))){
  print('States Error: States won are not unique.')
}

########################################################################
#Shortest Distance Matrix (Number of votes required for Democrats to win each state where race has not ended)
#Registered Voters for each county
reg_votes_pres <- historical_reg / 2 #50%

distance_matrix_D <- reg_votes_pres[which(row.names(reg_votes_pres) %in% row.names(Dresult)),] - Dresult  #Number of votes to reach historical 50% for D
distance_matrix_R <- reg_votes_pres[which(row.names(reg_votes_pres) %in% row.names(Rresult)),] - Rresult  #Number of votes to reach historical 50% for R
#Number of seats D has Definitely won

#Number of seats D has Definitely won
D_votes <- as.data.frame(do.call(rbind, lapply(unique(comparison_table$State), 
                                    function(x) sum(comparison_table[which(comparison_table$State == x),'D_Votes']))))
D_votes[is.na(D_votes)] <- 0
Democrat_score <- unique(comparison_table$State)[D_votes - reg_votes_pres > 0]
#Democrat_score <- unique(c(as.vector(Democrat_score), D_confirmed_states))
Democrat_score <- D_confirmed_states
#row.names(D_votes) <- unique(result$State)

#Number of seats R has definitely won
R_votes <- as.data.frame(do.call(rbind, lapply(unique(comparison_table$State), 
                                               function(x) sum(comparison_table[which(comparison_table$State == x),'R_Votes']))))
R_votes[is.na(R_votes)] <- 0
Republican_score <- unique(comparison_table$State)[R_votes - reg_votes_pres > 0]
#Republican_score <- unique(c(as.vector(Republican_score), R_confirmed_states))
Republican_score <- R_confirmed_states

if (length(Republican_score) > 9) {
  print('Republicans have won the Senate')
}

done_states <- unique(c(as.vector(Democrat_score), as.vector(Republican_score)))

overlaps <- done_states[which(done_states %in% Republican_score & done_states %in% Democrat_score)]
Democrat_score <- tryCatch({Democrat_score[Democrat_score %not in% overlaps[which(live_R[overlaps,,drop = F] > live_D[overlaps,,drop = F])]]}, error = function(e) Democrat_score)
Republican_score <- tryCatch({Republican_score[Republican_score %not in% overlaps[which(live_D[overlaps,,drop = F] > live_R[overlaps,,drop = F])]]},  error = function(e) Republican_score)

Democrat_score <- Democrat_score[Democrat_score %in% row.names(baseline)]
Republican_score <- Republican_score[Republican_score %in% row.names(baseline)]

###################################################################################
#How many more seats do the Democrats need to win in order to win the Senate race
similarity_data <- read.csv('Population/similarity_score.csv', row.names = 'X')

#Distance between live votes for D to 50% mark, in percentage of total population
live_seats_D = data.frame(sort(distance_matrix_D[which(row.names(distance_matrix_D) %not in% done_states),1]), 
                        row.names = row.names(distance_matrix_D)[which(row.names(distance_matrix_D) %not in% done_states)][order(distance_matrix_D[which(row.names(distance_matrix_D) %not in% done_states),1])])
names(live_seats_D) <- 'Live'
live_seats_D <- live_seats_D/reg_votes_pres[row.names(live_seats_D),]
live_seats_D <- live_seats_D[which(row.names(live_seats_D) %in% row.names(baseline)), ,drop=FALSE] #Additional check that all states in Live are in Baseline


for (i in 1:dim(live_seats_D)[1]){
  path = baseline[row.names(live_seats_D)[i],1]
  #Catching Exception of Error in baseline values
  if (is.na(path)){
    path = 0
  }
  similarity <- similarity_data[row.names(live_seats_D)[i],] #Extracting similarity scores for all states
  similarity = similarity[which(similarity != 0)] #Removing irrelevant states (=0)
  similarity[1,] <- baseline[colnames(similarity),1] * similarity[1,]/100 
  similarity <- similarity[which(!is.na(similarity))]
  d <- tryCatch({
  path + similarity[which(colnames(similarity) %in% Democrat_score)]}, 
  error = function(e) d <- 0)
  
  #Adding Premium to baseline for all won states
  r <- tryCatch({
   path - similarity[which(colnames(similarity) %in% Republican_score)]},
   error = function(e) 0)#Discounting baseline for all Republican states
  
  #Exceptions for states with 0 similarity to done_states
  if (length(d) != 0) {
    d_value <- mean(c(path, as.numeric(d)))
  } else {
    d_value <- path
  }
  
  if (length(r) != 0) {
    r_value <- mean(c(path, as.numeric(r)))
  } else {
    r_value <- path
  }
  #Final Probability is the average of the Premium & Discounted baseline probability
  live_seats_D$Prob[i] <- mean(c(d_value, r_value))
  
  #For Solid D states with high baseline, probability should be adjusted to99% 
  if (live_seats_D$Prob[i] >= 1){
    live_seats_D$Prob[i] = 0.99
  }
  
  #Since the state with the shortest distance should have Highest Baseline and Lowest Vote distance, Distance metric should be Baseline/Vote
  live_seats_D$Aggregate[i] <- mean(c(d_value, r_value)) / live_seats_D$Live[i]
}
#Finding the path to D
#The top n States with the Highest Aggregate scores, n being the remaining number of states needed by the party
path_to_D <- tryCatch({row.names(live_seats_D)[order(-live_seats_D$Aggregate)][1:(D_target - length(Democrat_score))]}, 
                            error=function(e) 'Democrats have won')
path_to_D <- path_to_D[!is.na(path_to_D)]

##########################

#Distance between live votes for R to 50% mark, in percentage of total population
live_seats_R = data.frame(sort(distance_matrix_R[which(row.names(distance_matrix_R) %not in% done_states),1]), 
                        row.names = row.names(distance_matrix_R)[which(row.names(distance_matrix_R) %not in% done_states)][order(distance_matrix_R[which(row.names(distance_matrix_R) %not in% done_states),1])])
names(live_seats_R) <- 'Live'
live_seats_R <- live_seats_R/reg_votes_pres[row.names(live_seats_R),]
live_seats_R <- live_seats_R[which(row.names(live_seats_R) %in% row.names(baseline)), ,drop=FALSE] #Additional check that all states in Live are in Baseline

for (i in 1:dim(live_seats_R)[1]){
  path = 1 - baseline[row.names(live_seats_R)[i],1] #Probability should be 1-Baseline since Baseline assessment is based on Democrat
  #Catching Exception of Error in baseline values
  if (is.na(path)){
    path = 0
  }
  similarity <- similarity_data[row.names(live_seats_R)[i],] #Extracting similarity scores for all states
  
  similarity = similarity[which(similarity != 0)] #Removing irrelevant states (=0)
  similarity[1,] <- (1 - baseline[colnames(similarity),1]) * similarity[1,]/100
  similarity <- similarity[which(!is.na(similarity))]
  
  r <-tryCatch(path + similarity[which(colnames(similarity) %in% Republican_score)], #Adding Premium to baseline for all won states
    error = function(e) 0)
  
  d <- tryCatch(
  path - similarity[which(colnames(similarity) %in% Democrat_score)],
   error = function(e) 0)                     #Discounting baseline for all Republican states
  
  #Exceptions for states with 0 similarity to done_states
  if (length(d) != 0) {
    d_value <- mean(c(path, as.numeric(d)))
  } else {
    d_value <- path
  }
  
  if (length(r) != 0) {
    r_value <- mean(c(path, as.numeric(r)))
  } else {
    r_value <- path
  }
  live_seats_R$Prob[i] <- mean(c(d_value, r_value))
  
  #For Solid D states with high baseline, probability should be adjusted to99% 
  if (live_seats_R$Prob[i] >= 1){
    live_seats_R$Prob[i] = 0.99
  }
  
  live_seats_R$Aggregate[i] <- mean(c(d_value, r_value)) / live_seats_R$Live[i]
}

#The top n States with the Highest Aggregate scores, n being the remaining number of states needed by the party
path_to_R <- tryCatch({row.names(live_seats_R)[order(-live_seats_R$Aggregate)][1:(R_target - length(Republican_score))]},
                      error=function(e) 'Republicans have won')
path_to_R <- path_to_R[!is.na(path_to_R)]

####################################### Shortest Distance Calculator Email ############################################################
library(RDCOMClient)
library(kableExtra)
library(dplyr)

################################      Formatting output for pre-specified Email format    #######################################
output <- data.frame(D_path = path_to_D, 
                     D_Pct_Left = paste0(round(((reg_votes_pres - D_votes) / historical_reg)[path_to_D,] * 100,2), "%"), 
                     D_Prob = paste0(round(live_seats_D[path_to_D,]$Prob * 100, 2), "%"),
                     stringsAsFactors=FALSE)
output <- merge(output, data.frame(R_path = path_to_R, 
                                   R_Pct_Left = paste0(round(((reg_votes_pres - R_votes) / historical_reg)[path_to_R,] * 100,2), "%"), 
                                   R_Prob = paste0(round(live_seats_R[path_to_R,]$Prob* 100,2), "%"), 
                                   stringsAsFactors=FALSE), by = 0, all=TRUE)

output$Row.names <- as.numeric(output$Row.names)
output <- output[order(output$Row.names),]
output$Row.names <- NULL
row.names(output) <- 1:nrow(output)
output$R_path[is.na(output$R_path)] = rep(' ', length(output$R_path[is.na(output$R_path)]))

seats_left <- total_seats - length(Democrat_score) + length(Republican_score)
averages <- data.frame('Democrats:', NA,paste0(round(mean(live_seats_D[path_to_D,]$Prob)*100,2), "%"), 
                       'Republican:', NA,paste0(round(mean(live_seats_R[path_to_R,]$Prob)*100,2), "%"))
names(averages) = names(output)                       
output <- rbind(output,averages)

colnames(output) <- c('Democrat_Path', '% Votes Left to 50% (D)', 'State Probability',
                      'Republican_Path', '% Votes Left to 50% (R)', 'State Probability')
#######################################################

swing_states <- row.names(baseline[which(baseline$Swing == 'Y'),])
swing_states <- swing_states[swing_states %not in% done_states]

output$Democrat_Path <- cell_spec(output$Democrat_Path, "html", color = ifelse(output$Democrat_Path %in% swing_states,
                                                                 "red", "black"), bold = T)
output$Republican_Path <- cell_spec(output$Republican_Path, "html", color = ifelse(output$Republican_Path %in% swing_states,
                                                                         "red", "black"), bold = T)

output[dim(output)[1],c(3,6)] <- cell_spec(output[dim(output)[1],c(3,6)], "html", bold = T)
output[is.na(output)] <- ''

y <- knitr::kable(output, format="html", booktabs = T, escape = F, full_width = F, linesep = c('','','\\hline')) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
      column_spec(c(2,4,6), border_left = TRUE, border_right = TRUE) %>%
      column_spec(1, border_left = TRUE) %>%
      column_spec(6, border_right = TRUE) %>%
      row_spec(0, background = "#e0e0eb") %>%
      row_spec(dim(output)[1], background = "#e0e0eb")

lead_string <- paste0(c('<body><p>. Democrats have won: ',
                        paste0(names(States)[which(States %in% as.character(Democrat_score))], collapse=', '),
                        '</p><p>',
                        '<p>. Republicans have won: ',
                        paste0(names(States)[which(States %in% as.character(Republican_score))], collapse=', '),
                        '</p><hr>'), collapse='') 

lag_string <- paste0('<footer><p><font size="1">*% Votes Left: Estimated amount of votes left for each State, as a percentage of total population</font></p>',
                      #'<p><font size="1">*% Votes Left (R): Estimated amount of votes left for each State in most probable Republican Path</font></p>',
                     '<p><font size="1">*Probability Calculations: Estimated based on baseline predictions and similarity scores to declared states</font></p>',
                     '<p><font size="1">*States highlighted in red: Swing States</font></p></footer>')

body <- paste0("<html>", lead_string, 
               '<table border = "0"><tr><td>', y, '</td></tr></table>',
               lag_string, "</html>")
OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "myself@gmail.com"
outMail[["Cc"]] = "myself@gmail.com"
outMail[["subject"]] = paste0("Senate Election Live Update - Path Probability Calculation ",as.character(format(Sys.time(),"%H:%M %p")), ' SGT')
outMail[["HTMLbody"]] = body

outMail$Send()

###################### Trader Version Email #####################################
swing_states <- row.names(baseline[which(baseline$Swing == 'Y'),])
swing_states <- swing_states[swing_states %not in% done_states]      
pct_gap <- (historical_reg - live_total)/historical_reg 
pct_gap <- data.frame(pct_gap[swing_states,], row.names = swing_states)
pct_gap <- pct_gap[order(pct_gap), , drop = FALSE]
swing_states <- row.names(pct_gap)[1:3]

if (length(Democrat_score) > length(Republican_score)){
  #header <- '<strong><u>Democrats are leading</u></strong><span style=""color:#80BFFF"">'
  lead_string <- paste0(c('<body><p>{US} SENATE Race Live Update - ',
                          as.character(format(Sys.time(),"%H:%M %p")),
                          ' SGT',
                          '</p><p>',
                          '<p>{P2} Dem. has won ',
                          paste0(c(length(Democrat_score), ' needs ', D_target - length(Democrat_score),' to win (Leading)')),
                          '</p><p>',
                          '<p>{P4} Rep. has won ',
                          paste0(c(length(Republican_score), ' needs ', R_target - length(Republican_score),' to win')),
                          '</p><p>',
                          '<p> Key races to watch: ',
                          paste0(baseline$Combined[row.names(baseline) %in% as.character(swing_states)], collapse=', '),
                          paste0('; ', total_seats - length(Democrat_score) - length(Republican_score), ' seats remaining'),
                          '</p><p>',
                          '</p><hr>'),
                        collapse='') 
  
} else if (length(Republican_score) > length(Democrat_score)) {
  #header <- '<strong><u>Republicans are leading</u></strong><span style=""color:#80BFFF"">'
  lead_string <- paste0(c('<body><p>{US} SENATE Race Live Update - ',
                          as.character(format(Sys.time(),"%H:%M %p")),
                          ' SGT',
                          '</p><p>',
                          '<p>{P2} Dem. has won ',
                          paste0(c(length(Democrat_score), ' needs ', D_target - length(Democrat_score),' to win')),
                          '</p><p>',
                          '<p>{P4} Rep. has won ',
                          paste0(c(length(Republican_score), ' needs ', R_target - length(Republican_score),' to win (Leading)')),
                          '</p><p>',
                          '<p> Key races to watch: ',
                          paste0(baseline$Combined[row.names(baseline) %in% as.character(swing_states)], collapse=', '),
                          paste0('; ', total_seats - length(Democrat_score) - length(Republican_score), ' seats remaining'),
                          '</p><p>',
                          '</p><hr>'),
                        collapse='') 
} else {
  lead_string <- paste0(c('<body><p>{US} SENATE Race Live Update - ',
                          as.character(format(Sys.time(),"%H:%M %p")),
                          ' SGT',
                          '</p><p>',
                          '<p>{P2} Dem. has won ',
                          paste0(c(length(Democrat_score), ' needs ', D_target - length(Democrat_score),' to win')),
                          '</p><p>',
                          '<p>{P4} Rep. has won ',
                          paste0(c(length(Republican_score), ' needs ', R_target - length(Republican_score),' to win')),
                          '</p><p>',
                          '<p> Key races to watch: ',
                          paste0(baseline$Combined[row.names(baseline) %in% as.character(swing_states)], collapse=', '),
                          paste0('; ', total_seats - length(Democrat_score) - length(Republican_score), ' seats remaining'),
                          '</p><p>',
                          '</p><hr>'),
                        collapse='') 
}


body <- paste0("<html>", lead_string, "</html>")
OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "myself@gmail.com"
outMail[["Cc"]] = "myself@gmail.com"
outMail[["subject"]] = paste0("[PASTE] Senate Race Live Update ", as.character(format(Sys.time(),"%H:%M %p")), ' SGT')
outMail[["HTMLbody"]] = body

outMail$Send()

}

#if (sum(is.na(live_results$D_Votes)) == length(live_results$D_Votes)){
#  stop()
#} else {
#  run()
#}

run()
rm(list = ls())
