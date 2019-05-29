rm(list = ls())

library('rvest')
library('tidyverse')
library(xmltools)
library('tidyr')
library('htmlTable')
library(openxlsx)
library(RDCOMClient)

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


live_results <- read.csv('house-race.csv')
baseline <- read.xlsx('baseline.csv', 
                      sheet = 'out', rowNames=TRUE)
baseline$Combined <- paste0(names(States)[which(States %in% row.names(baseline))], ' (', baseline$Incumbent, ')')

##########################################################################
#Manual Overridde for declaration of States (D = 1, R = 0)
'%not in%' <- Negate('%in%')
override_D <- row.names(baseline[which(baseline[,1] == 1),1,drop=FALSE])
override_R <- row.names(baseline[which(baseline[,1] == 0),1,drop=FALSE])

live_results <- live_results[row.names(live_results)%not in% union(override_D, override_R),]
#Modifying data values from string to numeric
live_results$Reporting <- as.numeric(gsub('%','',live_results$Reporting))

#done_districts <- live_results[which(live_results$Reporting > baseline$threshold[1]),]
done_districts <- live_results[which(live_results$District %in% row.names(baseline)[which(baseline$Race == 1)]),]

#D_wins <- done_districts[which(done_districts$D_Votes > done_districts$R_Votes),c('District')]
D_wins <- row.names(baseline)[which(baseline$Race == 1 & baseline$Party == 'D')]
D_wins <- unique(union(override_D, D_wins))

#R_wins <- done_districts[which(done_districts$R_Votes > done_districts$D_Votes),c('District')]
R_wins <- row.names(baseline)[which(baseline$Race == 1 & baseline$Party == 'R')]
R_wins <- unique(union(override_R, R_wins))

done <- done_districts <- union(D_wins, R_wins)
#######################################################################
#Key Criteria for Democrats to win the Senate Race
total_seats = 435
D_target = 218
R_target = 218
total_swing = nrow(baseline[which(baseline$Swing == 'Y'),])
solid_D = nrow(baseline[which(baseline$Forecast %in% c(6, 7)),])
solid_R = nrow(baseline[which(baseline$Forecast %in% c(1, 2)),])

################################   SWING   ##########################################
swing_districts <- row.names(baseline[which(baseline$Swing == 'Y'),])
swing_districts <- swing_districts[swing_districts %not in% done]

swing_declared <- baseline[which(baseline$Swing == 'Y' & baseline$Race == 1),]
swing_left <- baseline[which(baseline$Swing == 'Y' & baseline$Race != 1),]

#swing_D <- row.names(swing_declared)[order(-swing_declared$Pct.D)][1:(total_swing/2 - nrow(swing_declared[swing_declared$Party == 'D',]))]
#swing_R <- row.names(swing_declared)[order(swing_declared$Pct.D)][1:(total_swing/2 - nrow(swing_declared[swing_declared$Party == 'R',]))]
swing_D <- row.names(swing_declared)[which(swing_declared$Party == 'D')]
swing_R <- row.names(swing_declared)[which(swing_declared$Party == 'R')]

swing_D <- swing_D[!is.na(swing_D)]
swing_R <- swing_R[!is.na(swing_R)]
if (length(swing_D[is.na(swing_D)]) == length(swing_D)){
  swing_D = NULL
}
if (length(swing_R[is.na(swing_R)]) == length(swing_R)){
  swing_R = NULL
}

#path_to_D <- row.names(swing_left)[order(-swing_left$Pct.D)][1:(total_swing/2 - nrow(swing_declared[swing_declared$Party == 'D',]))]
#path_to_R <- row.names(swing_left)[order(swing_left$Pct.D)][1:(total_swing/2 - nrow(swing_declared[swing_declared$Party == 'R',]))]

path_to_D <- swing_left[order(-swing_left$Pct.D),]
path_to_D <- row.names(path_to_D)[which(path_to_D$Pct.D > 0.5)]
path_to_R <- swing_left[order(swing_left$Pct.D),]
path_to_R <- row.names(path_to_R)[which(path_to_R$Pct.D < 0.5)]

D_flip_R <- row.names(swing_declared)[which(swing_declared$Forecast %in% c(6,7) & swing_declared$Party == 'R')]
R_flip_D <- row.names(swing_declared)[which(swing_declared$Forecast %in% c(1,2) & swing_declared$Party == 'D')]
################################      Formatting output for pre-specified Email format    #######################################
library(RDCOMClient)
library(kableExtra)
library(dplyr)

output <- data.frame(D_path = path_to_D, 
                     D_Prob = paste0(round(baseline[path_to_D,1] * 100, 2), "%"),
                     stringsAsFactors=FALSE)
output <- merge(output, data.frame(R_path = path_to_R, 
                                   R_Prob = paste0(round((1 - baseline[path_to_R,1])* 100,2), "%"), 
                                   stringsAsFactors=FALSE), by = 0, all=TRUE)

output$Row.names <- as.numeric(output$Row.names)
output <- output[order(output$Row.names),]
output$Row.names <- NULL
row.names(output) <- 1:nrow(output)
output$R_path[is.na(output$R_path)] = rep(' ', length(output$R_path[is.na(output$R_path)]))

seats_left <- total_swing - length(path_to_D) + length(path_to_R)

colnames(output) <- c('Democrat_Path', 'State Probability',
                      'Republican_Path', 'State Probability')

output[is.na(output)] <- ''
output <- output[output$`State Probability` != 'NA%',]

averages <- data.frame('Democrats:', paste0(round(mean(baseline[path_to_D,1])*100,2), "%"), 
                       'Republican:', paste0(round(mean(1 - baseline[path_to_R,1])*100,2), "%"))
names(averages) = names(output)                       
output <- rbind(output,averages)

output[dim(output)[1],c(2,4)] <- cell_spec(output[dim(output)[1],c(2,4)], "html", bold = T)
#######################################################

y <- knitr::kable(output, format="html", booktabs = T, escape = F, full_width = F, linesep = c('','','\\hline')) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(c(2,3), border_left = TRUE, border_right = TRUE) %>%
  column_spec(1, border_left = TRUE) %>%
  column_spec(4, border_right = TRUE) %>%
  row_spec(0, background = "#e0e0eb") %>%
  row_spec(dim(output)[1], background = "#e0e0eb")


surprise <- paste0('<p>Safe D flipped R: ', paste0(as.character(D_flip_R),collapse=', '), 
                   '</p><p>Safe R flipped D: ', paste0(as.character(R_flip_D),collapse=', '),
                   '</p>')


lead_string <- paste0(c('<body><p>Total Swing States: ', total_swing, '</p>',
                        '<p>Democrats have won ', length(swing_D), ': ', 
                        paste0(as.character(swing_D), collapse=', '),
                        '</p><p>',
                        '<p>Republicans have won ', length(swing_R), ': ',
                        paste0(as.character(swing_R), collapse=', '),
                        '</p>', surprise, '<hr>'), collapse='') 

lag_string <- paste0('<footer><p><font size="1">*Probability Calculations: Estimated based on baseline predictions on Swing States</font></p>',
                     '</p></footer>')

body <- paste0("<html>", lead_string,
               '<table border = "0"><tr><td>', y, '</td></tr></table>',
               lag_string, "</html>")
OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "myself@gmail.com"
outMail[["Cc"]] = "myself@gmail.com"
outMail[["subject"]] = paste0("House Election Live Update - Path Probability Calculation (Swing States) ",as.character(format(Sys.time(),"%H:%M %p")), ' SGT')
outMail[["HTMLbody"]] = body

outMail$Send()

########################## Trader Update #######################################

if (length(D_wins) > length(R_wins)){
  #header <- '<strong><u>Democrats are leading</u></strong><span style=""color:#80BFFF"">'
  lead_string <- paste0(c('<body><p>{US} HOUSE Race Live Update - ',
                          as.character(format(Sys.time(),"%H:%M %p")),
                          ' SGT',
                          '</p><p>',
                          '<p>{P2} Dem. has won ',
                          paste0(c(length(D_wins), ' needs ', D_target - length(D_wins),' more to win (Leading)')),
                          '</p><p>',
                          '<p>{P4} Rep. has won ',
                          paste0(c(length(R_wins), ' needs ', R_target - length(R_wins),' more to win')),
                          '</p><p>',
                          paste0('<p> Total ', total_seats - length(done), ' remains (', length(swing_districts), ' swing districts remains, safe D ',
                                 solid_D - sum(baseline[row.names(baseline) %not in% done,]$Forecast %in% c('Solid D', 'Likely D')), 
                                 ' vs. safe R ', solid_R - sum(baseline[row.names(baseline) %not in% done,]$Forecast %in% c('Solid R', 'Likely R')), ')'),
                          '</p><p>',
                          '</p>', surprise, '<hr>'),
                        collapse='') 
} else if (length(R_wins) > length(D_wins)) {
  #header <- '<strong><u>Republicans are leading</u></strong><span style=""color:#80BFFF"">'
  lead_string <- paste0(c('<body><p>{US} HOUSE Race Live Update - ',
                          as.character(format(Sys.time(),"%H:%M %p")),
                          ' SGT',
                          '</p><p>',
                          '<p>{P2} Dem. has won ',
                          paste0(c(length(D_wins), ' needs ', D_target - length(D_wins),' more to win')),
                          '</p><p>',
                          '<p>{P4} Rep. has won ',
                          paste0(c(length(R_wins), ' needs ', R_target - length(R_wins),' more to win (Leading)')),
                          '</p><p>',
                          paste0('<p> Total ', total_seats - length(done), ' remains (', length(swing_districts), ' swing districts remains, safe D ',
                                 solid_D - sum(baseline[row.names(baseline) %not in% done,]$Forecast %in% c('Solid D', 'Likely D')), 
                                 ' vs. safe R ', solid_R - sum(baseline[row.names(baseline) %not in% done,]$Forecast %in% c('Solid R', 'Likely R')), ')'),
                          '</p><p>',
                          '</p>', surprise, '<hr>'),
                        collapse='')
} else {
  lead_string <- paste0(c('<body><p>{US} HOUSE Race Live Update - ',
                          as.character(format(Sys.time(),"%H:%M %p")),
                          ' SGT',
                          '</p><p>',
                          '<p>{P2} Dem. has won ',
                          paste0(c(length(D_wins), ' needs ', D_target - length(D_wins),' more to win')),
                          '</p><p>',
                          '<p>{P4} Rep. has won ',
                          paste0(c(length(R_wins), ' needs ', R_target - length(R_wins),' more to win')),
                          '</p><p>',
                          paste0('<p> Total ', total_seats - length(done), ' remains (', length(swing_districts), ' swing districts remains, safe D ',
                                 solid_D - sum(baseline[row.names(baseline) %not in% done,]$Forecast %in% c('Solid D', 'Likely D')), 
                                 ' vs. safe R ', solid_R - sum(baseline[row.names(baseline) %not in% done,]$Forecast %in% c('Solid R', 'Likely R')), ')'),
                          '</p><p>',
                          '</p>', surprise, '<hr>'),
                        collapse='')
}

body <- paste0("<html>", lead_string, "</html>")
OutApp <- COMCreate("Outlook.Application")
outMail = OutApp$CreateItem(0)
outMail[["To"]] = "myself@gmail.com"
outMail[["Cc"]] = "myself@gmail.com"
outMail[["subject"]] = paste0("[PASTE] House Race Live Update ", as.character(format(Sys.time(),"%H:%M %p")), ' SGT')
outMail[["HTMLbody"]] = body

outMail$Send()

rm(list = ls())
