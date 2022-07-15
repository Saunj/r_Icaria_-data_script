library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
source("tokens.R")

# PART2: Export and bind data from all health facilities
my.fields <- c('record_id','study_number')

my.events <- c('epipenta1_v0_recru_arm_1') 

data <- data.frame()
for (hf in names(kRedcapTokens)) {
  print(paste("Extracting data from", hf))
  
  rcon <- redcapConnection(kRedcapAPIURL, kRedcapTokens[[hf]])
  hf.data <- exportRecords(
    rcon,
    factors            = F,
    labels             = F,
    fields             = my.fields,
    events             = my.events,
    form_complete_auto = F
  )
  # cbind is attaching by columns so we use it to attach a new column with the HF code
  hf.data <- cbind(hf = hf, hf.data)
  # hf.data$hf <- hf  # This is an alternative way of doing the same
  
  # rbind is attaching by rows so we use it to attach the individual HF data frames together
  data <- rbind(data, hf.data)
}

# PART3: Tidy data - one row per study participant
data$id <- paste(data$hf, data$record_id, sep = "_") # Primary Key
data <- data[, c('id','hf','record_id','study_number')] # Remove redcap_ columns

data <- data[which(!is.na(data$study_number)), ] 

#----------------------------------------------------------------------------

my.fields <- c('record_id','hh_respondent','hh_caretaker','hh_date','se_date',
               'hh_child_seen','hh_mother_caretaker',
               'hh_other_caretaker','hh_other_respondent','socioeconomics_complete')

my.events <- c('epipenta1_v0_recru_arm_1','hhafter_1st_dose_o_arm_1') 

household.data <- data.frame()
for (hf in names(kRedcapTokens)) {
  print(paste("Extracting household data from", hf))
  
  rcon <- redcapConnection(kRedcapAPIURL, kRedcapTokens[[hf]])
  hf.da <- exportRecords(
    rcon,
    factors            = F,
    labels             = F,
    fields             = my.fields,
    events             = my.events,
    form_complete_auto = F
  )
  # cbind is attaching by columns so we use it to attach a new column with the HF code
  hf.da <- cbind(hf = hf, hf.da)
  # hf.data$hf <- hf  # This is an alternative way of doing the same
  
  # rbind is attaching by rows so we use it to attach the individual HF data frames together
  household.data <- rbind(household.data , hf.da)
}

# PART3: Tidy data - one row per study participant
household.data$id <- paste(household.data$hf, household.data$record_id, sep = "_") # Primary Key

filter <-  household.data$redcap_event_name == "hhafter_1st_dose_o_arm_1"
household.data1 <-household.data[filter, ]

household.data2  <- household.data1[, c('id','hf','record_id','hh_respondent','hh_caretaker','hh_date','se_date','hh_child_seen',
                                        'hh_other_caretaker','hh_mother_caretaker','hh_other_respondent','socioeconomics_complete')] # Remove redcap_ columns

household.data2 <- household.data2[which(!is.na(household.data2$hh_date)), ] 

filter <-  household.data2$hh_child_seen == 1
household.data3 <-household.data2[filter, ]


tcaretaker <- c("hh_caretaker","hh_mother_caretaker" )
household.data3$caretaker<- rowSums(household.data3[, tcaretaker], na.rm = T)


household.data3 <- household.data3[which(!is.na(household.data3$id)), ] 

#Socio Economic data
seo.data <-  household.data1[, c('id','hf','record_id','se_date','socioeconomics_complete')] 
seo.data <- seo.data[which(!is.na(seo.data$se_date)), ] 

my.data <- left_join(household.data3,seo.data , by = 'id')



hhf.data <- left_join(data,my.data , by = 'id')

hhf.data <- hhf.data[, c('id','hf.y','record_id.y','study_number','hh_respondent','hh_caretaker','hh_date','se_date.y','hh_child_seen',
                 'hh_other_caretaker','hh_mother_caretaker','hh_other_respondent','socioeconomics_complete.y')] # Remove redcap_ columns

tcaretaker1 <- c("hh_caretaker","hh_mother_caretaker" )
hhf.data$caretaker<- rowSums(hhf.data[, tcaretaker1], na.rm = T)



hhf.data <- hhf.data [, c('id','hf.y','record_id.y','study_number','hh_respondent','hh_caretaker','hh_date','se_date.y','hh_child_seen',
                          'hh_other_caretaker','hh_mother_caretaker','caretaker','hh_other_respondent','socioeconomics_complete.y')] # Remove redcap_ column

filter <-  hhf.data$caretaker != hhf.data$hh_respondent 
hhf.data1 <-hhf.data[filter, ]


hhf.data1 <- hhf.data1[which(!is.na(hhf.data1$se_date.y)), ]

socio <- read.csv("Socio Economic Error.csv")

socio$id <- paste(socio$hf, socio$record_id, sep = "_") 

filter <- !(hhf.data1$id %in% socio$id)
test <- hhf.data1[filter, ]

filter <- !(socio$id %in% hhf.data1$id)
test1 <- socio[filter, ]

test<- test[which(!is.na(test$id)), ]

write.csv(test, file = "ICARIA_Differ_caretaker.csv", row.names = F)














































