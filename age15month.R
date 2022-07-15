library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
source("tokens.R")

# PART2: Export and bind data from all health facilities
my.fields <- c('record_id','study_number','child_dob','int_date')

my.events <- c('epipenta1_v0_recru_arm_1','epimvr2_v6_iptisp6_arm_1') 

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
data <- data[, c('id','hf','record_id','study_number','child_dob','int_date')] # Remove redcap_ columns

#----------------------------------------------------------------------------------
my.fields <- c('record_id', 'study_number')
my.events <- c('epipenta1_v0_recru_arm_1')  # Screening

study.numbers <- data.frame()
for (hf in names(kRedcapTokens)) {
  print(paste("Extracting study numbers from", hf))
  
  rcon <- redcapConnection(kRedcapAPIURL, kRedcapTokens[[hf]])
  hf.sn <- exportRecords(
    rcon,
    factors            = F,
    labels             = F,
    fields             = my.fields,
    events             = my.events,
    form_complete_auto = F
  )
  # cbind is attaching by columns so we use it to attach a new column with the HF code
  hf.sn <- cbind(hf = hf, hf.sn)
  # hf.data$hf <- hf  # This is an alternative way of doing the same
  
  # rbind is attaching by rows so we use it to attach the individual HF data frames together
  study.numbers <- rbind(study.numbers, hf.sn)
}

study.numbers$id <- paste(study.numbers$hf, study.numbers$record_id, sep = "_")   # Primary Key (PK)
study.numbers <- study.numbers[, c('id', 'study_number')]                         # Remove redcap_ columns
#study.numbers <- study.numbers[which(!is.na(study.numbers$study_number)), ]       # Remove screening failures

ppt.data <- merge(study.numbers,data, sort = F)


my.fields <- c('record_id','int_date')

my.events <- c('epipenta1_v0_recru_arm_1', 'epimvr2_v6_iptisp6_arm_1') 

age.data <- data.frame()
for (hf in names(kRedcapTokens)) {
  print(paste("Extracting month 15 data from", hf))
  
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
  age.data<- rbind(age.data, hf.da)
}

filter <-  age.data$redcap_event_name == "epimvr2_v6_iptisp6_arm_1"
age.data1 <-age.data[filter, ]

age.data1$id <- paste(age.data1$hf, age.data1$record_id, sep = "_")# Primary Key (PK)

age.data1<- age.data1[, c('id','record_id','int_date','redcap_event_name')]  


test.data <-  household.data3(ppt.data,age.data1, by ='id')

age.Cal<- test.data[, c('id','record_id.y','hf','study_number','child_dob','int_date.y','redcap_event_name')]  

age.Cal <- age.Cal[which(!is.na(age.Cal$redcap_event_name)), ] 

age <- c(age.Cal$int_date.y - age.Cal$child_dob)/31

age.Cal$age <- age

age.Cal4<- age.Cal[, c('id','record_id.y','hf','study_number','child_dob','int_date.y','age','redcap_event_name')]
age.Cal5 <- age.Cal4[which(!is.na(age.Cal4$int_date.y)), ] 

