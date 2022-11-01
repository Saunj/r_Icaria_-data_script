library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
source("tokens.R")

# PART2: Export and bind data from all health facilities
my.fields <- c('record_id', 'int_date')

my.events <- c('epipenta1_v0_recru_arm_1', # Recruitment - AZi 1st dose
               'epimvr1_v4_iptisp4_arm_1', # AZi 2nd dose
               'epimvr2_v6_iptisp6_arm_1') # AZi 3rd dose

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
data <- data[, c('id', 'hf', 'record_id', 'int_date')] # Remove redcap_ columns
data <- data[which(!is.na(data$int_date)), ] # Remove screening failures

# Group records by participant and number AZi/Pbo doses for each participant
data <- group_by(data, id)

data <- mutate(data, azi_dose = row_number())

# Pivot AZi/Pbo doses from rows to columns
data.wide <- pivot_wider(data, names_from = azi_dose, values_from = int_date)

# Rename my columns
colnames(data.wide) <- c("id", "hf", "record_id", "azi1_date", "azi2_date", "azi3_date")

# PART4: Attach ICARIA study numbers and save CSV file
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
study.numbers <- study.numbers[which(!is.na(study.numbers$study_number)), ]       # Remove screening failures

my.report <- merge(study.numbers, data.wide, sort = F)

write.csv(my.report, file = "azi_doses_by_participant.csv", row.names = F)