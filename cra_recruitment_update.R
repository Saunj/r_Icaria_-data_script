library(redcapAPI)
library(dplyr) # group_by and mutate functions
library(tidyr) # pivot_wider function
library(lubridate)
source("tokens.R")
source("hf_names.R")

first_date_of_period <- "01-04-2022 00:00:00"
last_date_of_period <- "30-04-2022 23:59:59"

first_date_of_period <- as_datetime(first_date_of_period, format = "%d-%m-%Y %H:%M:%S")
last_date_of_period <- as_datetime(last_date_of_period, format = "%d-%m-%Y %H:%M:%S")


# PART2: Export and bind data from all health facilities
my.fields <- c('record_id',
               'screening_consent',
               'screening_number',
               'eligible',
               'study_number',
               'screening_date',
               'wdrawal_reported_date',
               'wdrawal_date',
               'death_reported_date',
               'death_date'
              )


my.events <- c('epipenta1_v0_recru_arm_1', # Recruitment - AZi 1st dose
               'end_of_fu_arm_1'
               ) 
#_______________________________________________________________________________

total_data <- data.frame()

period_data <- data.frame()

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
#_______________________________________________________________________________  
  print(paste("Processing data for ", hf ))

#_______________________________________________________________________________
  #HF name
  print ("Getting HF name")
  health_facility <- as.character(facility_names[hf])
#_______________________________________________________________________________
    #Withdrawals
  
  print("counting withdrawals")
  filter <- !is.na(hf.data$wdrawal_date)
  
  withdrawals <- hf.data[filter, c('record_id',
                                   'wdrawal_reported_date',
                                   'wdrawal_date'
                                   )
                         ]
  
 
   filter <- withdrawals$wdrawal_reported_date <= last_date_of_period
   withdrawals <- withdrawals[filter, ]
  
  filter <- withdrawals$wdrawal_reported_date >= first_date_of_period
            
  period_withdrawals <- withdrawals[filter, c('record_id', 'wdrawal_date')]
   
 total_withdrawals <- nrow(withdrawals)
 period_withdrawals <- nrow(period_withdrawals)
  

  
#_______________________________________________________________________________
  #Deaths
  print("counting Deaths")
  filter <- !is.na(hf.data$death_date)
  deaths <- hf.data[filter, c('record_id',
                              'death_reported_date',
                              'death_date'
                              )
                    ]
  
  filter <- deaths$death_reported_date <= last_date_of_period
  deaths <- deaths[filter, ]
  
  filter <- deaths$death_reported_date >= first_date_of_period

  period_deaths <- deaths[filter, c('record_id', 'death_date')]
  
  total_deaths <- nrow(deaths)
  period_deaths <- nrow(period_deaths)
  

  
#_______________________________________________________________________________
  
  
  #Screening Data to get consented, screened, screen failures, and randomized
  
  print("Sourcing penta 1 data")
  
  
  filter <- hf.data$redcap_event_name == "epipenta1_v0_recru_arm_1"
  hf.data <- hf.data[filter, ]

  
  filter <- hf.data$screening_date <= last_date_of_period
  hf.data <- hf.data[filter, ]

#_______________________________________________________________________________ 
  # Consented
  print("Getting Consented")
  
  filter  <- hf.data$screening_consent == "1"
  consented <- hf.data[filter, c('record_id', 'screening_date')]
  
  filter <- consented$screening_date >= first_date_of_period
  
  period_consented <- consented[filter, c('record_id', 'screening_date')]
  
  total_consented <- nrow(consented)
  period_consented <- nrow(period_consented)

#_______________________________________________________________________________  
  # Screened
  print("Getting Screened")
  
  filter  <- !is.na(hf.data$screening_number)
  screened <- hf.data[filter, c('record_id', 'screening_date')]
  
  filter <- screened$screening_date >= first_date_of_period
  
  period_screened <- screened[filter, c('record_id', 'screening_date')]
  
  total_screened <- nrow(screened)
  period_screened <- nrow(period_screened)

#_______________________________________________________________________________ 
  # Screening Failures
  print("Getting Screening Failures")
  
  filter  <- hf.data$eligible == "0"
  screening_failures <- hf.data[filter, c('record_id', 'screening_date')]
  
  filter <- screening_failures$screening_date >= first_date_of_period
  
  period_screening_failures <- screening_failures[filter, c('record_id',
                                                            'screening_date'
                                                            )
                                                  ]
  
  total_screening_failures <- nrow(screening_failures)
  period_screening_failures <- nrow(period_screening_failures)
  
#_______________________________________________________________________________
  #Randomized
  print("Getting Randomized Participants")
  
  filter  <- !is.na(hf.data$study_number)
  randomized <- hf.data[filter, c('record_id', 'screening_date')]
  
  filter <- randomized$screening_date >= first_date_of_period
  
  period_randomized <- randomized[filter, c('record_id', 'screening_date')]
  
  total_randomized <- nrow(randomized)
  period_randomized <- nrow(period_randomized)
#_______________________________________________________________________________  
  #Ongoing
  on_going = total_randomized - (total_deaths + total_withdrawals) # minus total completed
  
#_______________________________________________________________________________  
  data_total <- c(health_facility, total_consented, total_screened, total_screening_failures, total_randomized, total_withdrawals, total_deaths, on_going)
  data_period <- c(health_facility, period_consented, period_screened, period_screening_failures, period_randomized, period_withdrawals, period_deaths)
  
  total_data <- rbind(total_data, data_total)
  period_data <- rbind(period_data, data_period)
  
  
} 
columns_to_add <- as.numeric(as.character(total_data[2:8]))
total_data <- rbind(total_data, c("Total", colSums(columns_to_add)))
names(total_data) <- c("HF Name", "Total number Consented", "Total number Screened", "TotalScreening Failures", "Total number Randomized", "Total Withdrawals", "Total Deaths", "On-going")
names(period_data) <- c("HF Name", "Consented", "Screened", "Screening Failures", "Randomized", "Withdrawals", "Deaths")

columns_to_add <- c("Total number Consented", "Total number Screened", "TotalScreening Failures", "Total number Randomized", "Total Withdrawals", "Total Deaths", "On-going")
                  
numeric_total_data <- 
write.csv(total_data, file = "ICARIA Overall Recruitment Update.csv", row.names=F)
write.csv(period_data, paste("ICARIA Recruitment Update from", 
format(first_date_of_period,"%d-%m-%Y"), "to", format(last_date_of_period,"%d-%m-%Y"),".csv"),  row.names=F)


#include completed .  Leave this until well defined by management
#calculate on-going the on going can only be calcuated in the total's sheet
#include the HF name (add source)
#include total
#produce excel sheets

#total data should be up to last date requested by cra - done
