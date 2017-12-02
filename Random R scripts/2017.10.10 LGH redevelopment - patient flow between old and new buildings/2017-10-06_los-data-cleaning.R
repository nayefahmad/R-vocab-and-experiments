

#**********************************
# Script to clean data for example dataset: 
#**********************************

library("dplyr")
library("lubridate")
library("tidyr")

# rm(list=ls())


#**********************************
# TODO: 
#**********************************


# import data cleaning functoin: ------
source('data-cleaning_function.R')


#********************************
# Cleaning 4E data: -----------
#********************************

# losdata <- read.csv("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.04 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_4E-LOS_example-dataset.csv", 
#          na.strings = "NULL", 
#          stringsAsFactors = TRUE) 



losdata.4e <- read.csv("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_4E-LOS.csv", 
                    na.strings = "NULL", 
                    stringsAsFactors = TRUE) 

losdata.4e <- clean.los(losdata.4e)  # NAs returned for pts not discharged yet

# str(losdata.4e)
# summary(losdata.4e)
# summary(select(losdata.4e, 6:8))
# head(losdata.4e)


# save reformatted data: ----------------
write.csv(losdata.4e, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_4E-LOS-reformatted.csv", 
          row.names = FALSE)




#********************************
# Cleaning 6E data: -----------
#********************************

losdata.6e <- read.csv("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_6E-LOS.csv", 
                       na.strings = "NULL", 
                       stringsAsFactors = TRUE) 

losdata.6e <- clean.los(losdata.6e)

# str(losdata.6e)
# summary(losdata.6e)
# summary(select(losdata.6e, 6:8))
# head(losdata.6e)

# save reformatted data: ----------------
write.csv(losdata.6e, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_6E-LOS-reformatted.csv", 
          row.names = FALSE)





#********************************
# Cleaning 6W data: -----------
#********************************

losdata.6w <- read.csv("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_6W-LOS.csv", 
                       na.strings = "NULL", 
                       stringsAsFactors = TRUE) 

losdata.6w <- clean.los(losdata.6w)

# str(losdata.6w)
# summary(losdata.6w)
# summary(select(losdata.6w, 6:8))
# head(losdata.6w)

# save reformatted data: ----------------
write.csv(losdata.6w, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_6W-LOS-reformatted.csv", 
          row.names = FALSE)




#********************************
# Cleaning 4W data: -----------
#********************************

# note: input csv may have to be processed manually first: empty rows 
#     after SSLTC rows. 

losdata.4w <- read.csv("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_4W-LOS.csv", 
                       na.strings = "NULL", 
                       stringsAsFactors = TRUE) 

losdata.4w <- clean.los(losdata.4w)

# str(losdata.4w)
# summary(losdata.4w)
# summary(select(losdata.4w, 6:8))
# head(losdata.4w)

# save reformatted data: ----------------
write.csv(losdata.4w, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-10_LGH_4W-LOS-reformatted.csv", 
          row.names = FALSE)



#********************************
# Cleaning 7E data: -----------
#********************************

losdata.7e <- read.csv("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-17_LGH_7E-LOS.csv", 
                       na.strings = "NULL", 
                       stringsAsFactors = TRUE) 

losdata.7e <- clean.los(losdata.7e)  # NAs returned for pts not discharged yet

# str(losdata.7e)
# summary(losdata.7e)
# summary(select(losdata.7e, 6:8))
# head(losdata.7e)


# save reformatted data: ----------------
write.csv(losdata.7e, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-17_LGH_7E-LOS-reformatted.csv", 
          row.names = FALSE)




#********************************
# Cleaning IPS data: -----------
#********************************

losdata.ips <- read.csv("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-17_LGH_IPS-LOS.csv", 
                       na.strings = "NULL", 
                       stringsAsFactors = TRUE) 

losdata.ips <- clean.los(losdata.ips)  # NAs returned for pts not discharged yet

# str(losdata.ips)
# summary(losdata.ips)
# summary(select(losdata.ips, 6:8))
# head(losdata.ips)


# save reformatted data: ----------------
write.csv(losdata.ips, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-17_LGH_IPS-LOS-reformatted.csv", 
          row.names = FALSE)

