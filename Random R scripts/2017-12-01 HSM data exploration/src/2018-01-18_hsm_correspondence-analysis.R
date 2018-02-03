

#**********************************
# CORRESPONDENCE ANALYSIS ON HSM DATA - FYEAR 14-15 
#**********************************

library("tidyr")
library("ggplot2")
library("openxlsx")
library("magrittr")
library("dplyr")
library("ff")
library("data.table")
library("here")

options("fftempdir"= here("results", "output from src"))

# TODO: --------------------------
# > Name cols of df1 

#**********************************




#***************************************
# > READ IN ALL FYEAR 14-15 DATA:----------
# first use read.csv( ) to examine and recode cols as the right type of data
df1.1.hsm.fyear14_15 <- 
      read.csv(file=here("data", "2017-12-05_hsm_fyear14-15-data.csv"), 
               header=FALSE, nrow =  5)

col.class <- sapply(df1.1.hsm.fyear14_15, class)
col.class[grep("integer", col.class)] <- "factor"


# now use read.csv.ff( ): 
df1.1.hsm.fyear14_15 <- 
      read.csv.ffdf(file=here("data", "2017-12-05_hsm_fyear14-15-data.csv"), 
                    header=FALSE, 
                    colClasses = col.class)


# select cols we want: 
df1.cols <- c(3, 4, 5, 10, 11, 28:54)  # columns we want to select

df1.hsm.fyear14_15 <- df1.1.hsm.fyear14_15[ ,df1.cols]


colnames <- c( "fyear", 
            "hsm.id", 
            "pt.id",  
            "p.seg", 
            "p.seg.last.year",
        "[CYSTIC_FIBROSIS]",
        "[ALZHEIMER_DEMENTIA]",
        "[AMI]",
        "[ANGINA]",
        "[ASTHMA]",
        "[CABG]",
        "[CARDIOVASCULAR]",
        "[CKD]",
        "[COPD]",
        "[CORONARY_ANGIOGRAM]",
        "[DIABETES]",
        "[DIALYSIS]",
        "[EPILEPSY]",
        "[HEART_FAILURE]",
        "[HYPERTENSION]",
        "[IHD]",
        "[MOOD]",
        "[MS]",
        "[OSTEOARTHRITIS]",
        "[OSTEOPOROSIS]",
        "[PARKINSON]",
        "[PSYCHOSES]",
        "[PTCA]",
        "[RHEUMATOID_ARTHRITIS]",
        "[TRANSPLANT]",
        "[STROKE]",
        "[DEPRESSION_EPISODIC]")


names(df1.hsm.fyear14_15) <- colnames

# str(df1.hsm.fyear14_15)
# summary(df1.hsm.fyear14_15)
# head(df1.hsm.fyear14_15)



#**********************************************

