
#******************************
# Function to return all files of certain type in a folder 
#******************************

#todo: -------------
# > how to read all text files given filenames? 

# rm(list=ls())

#***********************
# define function: ---------
find.files <- function(filepath, filetype=NA){
      # inputs: full filepath in R format, specified filetype (e.g. "R", "sql")
      # output: full list of files as df
      
      require("dplyr")
      setwd(filepath)
      
      files.df <- data.frame(file=(list.files(recursive = TRUE)))
      files.df <- mutate(files.df, 
                         file=as.character(file), 
                         type=sapply(file, 
                                     function(x){
                                           splitname <- strsplit(x, split="\\.") %>% 
                                                 unname %>% unlist 
                                           l <- length(splitname)
                                           return(splitname[l])
                                     }) %>% unname 
                         )
      
      if(is.na(filetype)==TRUE) {
            return(files.df)
      } else {
            return(filter(files.df, 
                          type==filetype))
      }
      
}


# test function: ----------
all.files <- find.files("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests")
str(all.files)
head(all.files)
write.table(all.files, file="clipboard-2048", sep="\t",  # "clipboard-2048" used 
                                                         # to set size of clipboard in Kb
                         row.names=FALSE) 


text.files <- find.files("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests", "txt")
str(text.files)
head(text.files)

# read all the text files into 1 file:
setwd("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests")
lapply(text.files$file, read.table)
