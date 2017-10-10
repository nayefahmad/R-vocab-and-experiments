
# This script takes in a column of MRNs from excel, checks to ensure that all MRNs are 7-digit numbers (sometimes the leading 0 is dropped during copying), then outputs a character string that can be directly pasted into SQL server 


# 1. Add zeros in front of MRNs ---------------------

# select input column from a CSV file (include header in CSV) : 
# new window will open >> select file you want 
input.numbers <- read.csv(file.choose(), header=TRUE)


# function definition:
add.zero.in.front <- function(input.numbers){
  output <<- vector()
  for(i in 1:nrow(input.numbers)){
    if(nchar(input.numbers[i,1]) < 7){
      output[i] <- (paste0("0", input.numbers[i,1]))
    } else {
      output[i] <- input.numbers[i,1]
    }
  }
  writeClipboard(output)
  assign("output2", output, .GlobalEnv)
} 

# function call: 
add.zero.in.front(input.numbers)



# 2. Convert character vector to df ----------------- 
df <- data.frame()

as.vector(output2)  # output 2 is actually the input here (from section 1 above). Input any character vector (e.g. new.dates for dates)

for(i in 1:length(output2)){  # again, replace output2 with new.dates from section 3 when necessary 
  df[i,1] <- output2[i]
}

df


# Add apostrophes and commas to numbers  ---------------------------------

# input.numbers <- read.csv(file.choose(), header=TRUE) 

# function definition: 
add.apostrophes.commas <- function(input.numbers){
  v<- vector()  
  for(i in 1:nrow(input.numbers))
  {v[i] <- (paste0("'", input.numbers[i,1], "', "))
  }
  writeClipboard(paste(v, collapse=""))
  assign("finaldata", v, .GlobalEnv)
}


# function call: 
add.apostrophes.commas(df)


# Now paste in SQL Server! 




# 3. input dates from excel to R -----------------

input.dates <- read.csv(file.choose(), header=TRUE) 

new.dates <- vector()
for(i in 1:nrow(input.dates)) {
  new.dates[i] <- as.character(as.Date(input.dates[i,1], format= "%m/%d/%Y"))
}

new.dates

# now convert character vector to df (section 2), then input into function add.apostrophes.commas




# 4. generate list of hour markers to divide data by hour-long buckets: -----------
hr_starts <- seq(ISOdate(2017,1,1,0,0), ISOdate(2017,1,1,23,0), by="hour") %>% 
  substr(12,16) 

hr_ends <- paste0(substr(hr_starts, 1,2), ":59")

hr_intervals <- paste0(hr_starts, " to ", hr_ends)
writeClipboard(hr_intervals)

# data.frame(hr_starts=hr_starts, hr_ends=hr_ends)


sql_statement <- data.frame(0)

for(i in 1:length(hr_starts)){
  sql_statement[i] <- paste0("SUM(CASE WHEN StartTime BETWEEN '", hr_starts[i], 
         "'AND '", hr_ends[i], "' THEN 1 ELSE 0 END) as hour_", i, ",")
}

sql_statement <- unname(unlist(sql_statement))  #  %>% class
writeClipboard(sql_statement)


