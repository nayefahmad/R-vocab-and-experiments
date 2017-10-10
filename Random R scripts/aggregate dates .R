
df1 <- read.csv(file.choose(), header=TRUE) 

levels(df1$SurgeryPerformedDate) # 162 disctinct days 

output <- as.data.frame(aggregate(df1$LOSDays, by=list(df1$SurgeryPerformedDate), FUN= "mean"))

View(output)
# now select and copy to excel

hist(output$x)


# try boxplot (assume that program starts at date corresponding to row 101): 
for(i in 1:100){
  output$ProgramStart[i] <- 0
}

for(i in 101:162){
  output$ProgramStart[i] <- 1
}

boxplot(output$x ~ output$ProgramStart, date=output)




# remove outliers --------------------------
df2 <- df1[df1$LOSDays<=30, ]

output2 <- as.data.frame(aggregate(df2$LOSDays, by=list(df2$SurgeryPerformedDate), FUN= "mean"))

hist(output2$x)

# try boxplot (assume that program starts at date corresponding to row 101): 
for(i in 1:100){
  output2$ProgramStart[i] <- 0
}

for(i in 101:144){
  output2$ProgramStart[i] <- 1
}

boxplot(output2$x ~ output2$ProgramStart, date=output2)

