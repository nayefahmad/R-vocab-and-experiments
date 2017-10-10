library("RODBC", lib="H:/R packages")
library("ggplot2", lib="H:/R packages")
library("labeling", lib="H:/R packages")
library("digest", lib="H:/R packages")

# rm(list=ls())

# EXAMPLE 1: -----------------
cnx <- odbcConnect("nayef_cnxn")

query1 <- "SELECT  [AdmissionSourceCode], [AdmissionSourceDescriptionHighLevel], [AdmissionFacilityCode], [AdmissionFacilityLongName], count([AdmissionSourceCode]) as [count] 
FROM [ADTCMart].[ADTC].[vwAdmissionDischargeFact]
--WHERE [AdmissionNursingUnitCode] = 'EMRG'
WHERE [AdmissionFacilityCode] = '0112' 
  AND AdjustedAdmissionDate BETWEEN '2016-01-01' AND '2017-01-01'
GROUP BY  [AdmissionSourceCode], [AdmissionSourceDescriptionHighLevel], [AdmissionFacilityCode], [AdmissionFacilityLongName]
ORDER BY [AdmissionSourceCode] ;"
# if errors: remove tabs & new lines

(data1 <- sqlQuery(cnx, query1))


# Another example: --------------------
query2 <- "IF OBJECT_ID('tempdb.dbo.#tempORvolumes ') IS NOT NULL DROP TABLE #tempORvolumes ;"

query3 <- "SELECT ContinuumID, FacilityLongName, ORType, ORRoomCode, LoggedMainSurgeonSpecialty, (CASE WHEN SurgeryPerformedDate between '2016-10-01' and '2016-10-07' THEN 1 WHEN SurgeryPerformedDate between '2016-10-08' and '2016-10-14' THEN 2 WHEN SurgeryPerformedDate between '2016-10-15' and '2016-10-21' THEN 3 WHEN SurgeryPerformedDate between '2016-10-22' and '2016-10-28' THEN 4 WHEN SurgeryPerformedDate between '2016-10-29' and '2016-11-04' THEN 5 WHEN SurgeryPerformedDate between '2016-11-05' and '2016-11-11' THEN 6 WHEN SurgeryPerformedDate between '2016-11-12' and '2016-11-18' THEN 7 WHEN SurgeryPerformedDate between '2016-11-19' and '2016-11-25' THEN 8 WHEN SurgeryPerformedDate between '2016-11-26' and '2016-12-02' THEN 9 WHEN SurgeryPerformedDate between '2016-12-03' and '2016-12-09' THEN 10 WHEN SurgeryPerformedDate between '2016-12-10' and '2016-12-16' THEN 11 WHEN SurgeryPerformedDate between '2016-12-17' and '2016-12-23' THEN 12 WHEN SurgeryPerformedDate between '2016-12-24' and '2016-12-30' THEN 13 ELSE '99999' END) AS [weeknum] INTO #tempORvolumes  FROM [ORMart].[dbo].[vwRegionalORCompletedCase] WHERE [FacilityLongName] = 'Lions Gate Hospital' AND [SurgeryPerformedDate] between '2016-10-01' and '2016-12-31'AND ORType IN ('MAIN OR') AND IsScheduled = 1 AND ORRoomcode NOT IN ('LGHCAT1', 'LGHCAT2', 'LGHLON'); "

query4 <- "SELECT *FROM tempdb.dbo.#tempORvolumes"

# RUN QUERIES: 
sqlQuery(cnx, query2)
sqlQuery(cnx, query3)
(data2<- sqlQuery(cnx, query4))

# EXAMINE DATA: -------------------------------
# str(data2)
# 
# levels(data2$ORRoomCode)
# levels(data2$LoggedMainSurgeonSpecialty)

# table(data2$ORRoomCode)
rooms <- as.data.frame(table(data2$ORRoomCode))
names(rooms) <- c("RoomCode", "Count")
rooms$dummy <- rep(260, nrow(rooms))
rooms$somevar <- sample(50, nrow(rooms))

str(rooms)

# basic column plot: 
p1 <- ggplot(rooms, aes(x=rooms$Code, y=rooms$Count)) +
  geom_bar(stat='identity')

# bells and whistles: 
p1 + geom_point(aes(x=rooms$Code, y=rooms$dummy, size=rooms$somevar)) +
  scale_size(range=c(0,10)) +  #increase range to make diffs more noticable 
  theme_minimal() +
  annotate("text", x="LGHWHS", y=15, label="1", size=10)
  
