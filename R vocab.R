# __________________________________________________________________
# R VOCABULARY 
# __________________________________________________________________

library("dplyr")
library("reshape2")
library("lazyeval")
library("ggplot2")
library("labeling")
library("lubridate")


# help(package="base")
# rm(list=ls())

# TODO: -------
# duplicated, charmatch, aggregate
# use regular expressions to break filenames into type of file        
# reorder factors with forcats package 

# flatten a list: 
(l <- list(item1=4, item2=2, item3=c(100, 200)))  # surrounding with () calls print 
(unlist(l))
(unname(unlist(l)))  # returns numeric vector; also try: unlist(l) %>% unname  


# flattening a list of dates does weird stuff: 
datelist = as.list(c(ymd("2017-01-01"), ymd("2017-01-01")))
unlist(datelist)  # not good 
do.call("c", datelist)  # this is good 


# rep and seq: --------------------
rep(letters[1:3], times=2)  # same as rep(letters[1:3], 2)
rep(letters[1:3], each=2)
rep(letters[1:3], times=2, each=2)
rep(letters[1:3], times=2, each=2, length.out=2)
rep_len(letters[1:3], length.out=40)  # could be useful if you don't know the length of a vector, but you want to keep recycling it until total length reaches a target value. 
rep(letters[1:3], times=(1:3))
rep(letters[1:3], c(1,5,12))

seq(1,26)
seq(1,26,by=3)
seq(1,26, length.out=4)  # generates a sequence of length.out=4 equally spaced values from 1 to 16. (length.out is usually abbreviated to length or len, and seq_len is much faster.)
seq(along.with=month.abb)  # can abbreviate as e.g. seq(month.abb) or seq(letters); verbose alternative: seq(1, length(month.abb))
seq(along.with=rnorm(20))# generates sequence 1,2,3... ending with length(month.abb)
seq(c("apple", "orange", "banana"))


seq_along(mtcars$mpg)  # generate whole numbers from 1 to n where n in ths length of the vector inputted (useful for ID numbers, indexes, etc.)

# using seq_along in a for loop: 
means <- vector("double", ncol(mtcars))  # pre-allocate the vectors, otherwise it'll be really slow

# find mean of each column in mtcars: 
for (i in seq_along(mtcars)) {
      means[[i]] <- mean(mtcars[[i]], na.rm = TRUE)
}

means



x<-round(rnorm(20), 2); names(x) <- seq(rnorm(20)); x %>% plot(pch=16) 
x<-round(rnorm(2000), 2); names(x) <- seq(rnorm(2000)); x %>% 
  plot(pch=1) %>% abline(h=0, lwd=2.5, col="red")


any(c(rep(FALSE, 9), TRUE))  # any TRUEs in the vector? 
any(c(2<1, 4>5, 6==7))

x <- c(4,3, NA, 1,1,23)
any(is.na(x))  # is.na(x) returns a logical of same length as x 



x<- rnorm(10)
range(x)
all(x < 2)  # x<2 returns a logical vector of same length as x; all(x<2) returns TRUE only if all values are TRUE 
all(x < 2, x > (-2))
all(1<2, 2==2, 3>2)
all(c(TRUE, TRUE, TRUE, FALSE))


4 %in% c(1,1,4,1)  # TRUE 
"s" %in% letters  # TRUE
1:4 %in% 1:10  # returns vector of same length as vector on left side of %in%
c(1:4, 12) %in% 1:10
c("g", 7) %in% letters 


match("s", letters)
match(c("a", "s", "f"), letters)
x <- c(4,3, NA, 1,1,23, NA)
match(NA, x)  # Note that match() only returns first match 


which(c(rep(FALSE, 5), TRUE, rep(FALSE, 4), TRUE))  # Takes a logical vector, returns numeric pointing to positions of TRUE values 
which(c(4==5, 2==3))
which(c(4==5, 2==3, 3==3, 4<5, 9>4))


x <- rep(1.0,10); y <- rep(0.99999999999, 10)
all.equal(target=x, current=y, tolerance = 10^-10)
all.equal(target=x, current=y, tolerance = 10^-20)


identical(2, 2.0)  # TRUE 

set.seed(1)
x <- rnorm(10)
set.seed(1)
y <- rnorm(10)
identical(x,y) # TRUE 


stopifnot(c(TRUE, TRUE))
stopifnot(c(FALSE, TRUE))  # returns an error if not all TRUE 
stopifnot(c(2<3, 4>5))


x <- airquality[, -1] # x is a regression design matrix
y <- airquality[,  1] # y is the corresponding response
stopifnot(complete.cases(y) != is.na(y))  # complete.cases returns a logical 
ok <- complete.cases(x, y)  # returns logical of same length as num rows of x and y
sum(!ok) # how many are not "ok" ? NOTE that "!" on a logical gives its inverse: !c(TRUE, FALSE) = c(FALSE, TRUE)
x <- x[ok,]
y <- y[ok]


# subsetting: -----
# 3 ways of subsetting a df for first column:
airquality[1]  # returns data frame (which is a type of list; subsetting list with [] always gives list)
airquality[[1]]  # returns the *contents* of element 1 in the df - integer atomic vector. [[]] can only return 1 thing 
airquality$Ozone  # returns integer atomic vector. Note that $ is shorthand, where x$y = x[["y", exact=FALSE ]]



cumsum(1:10)  # pseudocode: for i in 1:10, result[1] = i, result[2]=i+result[1]
cumprod(1:10)
cummin(1:10)  # along the entire length of input vector, 1 is min 
cummax(10:1)


# row and column sums, means: ---------------
x <- data.frame(x=rnorm(10)*10, y=rnorm(10)) %>% print 
colSums(x)
rowSums(x) %>% data.frame()

apply(x, 1, mean)  # means of rows 
apply(x, 2, mean)  # means of columns 


# using sweep( ):-------------

# args: x, matrix/array input
#     margin = 1 for sweeping col ACROSS along rows ,
#                 2 for sweeping row DOWN along column 
# returns an array of same shape, but with "summary stats swept out" 

# example 1 : 
Product <- c("A", "B", "C", "Total")
Continent <- c("Africa", "America", "Asia", "Australia", "Europe")
values <- c(0.4, 0.2, 0.4, 0.1, 0.3, 0.4, 0.3, 0.4, 0.5, 0.2, 
            0.3, 0.2, 0.4, 0.3, 0.3, 0.1, 0.4, 0.4, 0.2, 0.2)

M <- matrix(values, ncol=5, dimnames=list(Product, Continent)) %>% print 

# create new matrix using sweep(): 
swept.M <- sweep(M[1:3, ],  # use only top 3 rows as input 
                 2,  # margin = 2 to sweep a row DOWN along the columns 
                 M[4,],  # stats = the summary stat to be swept out 
                         #    you could use a fn like colSums( ) 
                 
                 # specify fn as multiplication: 
                 "*"  ) %>% print
# note that swept.M is 3 x 5, just like M[1:3, ]


# example 2: 
input.matrix <- matrix(rpois(6, 30), 
                       ncol = 2)
colnames(input.matrix) <- c("Cat A", 
                            "Cat B") 

proportions.matrix <- sweep(input.matrix, 
                            1,  # margin =1 to sweep a column ACROSS along rows 
                            rowSums(input.matrix), 
                            "/") %>% print 



# first differencing: -----------
diff(1:10, lag = 1, differences = 1)  # diff gives differecnes between successive elements in a vector 
diff(seq(1,50, by=5), lag=1, differences=1)
(diffs1 <-diff(c(1,5,2,2,10,.5,-1,3,-4,30), lag = 1, differences = 1))
length(diffs1)

# second differencing: 
(diffs2 <-diff(c(1,5,2,2,10,.5,-1,3,-4,30), lag = 1, differences = 2))  
diffs2 == diff(diffs1, lag = 1, differences = 1)  #  setting arg differences > 1 means you recursively apply diff a specified number of times 
length(diffs2)  # note that this has one less element than diffs1

# differencing at lags (i.e. not immediately successive elements)
(diffs3 <-diff(c(1,5,2,2,10,.5,-1,3,-4,30), lag = 2, differences = 1))  
length(diffs3)


# removing duplicate rows from a df: -----------
# path.join.filter.dedup <- path.join.filter[!duplicated(path.join.filter), ]


# cut a range to get a factor: -------------------
x <- 1:100 
cut(x, breaks=seq(0,100,5), labels= paste("Level", 1:20))  # divide range of x into levels of a factor; 
# note that length of labels is 1 less than length of breaks
# also, first value of breaks is lower than min value of x 
table(cut(x, breaks=seq(0,100,5), labels= paste("Level", 1:20)))



# reorder factor levels using values in another column -------------
# todo: update with package forcats
# first create data: 
df <- data.frame(area=letters[1:10], 
                 total=c(10,2,1,rnorm(6),20)); df

# graph looks ugly: 
ggplot(df, aes(x=area, y=total)) + geom_bar(stat="identity")

# find order of factor levels that you want: 
order(df$total)  # this gives ascending order; for descending, rev(order(df$total))

# reset factor levels: 
df <- df %>% mutate(area=factor(area, 
                          levels=area[order(df$total)])); df

str(df)  # factor levels reset

# graph again: 
ggplot(df, aes(x=area, y=total)) + geom_bar(stat="identity")  # SUCCESS!


# if there is an obvious way to order your factor levels (e.g. alphabetical), 
# you can try using ordered(): 
# create a factor with default ordering: 
ordered(rep(1:12, length=24))
ordered(c("blue", "green", "yellow"))
ordered(paste0("level_", 1:30))  # doesn't work well 

# let's try again: 
two.digits.numbers <- 
      sapply(1:30, function(x){
            x.char <- as.character(x)
            if (nchar(x.char) < 2){
                  x <- paste0("0", x)
            } else {
                  x <- x.char
            }
            return(x)
      }
      )

ordered(paste0("level_", two.digits.numbers))  # works! 

#**************************************************





#**************************************************
pmin(1:4, 0:3)  # gives "parallel" min - min of each pair of elements, where one element is from vector1, the other from v2. 

x <- sort(rnorm(100));  cH <- 1.35
quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE)
# quantiles are cutpoints dividing the range of a probability distribution into contiguous intervals with equal probabilities. 
# quartiles are the three cut points that will divide a dataset into four equal-size groups
pmin(cH, quantile(x)) # no names
pmin(quantile(x), cH) # has names
plot(x=x, y=pmin(cH, pmax(-cH, x)), type = "b", main =  "Huber's function")



# & and && --------
x <- 1 
y <- 1

if (x==1 & y==1){print("yes")} else {print("no")}
if (x==1 && y==1){print("yes")} else {print("no")}
# when x and y are single element vectors, doesn't make a difference 

x <- c(1,1) 
y <- c(1,0)

if (x==1 & y==1){print("yes")} else {print("no")}
# "single &" returns warning: only first element used. Compare with: 
x==1 & y==1  # no warning, but this output can't be used in an if()
               # condition easily 

if (x==1 && y==1){print("yes")} else {print("no")}
# && evaluates left to right examining only the first element of each vector
# Hence, && is preferred in if conditions 

# compare: 
x==1 && y==1
x[2]==1 && y==1  # comparing second element of x (which is first element
                 # of x[2]) with first element of y


# merge: -------------------------------
# merging vectors (doesn't make much sense): 
x <- letters
y <- letters[3:4]
merge(x,y)  # prob not what you want: this is the cartesian product (?) of the 2 sets 


# MERGING DATA FRAMES: -----------
# if the column you're merging on has the same name in both dfs, there's no need to 
# specify by="" argument, because it is automatically interpreted that you are joining on that column 

x <- data.frame(a=letters, b.x=rnorm(26)) %>% print 
y <- data.frame(a=c(letters[3:4], "test"), d.y=rnorm(3), e.y=rep("yo", 3)) %>% print 

merge(x,y)  # inner join; 
merge(x,y, all=TRUE)  # outer join
merge(x,y, all.y=TRUE)  # right join
merge(x,y, all.x=TRUE)  # left join
merge(x,y, all.x=TRUE, all.y=TRUE)  # outer join again




# if the column you're merging on has the diff name in the 2 dfs, you need to specify the 
# by.x and by.y arguments 

x <- data.frame(a=letters, b.x=rnorm(26))
y <- data.frame(a.y=c(letters[3:4], "test"), d.y=rnorm(3), e.y=rep("yo", 3))

merge(x,y, by.x="a", by.y="a.y")  # INNER JOIN 
merge(x,y, all=TRUE, by.x="a", by.y="a.y")  # OUTER JOIN 
merge(x,y, all.y=TRUE, by.x="a", by.y="a.y")  # RIGHT JOIN
merge(x,y, all.x=TRUE, by.x="a", by.y="a.y")  # LEFT JOIN 


#... another example: -----------------
authors <- data.frame(
      surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
      nationality = c("US", "Australia", "US", "UK", "Australia"),
      deceased = c("yes", rep("no", 4)))
books <- data.frame(
      name = I(c("Tukey", "Venables", "Tierney",
                 "Ripley", "Ripley", "McNeil", "R Core")),
      title = c("Exploratory Data Analysis",
                "Modern Applied Statistics ...",
                "LISP-STAT",
                "Spatial Statistics", "Stochastic Simulation",
                "Interactive Data Analysis",
                "An Introduction to R"),
      other.author = c(NA, "Ripley", NA, NA, NA, NA,
                       "Venables & Smith"))

(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
(m2 <- merge(books, authors, by.x = "name", by.y = "surname"))


#... merging on dates: --------------
admits <- data.frame(dates = as.Date(c("2017-01-12", "2017-01-15")), 
                     num_cases = c(450,12))
census <- data.frame(dates = as.Date(c("2017-01-12", "2017-01-31")), 
                     num_pts = c(14030, 9912))
date_string <- data.frame(dates = seq(as.Date("2017-01-01"), as.Date("2017-01-31"), by="1 days")) 

# column to match on has same name in all 3 dfs, so no need to specify "by=" argument 
finaldf <- merge(date_string, admits, all.x=TRUE) %>% 
                  merge(census, all.x=TRUE) %>% print 

# if appropriate, replace NAs with zeroes: 
finaldf <- mutate(finaldf, 
                  num_cases=sapply(num_cases, function(x) if(is.na(x)==TRUE){0} else {x}), 
                  num_pts=sapply(num_pts, function(x) if(is.na(x)==TRUE){0} else {x})
                  ) %>% print 

finaldf.m <- melt(mutate(finaldf, dates=as.character(dates))) %>% print 
dcast(finaldf.m, dates ~ variable)  # reverses the melting operation 
dcast(finaldf.m, variable ~ dates)  # reverse rows and columns 

# ... merging with dplyr::right_join: --------
# path.join.phn <- right_join(path, path.waitlist.data,
#                             by =c("mrn" = "mrn.w", 
#                                   "ad.date.adtc" = "ad.date"))


# sort and order: ------------------
x <- c(1,1,3,-1,0,19,1,9,7,32)
sort(x)
sort(x, decreasing=TRUE)
rev(x)  # just reverses arguments 

order(x)  # returns an ordered numeric vector: order(x)[1] returns the position in x where the smallest element in x is found 
x[order(x)]  # arranges x in ascending order 


# Run-length encoding (RLE) is a very simple form of lossless data compression in which runs of data (that is, sequences in which the same data value occurs in many consecutive data elements) are stored as a single data value and count, rather than as the original run
x <- rev(rep(6:10, 1:5))
rle(x)
class(rle(x))  # type of list
rle(x)[1]
unname(unlist(rle(x)[1]))

z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
rle(z)
rle(as.character(z))

x <- c(rep("a", 4), "c", rep("g", 10))
unique(x)

# TABLE ----------------------------------------------
df <- data.frame(a=sample(100,10), 
                 b=sample(2000, 10))
table(df)  # you can call table() on purely numeric data, but it's kinda useless 
table(df$a)  # still useless 

df$c= sapply(1:10, function(x) sample(c("Group1", "Group2"), 1, replace=TRUE))
df

table(df)  # still pretty useless 
table(select(df, a,c))   # possibly useful: this converts the data from "long" to "wide" format; compare with > select(df[order(df$a), ], a,c)
table(select(df, b,c))  # possibly useful 
table(df$c)  # probably the best use of table() for this data 


# Simple frequency distribution:
table(rpois(100, 5))  # n=100, lambda=5
table(state.division, state.region)

with(airquality,
     table(OzHi = Ozone > 80, Month, useNA = "ifany"))  # with() seems to be a 
# precursor to the pipe operator (%>%)


# copy table from R to Excel: ---------------------
df <- data.frame(a=rnorm(20), b= sample(c(TRUE, FALSE), 20, replace=TRUE)) # %>% print
write.table(df, file="clipboard", sep="\t", row.names=FALSE)  # now paste in Excel 

# copy via clipboard from excel to R: 
#select column in excel %>%
x <- readClipboard()


# ifelse() : ------------------------------------
x <- c(6:-4)
sqrt(x)  #- gives warning
sqrt(ifelse(x >= 0, x, NA))  # no warning

## Note: the following also gives the warning !
ifelse(x >= 0, sqrt(x), NA)

l <- sample(c("a", "b", "c"), 1)
ifelse(l >= "b", "high", "low")


# DATES --------------------------------------------
# ...create strings of dates ----
seq(ISOdate(2017,1,1), ISOdate(2017,1,31), by="day") %>% substr(1,11)  # Jan to Feb 

seq(ISOdate(2017,1,1), ISOdate(2017,1,31), by="7 day") %>% 
  substr(1,11)  # only week start dates 

seq(ISOdate(2017,2,1), ISOdate(2017,1,1), by="-1 day") %>% substr(1,11)  # Feb to Jan 

# Note: ISOdate() is a wrapper for strptime()


# Alternatively, you can use as.Date: 
seq(as.Date("2017-01-01"), as.Date("2017-04-05"), by="1 day")

dates <- seq(as.Date("2017-01-01"), as.Date("2017-04-05"), by="7 day")  # only first day of weeks 
date_and_wkday <- data.frame(dates = dates, 
                             days = weekdays(dates)) %>% print 




# ...identify fiscal year from date : -------------------
x <- data.frame(date = c("2013-01-01", "2013-06-06", "2015-02-28", "2015-09-04"), 
                m=rep(NA,2), 
                y=rep(NA,2), 
                fyear = rep(NA,2)); x

for(i in 1:nrow(x)){
      x$m[i] <- substr(x$date[i], 6,7) %>% as.numeric
      x$y[i] <- substr(x$date[i], 1,4) %>% as.numeric
      if(m[i] %in% 4:12){
            x$fyear[i] <- x$y[i]+1
      } else {
            x$fyear[i] <- x$y[i]
      }
}
print(x)

# without extra columns for date and year: 
x <- data.frame(date = c("2013-01-01", "2013-06-06", "2015-02-28", "2015-09-04"), 
                fyear = rep(NA,2)
                ); x

for(i in 1:nrow(x)){
      m <- substr(x$date[i], 6,7) %>% as.numeric
      y <- substr(x$date[i], 1,4) %>% as.numeric
      if(m %in% 4:12){
            x$fyear[i] <- y+1
      } else {
            x$fyear[i] <- y
      }
}
print(x)



# using lubridate package: ----------------------------------------
dates <- c("2017-01-01", "2017-02-01", "2017-08-21")

class(dates)  # "character"


ymd(dates) # %>% class  # "Date"
# this function takes a char and converts to date, using the ymd format to 
# do the conversion. It DOESN'T *cast* to ymd format 

mdy(dates)  # doesn't work 
dates2 <- c('12-31-2017'); class(dates2)
mdy(dates2); class(mdy(dates2))

# convert to time in format hh:mm
timechar <- "12:30"
time <- strptime(timechar, format="%R"); time2
time <- strptime(timechar, format="%I"); time2


# convert ymd to mdy: 
ymd(dates) %>% format('%m%d%Y')

# arithmetic with dates and times: 



# SET OPERATIONS ---------------------------------------------

setA<-c("a", "b", "c", "d", "e")
setB<-c("d", "e", "f", "g")

union(setA,setB)
intersect(setA,setB)
setdiff(setA,setB)  # Note, however, that the difference between two sets is order-dependent. It is the material that is in the first named set, that is not in the second named set. 
setequal(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA)), 
         union(setA,setB))


# SAMPLE() --------------------------------------------------
sample(10)  # gives permutation of all elements; same as sample(1:10)
sample(10, 1) 
(x <- sample(10, replace=TRUE)); setdiff(1:10, x)
sample(letters, 3)

sample(c("a", "b"),1, prob=c(.9, .1))  # The optional prob argument can be used to give a vector of weights for obtaining the elements of the vector being sampled. They need not sum to one, but they should be non-negative and not all zero.

# create a discrete distribution with specified probabilities:
v <- vector()
for(i in 1:1000){
  v[i] <- sample(c(TRUE, FALSE, "error"), 1, prob=c(.90, .09, .01))
}

table(v) %>% barplot  # plotting with base R 

# plotting with ggplot: 
dat <- as.data.frame(table(v)) %>% melt %>% select(1,3)
ggplot(dat, aes(x=v, y=value)) + 
  geom_bar(stat="identity") + 
  geom_hline(yintercept=0, size=1)



# COMBINATIONS --------------------------------------------------
choose(3,2)  # number of combinations of 2 elements that can be chosen from a set of 3 elements 
choose(10,1); choose(10,2); choose(30,2)

# counting num permutations: 
n <- 10
k <- 5 
choose(n,k)*factorial(k)

par(mfrow=c(2,1))
# graph of num combinations 
sapply(1:10, function(x) choose(10, x)) %>% 
  plot(type='l', main="num permutations, combinations of x objects from a total of 10 objects", 
       xlab = "x", 
       ylab = "num combinations")

# graph of num permutations 
sapply(1:10, function(x) choose(10, x)*factorial(x)) %>% 
  plot(type='l', 
       xlab = "x", 
       ylab = "num permutations")
par(mfrow=c(1,1))


# sample with/without replacement: -----------
sample(10)  # same as sample(1:10); default is replace=FALSE 
sample(10,2)
sample(10,100, replace=TRUE) 
sample(c("hi", "bye"), 30, replace=TRUE)

# simulate 100 rolls of a die: 
sample(6, 100, replace=TRUE) %>% mean
sample(6, 100, replace=TRUE) %>% mean


# find all files in a folder and all subfolders: ---------
setwd("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings")
list.files()
list.files(recursive = TRUE)
writeClipboard(list.files(recursive = TRUE))  # paste in notepad

files.df <- data.frame(file=as.character(list.files(recursive = TRUE)))
files.df <- mutate(files.df, 
                   file=as.character(file), # for some reason as.character 
                                              # didn't work above  
                   type=sapply(file, 
                               function(x){
                                     splitname <- strsplit(x, split="\\.") %>% 
                                                unlist %>% unname 
                                     l <- length(splitname)
                                     return(splitname[l])
                               }
                   ))

# examine structure: 
str(files.df)
head(files.df, 10); tail(files.df, 10)  

# get only R files: 
r.files <- filter(files.df, 
                  type=="R") %>% print


# FILL CONTAINER WITH LOOP: ----------------------
# create an empty container and fill it using a loop. We require that any type and any number of objects can be placed in the container, regardless of whether they are numeric or character or logical: 
container <- vector(mode="list")
# container <- list()  # this also seems to work? 

for(i in 1:10){
  x <- rnorm(1)
  print(paste("x=", x))
  if (x < 0.01){
    container[i] <- list(1:10)    
  } else if (x < 0.8) {
    container[i] <- list(rnorm(i))
  } else {
    container[i] <- list(rep("test", i))
  }
}

str(container)
summary(container)



# BUILD A DATAFRAME WITH A LOOP: --------------
# set up empty list: 
datalist = list()

for (i in 1:5) {
      # ... make some data
      dat <- data.frame(x = rnorm(10), y = runif(10))
      dat$i <- i  # maybe you want to keep track of which iteration produced it?
      datalist[[i]] <- dat # add it to your list; NOTE THAT WE USE [[]] !!!!
}

datalist  # each element of hte list is a dataframe 
# compare datalist[1] with datalist[[1]]; first is a list, second is contents 
      # within that list 

# join elements of list into a single df with do.call()
big_data = do.call(rbind, datalist) %>% print 

# alternative without using do.call: 
df1 <- datalist[[1]]
for(i in 2:5) {df1 <- rbind(df1, datalist[[i]])}
df1

identical(big_data, df1)  # TRUE 

# LAPPLY AND FRIENDS: -------------------------------
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE)) %>% print 

# > lapply: -------------
# compute the list mean for each list element
lapply(x, mean)
# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
# Format for lapply is lapply(X, FUN, ...). Note that "probs=1:3/4" is being passed to "..." above, and is "passed to" the function quantile(). 

# > sapply and vapply ------
sapply(x, quantile)
i39 <- sapply(3:9, seq) # list of vectors
sapply(i39, fivenum)  # five number summary as a vector 
vapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))  
#^^ 3rd arg is FUN.VALUE: a vector template for the return value from FUN
# vapply is very similar to sapply, but with predefined format for output 




# > using apply to find row/column means -------
apply(x, 1, mean)  # means of rows 
apply(x, 2, mean)  # means of columns 

# > mapply: --------------
# mapply is a multivariate version of sapply
mapply(rep, x = 4:1, times=4:1)
# each iteration, rep takes 2 args one from the first arg from mapply, one 
#     from the second 

mapply(rep, times = 1:4, MoreArgs = list(x = 42))
# MoreArgs is used to pass arguments that are NOT "vectorized over"...? 
mapply(rep, times = 1:4, x=list(x = 42))  # what's the difference? 

mapply(function(x, y) seq_len(x) + y,  # define function to be applied 
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))
# pseudocode: create sequence of length a=1, then add A=10 to EACH element; 
#             create sequence of length b=2, then add B=0 

rnorms <- data.frame(n=rep(100,2), 
                     mean=c(4,50), 
                     var=c(.5, 3)) %>% print 
mapply(rnorm,
       n=rnorms$n, 
       mean=rnorms$mean, 
       sd=sqrt(rnorms$var)) %>% as.data.frame 

# **********************************
(v <- structure(10*(5:8), names = LETTERS[1:4], 
                types=c("red", "yellow", "blue", "green")))
# structure() returns the given object with further attributes that you can 
# set 

# not entirely sure what's happening here: 
f2 <- function(x, y) outer(rep(x, length.out = 3), y)
(a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))
a.2 <- vapply(v, f2, outer(1:3, 1:5), y = 2*(1:5))
stopifnot(dim(a2) == c(3,5,4), all.equal(a2, a.2),
          identical(dimnames(a2), list(NULL,NULL,LETTERS[1:4])))

# ...sampling distribution of sample mean of exp r.v.: ----
replicate(10000, mean(rexp(10))) %>% hist

# using ggplot: 
df <- replicate(10000, mean(rexp(10))) %>% as.data.frame  
colnames(df) <- "value"

# set a theme variable that can be used for graphs later: 
mytheme <- theme(axis.text.x=element_text(size = 16),
                 axis.text.y=element_text(size=14),
                 
                 axis.title.x=element_text(size=20), 
                 axis.title.y=element_text(size=18), 
                 
                 strip.text.x=element_text(size=18), 
                 
                 panel.background = element_rect(fill = NA))

# without setting theme: 
p1 <- ggplot(df, aes(x=value)) + 
  geom_histogram(aes(y=..count..)) + # not really necessary to specify 
  # aes(y=..count..), but good to be explicit. You can just give 
  # "+ geom_histogram() +"  ...
  scale_x_continuous(limits=c(0,3), 
                     labels=seq(0,3, .25), 
                     breaks=seq(0,3, .25), 
                     expand = c(0,0)) +  # used to remove space between 
                                         # axis and labels  
      scale_y_continuous(expand = c(0,0)) + 
      
      geom_hline(yintercept=0, size=1) + 
      geom_vline(xintercept = 0, size=1);p1

# with theme: 
p1 + mytheme

# add density: 
(p2 <- p1 + geom_density(aes(y=..density..*900), size=1, colour="blue", alpha=.8))

p3 <- ggplot(df, aes(x=value)) + 
  geom_density(aes(y=..density../sum(..density..))); p3 
#   geom_histogram(aes(y=..density../sum(..density..)), fill=NA, colour="black")


# summary(p1); summary(p2)


# ... use lapply to create multiple distribution curves: -----

# create input vector: 
input=seq(-4,4, length=1000)

# define dist function family: X~N(0, 1/n): 
fx <- function(n, input_vector){
      dnorm(input_vector, mean=0, sd=(1/sqrt(n)))  # change to pnorm() to get CDFs 
}

# create data frame with x and f(x) values for different values of n: 
normal_curves <- lapply(seq(1:20), fx, input_vector=input) %>% 
      # "input_vector=x" => specify that second arg of fx always 
      # takes value x
      as.data.frame

colnames(normal_curves) <- paste0("n", 1:20)
normal_curves <- mutate(normal_curves,
                        x=input) %>%  # x is input vector
                  select(21,1:20)

str(normal_curves)

# putting data in long form to allow facetting by value of n: 
normal_curves_long <- melt(normal_curves, id.vars = "x")
str(normal_curves_long)

p1 <- ggplot(normal_curves_long, aes(x=x, y=value, group=variable)) + 
      geom_line() + 
      geom_text(data=filter(normal_curves_long,
                            x>=1 & x<=1.01),  # filtering to get single point from each                                                  curve 
                aes(label=variable), 
                col="red") ; p1 

# splitting data frames with split( ) :  --------------------------
split(mtcars, mtcars$cyl)

# > split on more than 1 var: approach 1: ------
list1.mtcars.split <- split(mtcars, 
                            list(mtcars$cyl, 
                                 mtcars$hp))
list1.mtcars.split  
# note that this takes all combinations of cyl and hp, even if 
#     there are no rows that correspond to a particular 
#     combination (e.g. no cars with 6 cyl and hp=52)

# > split on more than 1 var: approach 2: ------
mtcars2 <- mutate(mtcars, 
                  split.var = paste0(cyl, hp)) %>% 
      mutate(split.var = as.factor(split.var))
str(mtcars2)

list2.mtcars.split <- split(mtcars2, 
                            mtcars2$split.var)
list2.mtcars.split
# note that here there are no elements of the list that have 9 rows 

#***********************************************
# > example from split( ) help: ------------ 
require(stats); require(graphics)
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))  # 1000 integer values between 0:10  
x <- rnorm(n * nn) + sqrt(as.numeric(g))  # 1000 random values 
xg <- split(x, g)  %>% print  # note that each vector is of a different length 
boxplot(xg, col = "lavender")  # when g is large, sqrt(g) is large, so x values 
      # are higher 

sapply(xg, length)  # how many values in each group? lapply() %>% unlist( )
      # also works: 
# lapply(xg, length) %>% unlist
sapply(xg, mean)  # mean x value by level of g
# lapply(xg, mean) %>% unlist 


### Calculate 'z-scores' by group (standardize to mean zero, variance one)
z <- unsplit(lapply(split(x, g), scale), g)

# same thing, using pipes: 
z <- split(x,g) %>%  # split by levels of g 
      lapply(scale) %>%  # apply function scale() to each component 
      unsplit(g)   # combine back into a vector
plot(z)



# and check that the within-group std dev is indeed one:  
tapply(z, g, sd)  
# Interpretation: tapply takes 2 vectors, *splits* the first according to levels of 
# the second (thus creating a "ragged array"), then it *applies* a function to 
# each component, then it *combines* the results back into a vector.


# scale(x, center = TRUE, scale = TRUE): -----------
# If center is TRUE then centering is done by subtracting the column 
# means (omitting NAs) of x from their corresponding columns, and if 
# center is FALSE, no centering is done. 
# You can also supply a numeric vector of same length as num cols of x 
# to specify a particular value to subtract from each column entry. 
# 
# If scale is TRUE then scaling is done by dividing the (centered) 
# columns of x by their standard deviations if center is TRUE, and 
# the root mean square otherwise.



# Compound assignment pipe operator: ---------
# let's say we want an input vector of norm rvs, but it's lenght has to also be 
# a POISSON rv. We start by assigning the lenght, then generate a rnorm vector of 
# that length: 

# > approach 1: --------
set.seed(111)
(input <- rpois(1, 10))

# now update input
input <- input %>% rnorm %>% print 

# > approach 2: ----------
input %<>% rnorm %>% print 


# the "tee pipe" operator: --------------------------------------------

# using "tee pipe" (%T>%) 
rnorm(200) %>%
      matrix(ncol = 2) %T>%
      plot %>% 
      # call plot(), but don't take the result of plot through the rest of the 
      # pipe. 
      # Instead, the thing that was passed to plot( ) is piped through
      colSums


# without the tee pipe (doesn't work): 
rnorm(200) %>%
      matrix(ncol = 2) %>%
      plot %>% 
      colSums  # doesn't work because you can't call colSums on a plot 




# Pattern Matching and Replacement -------------------------------
# grep(value = FALSE) returns a vector of the indices of the elements of x that yielded a match (or not, for invert = TRUE. This will be an integer vector unless the input is a long vector, when it will be a double vector.                                                                                            

# grep(value = TRUE) returns a character vector containing the selected elements of x (after coercion, preserving names but no other attributes).

# grepl returns a logical vector (match or not for each element of x).

grep("[a-z]", letters)
grep("May", month.abb, invert=TRUE)  # find months that are NOT "May"
grep("wed", tolower(c("Monday", "Tuesday", "Wednesday")))
grep(c("wed", "mon"), tolower(c("Monday", "Tuesday", "Wednesday")))  # In grep(c("wed", "mon"), tolower(c("Monday", "Tuesday", "Wednesday"))):argument 'pattern' has length > 1 and only the first element will be used

charmatch(c("mon", "wed"), tolower(c("Monday", "Tuesday", "Wednesday", "Monday")))
# note that charmatch can take more than one element in its first argument. However, its rules for return values are a bit strange: 
      # if unique exact match OR (unique partial match and no exact match) -> 
            # match index returned
      # if multiple exact OR multiple partial -> 0 returned 
      # if no match, then nomatch argument value returned 
      # Thus, charmatch can be useful in seeing whether a value can be used 
            # as a unique identifier?? 

grep("o", letters)  # 15
grep("o", "food")  # 1
grep("o", c("food", "wood"))  # 1,2 
grep("o", c("food", "wood"), value=TRUE)
grepl("o", c("food", "wood"))

# note that "food" is a single element, which does contain an "o", so we get 1 returned. The 2 "o"s in food are not counted seperately. To do that, try: 

countOs <- function(x){
  require("dplyr", lib="H:/R packages")
  
  x.split <- strsplit(x, split=NULL) %>% unlist 
  
  returnOs <- function(x1){
    x2 <- tolower(x1)
    if(x2=="o"){1}
    else{0}
  }
  
  sapply(x.split, returnOs) %>% sum
}

# test function: 
test_countOs <- function(){
  sapply(list("hi", "hO", "OOoo", "yo", "only one", "frib", "N00", "123"),
         countOs)
}
test_countOs()

# apply the function: 
countOs("food")  # 2
countOs("Markov chains were introduced by Andrei Andreevich Markov (1856-1922) and were named in his honor.")  # 5 

# note how the function "countOs" promotes modularity and abstraction in the code: it is completely self-contained, so it doesn't need to refer to a function or object defined anywhere else, so you can use it in any environment. Just give it the input it expects and it will give you the output you expect. This is how you keep code from getting ridiculously complicated and interconnected. 

gsub("g", "Goose", letters)
gsub("g", NA, letters)

strings <- c("foo", "foo", "bar", "foo", "qux")
sub("foo", "yib", strings) 
gsub("foo", "yib", strings) 
# what's the difference between sub and gsub? 

# FUNCTIONS ------------------------------------------

# Use of "...": --------------------------------------
f <- function(...){
  list(...)
}
# f takes a variable number of arguments (i.e. user can put in anything they like there), then just lists what those arguments were. 


# TEST CASES: 
# function call: 
f(x=2, l=TRUE)

# Another function: 
f2 <- function(x,y, ...){
  print(x/y)
  otherArgs <- list(...)
  print(otherArgs)
  if (length(list(...)) > 0){
      if (any(unlist(list(...)))){
          print("At least one TRUE in otherArgs")
      } else {
          print("No TRUEs in otherArgs")
      }
  } else {
      print("No TRUEs in otherArgs")
  } 
}

# function call:
f2(1,5,TRUE, FALSE)
f2(1,5)
f2(1,5,FALSE, FALSE)

# Using ... in a useful way: ----------------------------------

# write a function that drops extreme values, then returns quantiles:
# in general, this approach could be useful because you can build in several steps into a single function (step1: drop extreme values, step2: calculate quantiles). However, you need a way to pass in arguments that will be used in later steps. That's what "..." is for. 

x <- rnorm(50)

f3 <- function(x){
  x2 <- sort(x)
  lower.limit <- quantile(x2, prob=.1)
  upper.limit <- quantile(x2, prob=.9)
  x3 <- x2[x2>lower.limit & x2<upper.limit]
  quantile(x3)
}

f3(x)  # compare with quantile(x)
f3(x, probs=c(.25, .5, .75))  
# Error in f3(x, probs = c(0.25, 0.5, 0.75)): unused argument (probs = c(0.25, 0.5, 0.75))

# To prevent the error, we need to rewrite f3 to allow it to take extra arguments: 

f4 <- function(x, ...){
  x2 <- sort(x)
  lower.limit <- quantile(x2, prob=.1)
  upper.limit <- quantile(x2, prob=.9)
  x3 <- x2[x2>lower.limit & x2<upper.limit]
  otherargs <- list(...)
  
  if(!is.null(otherargs)){
#     print(otherargs)  # note that this is just printed, not returned by the function
  }
  
  if(!is.null(otherargs$probs)){
#     print(quantile(x3, probs=otherargs$probs))
    quantile(x3, probs=otherargs$probs) %>% unlist
  } else {
#     print(quantile(x3))
#     print("default probs used")
    quantile(x3) %>% unlist
  }
}

# FUNCTION CALLS: 
f4(x)  # compare with quantile(x)
f4(x, probs=c(.08, .35, .57, .75, .99))  # compare with quantile(x, probs=c(.08, .35, .57, .75, .99))
f4(x, probs=c(.08, .35, .57, .75, .99), na.rm=TRUE)

# note what we can do with function f4: 
vectors <- vector(mode="list")
for(i in 1:10000){
  vectors[i] <- list(rnorm(500))
}

result <- lapply(vectors, f4, probs=c(.08, .35, .57, .75, .99), arg=NULL) %>%
  as.data.frame
names(result) <- paste0("sample_", 1:length(vectors))
str(result)
summary(result)
# result

result.long <-  melt(result) %>% 
  mutate(probs=as.factor(rep(c(.08, .35, .57, .75, .99), length(vectors)))) %>% 
  select(variable, probs, value)
str(result.long)
head(result.long, 100)

# quick analysis: 
boxplot(result.long$value)
filter(result.long, probs==.08) %>% select(value) %>% boxplot 
plot(value~probs, data=result.long)

# using ggplot: 
p1 <- ggplot(result.long, aes(x=probs, y=value)) + 
  geom_boxplot()  
#   facet_wrap(~probs, ncol=2)
p1


p08 <- filter(result.long, probs==.08) %>% select(value) %>% unlist
p57 <- filter(result.long, probs==.57) %>% select(value) %>% unlist

plot(p57~p08)

# ...rationale-----------------------------------------------------------
# without f4, how would you do this? First loop over vectors to remove extremes, then loop over the result to calculate quantiles. Okay, but what if there were e.g. 10 intermediate steps requiring a different function for each? It would be very inelegant to have to write 10 loops and keep passing the result from one to the next. 

# Instead, we can put all the intermediate functions inside f4, and pass their arguments to them through f4!!! 



# SAVING R OBJECTS TO FILE: ---------------------------------
# let's say it takes 30 min to read in a csv. You don't want to do that 
#     every time. So, read in once, save as an R object that you can 
#     quickly load later. 

# 2 options: 
#     > save( )         : produces Rdata (aka Rda) file 
#     > saveRDS( )      : produces RDA file 

# apparently saveRDS is better, because when you load, you can edit the 
#     object?? 

toyota.rows <- grep("Toyota", rownames(mtcars))
mtcars[toyota.rows, ] %>% 
      saveRDS("mtcars-toyota.rds")


# now read it in with readRDS( ): 
toyo <- readRDS("mtcars-toyota.rds")

rownames(toyo)[1] <- "BEST CAR EVER"
print(toyo) 






# RESHAPE PACKAGE:  ---------------------------------------------
# Melting: going from wide to long 
head(airquality)
aql <- melt(airquality) # [a]ir [q]uality [l]ong format; no ID vars
head(aql)
aql[c(1:5, 154:158, 307:311), ]  

# using Month and Day as ID vars: 
aql <- melt(airquality, id.vars = c("Month", "Day"), 
            variable.name = "climate_variable", 
            value.name = "climate_value")
head(aql)  # note that long data is grouped by measured variable name, and that id vars repeat after all data on one single measured variable name: e.g. month=5, day=1 will appear next to "Ozone" var, then again next to "Solar.R". This is how rows/tuples for particular (Ozone, Solar.R, etc.) values are recorded; otherwise there would be no way to know which values were associated with others. 

# only id vars get column names named after them. All other vars are in the "variable" column, which we've renamed to "climate_variable"
summary(aql)
aql[c(1:5, 154:158, 307:311, 460:464), ]


# In reshape2 there are multiple cast functions. Since you will most commonly work with data.frame objects, we'll explore the dcast function.

# Casting: going from long to wide: 
aql <- melt(airquality, id.vars = c("Month", "Day"))
aqw <- dcast(aql, Month + Day ~ variable)  # Month & Day remain as columns (with duplicates removed), while the distinct names in variable "swing out" to create new column names. The arguments on the left refer to the ID variables and the arguments on the right refer to names the measured variables. Everything not in the equation is assumed to be a column of values. 
head(aqw)


dcast(aql, Month ~ variable + Day)  # wide format with each day a column: e.g Month=5, var="Ozone" and Day=1 gets 1 column; Month=5, var="Ozone" and Day=2 gets another, etc. 
# "Month ~ variable + Day" ==> LHS args specify how many rows in output; RHS args specify how many columns 
dcast(aql, Month ~ variable)  # "Aggregation function missing: defaulting to length". E.g. for month=5 and column=Ozone, there are now 31 values; we can't display 31 values in a cell, so we default to aggregating by counting 


dcast(aql, Month ~ variable, fun.aggregate = mean, 
      na.rm = TRUE)  # Averages by month
dcast(aql, Day ~ variable, fun.aggregate = mean, 
      na.rm = TRUE)  # Averages by day of month, across months. 


# NOTE: the above could have been done with dplyr::group_by() and summarize(): 
# however, this is more tedious because every var has to be named
# group_by(airquality, Month) %>% summarize(mean(Ozone, na.rm=TRUE), 
#                                           mean(Solar.R, na.rm=TRUE),
#                                           mean(Wind, na.rm=TRUE),
#                                           mean(Temp, na.rm=TRUE)) %>% 
#                                     as.data.frame 




## ...another example: means by day of week----------------------
days.of.week <- weekdays(Sys.Date()+0:6)  # list of days in the week 

data <- data.frame(var1=rnorm(7000, mean=50,sd=4), 
                   var2=rpois(7000, lambda=3), 
                   day=rep(days.of.week, 1000))
head(data, 50)

# MELT: 
longdata <- melt(data)
# head(longdata)
# longdata[c(1:8, 7001:7008), ]  # note that long data is grouped by measured variable name, and that id vars repeat after all data on one single measured variable name 

# CAST: 
# avg.by.day <- dcast(longdata, day ~ variable)  # Aggregation function missing: defaulting to length
avg.by.day <- dcast(longdata, day ~ variable, 
                    fun.aggregate=mean, 
                    na.rm=TRUE)
avg.by.day

# ...playing with the example ---------------------------------------------
# let's see if means of var1 differ across days 
anova.model1 <- lm(value~day, data=filter(longdata, variable=="var1"))
summary(anova.model)  # compare with "avg.by.day" Note that avg.by.day is easier to read. 

p1 <- ggplot(avg.by.day, aes(x=day, y=var1)) + 
  geom_bar(stat="identity")  
#   scale_y_continuous(limits=c(49.7, 50.1))  # not sure why the bars disappear when I do this 
p1

(p1zoom <- p1 + coord_cartesian(ylim=c(49.88, 50.1)))  # Setting limits on the coordinate system will zoom the plot (like you're looking at it with a magnifying glass), and will not change the underlying data like setting limits on a scale will.



# DPLYR PACKAGE: -------------------------------------

# > group_by( ): --------------
# converts df to tbl, with meta-instruction to do all subsequent operations in terms of groups. Note that on its own, group_by does not do any operation; it is usually used in conjunction with summarize()

mtcars
group_by(mtcars, cyl)  # a tibble, with instructions to do subsequent operations in terms of groups of cyl 
group_by(mtcars, cyl) %>% as.data.frame  # identical to initial df 


# > comparing summarize() on grouped versus original df: ----------
grouped <- group_by(mtcars, cyl)

summarize(grouped, mean(hp)) %>% as.data.frame # compare with following: 
summarize(mtcars, mean(hp))

summarize(grouped, mean(hp), sd(hp), mean(disp), sd(disp)) %>% 
      as.data.frame
# compare with following: 
summarize(mtcars, mean(hp), sd(hp), mean(disp), sd(disp))


# > try alternative groupings: --------------------------
(grouped_by_cyl <- group_by(mtcars, cyl) %>% 
       summarize(mean(hp), sd(hp))) %>% as.data.frame
(grouped_by_gear <- group_by(mtcars, gear) %>% 
       summarize(mean(hp), sd(hp))) %>% as.data.frame

# grouped on 2 vars: 
(grouped_by_gear_and_cyl <- group_by(mtcars, gear, cyl) %>% 
       summarize(mean(hp), sd(hp))) %>% as.data.frame

# checking result of grouping on 2 vars: 
filter(mtcars, gear==3) %>% select(cyl) %>% arrange(cyl) %>% unique 
filter(mtcars, gear==4) %>% select(cyl) %>% arrange(cyl) %>% unique 
filter(mtcars, gear==5) %>% select(cyl) %>% arrange(cyl) %>% unique 

# find how many rows in particular combinations: 
count(mtcars, cyl)
count(mtcars, cyl, gear)

# another option to count rows: 
mtcars %>% group_by(cyl, gear) %>% summarise(n= n() ) 

# > replace() ----
head(mtcars); str(mtcars)
cyl_order <- order(mtcars$cyl)
mtcars2 <- mtcars[cyl_order,]

# replace cylinder value of 4 with 99999
mtcars2 %>%
      mutate(hp=replace(hp, cyl==4, 99999),  # replace values 
                                    # in cyl column where value in 
                                    # cyl column ==4 
                                    # NOTE: you could also replace 
                                    # values in some other column where
                                    # cyl==4
             brands=rownames(mtcars)) %>%
      select(12,1:11) %>% 
      as.data.frame %>% print 

# > reorder cols to bring specified cols to the front: --------
str(mtcars)

# let's say you want disp and qsec at the front: 
mtcars %>% 
      select(disp, qsec, everything()) %>% 
      str



# > mutate_if( ) for changing coltypes: -----------
str(mtcars)
mtcars2 <- mutate_if(mtcars, 
                     is.numeric, 
                     factor)

str(mtcars2)


# case_when() for multiple "if" conditions 
# case_when is a general vectorized if 

mtcars %>% 
      mutate(type = case_when(
            hp < 100 ~ "slow",  # this is a 2-sided formula, with "hp<100" on Lhs. Here, LGH must evaluate to logical
            hp < 120 ~ "ok",
            hp >= 120 ~ "fast"
            )) %>% 
      select(hp, type)


# fizzbuzz using case_when: 
x <- 1:100
case_when(
      x %% 15 == 0 ~ "fizzbuzz", 
      x %% 3 == 0 ~ "fizz", 
      x %% 5 == 0 ~ "buzz", 
      TRUE ~ as.character(x)  # all RHSs must evaluate to same type, character here. 
)



# > purrr::set_names(): ----------
# set all colnames to lower/upper case: 
mtcars %>%
      set_names(toupper(names(.))) %>% 
      head


# use a separate vector as colnames: 
cols <- paste0("col", 1:11)

mtcars %>% 
      set_names(cols) %>% 
      head


#***************************************************
# DATAPASTA PACKAGE ----
#***************************************************
# help(package="datapasta")

# > to paste data as tibble into R from clipboard: -----
# 1. copy the data
# 2. in your R script (not console?) use the addin "paste as tribble" 
#           > tools>>addins OR 
#           > use the toolbar "Addins" in RStudio
#           > shortcut: ctrl + shift + t


data <- tibble::tribble(
                                     ~X,           ~Location, ~Min, ~Max,
                       "Partly cloudy.",          "Brisbane",  19L,  29L,
                       "Partly cloudy.",  "Brisbane Airport",  18L,  27L,
                     "Possible shower.",        "Beaudesert",  15L,  30L,
                       "Partly cloudy.",         "Chermside",  17L,  29L,
       "Shower or two. Possible storm.",            "Gatton",  15L,  32L,
                     "Possible shower.",           "Ipswich",  15L,  30L,
                       "Partly cloudy.",     "Logan Central",  18L,  29L,
                        "Mostly sunny.",             "Manly",  20L,  26L,
                       "Partly cloudy.",     "Mount Gravatt",  17L,  28L,
                     "Possible shower.",             "Oxley",  17L,  30L,
                       "Partly cloudy.",         "Redcliffe",  19L,  27L
      )

# does it work with excel? YES!
df2 <- tibble::tribble(
                  ~task, ~time,
                  "sim", 125L,
              "vgh mdc",  25L
             )  # %>% as.data.frame 


# > paste as data frame from Excel: ------
# > shortcut: ctrl + shift + d 
df3 <- data.frame(
              CTAS.Code = c(1L, 2L, 3L),
          X2016.Wkend.1 = c(2L, 66L, 293L)
       )


# > try pasting as vector from excel: --------
# > shortcut: ctrl + shift + v 
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct")

# you can also paste as vertical vector
# > shortcut: ctrl + shift + alt + v 
months.v <- c("jan",
              "feb",
              "mar",
              "apr",
              "may",
              "jun",
              "jul",
              "aug",
              "sep",
              "oct")




# > copy to clipboard from R: -------
mtcars$cyl %>% dmdclip()  # doesn't actually seem that useful 



# HERE PACKAGE: ------------------
# motivation: hardcoded file paths are bad. Instead, do everything relative 
#     to the top-level folder. 
# here() does this by searching for either a .git or .Rproj file 
#     > Create .Rproj files in the project root folder, not in src!!!
# NOTE: here() is kinda useless without an R project!!

library("here")
here()  # gives current working directory; should be the project root
here("docs")




# View( ) function :----
# you can specify a name for the view: 
mtcars %>% filter(cyl == 6) %>% View("mtcars 6 cyl")

