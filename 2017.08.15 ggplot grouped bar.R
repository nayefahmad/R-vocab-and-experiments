raw <- read.csv("http://pastebin.com/raw.php?i=L8cEKcxS",sep=",") %>% print

raw[,2]<-factor(raw[,2],levels=c("Very Bad","Bad","Good","Very Good"),ordered=FALSE)
raw[,3]<-factor(raw[,3],levels=c("Very Bad","Bad","Good","Very Good"),ordered=FALSE)
raw[,4]<-factor(raw[,4],levels=c("Very Bad","Bad","Good","Very Good"),ordered=FALSE)

raw=raw[,c(2,3,4)] # getting rid of the "people" variable as I see no use for it

freq=table(col(raw), as.matrix(raw)) %>% print # get the counts of each factor level

Names=c("Food","Music","People")     # create list of names
data=data.frame(cbind(freq),Names)   # combine them into a data frame
data=data[,c(5,3,1,2,4)]             # sort columns

# melt the data frame for plotting
data.m <- melt(data, id.vars='Names')

# plot everything

# grouped bar plot-----
ggplot(data.m, aes(x=Names, y=value)) +   
      geom_bar(aes(fill = variable), position = "dodge", stat="identity")

# stacked bar plot?? ------
ggplot(data.m, aes(x=Names, y=value)) +   
      geom_bar(aes(fill = variable), stat="identity")

# this also works: 
ggplot(data.m, aes(x=Names, y=value, fill=variable)) +   
      geom_bar(position="dodge", stat="identity")

# conclusion: just make sure the "variable" column from the long-format dataset is included in aes(), so that ggplot knows not to aggregate by ignoring that. 
