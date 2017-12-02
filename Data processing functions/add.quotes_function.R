

#**********************************
# Add quote marks before and after numbers to allow pasting in SQL
#**********************************

# test data: 
# x <- c(50, 75)

# define fun: 
add.quotes <- function(input.number, isInsertedIntoSqltable){
      if (isInsertedIntoSqltable == FALSE) {
            paste0("'", input.number, "', ")
      } else {
            paste0("('", input.number, "'), ")
      }
      
}

# test fun: 
# sapply(x, add.quotes, isInsertedIntoSqltable=FALSE)
# sapply(x, add.quotes, isInsertedIntoSqltable=TRUE)

# **********************************************
# test with numbers from Excel: ------
input.numbers <- readClipboard()  # copy numbers from excel
output.numbers <- sapply(input.numbers, 
                         add.quotes, 
                         isInsertedIntoSqltable=FALSE) %>% as.data.frame

write.table(output.numbers, file="clipboard", sep="\t",  # copy to Excel
            row.names=FALSE,
            col.names = FALSE)
