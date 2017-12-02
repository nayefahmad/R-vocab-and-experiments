
#**********************************
# Add zeroes in front of numbers to get a specified output length 
#**********************************

# create test data: -------- 
# x <- c(2,400, 12001)

# define function:---------
add.zero.in.front <- function(input.number, output.length){
      # inputs: a single number, number of zeroes to add in front 
      # output: new number of desired total length 
      
      input.number <- as.character(input.number)
      length <- nchar(input.number)
      # print(length)
      if (length < output.length) {
            zeroes <- rep("0", times=(output.length-length)) %>% 
                  paste(collapse="")
            return(paste0(zeroes, input.number))
      } else {
            return(input.number)
      }
}
      
      
# test the function: 
# add.zero.in.front(1,5)
# lapply(x, add.zero.in.front, output.length=5)


#******************************************************
# test with numbers from Excel: ----------
input.numbers <- readClipboard()  # copy numbers from excel
output.numbers <- sapply(input.numbers, add.zero.in.front, output.length=7)

write.table(output.numbers, file="clipboard", sep="\t",  # copy back to Excel
             row.names=FALSE,
             col.names = FALSE)

