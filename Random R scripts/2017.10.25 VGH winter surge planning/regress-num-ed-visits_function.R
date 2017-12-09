
#**********************
# FUNCTION FOR REGRESSIGN num.visits ~ day.of.week + month.fyear
#**********************

# todo: ------
# names(df) needs to be changed: refers to global var "visits". Prob 
#     better to add args to specify col numbers for numvisits, day.of.wk,
#     and month.fyear
#*************

regress.fn <- function(df, summary=TRUE) {
      
      # input: df with ed visits data
      # output: lm object 
      
      df <- as.data.frame(df)
      names(df) <- names(visits)
      head(df)
      if (summary==TRUE) {
            summary(lm(num.visits ~ day.of.week + month.fyear,
                       data=df))
      } else {
            lm(num.visits ~ day.of.week + month.fyear,
               data=df)
      }
}