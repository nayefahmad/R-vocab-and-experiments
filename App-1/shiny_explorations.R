
#***********************************
# Experimenting with Shiny 
#***********************************

library("shiny")
library("here")

# Example: Hello Shiny ---------------
runExample("01_hello")

# Shiny apps are contained in a single script called app.R. The script app.R lives in a directory (for example, newdir/) and the app can be run with runApp("newdir").
# 
# app.R has three components:
#       
#     a user interface object ("ui")
# 
#     a server function ("server")
# 
#     a call to the shinyApp function ("shinyApp(ui = ui, server = server)")



# Example 2: ----------------
runApp(here("App-1", "app.R"))
