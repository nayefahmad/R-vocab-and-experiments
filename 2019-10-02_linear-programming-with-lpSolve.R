
#'--- 
#' title: "Linear programming in R with `{lpSolve}` and `{lpSolveAPI}`"
#' author: "Nayef Ahmad"
#' date: "2019-10-02"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---

#' Reference: https://www.r-bloggers.com/linear-programming-in-r/
#' 
#' All credits to the writer of that post. This is just me fooling around with the code they wrote. 
#'
#' # Problem Statement
#'
#' "This is an example of linear optimization that I found in the book “Modeling
#' and Solving Linear Programming with R” by Jose M. Sallan, Oriol Lordan and
#' Vincenc Fernandez.  The example is named “Production of two models of chairs”
#' and can be found at page 57, section 3.5. I’m going to solve only the first
#' point."
#'
#'
#' > A company produces two models of chairs: 4P and 3P. The model 4P needs 4
#' legs, 1 seat and 1 back. On the other hand, the model 3P needs 3 legs and 1
#' seat. The company has a initial stock of 200 legs, 500 seats and 100 backs.
#' If the company needs more legs, seats and backs, it can buy standard wood
#' blocks, whose cost is 80 euro per block. The company can produce 10 seats, 20
#' legs and 2 backs from a standard wood block. The cost of producing the model
#' 4P is 30 euro/chair, meanwhile the cost of the model 3P is 40 euro/chair.
#' Finally, the company informs that the minimum number of chairs to produce is
#' 1000 units per month. Define a linear programming model, which minimizes the
#' total cost (the production costs of the two chairs, plus the buying of new
#' wood blocks).
#' 
#' 




#+ lib, include = FALSE 
# Load lpSolve
library(lpSolve)
library(lpSolveAPI)


#' # Solution with lpSolve
# Solution with lpSolve -----------
#+ rest 

#' ## Objective function  
#' 
#' Here are the coefficients of the decision variables: 
#' 
#' * Cost of each unit of **4P** is $30 
#' * Cost of each unit of **3P** is $40 
#' * Cost of each unit of **wooden blocks** is $80 
#' 
#' Therefore, the obj function is: 
#' 
#' $Cost = 30*4P + 40*3P + 80*WoodenBlocks$ 
#' 


## Set the coefficients of the decision variables -> C
C <- c(30, 40, 80)



#' ## Constraint matrix 
#' 
#' There is one row for each constraint, and one column for each decision variable.  
#' 
#' * First row is for the **Seat constraint**. It says that: 
#' 
#'     * Each unit of **4P** uses 1 seat from the seat inventory 
#'     * Each unit of **3P** uses 1 seat from the seat inventory 
#'     * Each unit of **wooden blocks** *adds* 1 seat to the seat inventory 
#' 
#' * Second row is for the **Legs constraint** 
#' 
#' * Third row is for the **Backs constraint** 
#' 
#' * Fourth row is for the **Min production constraint**
#' 
#' 

# Create constraint martix B
A <- matrix(c(1, 1, -10,
              4, 3, -20,
              1, 0, -2,
              1, 1, 0), nrow=4, byrow=TRUE)


# Right hand side for the constraints
B <- c(500, 200, 100, 1000)

# Direction of the constraints
constranints_direction  <- c("<=", "<=", "<=", ">=")


#' ## Solution with `lp` function 
#' 
# Find the optimal solution
optimum <-  lp(direction="min",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

str(optimum)

# Print status: 0 = success, 2 = no feasible solution
print(optimum$status)

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("x_4p", "x_3p", "x_w") 
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total cost: ", optimum$objval, sep=""))

#*********************************************************
#   Output      #
#*********************************************************

# [1] 0
# x_4p x_3p  x_w 
# 420  580  161 

# "Total cost: 48680"

rm(optimum, constranints_direction, best_sol)

#*********************************************************

#' # Solution with lpSolveAPI
# Solution with lpSolveAPI -------------------
# Let's try to solve the problem again using lpSolveAPI

# Use lpSolveAPI
require(lpSolveAPI)

# Set 4 constraints and 3 decision variables
lprec <- make.lp(nrow = 4, ncol = 3)
# Set the type of problem we are trying to solve
lp.control(lprec, sense="min")
# Set type of decision variables
set.type(lprec, 1:3, type=c("integer"))

# Set objective function coefficients vector C
set.objfn(lprec, C)

# Add constraints
add.constraint(lprec, A[1, ], "<=", B[1])
add.constraint(lprec, A[2, ], "<=", B[2])
add.constraint(lprec, A[3, ], "<=", B[3])
add.constraint(lprec, A[4, ], ">=", B[4])

# Display the LPsolve matrix
lprec

# Solve problem
solve(lprec)

# Get the decision variables values
get.variables(lprec)
# Get the value of the objective function
get.objective(lprec)

# Note that the default boundaries on the decision variable are c(0, 0, 0) and c(Inf, Inf, Inf)
get.bounds(lprec)

# Boundaries can be set with following function
#lpSolveAPI::set.bounds()

#*********************************************************
#   Output      #
#*********************************************************

# [1] 420 580 161
# [1] 48680