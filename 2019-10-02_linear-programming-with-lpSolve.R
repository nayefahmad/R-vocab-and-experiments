
#'--- 
#' title: "Linear programming in R with lpSolve and lpSolveAPI"
#' author: "Nayef Ahmad"
#' date: "2019-10-02"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---


# Load lpSolve
require(lpSolve)

## Set the coefficients of the decision variables -> C
C <- c(30, 40, 80)

# Create constraint martix B
A <- matrix(c(1, 1, -10,
              4, 3, -20,
              1, 0, -2,
              1, 1, 0), nrow=4, byrow=TRUE)

# Right hand side for the constraints
B <- c(500, 200, 100, 1000)

# Direction of the constraints
constranints_direction  <- c("<=", "<=", "<=", ">=")

# Find the optimal solution
optimum <-  lp(direction="min",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print(optimum$status)

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("x_4p", "x_3p", "x_w") 
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total cost: ", optimum$objval, sep=""))

#################
#   Output      #
#################

# [1] 0
# x_4p x_3p  x_w 
# 420  580  161 

# "Total cost: 48680"

rm(optimum, constranints_direction, best_sol)

#-------------------------------------------------------------------------------
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

#################
#   Output      #
#################

# [1] 420 580 161
# [1] 48680