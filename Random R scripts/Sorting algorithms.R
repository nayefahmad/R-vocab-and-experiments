
# rm(list=ls())

# straight insertion sort: ----------------

# define function: 
st.insert.sort <- function(keys){
    for(j in 2:length(keys)){
        i <- j-1
        k <- keys[j]
        if(k < keys[i]){
            while(i!=0 && k < keys[i]){
                keys[i+1] <- keys[i]
                keys[i] <- k  # these 2 lines basically swap these keys 
                i <- i-1  # compare again to see if we should swap 
            }
        } else if (k >= keys[i]){
            keys[i+1] <- k
        }
    }
return(keys)
}

# create data: 
keys <- c(9,3,6,2,0,-.2, 2)

# call function: 
st.insert.sort(keys)

# try with larger dataset: 
keys <- sample(-2000:2000, size = 3000, replace = TRUE) 
st.insert.sort(keys)  # seems to take a fairly long time
sort(keys)  # MUCH faster 
sort(keys)==st.insert.sort(keys)  # IT WORKS!!  




# bubble sort:--------------------
bubble.sort <- function(keys){
    bound <- length(keys)
    t <- 0 
    iteration <- 0
    while(t != 1){
        iteration <- iteration + 1 
        print(paste("'while' iteration=", iteration))
        for(j in 1:(bound-1)){  # j indexes positions 
            # during a pass along the list 
            k <- keys[j]
            print(paste("j=", j, "t=", t))
            if (k > keys[j+1]){
                temp <- keys[j]
                keys[j] <- keys[j+1]
                keys[j+1] <- temp  # these lines switch the two elements 
                
                t <- j  
                print(paste("last switch, t=", t))
                print(keys)
                # t records position of last switch during
                # this pass through the list. When last switch 
                # is in position 1, list is fully sorted
            } else if (k <= keys[j+1] && j==(bound-1) && t==bound) {
                print("entered else if")
                t <- 1  
                # exit the while loop; list is sorted if 
                # we can't find any positions were switches are 
                # necessary 
            } 
        }
    bound <- t  # everything after position of last switch has 
    # already been sorted, so we ignore it
    print(paste("bound=", bound))
    }
    return(keys)
}                


bubble.sort.p <- function(keys, print=FALSE){
    bound <- length(keys)
    t <- 0 
    iteration <- 0
    while(t != 1){
        iteration <- iteration + 1 
        if (print==TRUE){
            print(paste("'while' iteration (pass number)=", iteration))
        }
        for(j in 1:(bound-1)){  # j indexes positions 
            # during a pass along the list 
            k <- keys[j]
            if (print==TRUE){
                print(paste("j=", j, "t=", t))
            }
            if (k > keys[j+1]){
                temp <- keys[j]
                keys[j] <- keys[j+1]
                keys[j+1] <- temp  # these lines switch the two elements 
                
                t <- j  
                if (print==TRUE) {
                    print(paste("switch at t=", t))
                    print(keys)
                }
                # t records position of last switch during
                # this pass through the list. When last switch 
                # is in position 1, list is fully sorted
            } else if (k <= keys[j+1] && j==(bound-1) && t==bound) {
                print("entered else if")
                t <- 1  
                # exit the while loop; list is sorted if 
                # we can't find any positions were switches are 
                # necessary 
            } 
        }
        bound <- t  # everything after position of last switch has 
        # already been sorted, so we ignore it
        if(print==TRUE){print(paste("bound=", bound))}
    }
    return(keys)
}      



# create data: 
keys <- c(9,3,6,2,0,-.2, 2)

# call function: 
bubble.sort(keys)
bubble.sort.p(keys)
bubble.sort.p(keys, print = TRUE)

sort(keys)==bubble.sort(keys)  


# try with larger dataset: 
keys <- sample(-2000:2000, size = 3000, replace = TRUE) 
bubble.sort.p(keys) # takes a pretty long time 
bubble.sort(keys, print=TRUE)
sort(keys)  # SO MUCH FASTER!    
sort(keys)==bubble.sort.p(keys)  # IT WORKS!!  
