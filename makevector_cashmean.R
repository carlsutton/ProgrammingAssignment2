#  Function named makeVector
#  1.  Set the value of the vector
#  2.  get the value of the vector
#  3.  set the value of the mean
#  4.  get the value of the mean

makeVector <- function(x = numeric()) {
        m <- NULL                #  setting variable to NULL
        #  the calling envirnment is makeVector for set, get, etc
        #  no clue as to why set function is here
        set <- function(y) {     #  y appears to be a free variable
                                 #  but actually appears to have the
                                 #  value of x????
                x <<- y          #  assign x in the calling envirnment, why
                                 # do this, isnt that  where x exists?
                                 #  totally confused as to 1.  why this
                #function exists, and 2. it appears to assign x to itself
                m <<- NULL       #  assign m in the calling envirnment 
        }
        get <- function() x      # Ah Ha, x is actually the data cachemean
                                 # Will use to calculate its mean
        setmean <- function(mean) m <<- mean  #  assgn the variable m  
                                              #  a value in the calling 
                                              #  envirnoment
        getmean <- function() m   # retrieves the variable just stored
        #  returns the list of functions for cachemean to use, except it
        #  does not use set so maybe we typed it for exercise???
        list(set = set, get = get, setmean = setmean,getmean = getmean)
}        

#  create cachemean function
cachemean <- function(x, ...) {
        m <- x$getmean()    #  retrieves mean from calling envirnoment
        if(!is.null(m)) {   #  test if mean has a value other than NULL
                message("getting cache data")
                return(m)   # returns the mean to the calling envirnoment
                            # where it already resided
        }
        data <- x$get()     #  the mean has not been calculated so 
                            #  this line loads the data
        m <- mean(data, ...)  # Calculates the mean for the data in x
        x$setmean(m)        # assigns to m the data mean
        m                   #  returns the mean
}
count <- 50
for (i in 1:5) {        
        data <- 1:count
        a <- makeVector(data)
        b <- cachemean(a)
        print(b)
#        count <- count + 10
#        if (count - 50 > 25) count <- 50
}
#  after running this code the message("getting cache mean") never appears
#  what is going on??
#   maybe its the for loop, will just call it again
ccd <- makeVector(data)
bbb <- cachemean(ccd)
print(bbb)
