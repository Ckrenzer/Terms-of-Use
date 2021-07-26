# Setup ------------------------------------------------------------------
# Packages
pacman::p_load(microbenchmark, rbenchmark)



# Sources scripts
sourceDir <- function(path, trace = TRUE, ...) {
  op <- options(); on.exit(options(op)) # to reset after each 
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
    options(op)
  }
}

# Loading in helper functions and related objects
sourceDir(path = "scripts/functions/")



# The data to calculate the SVD
my_iris <- as.matrix(iris[, -5, drop = FALSE])
# Let's make the size a bit bigger...
my_iris <- as.matrix(rep(my_iris, 15000))




# Tests ------------------------------------------------------------------
# One hundred replications
microbenchmark(
  manual = singular_value_decomp(my_iris),
  
  base = svd(my_iris), 
  
  times = 100, unit = "ms"
)

# singular_value_decomp() is very close to base R's speed (and has more functionality)
#
# (milliseconds)
#   expr      min       lq     mean   median       uq      max neval
# manual 200.0101 228.9913 250.6779 238.4315 249.8281 372.8541   100
#   base 195.9959 232.2532 246.3027 237.6130 243.2878 390.8165   100





# One hundred replications
rbenchmark::benchmark(
  manual = singular_value_decomp(my_iris),
  
  base = svd(my_iris), 
  
  replications = 100
)

# singular_value_decomp() is somewhere between 3 and 13 percent slower than svd()
#
#     test replications elapsed relative user.self sys.self user.child sys.child
# 1 manual          100   27.64    1.131     18.08     9.05         NA        NA
# 2   base          100   24.44    1.000     17.47     6.92         NA        NA









# svd() has an argument allowing the user to decide the number of singular
# values to return, but if you want all of them, the function has to be run
# to completion (and will return all parts of the decomposition)
#
# singular_value_decomp() allows the user to end the function early to
# only return the singular values
#
#
# One hundred replications
microbenchmark(
  manual = singular_value_decomp(my_iris, singular_values_only = TRUE),
  
  base = svd(my_iris), 
  
  times = 100, unit = "ms"
)

# singular_value_decomp() is now blazing fast!
#
# Unit: milliseconds
#   expr     min        lq      mean   median       uq      max neval
# manual  62.743  67.96825  82.02071  70.5701  93.5641 224.0755   100
#   base 194.614 233.38750 242.80549 240.4846 247.7697 347.0858   100






# One million replications
rbenchmark::benchmark(
  manual = singular_value_decomp(my_iris, singular_values_only = TRUE),
  
  base = svd(my_iris), 
  
  replications = 100
)

# siular_value_decomp() is about 3 times faster than svd()!
#
#     test replications elapsed relative user.self sys.self user.child sys.child
# 1 manual          100    7.92    1.000      6.00     1.91         NA        NA
# 2   base          100   24.45    3.087     17.55     6.86         NA        NA