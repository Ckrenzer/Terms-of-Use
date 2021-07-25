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





# Tests ------------------------------------------------------------------
# One hundred thousand replications
microbenchmark(
  manual = singular_value_decomp(my_iris),
  
  base = svd(my_iris), 
  
  times = 100000, unit = "ms"
)

# singular_value_decomp() is about 1.6 times slower
#
# (milliseconds)
#   expr    min     lq       mean median     uq      max neval
# manual 0.0671 0.0723 0.08780684 0.0747 0.0790 148.5569 1e+05
#   base 0.0425 0.0458 0.05506754 0.0492 0.0517  17.7723 1e+05





# One million replications
rbenchmark::benchmark(
  manual = singular_value_decomp(my_iris),
  
  base = svd(my_iris), 
  
  replications = 1000000
)

# singular_value_decomp() is still about 1.6 times slower
# But it didn't get blown out of the water!
#
# (seconds)
#     test replications elapsed relative user.self sys.self user.child sys.child
# 1 manual      1000000   89.97     1.61     89.74     0.07         NA        NA
# 2   base      1000000   55.88     1.00     55.70     0.00         NA        NA









# svd() has an argument allowing the user to decide the number of singular
# values to return, but if you want all of them, the function has to be run
# to completion (and will return all parts of the decomposition)
#
# singular_value_decomp() allows the user to end the function early to
# only return the singular values
#
#
# One hundred thousand replications
microbenchmark(
  manual = singular_value_decomp(my_iris, singular_values_only = TRUE),
  
  base = svd(my_iris), 
  
  times = 100000, unit = "ms"
)

# singular_value_decomp() is now faster than svd()!
#
# (milliseconds)
#   expr    min     lq       mean median     uq     max neval
# manual 0.0374 0.0400 0.04410814 0.0410 0.0423 12.8073 1e+05
#   base 0.0427 0.0453 0.05006944 0.0476 0.0490 12.3931 1e+05






# One million replications
rbenchmark::benchmark(
  manual = singular_value_decomp(my_iris, singular_values_only = TRUE),
  
  base = svd(my_iris), 
  
  replications = 1000000
)

# siular_value_decomp() is about 8 percent faster than svd()!
#
#     test replications elapsed relative user.self sys.self user.child sys.child
# 1 manual      1000000   47.31    1.000     47.20        0         NA        NA
# 2   base      1000000   51.02    1.078     50.95        0         NA        NA