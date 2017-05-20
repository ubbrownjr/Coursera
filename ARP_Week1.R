.libPaths("C:/R_Libraries")
setwd("C:/Users/USER/OneDrive/VSTS/Coursera")

library(swirl)
swirl()

#################################
## Generate a uniform random number
x <- runif(1, 0, 6)
if (x > 3) {
 y <- 10
} else {
 y <- 0
}
y

1:10
for (i in 1:10) {
print(rnorm(10)[i])
}

x <- c("a", "b", "c", "d", "e")
x
for (i in 1:4) {
 print(x[i]) }
for (i in seq_along(x)) {
  print(x[i]) }
for (letter in x) {
 print(letter) }
for (i in 2:3)
 print(x[i])

x <- matrix(1:6, 2, 3)
x
for (i in seq_len(nrow(x))) {
 for (j in seq_len(ncol(x))) {
  print(x[i, j])
 }
}

x <- 1
for (i in 1:100) {
 if (i <= 20) {
  ## Skip the first 20 iterations
  next
 }
 print(x)
}

for (i in 1:100) {
 print(i)
 if (i > 20) {
  ## Stop loop after 20 iterations
  break
 }
}

sprintf("%s is %f feet tall\n", "Sven", 7.1) # OK
try(sprintf("%s is %i feet tall\n", "Sven", 7.1)) # not OK
sprintf("%s is %i feet tall\n", "Sven", 7) # OK

basename(file.path("", "p1", "p2", "p3", c("file1", "file2")))
dirname(file.path("", "p1", "p2", "p3", "filename"))
dirname(file.path("", "p1", "p2", "p3", c("file1", "file2")))

####################################################

.libPaths("C:/R_Libraries")
setwd("C:/Users/USER/OneDrive/VSTS/Coursera")

library(readr)
library(dplyr)

search()

## Download data from RStudio (if we haven't already)
if (!file.exists("data/2016-07-20.csv.gz")) {
 download.file("http://cran-logs.rstudio.com/2016/2016-07-20.csv.gz", "data/2016-07-20.csv.gz")
}
cran <- read_csv("data/2016-07-20.csv.gz", col_types = "ccicccccci")
cran %>% filter(package == "filehash") %>% nrow


## CREATE FUNCTION TO PERFORM DOWNLOAD WITH PARAMETERS
## pkgname: package name (character)
## date: YYYY-MM-DD format (character)
num_download <- function(pkgname, date = "2016-07-20") {
 year <- substr(date, 1, 4)
 src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz", year, date)

 ## Construct path for storing local file
 dest <- file.path("data", basename(src))

 ## Don't download if the file is already there!
 if (!file.exists(dest))
  download.file(src, dest, quiet = TRUE)

 cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
 cran %>% filter(package == pkgname) %>% nrow
}

num_download("filehash", "2016-07-20")
num_download("Rcpp", "2016-07-19")
num_download("Rcpp")

## SPLIT PREVIOUS FUNCTION INTO PIECES
check_pkg_deps <- function() {
 if (!require(readr)) {
  message("installing the 'readr' package")
  install.packages("readr")
 }
 if (!require(dplyr))
  stop("the 'dplyr' package needs to be installed first")
 }

check_for_logfile <- function(date) {
 year <- substr(date, 1, 4)
 src <- sprintf("http://cran-logs.rstudio.com/%s/%s.csv.gz", year, date)
 dest <- file.path("data", basename(src))
 if (!file.exists(dest)) {
  val <- download.file(src, dest, quiet = TRUE)
  if (!val)
   stop("unable to download file ", src)
  }
 dest
}

## 'pkgname' can now be a character vector of names
num_download <- function(pkgname, date = "2016-07-20") {
 check_pkg_deps()

 ## Check arguments
 if (!is.character(pkgname))
  stop("'pkgname' should be character")
 if (!is.character(date))
  stop("'date' should be character")
 if (length(date) != 1)
  stop("'date' should be length 1")

 dest <- check_for_logfile(date)
 cran <- read_csv(dest, col_types = "ccicccccci", progress = FALSE)
 #cran %>% filter(package == pkgname) %>% nrow
 cran %>% filter(package %in% pkgname) %>% group_by(package) %>% summarize(n = n())
}

num_download("Rcpp")
num_download(c("filehash", "Rcpp", "weathermetrics"), "2016-07-19")
num_download("filehash", c("2016-07-20", "2016-07-21"))

############################################################

.libPaths("C:/R_Libraries")
setwd("C:/Users/USER/OneDrive/VSTS/Coursera")
library(swirl)
swirl()

#############################################################
# You're about to write your first function! Just like you would assign a value 
# to a variable with the assignment operator, you assign functions in the following
# way:
#
# function_name <- function(arg1, arg2){
#	# Manipulate arguments in some way
#	# Return a value
# }
#
# The "variable name" you assign will become the name of your function. arg1 and
# arg2 represent the arguments of your function. You can manipulate the arguments
# you specify within the function. After sourcing the function, you can use the 
# function by typing:
# 
# function_name(value1, value2)
#
# Below we will create a function called boring_function. This function takes
# the argument `x` as input, and returns the value of x without modifying it.
# Delete the pound sign in front of the x to make the function work! Be sure to 
# save this script and type submit() in the console after you make your changes.

boring_function <- function(x) {
 x
}

################################################################################

# You're free to implement the function my_mean however you want, as long as it
# returns the average of all of the numbers in `my_vector`.
#
# Hint #1: sum() returns the sum of a vector.
# 	Ex: sum(c(1, 2, 3)) evaluates to 6
#
# Hint #2: length() returns the size of a vector.
# 	Ex: length(c(1, 2, 3)) evaluates to 3
#
# Hint #3: The mean of all the numbers in a vector is equal to the sum of all of
#		   the numbers in the vector divided by the size of the vector.
#
# Note for those of you feeling super clever: Please do not use the mean()
# function while writing this function. We're trying to teach you something 
# here!
#
# Be sure to save this script and type submit() in the console after you make 
# your changes.

my_mean <- function(my_vector) {
 sum(my_vector) / length(my_vector)
}

################################################################################

# Let me show you an example of a function I'm going to make up called
# increment(). Most of the time I want to use this function to increase the
# value of a number by one. This function will take two arguments: "number" and
# "by" where "number" is the digit I want to increment and "by" is the amount I
# want to increment "number" by. I've written the function below. 
#
# increment <- function(number, by = 1){
#     number + by
# }
#
# If you take a look in between the parentheses you can see that I've set
# "by" equal to 1. This means that the "by" argument will have the default
# value of 1.
#
# I can now use the increment function without providing a value for "by": 
# increment(5) will evaluate to 6. 
#
# However if I want to provide a value for the "by" argument I still can! The
# expression: increment(5, 2) will evaluate to 7. 
# 
# You're going to write a function called "remainder." remainder() will take
# two arguments: "num" and "divisor" where "num" is divided by "divisor" and
# the remainder is returned. Imagine that you usually want to know the remainder
# when you divide by 2, so set the default value of "divisor" to 2. Please be
# sure that "num" is the first argument and "divisor" is the second argument.
#
# Hint #1: You can use the modulus operator %% to find the remainder.
#   Ex: 7 %% 4 evaluates to 3. 
#
# Remember to set appropriate default values! Be sure to save this 
# script and type submit() in the console after you write the function.

remainder <- function(num, divisor = 2) {
 num %% divisor
}

################################################################################

# You can pass functions as arguments to other functions just like you can pass
# data to functions. Let's say you define the following functions:
#
# add_two_numbers <- function(num1, num2){
#    num1 + num2
# }
#
# multiply_two_numbers <- function(num1, num2){
#	num1 * num2
# }
#
# some_function <- function(func){
#    func(2, 4)
# }
#
# As you can see we use the argument name "func" like a function inside of 
# "some_function()." By passing functions as arguments 
# some_function(add_two_numbers) will evaluate to 6, while
# some_function(multiply_two_numbers) will evaluate to 8.
# 
# Finish the function definition below so that if a function is passed into the
# "func" argument and some data (like a vector) is passed into the dat argument
# the evaluate() function will return the result of dat being passed as an
# argument to func.
#
# Hints: This exercise is a little tricky so I'll provide a few example of how
# evaluate() should act:
#    1. evaluate(sum, c(2, 4, 6)) should evaluate to 12
#    2. evaluate(median, c(7, 40, 9)) should evaluate to 9
#    3. evaluate(floor, 11.1) should evaluate to 11

evaluate <- function(func, dat) {
 # Write your code here!
 # Remember: the last expression evaluated will be returned! 
}

################################################################################


################################################################################


################################################################################


################################################################################