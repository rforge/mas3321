globalVariables("movies")
#' Get random subset of a dataset by providing a seed.
#'
#' This function creates a subset (of random rows) of a data set (by
#' default, the IMDB movie data frame). Given a student id, this
#' function will always return the same subset.
#'
#' @param seed, e.g "b1234567" or 18
#' @param d, optional (default: movies), dataset to get a random subset from.
#' @return A data frame with a random numer of rows
#' @importFrom utils data
#' @importFrom digest digest
#' @export
#' @examples
#' ##Given the same student ID
#' ##the same data set is returned
#' d1 = getRandomData("b1234567")
#' d2 = getRandomData("b1234567")
#' identical(d1, d2)
#' ##Different IDs get different data
#' d3 = getRandomData("b1234568")
#' identical(d1, d3)
getRandomData <- function(seed,d=movies) UseMethod("getRandomData")

#' @export
getRandomData.numeric = function(seed,d=movies) {
  #tumble is a simple pseudo-random number generator defined below
  r = tumble(seed)
  # Get a random number between 100 and 200
  l = 100+r%%100
  # Get l random rows
  del_rows = tumbleVector(r,l)%%(nrow(d)+1)
  # Remove the rows defined in del_rows from the the movies database
  d = d[-del_rows,]
  rownames(d) = 1:nrow(d)
  return(d)
}

# Convert a string to an integer seed using a hash function.
#  Take md5 of the string, interpret the first 5 digits as
#  a hex integer and then convert it to decimal.
#' @export
getRandomData.character = function(seed,d=movies) {
  hash = digest(seed, algo="md5")
  seedInt = strtoi(substr(hash,1,5),16)
  getRandomData(seedInt,d)
}

# This is a so-called "Linear congruential generator", a simple way of producing pseudo-random numbers.
# This one uses the same parameters as used in Numerical Recipies.
# modulus, m: 4294967296
# multiplier, a: 1664525
# increment, c: 1013904223
#
# I have deliberately used this rather than R's built in random number generators so
# that it is easy to reproduce the random numbers output by this algorithm in other
# languages/environments.
tumble = function(a){
  (a*1664525 + 1013904223)%%4294967296
}
tumbleVector = function(a,length=10){
  v=numeric()
  for(i in 1:length){
    a=tumble(a)
    v=c(v,a)
  }
  v
}
