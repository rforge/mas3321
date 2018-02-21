globalVariables("movies")
#' Get a random subset of the movie data set with optional seed.
#'
#' This function creates a subset (of random rows) of the IMDB movie
#' data frame. Given a student id, this function will always return the
#' same subset.
#' @param seed, e.g "b1234567" or 18
#' @return A data frame with a random numer of rows
#' @importFrom utils data
#' @importFrom digest digest
#' @export
#' @examples
#' ##Given the same student ID
#' ##the same data set is returned
#' d1 = getRandomMovies("b1234567")
#' d2 = getRandomMovies("b1234567")
#' identical(d1, d2)
#' ##Different IDs get different data
#' d3 = getRandomMovies("b1234568")
#' identical(d1, d3)
getRandomMovies <- function(seed) UseMethod("getRandomMovies")

#' @export
getRandomMovies.numeric = function(seed) {
  d = movies
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

# Convert string to integer seed.
#  Take md5 of the string, interpret the first 5 digits as
#  a hex integer and convert to decimal.
#' @export
getRandomMovies.character = function(seed) {
  hash = digest(seed, algo="md5")
  seedInt = strtoi(substr(hash,1,5),16)
  getRandomMovies(seedInt)
}

# This is a so-called "Linear congruential generator", a simple way of producing pseudo-random numbers.
# This one uses the same parameters used in Numerical Recipies.
# modulus, m: 4294967296
# multiplier, a: 1664525
# increment, c: 1013904223
#
#It should be fairly easy to reproduce the numbers output by this algorithm in other languages.
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
