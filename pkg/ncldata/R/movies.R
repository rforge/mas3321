#' Movie information and user ratings from imdb.com.
#'
#' The Internet Movie Database, \url{http://imdb.com/}, is a
#' website devoted to collecting movie data supplied by studios and fans.
#' It claims to be the biggest movie database on the web and is run by
#' Amazon.
#'
#' Movies were selected for inclusion if they were marked as "not adult",
#' and had at least 20000 rating votes.
#'
#' The file generateMovies.R demonstates how the data was constructed.
#' The contents of generateMovies.R is not designed for teaching purposes,
#' but instead as documentation for future updating of the dataset.
#'
#' @name movies
#' @docType data
#' @usage data(movies)
#' @return A data frame with 5000 rows and 24 variables.
#' @references Information courtesy of IMDb (http://www.imdb.com). IMDb data files are publically available at https://datasets.imdbws.com/
#' @note The data set contains the following fields:
#' \describe{
#' \item{Title}{Title of the movie.}
#' \item{Year}{Year of release.}
#' \item{Rating}{Average IMDb user rating.}
#' \item{Votes}{Number of IMDb users who rated this movie.}
#' \item{Length}{Length in minutes.}
#' \item{Budget}{Total budget (if known).}
#' \item{BudgetCurrency}{Currency of the budget (if known).}
#' \item{mpaa}{MPAA rating.}
#' \item{Action, Animation, Comedy, Drama, Documentary, Fantasy, Horror, Romance, SciFi, Short}{Binary variables representing if movie was classified as belonging to that genre.}}
#' @keywords datasets
#' @examples
#' data(movies)
"movies"
