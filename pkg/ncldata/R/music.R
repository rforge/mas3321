#' Music information for a selection of popular music from 1950 to 2009
#'
#' @name music
#' @docType data
#' @usage data(music)
#' @return A data frame with 424 rows and 10 variables.
#' @references https://tsort.info/music, https://github.com/nvempala/Music-Decades-Top-100, https://www.discogs.com/developers/
#' @note The data set contains the following fields:
#' \describe{
#' \item{artist}{Artist name.}
#' \item{track}{Track name.}
#' \item{year}{Year of release.}
#' \item{decade}{Decade of release.}
#' \item{duration}{Track length in seconds.}
#' \item{tempo}{Tempo of the song in BPM (beats per minute)}
#' \item{energy}{A number that ranges from 0 to 1, representing how energetic the track is.}
#' \item{danceability}{A number that ranges from 0 to 1, representing how suitable a track is for dancing, using a number of musical elements. The combination of musical elements that best characterize danceability include tempo, rhythm stability, beat strength, and overall regularity.}
#' \item{key}{The musical key that the track is in.}
#' \item{genre}{A list of comma separated styles or categories taht the track is in.}
#' }
#' @keywords datasets
#' @examples
#' data(music)
"music"

