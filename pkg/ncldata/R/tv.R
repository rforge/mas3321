#' TV show information and user ratings from imdb.com.
#'
#' The Internet Movie Database, \url{http://imdb.com/}, is a
#' website devoted to collecting movie data supplied by studios and fans.
#' It claims to be the biggest movie database on the web and is run by
#' Amazon.
#'
#' TV shows were selected for inclusion if they were marked as "not adult",
#' and had at least 5000 rating votes.
#'
#' @name tv
#' @docType data
#' @usage data(tv)
#' @return A data frame with 1562 rows and 22 variables.
#' @references Information courtesy of IMDb (http://www.imdb.com). IMDb data files are publically available at https://datasets.imdbws.com/
#' @note The data set contains the following fields:
#' \describe{
#' \item{Title}{Title of the tv show}
#' \item{StartYear}{Year of release.}
#' \item{EndYear}{Year the show ended (if known).}
#' \item{Type}{Type of show (tvMiniSeries,tvSeries,tvShort,tvSpecial).}
#' \item{Rating}{Average IMDb user rating.}
#' \item{Votes}{Number of IMDb users who rated this movie.}
#' \item{Length}{Length in minutes.}
#' \item{Action, Animation, Comedy, Drama, Documentary, Fantasy, Horror, Romance, SciFi, Short}{Binary variables representing if movie was classified as belonging to that genre.}}
#' @keywords datasets
#' @examples
#' data(tv)
"tv"

# FURTHER INFO
# ---------------------------------------------
# This source of data provides .tsv formatted files: https://datasets.imdbws.com/
# The data was imported with something like the following R commands:
#
# imdbtitles = read.table(file = "title.basics.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="utf-8")
# imdbratings = read.table(file = "title.ratings.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="utf-8")
# m=5000
# imdbdata = merge(imdbtitles,imdbratings,by = "tconst")
# imdbclean = imdbdata[which(imdbdata$isAdult == 0 & (imdbdata$titleType=="tvSeries"|imdbdata$titleType=="tvMiniSeries"|imdbdata$titleType=="tvShort"|imdbdata$titleType=="tvSpecial") & imdbdata$numVotes > m),]
# imdbclean = imdbclean[order(imdbclean$primaryTitle),]
# tv_new = data.frame(Title=imdbclean$primaryTitle,StartYear=imdbclean$startYear,
#                     EndYear=imdbclean$endYear,Type=imdbclean$titleType,
#                     Rating=imdbclean$averageRating,Votes=imdbclean$numVotes,
#                     Length=imdbclean$runtimeMinutes,
#                     Action=grepl("Action",imdbclean$genres),
#                     Adventure=grepl("Adventure",imdbclean$genres),
#                     Animation=grepl("Animation",imdbclean$genres),
#                     Biography=grepl("Biography",imdbclean$genres),
#                     Comedy=grepl("Comedy",imdbclean$genres),
#                     Crime=grepl("Crime",imdbclean$genres),
#                     Documentary=grepl("Documentary",imdbclean$genres),
#                     Drama=grepl("Drama",imdbclean$genres),
#                     Fantasy=grepl("Fantasy",imdbclean$genres),
#                     Horror=grepl("Horror",imdbclean$genres),
#                     Mystery=grepl("Mystery",imdbclean$genres),
#                     Romance=grepl("Romance",imdbclean$genres),
#                     SciFi=grepl("Sci-Fi",imdbclean$genres),
#                     Short=grepl("Short",imdbclean$genres),
#                     Thriller=grepl("Thriller",imdbclean$genres))
#
# tv <- tv_new %>%
#   mutate_if(is.character,
#             stri_trans_general,
#             id = "latin-ascii")
#
# tv$EndYear[which(tv$EndYear == "\\N")] = NA
# tv$Type[which(tv$Type == "\\N")] = NA
#
# tv$StartYear = as.numeric(tv$StartYear)
# tv$EndYear = as.numeric(tv$EndYear)
# tv$Type = as.factor(tv$titleType)
# tv$Rating = as.numeric(tv$Rating)
# tv$Votes = as.numeric(tv$Votes)
# tv$Length = as.numeric(tv$Length)
# tv$Action = as.logical(tv$Action)
# tv$Adventure = as.logical(tv$Adventure)
# tv$Animation = as.logical(tv$Animation)
# tv$Biography = as.logical(tv$Biography)
# tv$Comedy = as.logical(tv$Comedy)
# tv$Crime = as.logical(tv$Crime)
# tv$Documentary = as.logical(tv$Documentary)
# tv$Drama = as.logical(tv$Drama)
# tv$Fantasy = as.logical(tv$Fantasy)
# tv$Horror = as.logical(tv$Horror)
# tv$Mystery = as.logical(tv$Mystery)
# tv$Romance = as.logical(tv$Romance)
# tv$SciFi = as.logical(tv$SciFi)
# tv$Short = as.logical(tv$Short)
# tv$Thriller = as.logical(tv$Thriller)
#
#for (i in unique(tv$Title[which(duplicated(tv$Title))])){
#  tv$Title[which(tv$Title==i)] = paste(tv$Title[which(tv$Title==i)]," (",as.roman(1:length(which(tv$Title==i))), ")",sep="")
#}

