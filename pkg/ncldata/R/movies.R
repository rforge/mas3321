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
#' @name movies
#' @docType data
#' @usage data(movies)
#' @return A data frame with 4988 rows and 23 variables.
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

# FURTHER INFO
# ---------------------------------------------
# There are two sources of data, the first provides .tsv formatted files: https://datasets.imdbws.com/
# The second provides .list files: ftp://ftp.fu-berlin.de/pub/misc/movies/database/frozendata/
#
# The .list files were converted to .tsv with:
#
# gawk -v RS='-------------------------------------------------------------------------------'
# -v FS="\n" '{printf "%s\t",gensub("MV: | \\([0-9]+.*\\)","","g",$2);printf "%s\t",gensub(/.*
# ([0-9][0-9][0-9][0-9]).*/,"\\1","g",$2);for(j=3;j<=NF;j++){if($j ~ /RE: [rR]ated/) {printf
# "%s",gensub("RE: [rR]ated ([A-Z]+-*?[0-9]*?) .*","\\1","g",$j)}};print " ";}' mpaa-ratings-reasons.list
# | sed 's/"//g' | sed 's/"//g' > mpaa.tsv
#
# and
#
# gawk -v RS='-------------------------------------------------------------------------------'
# -v FS="\n" '{printf "%s\t",gensub("MV: |BT:| \\([0-9]+.*\\)","","g",$2);printf "%s\t",gensub(/.*
# ([0-9][0-9][0-9][0-9]).*/,"\\1","g",$2);for(j=3;j<=NF;j++){if($j ~ /BT:/) {printf "%s",gensub
# ("MV: |BT: | \\([0-9]+.*\\)","","g",$j)}};print " ";}' business.list | sed 's#"##g' > budget.tsv
#
# The headers in budget.tsv and mpaa.tsv were inserted by hand.
# Approximately 10 rows with bad data were corrected by hand.
#
# The data was imported with something like the following R commands:
#
#  imdbtitles = read.table(file = "title.basics.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="utf-8")
#  imdbratings = read.table(file = "title.ratings.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="utf-8")
#  imdbbudget = read.table(file = "budget.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="iso-8859-13")
#  imdbmpaa = read.table(file = "mpaa.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="iso-8859-13")
#  imdbdata = merge(imdbtitles,imdbratings,by = "tconst")
#  m=20000
#  imdbclean = imdbdata[which(imdbdata$isAdult == 0 & imdbdata$titleType=="movie" & imdbdata$numVotes > m),]
#  c = mean(imdbclean$averageRating)
#  imdbwr = (imdbclean$numVotes / (imdbclean$numVotes+m)) * imdbclean$averageRating + (m / (imdbclean$numVotes+m)) * c
#  imdbclean = imdbclean[order(imdbclean$primaryTitle),]
#
#  imdbclean = merge(imdbclean, imdbbudget,all.x=T,all.y=F)
#  imdbclean=imdbclean[!duplicated(imdbclean$tconst), ]
#  imdbclean$BudgetValue = strtoi(gsub(",", "", sub(".*?([0-9,]+).*","\\1",imdbclean$Budget)))
#  imdbclean$BudgetCurrency = sub("([A-z]*).*","\\1",imdbclean$Budget)
#  imdbclean$BudgetCurrency[which(imdbclean$BudgetCurrency=="")]=NA
#
#  imdbmpaa = read.table(file = "mpaa.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="iso-8859-13")
#  imdbclean = merge(imdbclean, imdbmpaa,all.x=T,all.y=F)
#  imdbclean=imdbclean[!duplicated(imdbclean$tconst), ]
#
#  movies_new = data.frame(Title=imdbclean$primaryTitle,Year=imdbclean$startYear,
#                          Rating=imdbclean$averageRating,Votes=imdbclean$numVotes,
#                          Length=imdbclean$runtimeMinutes,
#                          Budget=imdbclean$BudgetValue,
#                          BudgetCurrency=imdbclean$BudgetCurrency,
#                          mpaa=imdbclean$mpaa,
#                          Action=grepl("Action",imdbclean$genres),
#                          Adventure=grepl("Adventure",imdbclean$genres),
#                          Animation=grepl("Animation",imdbclean$genres),
#                          Biography=grepl("Biography",imdbclean$genres),
#                          Comedy=grepl("Comedy",imdbclean$genres),
#                          Crime=grepl("Crime",imdbclean$genres),
#                          Documentary=grepl("Documentary",imdbclean$genres),
#                          Drama=grepl("Drama",imdbclean$genres),
#                          Fantasy=grepl("Fantasy",imdbclean$genres),
#                          Horror=grepl("Horror",imdbclean$genres),
#                          Mystery=grepl("Mystery",imdbclean$genres),
#                          Romance=grepl("Romance",imdbclean$genres),
#                          SciFi=grepl("Sci-Fi",imdbclean$genres),
#                          Short=grepl("Short",imdbclean$genres),
#                          Thriller=grepl("Thriller",imdbclean$genres))
#
#  movies <- movies_new %>%
#    mutate_if(is.character,
#              stri_trans_general,
#              id = "latin-ascii")
#
#  movies$mpaa[which(movies$mpaa == "NC-17 ")] = "NC-17"
#  movies$mpaa[which(movies$mpaa == "PG ")] = "PG"
#  movies$mpaa[which(movies$mpaa == "PG-13 ")] = "PG-13"
#  movies$mpaa[which(movies$mpaa == "R ")] = "R"
#  movies$mpaa[which(movies$mpaa == " ")] = NA
#
#  movies$Year = as.numeric(movies$Year)
#  movies$Rating = as.numeric(movies$Rating)
#  movies$Votes = as.numeric(movies$Votes)
#  movies$Length = as.numeric(movies$Length)
#  movies$Budget = as.numeric(movies$Budget)
#  movies$BudgetCurrency = as.factor(movies$BudgetCurrency)
#  movies$mpaa = as.factor(movies$mpaa)
#  movies$Action = as.logical(movies$Action)
#  movies$Adventure = as.logical(movies$Adventure)
#  movies$Animation = as.logical(movies$Animation)
#  movies$Biography = as.logical(movies$Biography)
#  movies$Comedy = as.logical(movies$Comedy)
#  movies$Crime = as.logical(movies$Crime)
#  movies$Documentary = as.logical(movies$Documentary)
#  movies$Drama = as.logical(movies$Drama)
#  movies$Fantasy = as.logical(movies$Fantasy)
#  movies$Horror = as.logical(movies$Horror)
#  movies$Mystery = as.logical(movies$Mystery)
#  movies$Romance = as.logical(movies$Romance)
#  movies$SciFi = as.logical(movies$SciFi)
#  movies$Short = as.logical(movies$Short)
#  movies$Thriller = as.logical(movies$Thriller)
#
#for (i in unique(movies$Title[which(duplicated(movies$Title))])){
#  movies$Title[which(movies$Title==i)] = paste(movies$Title[which(movies$Title==i)]," (",as.roman(1:length(which(movies$Title==i))), ")",sep="")
#}
