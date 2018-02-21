# Generation of the movie data set
#
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

#' Generation of the movie data set
#'
#' @importFrom utils read.table
generateMoviesDataSetFromSourceData = function() {
  imdbtitles = read.table(file = "title.basics.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="utf-8")
  imdbratings = read.table(file = "title.ratings.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="utf-8")
  imdbbudget = read.table(file = "budget.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="iso-8859-13")
  imdbmpaa = read.table(file = "mpaa.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="iso-8859-13")
  imdbdata = merge(imdbtitles,imdbratings,by = "tconst")
  m=20000
  imdbclean = imdbdata[which(imdbdata$isAdult == 0 & imdbdata$titleType=="movie" & imdbdata$numVotes > m),]
  c = mean(imdbclean$averageRating)
  imdbwr = (imdbclean$numVotes / (imdbclean$numVotes+m)) * imdbclean$averageRating + (m / (imdbclean$numVotes+m)) * c
  imdbclean = imdbclean[order(imdbclean$primaryTitle),]

  imdbclean = merge(imdbclean, imdbbudget,all.x=T,all.y=F)
  imdbclean=imdbclean[!duplicated(imdbclean$tconst), ]
  imdbclean$BudgetValue = strtoi(gsub(",", "", sub(".*?([0-9,]+).*","\\1",imdbclean$Budget)))
  imdbclean$BudgetCurrency = sub("([A-z]*).*","\\1",imdbclean$Budget)
  imdbclean$BudgetCurrency[which(imdbclean$BudgetCurrency=="")]=NA

  imdbmpaa = read.table(file = "mpaa.tsv", header=T, sep="\t",quote="",comment.char="",fileEncoding="iso-8859-13")
  imdbclean = merge(imdbclean, imdbmpaa,all.x=T,all.y=F)
  imdbclean=imdbclean[!duplicated(imdbclean$tconst), ]

  movies_new = data.frame(Title=as.character(imdbclean$primaryTitle),Year=as.integer(as.character(imdbclean$startYear)),
                          Rating=imdbclean$averageRating,Votes=imdbclean$numVotes,
                          Length=as.integer(as.character(imdbclean$runtimeMinutes)),
                          Budget=imdbclean$BudgetValue,
                          BudgetCurrency=imdbclean$BudgetCurrency,
                          mpaa=imdbclean$mpaa,
                          Action=grepl("Action",imdbclean$genres),
                          Adventure=grepl("Adventure",imdbclean$genres),
                          Animation=grepl("Animation",imdbclean$genres),
                          Biography=grepl("Biography",imdbclean$genres),
                          Comedy=grepl("Comedy",imdbclean$genres),
                          Crime=grepl("Crime",imdbclean$genres),
                          Documentary=grepl("Documentary",imdbclean$genres),
                          Drama=grepl("Drama",imdbclean$genres),
                          Fantasy=grepl("Fantasy",imdbclean$genres),
                          Horror=grepl("Horror",imdbclean$genres),
                          Mystery=grepl("Mystery",imdbclean$genres),
                          Romance=grepl("Romance",imdbclean$genres),
                          SciFi=grepl("Sci-Fi",imdbclean$genres),
                          Short=grepl("Short",imdbclean$genres),
                          Thriller=grepl("Thriller",imdbclean$genres))
}

