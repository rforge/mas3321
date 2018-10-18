#' Fifa World Cup 2018 Player Statistics
#'
#' Statistics for each player taking part in the Fifa World Cup 2018,
#' held in Russia.
#'
#' There were 32 teams that qualified for the World Cup
#' tournament, with 23 players on each team.
#'
#' The values for each player was scraped from a PDF released by Fifa
#' (\url{https://www.fifadata.com/documents/FWC/2018/pdf/FWC_2018_Squadlists.pdf},
#' accessed 17 October 2018).
#'
#' @name worldCup18
#' @docType data
#' @usage data(worldCup18)
#' @return A data frame with 736 rows and 13 variables.
#' @references Data was extracted from a PDF released by Fifa before the World Cup Tournament. The file is available online at \url{https://www.fifadata.com/documents/FWC/2018/pdf/FWC_2018_Squadlists.pdf} [Accessed: 2018-10-17]
#' @note The data set contains the following fields:
#' \describe{
#' \item{#}{The number that appears on that player's shirt.  The convention at the tournament means these range from 1 to 23 (inclusive) for each team and that goalkeepers will usually be given number 1.}
#' \item{Fifa Display Name}{The name as Fifa would display it on screen or online in most situations. This typically drops middle names.}
#' \item{Last Name}{The last/family name(s) of the player.}
#' \item{First Name}{The first name(s) of the player.}
#' \item{Shirt Name}{The player's name as it will appear on the back of the player's shirt during matches.}
#' \item{DOB}{A date-stamp representing the player's date of birth.}
#' \item{POS}{The player's normal position on the team, represented as one of 4 codes: GK = Goalkeeper; DF = Defence; MF = Midfield; FW = Forward.}
#' \item{Club}{The name of the domestic club the player is signed to, along with a 3-letter code representing which country that club is located in.}
#' \item{Height}{Player height in centimetres}
#' \item{Caps}{Number of international matches the player has played in before the start of the World Cup tournament.  See \url{https://en.wikipedia.org/wiki/Cap_(sport)} for general information.}
#' \item{Goals}{The number of goals the player has previously scored in international matches before the start of the World Cup tournament.}
#' \item{Team}{The international team this player will be playing for during the 2018 Fifa World Cup.}
#' \item{Weight}{Player weight in kilograms.  In cases where this value is missing (NA), it is believed that the player was swapped onto the official team lists close to the start of the tournament, causing data about the weight to be unavailable.}}
#' @keywords datasets, football, world cup
#' @author Keith Newman
#' @examples
#' data(worldCup18)
"worldCup18"
