#' botscan
#'
#' Scans Twitter Conversations For Bots
#'
#' Takes a Twitter query and produces the percentage of users within that
#' conversation that are likely to be bots.
#'
#' @author Kurt Wirth
#'
#' @param x A Twitter search query in quotation marks.
#'
#' @return A number
#'
#' @examples
#' \dontrun{botscan("#rtweets")}
#' \dontrun{botscan("trump")}
#' @export

botscan <- function(x){
  tweets <- rtweet::search_tweets(x, n = 18000, include_rts = FALSE)

  userbots <- botrnot::botornot(tweets, fast = TRUE)

  nbots <- sum(userbots$prob_bot > "0.899")

  n <- length(unique(tweets$screen_name))

  return(nbots/n)
}
