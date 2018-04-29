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
#' @param y Number, less than one, that determines which botornot probability 
#' threshhold to return. Default is set at 0.899. Only users estimated to be 
#' more likely than the threshhold provided will be regarded as a bot.
#'
#' @return Percentage of users within the requested conversation that are
#' estimated by botornot to be a bot.
#'
#' 
#' @examples
#' \dontrun{botscan("#rtweets")}
#' \dontrun{botscan("trump")}
#' 
#' Examples fail unless you have created and installed Twitter tokens, per
#' instructions provided at http://rtweet.info/articles/auth.html.
#' 
#' @export

botscan <- function(x,y = 0.899){
  tweets <- rtweet::search_tweets(x, n = 1000, include_rts = FALSE)

  userbots <- botrnot::botornot(tweets, fast = TRUE)

  nbots <- sum(userbots$prob_bot > y)

  n <- length(unique(tweets$screen_name))

  return(nbots/n)
}
