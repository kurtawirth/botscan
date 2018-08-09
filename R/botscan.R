#' botscan
#'
#' Scans Twitter Conversations For Bots
#'
#' Takes a Twitter query and produces the proportion of users within that
#' conversation that are likely to be bots.
#'
#' @author Kurt Wirth
#'
#' @param x A Twitter search query in quotation marks.
#' 
#' @param th A number between zero and one that determines which botornot probability 
#' threshhold to return. Default is set at 0.899. Only users estimated to be 
#' more likely than the threshhold provided will be regarded as a bot.
#' 
#' @param user_level A logical that determines whether to analyze
#' conversation-level or user-level data. Default is set to \code{FALSE}, understood
#' as analyzing conversation-level data.
#'
#' @return Percentage of users, equal to or less than 1 (read: 100%) and zero 
#' within the requested conversation that are estimated by botornot to be a bot.
#' 
#' When \code{user_level} is set to \code{TRUE}, the value returned is the percentage of
#' users within the conversation that are estimated to be bots. When
#' \code{user_level} is set to \code{FALSE}, the value returned is the percentage of the
#' conversation that is estimated to be authored by bots.
#'
#' @examples
#' \dontrun{botscan("#rtweets")}
#' \dontrun{botscan("trump")}
#' 
#' ##The above examples fail unless you have created and installed Twitter 
#' ##tokens, per instructions provided at http://rtweet.info/articles/auth.html.
#' 
#' @export

botscan <- function(x, th = 0.899, user_level = FALSE) {
  tweets <- rtweet::search_tweets(x, n = 1000, include_rts = FALSE)

  userbots <- botrnot::botornot(tweets, fast = TRUE)
  
  nbots <- sum(userbots$prob_bot > th)
  
  bots <- dplyr::filter(userbots, (userbots$prob_bot > th))
  
  if(user_level) {
  
    n <- length(unique(tweets$screen_name))
    
    return(nbots/n)
  
  } else {
    
    n <- length(tweets$screen_name)
    
    return(sum(tweets$screen_name %in% bots$user)/n)
    
  }

}
