#' botscan
#'
#' Scans Twitter Conversations For Bots
#'
#' Takes a Twitter query and produces the proportion of users within that
#' conversation that are likely to be bots.
#'
#' @author Kurt Wirth and Ryan T. Moore
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

# Introduce the function

botscan <- function(x, th = 0.899, user_level = FALSE) {
  
  tweets <- rtweet::search_tweets(x, n = 1000, include_rts = FALSE)

  # Take the usernames and turn them into a vector
  
  users <- tweets$screen_name
  
  # Initialize user data list:
  
  df_userbots <- data.frame()
  
  # Run these usernames through botcheck via a loop
  
  for(user_idx in 1:length(users)){
    
    tmp_userlist <- bom$check_account(users[user_idx])
    
    tmp_user_df <- as.data.frame(tmp_userlist)
    
    tmp_user_df <- tmp_user_df %>% 
      mutate_if(is.factor, as.character)
 
    df_userbots <- dplyr::bind_rows(df_userbots, tmp_user_df)   

  }
  
  # Check scores against given threshold
  
  nbots <- sum(df_userbots$scores.universal > th)
  
  # Filter out accounts that fall below the given threshold
  
  bots <- dplyr::filter(df_userbots, (df_userbots$scores.universal > th))
  
  # Return what percentage of users in the search are estimated to be bots
  # according to the given threshold
  
  if(user_level) {
  
    n <- length(unique(tweets$screen_name))
    
    return(nbots/n)
    
  # Return what percentage of tweets in the search were authored by suspected
  # bots according to the given threshold
  
  } else {
    
    n <- length(tweets$screen_name)
    
    return(sum(tweets$screen_name %in% bots$user.screen_name) / n)

  }

}