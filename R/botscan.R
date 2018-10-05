#' botscan
#'
#' Scans Twitter Conversations For Bots
#'
#' Takes a Twitter query and produces the proportion of users within that
#' conversation that are likely to be bots.
#'
#' @author Kurt Wirth and Ryan T. Moore
#'
#' @param x A string representing a Twitter search query, in quotation marks.
#' 
#' @param n_tweets A number representing how many tweets to extract.
#' 
#' @param retweets A logical specifying whether to include retweets in 
#' the set of tweets to be extracted.  Default is set to \code{FALSE}.
#'
#' @param threshold A number between zero and one that determines which botornot 
#' probability threshold to return. Default is set at 0.899. Only users estimated to be 
#' more likely than the threshold provided will be regarded as a bot.
#' 
#' @param user_level A logical that determines whether to analyze
#' conversation-level or user-level data. Default is set to \code{FALSE}, understood
#' as analyzing conversation-level data.
#' 
#' @param verbose A logical that determines whether to print periodic progress
#' updates.
#'
#' @return Proportion of users within the requested conversation that are 
#' estimated by botometer to be bots.
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

botscan <- function(x, n_tweets = 1000, retweets = FALSE, threshold = 0.899, 
                    user_level = FALSE, verbose = TRUE) {
  
  tweets <- rtweet::search_tweets(x, n = n_tweets, include_rts = retweets)

  # Take the usernames and turn them into a vector
  
  users <- tweets$screen_name
  
  # Initialize user data list:
  
  df_userbots <- data.frame()
  
  if(verbose == TRUE){
    cat("Starting user account checking\n")
  } 
  
  # Run these usernames through botcheck via a loop
  
  for(user_idx in 1:length(users)){
    
    if(verbose == TRUE){
      if(user_idx %% 100 == 0){
        cat("Checking user account ", users[user_idx], ", number ", user_idx, "\n", sep = "")
      }
    }
    
    tryCatch({
      
      tmp_userlist <- bom$check_account(users[user_idx])
    
      tmp_user_df <- as.data.frame(tmp_userlist)
    
      tmp_user_df <- tmp_user_df %>% 
        dplyr::mutate_if(is.factor, as.character)
 
      df_userbots <- dplyr::bind_rows(df_userbots, tmp_user_df)
    
    }, error = function(e) print(e))

  }
  
  # Check scores against given threshold
  
  nbots <- sum(df_userbots$cap.universal > threshold)
  
  # Filter out accounts that fall below the given threshold
  
  bots <- dplyr::filter(df_userbots, (df_userbots$cap.universal > threshold))
  
  # Return the proportion of users in the search estimated to be bots
  # (according to the given threshold)
  
  if(user_level) {
  
    n <- length(unique(tweets$screen_name))
    
    return(nbots / n)
    
  # Return the proportion of tweets in the search that were authored by suspected
  # bots (according to the given threshold)
  
  } else {
    
    n <- length(tweets$screen_name)
    
    return(sum(tweets$screen_name %in% bots$user.screen_name) / n)

  }

}