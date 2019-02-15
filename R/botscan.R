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
#' @param timeout A number representing the number of seconds the user wishes to
#' stream tweets on the given search term. This is only applied when using the
#' default STREAM Twitter API. Default is 30 seconds.
#'
#' @param threshold A number between zero and one that determines which botornot 
#' probability threshold to return. Default is set at 0.43. Only users estimated 
#' to be more likely than the threshold provided will be regarded as a bot.
#' 
#' @param stream A logical indicating whether the streaming API or the search API 
#' is queried.  Defaults to \code{TRUE}, using the streaming API.
#' 
#' @param n_tweets A number representing how many tweets to extract.
#' 
#' @param retweets A logical specifying whether to include retweets in 
#' the set of tweets to be extracted.  Defaults to \code{FALSE}.
#' 
#' @param parse A logical specifying whether to automatically parse data into
#' structured data when querying the streaming API. Defaults to \code{TRUE}, 
#' making results more usable for most users. Setting to \code{FALSE} will
#' produce less structured data but allow for faster processing for very large
#' data projects.
#' 
#' @param verbose A logical that determines whether to print periodic progress
#' updates.
#'
#' @return A list of length 3: \enumerate{
#'    \item Dataframe including all data. This includes original data for each tweet as
#'          well as BotOMeter scores for each.
#'    \item User-level estimate for the given search.
#'    \item Conversation-level estimate for the given search.
#' }
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

botscan <- function(x, timeout = 30, threshold = 0.430, stream = TRUE, 
                    n_tweets = 1000, retweets = FALSE, parse = TRUE, 
                    verbose = TRUE) {
  
  # If "stream" is TRUE (default), then use Twitter's Streaming API
  
  if(stream) {
    
    tweets <- rtweet::stream_tweets(x, timeout = timeout, parse = parse)
  
  # If "stream" if FALSE, then use Twitter's Search API
    
    } else {
    
    tweets <- rtweet::search_tweets(x, n = n_tweets, include_rts = retweets)
    
  }

  # Store unique usernames as a vector:
  
  users_unique <- as.character(unique(tweets$screen_name))
  
  # Initialize user data list:
  
  df_userbots <- data.frame()
  
  if(verbose == TRUE){
    
    cat("Starting user account checking\n")
  
  } 
  
  # Run these usernames through botcheck via a loop
  
  for(user_idx in 1:length(users_unique)){
    
    if(verbose == TRUE){
      
      if(user_idx %% 100 == 0){
        cat("Checking user account ", users_unique[user_idx], 
            ", number ", user_idx, "\n", sep = "")
      }
      
    }
    
    tryCatch({
      
      tmp_userlist <- bom$check_account(users_unique[user_idx])
    
      tmp_user_df <- as.data.frame(tmp_userlist)
    
      tmp_user_df <- tmp_user_df %>% 
        dplyr::mutate_if(is.factor, as.character)
 
      df_userbots <- dplyr::bind_rows(df_userbots, tmp_user_df)
    
    }, error = function(e) print(e))

  }
  
  # Replicate results from unique screen names to embody all screen names:
  # (Also, adds the variables from tweets to the df_userbots)
  
  df_userbots <- left_join(df_userbots, tweets, by = c("user.screen_name" = "screen_name"))
  
  # Filter out accounts that fall below the given threshold
  
  bots <- dplyr::filter(df_userbots, cap.universal > threshold)
  
  # Calculate the proportion of users in the search estimated to be bots
  # (according to the given threshold)
  
  # Check scores against given threshold
  
  nbots <- sum(
    unique(tweets$screen_name) %in% bots$user.screen_name)
    
  n <- length(unique(tweets$screen_name))
  
  prop_user_level_bots <- (nbots / n)
    
  # Calculate proportion of tweets in the search that were authored by suspected
  # bots (according to the given threshold)
  
  n <- length(tweets$screen_name)
  
  prop_convo_level_bots <- (sum(tweets$screen_name %in% bots$user.screen_name) / n)
  
  
  return(list(df = df_userbots, 
              prop_user_level_bots = prop_user_level_bots,
              prop_convo_level_bots = prop_convo_level_bots))

}