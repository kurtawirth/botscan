#' botscan
#'
#' Scans Twitter Conversations For Bots
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
#' @param api A string specifying which Twitter API to collect data from, the 
#' \code{"stream"} or \code{"search"} API.  Defaults to \code{"stream"}. 
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
#' @details 
#' 
#' \code{botscan()} takes a Twitter query and produces the proportion of the 
#' conversation and the users within that conversation that are likely to be bots.
#' 
#' To get a sample of tweets from the stream API, set \code{x = ""}.
#' 
#' The \code{api} argument defaults to \code{"stream"}, using Twitter's 
#' streaming API and collecting tweets which post after the function is 
#' called.  The other option, \code{"search"}, uses Twitter's search API, 
#' collecting tweets which posted before the function was called.
#'
#' @return A list of length 3: \itemize{
#'    \item \code{df}  Dataframe including all data. This includes original data 
#'    for each tweet as well as BotOMeter scores for each.
#'    \item \code{prop_user_level_bots}  User-level estimate of proportion of 
#'    bots among accounts in the searched conversation.
#'    \item \code{prop_convo_level_bots}  Conversation-level estimate of the 
#'    proportion of tweets in the searched conversation that are by bots.
#' }
#' 
#'
#' @examples
#' \dontrun{botscan("#rtweets")}
#' \dontrun{botscan("biden")}
#' 
#' ## The above examples fail unless you have created and installed Twitter 
#' ## tokens, per instructions provided at http://rtweet.info/articles/auth.html.
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

botscan <- function(x = "#rstats", external_data = "NA", timeout = 30, threshold = 0.430, api = "stream", 
                    n_tweets = 1000, retweets = FALSE, parse = TRUE, 
                    verbose = TRUE) {
  
  # If external_data is provided, use that data. Otherwise, find data.
  
  if(length(external_data) == 1) {
  
    # If api = "stream" (default), then use Twitter's Streaming API
    
    if(api == "stream") {
      
      tweets <- rtweet::stream_tweets(x, timeout = timeout, parse = parse)
    
    # If api = "search", then use Twitter's Search API
      
      } else {
        
        if(api == "search") {
          
          tweets <- rtweet::search_tweets(x, n = n_tweets, include_rts = retweets)
          
        } else {
          
          stop("Argument 'api' must be one of 'stream' or 'search'.  Please 
          reset this argument and rerun botscan().")
          
        }
        
      }
  } else {tweets = external_data}

  # Store unique usernames as a vector:
  
  users_unique <- as.character(unique(tweets$screen_name))
  
  # Initialize user data list:
  
  df_userbots <- data.frame()
  
  if(verbose == TRUE){
    
    cat("Starting user account checking,", length(users_unique), "accounts.\n")
  
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
  
  df_userbots <- dplyr::left_join(df_userbots, tweets, 
                                  by = c("user.user_data.screen_name" = "screen_name"))
  
  # Filter out accounts that fall below the given threshold
  
  bots <- dplyr::filter(df_userbots, cap.universal > threshold)
  
  # Calculate the proportion of users in the search estimated to be bots
  # (according to the given threshold)
  
  # Check scores against given threshold
  
  nbots <- sum(
    unique(tweets$screen_name) %in% bots$user.user_data.screen_name)
    
  n <- length(unique(tweets$screen_name))
  
  prop_user_level_bots <- (nbots / n)
    
  # Calculate proportion of tweets in the search that were authored by suspected
  # bots (according to the given threshold)
  
  n <- length(tweets$screen_name)
  
  prop_convo_level_bots <- (sum(tweets$screen_name %in% bots$user.user_data.screen_name) / n)
  
  
  return(list(df = df_userbots, 
              prop_user_level_bots = prop_user_level_bots,
              prop_convo_level_bots = prop_convo_level_bots))

}