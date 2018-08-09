#' setup_botscan
#'
#' Initialize Keys, Tokens, and Secrets for Twitter Access
#'
#' Takes a Twitter query and produces the percentage of users within that
#' conversation that are likely to be bots.
#'
#' @author Ryan T. Moore
#'
#' @param mashape_key A string representing the mashape key
#' 
#' @param consumer_key A string representing the consumer key
#' 
#' @param consumer_secret A string representing the consumer secret
#' 
#' @param access_token A string representing the access token
#' 
#' @param access_token_secret A string representing the access token secret
#' 
#' @return A list of length two with components
#' \item{mashape_key}{The mashape key}
#' \item{twitter_app_auth}{A list of length four, including the consumer_key, 
#' consumer_secret, access_token, and access_token_secret}
#'
#' @examples
#' \dontrun{setup_botscan("a", "b", "c", "d", "e")}
#' 
#' # The above example fails, since each key, secret, or token must be created
#' # by the user via ...
#' 
#' @export

setup_botscan <- function(mashape_key, consumer_key, consumer_secret, 
                          access_token, access_token_secret){
  
  return(list(mashape_key = mashape_key, 
              twitter_app_auth = list(consumer_key = consumer_key,
                                      consumer_secret = consumer_secret,
                                      access_token = access_token,
                                      access_token_secret = access_token_secret)
              )
         )
}