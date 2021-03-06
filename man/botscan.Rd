% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/botscan.R
\name{botscan}
\alias{botscan}
\title{botscan}
\usage{
botscan(
  x = "#rstats",
  external_data = NA,
  timeout = 30,
  threshold = NA,
  api = "stream",
  n_tweets = 1000,
  retweets = FALSE,
  parse = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{x}{A string representing a Twitter search query, in quotation marks.}

\item{external_data}{An optional dataframe with variable `screen_name`, 
containing Twitter handles, for example, from a conversation. By default, 
`botscan` collects tweets and creates such a data frame. If `external_data` 
is set, then `x` is ignored.}

\item{timeout}{A number representing the number of seconds the user wishes to
stream tweets on the given search term. This is only applied when using the
default STREAM Twitter API. Default is 30 seconds.}

\item{threshold}{An optional number between zero and one that determines 
which botometer probability threshold to return. Default is missing (no 
threshold). If specified, only users estimated to be more likely than the 
threshold will be regarded as a bot.}

\item{api}{A string specifying which Twitter API to collect data from, the 
\code{"stream"} or \code{"search"} API.  Defaults to \code{"stream"}.}

\item{n_tweets}{A number representing how many tweets to extract.}

\item{retweets}{A logical specifying whether to include retweets in 
the set of tweets to be extracted.  Defaults to \code{FALSE}.}

\item{parse}{A logical specifying whether to automatically parse data into
structured data when querying the streaming API. Defaults to \code{TRUE}, 
making results more usable for most users. Setting to \code{FALSE} will
produce less structured data but allow for faster processing for very large
data projects.}

\item{verbose}{A logical that determines whether to print periodic progress
updates.}
}
\value{
A list of length 3: \itemize{
   \item \code{df}  Dataframe including all data. This includes original data 
   for each tweet as well as BotOMeter scores for each.
   \item \code{prop_user_level_bots}  User-level estimate of proportion of 
   bots among accounts in the searched conversation. Missing botiness scores 
   are omitted.
   \item \code{prop_convo_level_bots}  Conversation-level estimate of the 
   proportion of tweets in the searched conversation that are by bots. 
   Missing botiness scores are omitted.
}
}
\description{
Scans Twitter Conversations For Bots
}
\details{
\code{botscan()} takes a Twitter query and produces the proportion of the 
conversation and the users within that conversation that are likely to be bots.

To get a sample of tweets from the stream API, set \code{x = ""}.

The \code{api} argument defaults to \code{"stream"}, using Twitter's 
streaming API and collecting tweets which post after the function is 
called.  The other option, \code{"search"}, uses Twitter's search API, 
collecting tweets which posted before the function was called.
}
\examples{
\dontrun{botscan("#rtweets")}
\dontrun{botscan("biden")}

## The above examples fail unless you have created and installed Twitter 
## tokens, per instructions provided at http://rtweet.info/articles/auth.html.

}
\author{
Kurt Wirth and Ryan T. Moore
}
