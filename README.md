botscan
================
Kurt Wirth
2018-10-04

A package extending the capability of
[botometer](https://github.com/IUNetSci/botometer-python) by measuring
suspected bot activity in any given Twitter query. This README is
derived from Matt Kearney’s excellent
[rtweet](\(https://github.com/mkearney/rtweet\)) documentation.

## Install

Install from GitHub with the following code:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("mkearney/botrnot")
devtools::install_github("kurtawirth/botscan")
```

This package connects <code>botometer</code> to <code>rtweet</code>. As
a result, each user must have previously acquired authentication from
Twitter and instructions to do that [can be found
here](http://rtweet.info/articles/auth.html).

You will also need to install the latest version of Python, as botscan
accesses Python-based botometer.

## Usage

There are two functions currently live for botscan.

To begin, the user must first enter the following code, inserting their
keys where appropriate:

``` setup
bom <- setup_botscan("YourMashapeKey", 
                     "YourTwitterConsumerKey", 
                     "YourTwitterConsumerSecret", 
                     "YourTwitterAccessToken", 
                     "YourTwitterAccessTokenSecret")
```

Next, the fun begin with `botscan`.

Its first argument takes any Twitter query, complete with boolean
operators if desired, surrounded by quotation marks.

The next two arguments specify the number of tweets to extract and
whether to include retweets. The fourth argument takes a number, less
than one, that represents the desired threshold at which an account
should be considered a bot. The default is .899, understood as any
account that is more than 89.9 percent likely to be a bot should be
considered a bot.

The next argument allows the user to toggle between user-level and
conversation-level summaries. The default is set to conversation-level
data, understood as the proportion of the queried conversation that is
bot-related. If <code>user\_level</code> is set to <code>TRUE</code>,
<code>botscan</code> will return user-level data, understood to be the
proportion of the queried conversation’s authors that are estimated to
be bots.

``` r
## load botscan
library(botscan)

## Enter query surrounded by quotation marks
botscan("#rstats")
#> [1] 0.1642276

## Result is percentage - in this case, 16.42276%.

## If desired, choose the threshold
botscan("#rstats", threshold = .995)
#> [1] 0.02398524

## Result is percentage - in this case, 2.398524%.

##If desired, scan only users rather than the conversation as a whole.
botscan("rstats", user_level = TRUE)
#> [1] 0.1505155

## Result is percentage - in this case, 15.05155%.
```

This process takes some time, as botscan is currently built on a loop of
botometer. Efforts to mainstream this process are set as future goals. A
standard pull of tweets via <code>botscan</code> can take anywhere from
fifteen minutes to an hour or more.

Twitter rate limits cap the number of search results returned to 18,000
every 15 minutes. Thus, excessive use of <code>botscan</code> in a short
amount of time may result in a warning and inability to pull results. In
this event, simply wait 15 minutes and try again. In an effort to avoid
the Twitter rate limit cap, <code>botscan</code> defaults to returning
1000 results.
