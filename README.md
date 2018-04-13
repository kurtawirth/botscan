botscan
================
Kurt Wirth
April 13, 2018

A package extending the capability of [botrnot](https://github.com/mkearney/botrnot) by measuring suspected bot activity in any given Twitter query. The model is 91.78% accurate when classifying bots and 92.61% accurate when classifying non-bots, per [mkearney](https://github.com/mkearney).

Install
-------

Install from Github with the following code:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("kurtawirth/botscan")
```

This package connects <code>botrnot</code> to [<code>rtweet</code>](https://github.com/mkearney/rtweet). As a result, each user must have previously acquired authentication from Twitter and instructions to do that [can be found here](http://rtweet.info/articles/auth.html).

Usage
-----

There is only one function currently live for botscan. The sole argument takes any Twitter query and produces the percentage of the conversation's authors that are at least 90% likely to be a bot.

``` r
## load botscan
library(botscan)

## enter query surrounded by quotation marks
botscan("#rstats")
#> [1] 0.1642276

## Result is percentage - in this case, 16.42276%.
```

Twitter rate limits cap the number of search results returned to 18,000 every 15 minutes. Thus, excessive use of <code>botscan</code> in a short amount of time may result in a warning and inability to pull results. In this event, simply wait 15 minutes and try again.

Notes
-----

\*<code>Botscan</code> uses a threshold of 90% likeliness to classify a handle as a bot.

\*Probability is calculated by unique users, not per message. Thus, <code>botscan</code> has no way to control for bot conversation volume.

\*Tweets are gathered by searching Twitter for a given query.

\*In an effort to avoid the Twitter rate limit cap, <code>botscan</code> limits searches to 1000 results.

Expected Updates
----------------

\*Add argument to allow for calculation based on conversation volume rather than unique authors.

\*Add argument that will make the <code>botscan</code> threshold of 90% customizable.

\*Add argument to allow for customization of method by which handles are classified, including averaging all probabilities

\*Add argument to allow access of gathered data such as <code>rtweet</code> file and <code>botrnot</code> list of handles and corresponding bot probability.

\*Add argument to introduce additional Twitter gathering data techniques (such as streaming and geo-location).
