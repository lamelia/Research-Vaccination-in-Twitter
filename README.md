# Research Vaccination in Twitter
Codes are written in R

The focus of this research is semantic analysis of the opinions toward vaccination from the tweets by using
text mining approach, study case in Indonesia. 

Data set:
Data consists of 807800 records/tweets collected from 2012 to 2015. There are 4 variables: tweets, date, username, and twitter account name.

Steps of analysis:

>> Data collection 

Collect all tweets from 2012 to 2015 that contain vaccination related words. The related words (in Indonesian) are: "vaksin", "imunisasi", "vaksinasi"

>> Data exploration and preparation that includes: 

- removing duplicate records. Duplicate records are records with the same tweet, username, twitter account , and date.
- removing link from tweet
- transforming emoticons to words, ex: transform :) to emoticon_happy and :( to emoticon_sad
- transforming all tweets to lower cases
- deleting tweets that does not associate with vaccine/does not contain vaccination related words, but the username and twitter account contain vaccination related words, ex: @vaksin, @imunisasi_anak
- checking number of tweets collected per month from 2012 to 2015.
- removing all punctuation in the tweets except @, #, and numeric
- stemming words using Indonesian stemmer: Sastrawi (https://github.com/sastrawi/sastrawi)
- Creating sparse matrix from all tweets

>> Data analysis 

I try to find interesting things from the data before building my model to predict a tweet as pro, anti, neutral, or irrelevant to vaccination. 

What I've done:

- Checking the most mentioned words from the tweets collected
- Will remove least mentioned words form the tweets, will be using number of mentioned words threshold, for example, i would remove words that are mentioned less than 500 times from analysis.
- Will create cluster analysis of words and figuring what words are frequently appear with other particular words
