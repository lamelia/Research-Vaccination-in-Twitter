#installing tm packages
install.packages('tm')
library('tm')
stop_stem_result <- read.csv("stop_stem_result.csv")
#create corpus
twit.corpus <- Corpus(VectorSource(stop_stem_result$stop_stemmed))
#create sparse matrix
twit.dtm <- TermDocumentMatrix(twit.corpus)
twit.dtm

#most mentioned words: words that are mention more than or equal to 7000 times
z <- inspect( twit.dtm[ findFreqTerms(twit.dtm,lowfreq=7000), dimnames(twit.dtm)$Docs] )
li <- rowSums(z)
li <- sort(li, decreasing  = TRUE)
#top 50 words mentioned >= 7000 times
li[1:50]

#Next:
#remove stopwords, words that are not relevant ex: di, dari, ini, itu using sastrawi
#make distribution of words count 
#remove least frequently mentioned words


#lowfreq=500
#most mentioned words: words that are mention more than or equal to 500 times
z <- inspect( twit.dtm[ findFreqTerms(twit.dtm,lowfreq=500), dimnames(twit.dtm)$Docs] )
li <- rowSums(z)
li <- sort(li, decreasing  = TRUE)
write.csv(li,"li500.csv")


#library(dplyr), count number of tweets per user
library(dplyr)
byUsername <- group_by(stop_stem_result,Username)
(sumUsername <- summarize(byUsername,count=n()))
sumUsername_tbl <- as.data.frame(sumUsername)
write.csv(sumUsername_tbl,"sumUsername_tbl.csv")
