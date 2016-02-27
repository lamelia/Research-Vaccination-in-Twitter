install.packages('tm')
library('tm')
twit.corpus <- Corpus(VectorSource(stem_result$stemming))
twit.dtm <- TermDocumentMatrix(twit.corpus)
twit.dtm

#most mentioned words: words that are mention more than or equal to 7000 times
z <- inspect( twit.dtm[ findFreqTerms(twit.dtm,lowfreq=7000), dimnames(twit.dtm)$Docs] )
li <- rowSums(z)
li <- sort(li, decreasing  = TRUE)
#top 50 words mentioned >= 7000 times
li[1:50]