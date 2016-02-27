install.packages('tm')
library('tm')
twit.corpus <- Corpus(VectorSource(stem_result$stemming))
twit.dtm <- TermDocumentMatrix(twit.corpus)
twit.dtm
#most used words
muw <- findFreqTerms(twit.dtm,lowfreq=20000)

z <- inspect( twit.dtm[ findFreqTerms(twit.dtm,lowfreq=7000), dimnames(twit.dtm)$Docs] )
li <- rowSums(z)
li <- sort(li, decreasing  = TRUE)
li[1:50]