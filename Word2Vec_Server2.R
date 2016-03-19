
#kmeans with from bag of words doesnt work well/yield satisfied result, so we use word2vec instead, to help us with clustering
#references: https://github.com/bmschmidt/wordVectors
#work at aws

install.packages("devtools")
library(devtools)
install_github("bmschmidt/wordVectors")
library(wordVectors)
model = train_word2vec("stop_stem_tweet.txt",output="tweet.vectors",threads = 3,vectors = 100,window=12) #now we can work with tweet.vectors at R GUI
nearest_to(model,model[["halal"]])

model = read.vectors("tweet.vectors")
km <- kmeans(model, 10)
write.csv(km$cluster, "clus.csv") #now we have 10 clusters

#since each clusters contain thousands words, we want to simplify this by picking 100ish words that are most mentioned from the tweets.

#list of top 100 mentioned words we obtained from previous bag-of-words method
#from file more than 500 mentioned words list, i remove very frequently mention words,e.g. vaksin, punct_question, imunisasi, vaksinasi, punct_exclam, 

#to be able to use tweet.vectors in Rgui, convert tweet.vectors to csv file, so I dont need to install word2vec in R to run tweet.vectors.

D <- read.delim("tweet.vectors", skip=1)
dim(D)
LT4000 <- read.csv("LT4000mentioned.csv") #delete 0
library(dplyr)
write.csv(D,"w2v.csv") #
w2v <- read.csv("w2v.csv")
dt_cluster <- inner_join(w2v,LT4000)
write.csv(dt_cluster,"dt_cluster.csv")
dim(dt_cluster)
dc <- dt_cluster[,3:102] #numeric variable starts from col 3 to 102, col 2 is the Term
rownames(dc) <- data_cluster[,2] #put the rownames (Term), but these Term are not in the dataframe, so the dc basically only contain numeric (matrix), we're good to go to kmeans
kmean_cluster <- kmeans(dc,10)
write.csv(kmean_cluster$cluster,"kmean_cluster.csv")


#yield 10 clusters at kmean_cluster.csv -> the words in each cluster show association or similar context

