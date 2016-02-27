#get data dfnew
#delete all punctuation except @, #, and numbers from dfnew$Tweet_edit
library(stringr)
library(stringi)

# #replace all emoticons into words, since regex has issue with emoticons
# 
# emoct <- gsub(":-\\)|:\\)|:D|:o\\)|:\\]|:3|:c\\)|:>|=\\]|8\\)|=\\)|:}|:\\^\\)| :\\)\\)|:-\\)\\)|\\^\\^|\\^_\\^",
#      " emo_happy ", df2$Tweet_edit) #give space before and after emo_happy, bc emoji in twit doesnt have space after the word before ex:"vaksin anak pertama:)"
# 
# emoct <- as.data.frame(emoct)
# gsub(":-D|:D|8-D|8D|x-D|xD|X-D|XD|=-D|=D|=-3|=3)", "emo_laugh", emoct$emoct)

#findings:
#1. give space before and after emo_happy, bc emoji in twit doesnt have space after the word before ex:"vaksin anak pertama:)"
#2. :D cant be found since all alphabet has been transformed to lowercase, instead i got :d.
#solution is to replace all emoticon into words before transform twit to lowercase


#####################remove punctuation############################################

#6 remove all punctuation with blank space except alpha numeric, at, and hastag, and underscore from df2
stem1 <- str_replace_all (df1$emo3, "[^[[:alnum:]|@|#|?|!|_| ]]", " ")
stem1 <- as.data.frame(stem1)
df1 <- cbind(df1,stem1)
#findings:
#1 still find | in stem1
#delete |
stem2 <- str_replace_all(df1$stem1, "[|]", " ")
df1 <- cbind(df1,stem2)

#findings:
#keep: _ emoticon, solution, keep _, and emoticon is converted to words, like :o -> emoji_kaget
#how to treat / - , cth: batuk/rejan solution: replace / with space before stem

write.csv(df1,"df1.csv")

#################stemming words#########################
#7 stemming words in php
#create one new table stemming with one column stem2
stemming <- df1[c(12)]
write.csv(stemming,"df_stemming-source.csv")

#after stemming in php the csv file name df_stemming-result.csv
stem_result <- read.csv("df_stemming-result.csv")
#add several columns from df1 to stem_result
stem_result <- cbind(stem_result,df1$User,df1$Username,df1$Tweet,df1$Date,df1$my)
colnames(stem_result) <- c("X","stem2","stemming","User","Username","TweetORIGINAL","Data","monthyear")
write.csv(stem_result,"stem_result.csv")
#findings:
#1. belom -> bom
# 2. berikan -> ikan
# 3. @ kehapus
# 4. _ kehapus: emo_happy -> emo happy
# 5. # kehapus
# 6. lakukan -> laku
# 7. alasan -> alas
# jangan jalanin stopword dulu


#################issue#######################

#8. issue

####religion#####
search_religion = "\\bbabi\\b|\\bhalal\\b|\\btoyyib\\b|\\bharam\\b|\\bmui\\b|\\bagama\\b|\\bislam\\b|\\benzim\\b|\\bmuslim\\b|\\bulama\\b|\\bfatwa\\b|\\bsubhat\\b|\\bmubah\\b"
stem_result$contains_religion <-  as.numeric(!is.na(str_match(stem_result$stemming, search_religion)))
sum(stem_result$contains_religion==1) #yield 24366 twit with religion issue
#aggregate tweet that contain religion issue per month
aggr_mon_rel <- aggregate(contains_religion ~ monthyear,data=stem_result,sum)

#order by month
library(qdap)
library(dplyr)
split1 <- colsplit2df(data.frame(aggr_mon_rel), sep = "/")
split1 <- cbind(split1,aggr_mon_rel$monthyear)
colnames(split1) <- c("month","year","tweet_religion","month_year")
split1$month <- as.integer(split1$month)
split1$year <- as.integer(split1$year)
order_mo_rel <- arrange(split1,year,month)
order_mo_rel <- cbind(order_mo_rel,bydate$count) #to get percentage of vac tweet related to religion
colnames(order_mo_rel) <- c('month','year','tweet_religion','month_year','alltweet')

order_mo_rel$rate_rel <- (order_mo_rel$tweet_religion/order_mo_rel$alltweet)*100
plot(order_mo_rel$tweet_religion,type="o",main="Number of tweets related to vaccination \nwith religion content ")

plot(order_mo_rel$rate_rel,type="o",main="Percentage of tweets related to vaccination \nwith religion content ")

####safety/side effects#####
search_effect = "\\bautis\\b|\\bautisme\\b|\\bautism\\b|\\befek samping\\b|\\baman\\b|\\bbahaya\\b|\\bmati\\b|\\bkebal\\b|\\bdaya tahan\\b"
stem_result$contains_effect <-  as.numeric(!is.na(str_match(stem_result$stemming, search_effect)))
sum(stem_result$contains_effect==1) #37613 twit contain side effect issue

#autis, autisme, efek samping = 5228
#autis = 1900
#autisme = 1741
#efek samping = 1613 
#total 5254  


#aggregate tweet that contain side effect issue per month
aggr_mon_eff <- aggregate(contains_effect ~ monthyear,data=stem_result,sum)

#order by month
library(qdap)
split2 <- colsplit2df(data.frame(aggr_mon_eff), sep = "/")
split2 <- cbind(split2,aggr_mon_eff$monthyear)
colnames(split2) <- c("month","year","tweet_effect","month_year")
split2$month <- as.integer(split2$month)
split2$year <- as.integer(split2$year)
order_mo_eff <- arrange(split2,year,month)

order_mo_eff <- cbind(order_mo_eff,bydate$count) #to get percentage of vac tweet related to religion
colnames(order_mo_eff) <- c('month','year','tweet_effect','month_year','alltweet')

order_mo_eff$rate_eff <- (order_mo_eff$tweet_effect/order_mo_eff$alltweet)*100
plot(order_mo_eff$tweet_effect,type="o",main="Number of tweets related to vaccination \nwith side effect content ")

plot(order_mo_eff$rate_eff,type="o",main="Percentage of tweets related to vaccination \nwith side effect content ")

####vaccine material#####
search_material = "\\bmerkuri\\b|\\bmercury\\b|\\balumunium\\b|\\baluminium\\b|\\blogam\\b|\\blogam berat\\b|\\braksa\\b|\\bbahan baku\\b|\\bthimerosal\\b|\\btimerosal\\b|\\bzat kimia\\b|\\bbahan kimia\\b|\\btimbal\\b|\\bnanah\\b|\\blemak babi\\b|\\bembrio anjing\\b|\\bginjal kera\\b|\\bracun\\b|\\blambung babi\\b|\\bginjal anjing\\b"
stem_result$contains_material <-  as.numeric(!is.na(str_match(stem_result$stemming, search_material)))
sum(stem_result$contains_material==1) #6224 twit contain material issue

#aggregate tweet that contain material issue per month
aggr_mon_mat <- aggregate(contains_material ~ monthyear,data=stem_result,sum)

#order by month
library(qdap)
split3 <- colsplit2df(data.frame(aggr_mon_mat), sep = "/")
split3 <- cbind(split3,aggr_mon_mat$monthyear)
colnames(split3) <- c("month","year","tweet_material","month_year")
split3$month <- as.integer(split3$month)
split3$year <- as.integer(split3$year)
order_mo_mat <- arrange(split3,year,month)

order_mo_mat <- cbind(order_mo_mat,bydate$count) #to get percentage of vac tweet related to religion
colnames(order_mo_mat) <- c('month','year','tweet_material','month_year','alltweet')

order_mo_mat$rate_mat <- (order_mo_mat$tweet_material/order_mo_mat$alltweet)*100
plot(order_mo_mat$tweet_material,type="o",main="Number of tweets related to vaccination \nwith vaccine material content ")

plot(order_mo_mat$rate_mat,type="o",main="Percentage of tweets related to vaccination \nwith vaccine material content ")


####konspirasi#####

search_conspiracy = "\\bkonspirasi\\b|\\byahudi\\b|\\bconspiracy\\b|\\bkonspiracy\\b"
stem_result$contains_conspiracy <-  as.numeric(!is.na(str_match(stem_result$stemming, search_conspiracy)))
sum(stem_result$contains_conspiracy==1) #3780 twit contain conspiracy issue

#aggregate tweet that contain konspirasi issue per month
aggr_mon_cons <- aggregate(contains_conspiracy ~ monthyear,data=stem_result,sum)

#order by month
library(qdap)
split4 <- colsplit2df(data.frame(aggr_mon_cons), sep = "/")
split4 <- cbind(split4,aggr_mon_cons$monthyear)
colnames(split4) <- c("month","year","tweet_conspiracy","month_year")
split4$month <- as.integer(split4$month)
split4$year <- as.integer(split4$year)
order_mo_cons <- arrange(split4,year,month)

order_mo_cons <- cbind(order_mo_cons,bydate$count) #to get percentage of vac tweet related to religion
colnames(order_mo_cons) <- c('month','year','tweet_conspiracy','month_year','alltweet')

order_mo_cons$rate_cons <- (order_mo_cons$tweet_conspiracy/order_mo_cons$alltweet)*100
plot(order_mo_cons$tweet_conspiracy,type="o",main="Number of tweets related to vaccination \nwith conspiracy content ")

plot(order_mo_cons$rate_cons,type="o",main="Percentage of tweets related to vaccination \nwith conspiracy content ")




#save data stem_result into csv
write.csv(stem_result,"stem_result.csv")

#ideas
#1 predict tweet into 4 categories
#2 categories anti will be classified into anti because of what issues (religion, material, side effect, and conspiracy)
#3 categories pro will also be classified pro because of what issues

#there are only 24366 tweets with religion content, that's pretty disappointing number:(
#so lets check again if random tweet really contain religion content or not
#take 10000 samples from data frame stem_result

sample_forlabelling <- stem_result[sample(1:nrow(stem_result),10000,replace=FALSE),]
write.csv(sample_forlabelling,"sample_forlabelling.csv")


###############take samples for labelling############3
#9. take 10K samples from tweet, these data will be labelled into pro, anti, neutral, or irrelevant
#we will lable tweet using ORIGINAL tweet, first create a table
#consists only columns of original tweet

