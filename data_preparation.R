#############get data and remove duplicat twit by Username and twit############################
#get data
setwd("d:/research/TWEET")

vac_twit1 <- read.csv("2015_1.csv")
vac_twit2 <- read.csv("2015_2.csv")
vac_twit3 <- read.csv("2014_1.csv")
vac_twit4 <- read.csv("2014_2.csv")
vac_twit5 <- read.csv("2013_1.csv")
vac_twit6 <- read.csv("2012_1.csv")

#######################check and delete duplicate plus check number of tweet per months ########################

#1. combine data, no record duplicate
alltwit <- rbind(vac_twit1,vac_twit2,vac_twit3,vac_twit4,vac_twit5,vac_twit6)
library(dplyr)
#remove all duplicate based on col Username, Date, and Tweet
#df1 <- alltwit %>% distinct(Username,Date,Tweet)

#remove duplicate based on col Username and Tweet
df <- alltwit %>% distinct(Username,Tweet)

#check if there's any duplicate
cek_df <- df[duplicated(df),] #result is no duplicate

##########delete links###############################

#2.delete link

#delete link in col Tweet
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
test <- removeURL(df$Tweet)
test <- data.frame(test)

#cbind data df and test
df <- cbind(df,test)
#rename test to Tweet_edit
names(df)[5]<-paste("Tweet_edit")


##########transform emoticons to words###############################
#3. emoticons to word
#create a data frame name "emoct" of tranformed emoticons in twit column only
#add (^|\\s) at the beginning -> emoticon must be at the beginning of sentence OR after space -> delete this there are emoticons have no space before next word, ex: "Weh aku tu habis imunisasi jadi sehat X-D"@xxx: @yyyy len?sehat?"
#add (\\s|$) at the end -> emoticon must be before space OR at the end of sentence

#happy
emo <- gsub("(:+-+\\)+|:+\\)+|:+D+|:+-+D+|:+o+\\)+|:+\\]+|:+3+|:+c+\\)+|:+>+|=+\\]+|8+\\)+|=+\\)+|:+}+|:+\\^+\\)+|:+\\)+\\)+|:+-+\\)+\\)+|\\^+\\^+|\\^+_+\\^+)(\\s|$)",
            " emo_happy ", df$Tweet_edit) #give space before and after emo_happy, because emoji in twit doesnt have space after the word before ex:"vaksin anak pertama:)"
emo <- as.data.frame(emo)



# use data frame emoct to transform more emoticons below
#laugh
emo <- gsub("(8+-+D+|8+D+|x+-+D+|x+D+|X+-+D+|X+D+|=+-+D+|=+D+|=+-+3+|=+3+)(\\s|$)", " emo_laugh ", emo$emo)
emo <- as.data.frame(emo)

#angry
emo <- gsub("(:+@+|:+-+@+|:+-+\\|+|>+:+\\(+)(\\s|$)", " emo_angry ", emo$emo)
emo <- as.data.frame(emo)

#cry
emo <- gsub("(:+'+\\(+|:+'+-+\\(+)(\\s|$)", " emo_cry ", emo$emo)
emo <- as.data.frame(emo)

#surprised
emo <- gsub("(:+o+|:+-+o+|:+O+|:+-+O+)(\\s|$)", " emo_surprised ", emo$emo)
emo <- as.data.frame(emo)

#kiss
emo <- gsub("(:+\\*+|:+\\^+\\*+|:+-+\\*+)(\\s|$)", " emo_kiss ", emo$emo)
emo <- as.data.frame(emo)

#wink
emo <- gsub("(;+-+\\)+|;+\\)+|\\*+-+\\)+|\\*+\\)+|;+-+\\]+|;+\\]+|;+D+|;+\\^+\\)+)(\\s|$)", " emo_wink ", emo$emo) 
emo <- as.data.frame(emo)

#confused
emo <- gsub("(:+S+|:+s+|:+-+S+|:+-+s+)(\\s|$)", " emo_confused ", emo$emo)
emo <- as.data.frame(emo)

#donttell
emo <- gsub("(:+-+#+|:+#+|:+-+X+|:+-+x+|:+X+|:+x+)(\\s|$)", " emo_donttell ", emo$emo)
emo <- as.data.frame(emo)

#thumbsup
emo <- gsub("\\(y\\)|\\(Y\\)(\\s|$)", " emo_thumbsup ", emo$emo)
emo <- as.data.frame(emo)

#sad
emo <- gsub("(:+\\{+|:+-+\\{+|;+\\{+|;+-+\\{+|>+:+\\[+|:+-+\\(+|:+\\(+|:+-+c+|:+c+|:+-+<+|:+<+|:+-+\\[+|:+\\[+|;+\\(+)(\\s|$)", " emo_sad ", emo$emo)
emo <- as.data.frame(emo)

#cbind df and emo
df <- cbind(df,emo)
# #delete Tweet_edit
# df <- df[c(-5)]
# #rename emo as Tweet_edit again!
# names(df)[5]<-paste("Tweet_edit")



#findings:
#1. emoticons are still there failed to convert bc there are no space b/w emoticons with the next puctuation or words
#2. emoticons yg stelahnya tanpa space and lgs punctuation will be transform as variable
#emoticons yg setelahnya tanpa space and lgs letter wont be transformed as variable

######################create emo1 column to solve emo#######################
#emoticon with no space after it but a punctuation, does count as emoticon and transform to word variable 

#happy

emo1 <- gsub("(:+-+\\)+|:+\\)+|:+d+|:+-+d+|:+D+|:+-+D+|:+o+\\)+|:+\\]+|:+3+|:+c+\\)+|:+>+|=+\\]+|8+\\)+|=+\\)+|:+}+|:+\\^+\\)+|:+\\)+\\)+|:+-+\\)+\\)+|\\^+\\^+|\\^+_+\\^+)([^a-zA-Z]|$)"," emo_happy ", df$emo)
emo1 <- as.data.frame(emo1)

#laugh
emo1 <- gsub("(8+-+D+|8+D+|x+-+D+|x+D+|X+-+D+|X+D+|=+-+D+|=+D+|=+-+3+|=+3+)([^a-zA-Z]|$)", " emo_laugh ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#angry
emo1 <- gsub("(:+@+|:+-+@+|:+-+\\|+|>+:+\\(+)([^a-zA-Z]|$)", " emo_angry ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#cry
emo1 <- gsub("(:+'+\\(+|:+'+-+\\(+)([^a-zA-Z]|$)", " emo_cry ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#surprised
emo1 <- gsub("(:+o+|:+-+o+|:+O+|:+-+O+)([^a-zA-Z]|$)", " emo_surprised ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#kiss
emo1 <- gsub("(:+\\*+|:+\\^+\\*+|:+-+\\*+)([^a-zA-Z]|$)", " emo_kiss ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#wink
emo1 <- gsub("(;+-+\\)+|;+\\)+|\\*+-+\\)+|\\*+\\)+|;+-+\\]+|;+\\]+|;+D+|;+\\^+\\)+)([^a-zA-Z]|$)", " emo_wink ", emo1$emo1) 
emo1 <- as.data.frame(emo1)

#confused
emo1 <- gsub("(:+S+|:+s+|:+-+S+|:+-+s+)([^a-zA-Z]|$)", " emo_confused ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#donttell
emo1 <- gsub("(:+-+#+|:+#+|:+-+X+|:+-+x+|:+X+|:+x+)([^a-zA-Z]|$)", " emo_donttell ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#thumbsup
emo1 <- gsub("\\(y\\)|\\(Y\\)([^a-zA-Z]|$)", " emo_thumbsup ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#sad
emo1 <- gsub("(:+\\{+|:+-+\\{+|;+\\{+|;+-+\\{+|>+:+\\[+|:+-+\\(+|:+\\(+|:+-+c+|:+c+|:+-+<+|:+<+|:+-+\\[+|:+\\[+|;+\\(+)([^a-zA-Z]|$)", " emo_sad ", emo1$emo1)
emo1 <- as.data.frame(emo1)

#cbind df and emo1
df <- cbind(df,emo1)

#findings:
#1. masih ditemukan emoticon yg belum ketransform krn setelahnya (tanpa space) ada huruf, misal RT, cth
#Dari usia 3bulan keatas udh bisa vaksin:)RT @xxx: mau nanya dong ? Kalau anakan kucing kira-kira boleh vaksin usia brp

##########################create emo2 column to solve emo1####################
#if (no space) after emoticons is RT, then count as emoticons, transfer to words


#happy
emo2 <- gsub("(:+-+\\)+|:+\\)+|:+d+|:+-+d+|:+D+|:+-+D+|:+o+\\)+|:+\\]+|:+3+|:+c+\\)+|:+>+|=+\\]+|8+\\)+|=+\\)+|:+}+|:+\\^+\\)+|:+\\)+\\)+|:+-+\\)+\\)+|\\^+\\^+|\\^+_+\\^+)(RT|Rt|rt|rT)"," emo_happy RT ", df$emo1)
emo2 <- as.data.frame(emo2)

#laugh
emo2 <- gsub("(8+-+D+|8+D+|x+-+D+|x+D+|X+-+D+|X+D+|=+-+D+|=+D+|=+-+3+|=+3+)(RT|Rt|rt|rT)", " emo_laugh RT", emo2$emo2)
emo2 <- as.data.frame(emo2)

#angry
emo2 <- gsub("(:+@+|:+-+@+|:+-+\\|+|>+:+\\(+)(RT|Rt|rt|rT)", " emo_angry RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)

#cry
emo2 <- gsub("(:+'+\\(+|:+'+-+\\(+)(RT|Rt|rt|rT)", " emo_cry RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)

#surprised
emo2 <- gsub("(:+o+|:+-+o+|:+O+|:+-+O+)(RT|Rt|rt|rT)", " emo_surprised RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)

#kiss
emo2 <- gsub("(:+\\*+|:+\\^+\\*+|:+-+\\*+)(RT|Rt|rt|rT)", " emo_kiss RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)

#wink
emo2 <- gsub("(;+-+\\)+|;+\\)+|\\*+-+\\)+|\\*+\\)+|;+-+\\]+|;+\\]+|;+D+|;+\\^+\\)+)(RT|Rt|rt|rT)", " emo_wink RT ", emo2$emo2) 
emo2 <- as.data.frame(emo2)

#confused
emo2 <- gsub("(:+S+|:+s+|:+-+S+|:+-+s+)(RT|Rt|rt|rT)", " emo_confused RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)

#donttell
emo2 <- gsub("(:+-+#+|:+#+|:+-+X+|:+-+x+|:+X+|:+x+)(RT|Rt|rt|rT)", " emo_donttell RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)

#thumbsup
emo2 <- gsub("\\(y\\)|\\(Y\\)(RT|Rt|rt|rT)", " emo_thumbsup RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)

#sad
emo2 <- gsub("(:+\\{+|:+-+\\{+|;+\\{+|;+-+\\{+|>+:+\\[+|:+-+\\(+|:+\\(+|:+-+c+|:+c+|:+-+<+|:+<+|:+-+\\[+|:+\\[+|;+\\(+)(RT|Rt|rt|rT)", " emo_sad RT ", emo2$emo2)
emo2 <- as.data.frame(emo2)


#cbind df and emo1
df <- cbind(df,emo2)


#findings:
#1. still found emoticons among alphabet without space, ex:
#@xxx iyaa nt klo dah lancar baru d ksih tw:)chenull 

#####################create emo3 column to solve emo2##############################################3
#convert any emoticons with no alphanumeric in it

#happy
emo3 <- gsub("(:+-+\\)+|:+\\)+|:+\\]+|=+\\)+|:+}+|:+\\^+\\)+|:+\\)+\\)+|:+-+\\)+\\)+|\\^+\\^+|\\^+_+\\^+)"," emo_happy ", df$emo2)
emo3 <- as.data.frame(emo3)

#angry
emo3 <- gsub("(:+@+|:+-+@+|:+-+\\|+", " emo_angry ", emo3$emo3)
emo3 <- as.data.frame(emo3)

#cry
emo3 <- gsub("(:+'+\\(+|:+'+-+\\(+)", " emo_cry ", emo3$emo3)
emo3 <- as.data.frame(emo3)


#kiss
emo3 <- gsub("(:+\\*+|:+\\^+\\*+|:+-+\\*+)", " emo_kiss ", emo3$emo3)
emo3 <- as.data.frame(emo3)

#wink
emo3 <- gsub("(;+-+\\)+|;+\\)+|\\*+-+\\)+|\\*+\\)+|;+-+\\]+|;+\\]+|;+\\^+\\)+)", " emo_wink ", emo3$emo3) 
emo3 <- as.data.frame(emo3)

#donttell
emo3 <- gsub("(:+-+#+|:+#+)", " emo_donttell ", emo3$emo3)
emo3 <- as.data.frame(emo3)

#sad
emo3 <- gsub("(:+\\{+|:+-+\\{+|;+\\{+|;+-+\\{+|:+-+\\(+|:+\\(+|:+-+\\[+|:+\\[+|;+\\(+)", " emo_sad ", emo3$emo3)
emo3 <- as.data.frame(emo3)


#cbind df and emo1
df <- cbind(df,emo3)


#findings:
#1 must find exactly the emoticon, unless will be misinterprting with other words, such as "infanrix-dtp"
#solution use \b in before and after emoticons
#2. how to apply \\b to ":-\\)"?? -> wrong way, add (^|\\s) at the beginning and (\\s|$) at the end
#3. some emoticons have no space between previous and next word
#4. X-D found in link, delete link before transform emoticon to words
#ex: Weh aku tu habis imunisasi jadi sehat X-D"@yyyy: @sekar_helena len?sehat?', solution -> manually space x-D @Angela_...
#5. excessively emoticons like ex: ":))))))))" replace with emo_happy -> 995 records
#":((((" replaced with emo_sad -> 946 records. solution -> add + after the emoticon, ex: (":+\\)+")


##########all twit to lower case for emo3###############################

#4. All twit in emo3 to lowercase
#fields Username and Tweet are kept as original
#make new table df2 consists of User, Username, Date, emo3
df1 <- df[c(1:4,9)] 

#transform df1$User to lowercase
df1 <- mutate_each(df1,funs(tolower),User)

#transform df1$Username to lowercase
df1 <- mutate_each(df1,funs(tolower),Username)

#transform df1$emo3 to lowercase
df1 <- mutate_each(df1,funs(tolower),emo3)

#3. aggregation of vaccination related keywords by months
#source: http://www.r-bloggers.com/dplyr-example-1/
#source: http://stackoverflow.com/questions/29154590/count-number-of-occurrences-of-words-from-a-list-in-text-entries-r
#source: http://stackoverflow.com/questions/31796744/r-plot-count-frequency-of-tweets-for-word-by-month


#################delete twit with account name cointain vaccine but twit contain no vaccine words########################################

#5 From findings, fix the df data

#1. twit account contains words: vaksin, vaksinasi, and imunisasi with irrelevant twit status will be deleted
#so we make sure we will only get twit that contains vaccine related words regardless the account name, we dont
#want vaccine related words only found in account
library(stringr)
search_account = "vaksin|vaksinasi|imunisasi"
df1$check_account <- as.numeric(!is.na(str_match(df1$Username,search_account)))
sum(df1$check_account==1) #9288 twits contain vaccine words in their account name
#now check if twit from re data contain vaccine related twit. Those twit that contain vaccine words
#will be kept, otherwise be deleted. delete_twit=1 is deleted, 0 will be kept
#twits to delete are df1$check_account * (0 or 1 if emo3 contain words in search_account)
df1$delete_twit <- df1$check_account * as.numeric(is.na(str_match(df1$emo3,search_account)))
sum(df1$delete_twit==1) #3353 contain vaccine words in the account (Username) but no vaccine words in twit will be deleted after delete link in twit
#delete 3353 twit
#yield 3447 twits contain vaccine words in the account but no vaccine words in the twit after links are deleted
#delete 3447
df1 <- df1[!(df1$delete_twit==1),]
#now that df has 807900 twits left with links
#now that df has 807806 twits left without links

#cek how many User contain words related to vaccine
df1$check_user <- as.numeric(!is.na(str_match(df1$User,search_account)))
sum(df1$check_user==1) #6153

#cek how many twit doesnt contain words related to vaccine but User does
df1$delete_twit_user <- df1$check_user * as.numeric(is.na(str_match(df1$emo3,search_account)))
sum(df1$delete_twit_user==1) #6 contain vaccine words in User but no vaccine words in twit will be deleted after delete link in twit

df1 <- df1[!(df1$delete_twit_user==1),] #807800 records now

##########cek jumlah twit per bulan###############################
#6. cek kelengkapan twit
#by month every year (my)
library(stringi)
library(stringr)
library(dplyr)
#month 
df1$my <- sapply(df1$Date,  function(x) paste(str_split(x,"/")[[1]][1],str_split(x,"/")[[1]][3], sep="/"))
#all twit regardless the number of vaksin, vakisnasi, imunisasi in one twit will be classified as 1 (twit with vaccination related words) and otherwise as 0


#aggregate by month
by_my <- group_by(df1,my)
(sum_my <- summarize(by_my,count=n()))

#order by month
library(qdap)
aggr <- colsplit2df(data.frame(sum_my), sep = "/")
colnames(aggr) <- c("month","year","count")
aggr$month <- as.integer(aggr$month)
aggr$year <- as.integer(aggr$year)
bydate <- arrange(aggr,year,month)
plot(bydate$count,type="o")


###########cek kelengkapan twit per bulan (by number of days)##############################


#aggregate by month
by_day <- group_by(df1,Date)
(sum_day <- summarize(by_day,count=n()))

#order by day
library(qdap)
aggr_day <- colsplit2df(data.frame(sum_day), sep = "/")
colnames(aggr_day) <- c("month","day","year","count")
aggr_day$month <- as.integer(aggr_day$month)
aggr_day$day <- as.integer(aggr_day$day)
aggr_day$year <- as.integer(aggr_day$year)
byday <- arrange(aggr_day,month,day,year)

#check if days are completed
cek_num_day <- group_by(byday,month,year)
sum_num_day <- summarize(cek_num_day,count=n())
write.csv(sum_num_day,file="sum_num_day.csv")

###############Final data is "df1" with 807800 twit before step 2-5, after step 2-5 twit is 807806#####################

###########continue to stemming.R####################

##############twit with issue############################
#7 twit with issue 
############religion##########################
library(stringr)
#tweet contains words below will be classified 1 otherwise 0
#before data is fixed based on findings
search_religion1 = "babi|halal|toyyib|haram|mui|agama|islam|enzim|muslim|ulama|fatwa|subhat|mubah"
df$contains_religion1 <-  as.numeric(!is.na(str_match(df$Tweet_edit, search_religion1)))
#findings:
#1. the twit does not contain words related to religion, but the link does. possibly will delete link from twit
#2. word irrelevant: babinsa -> dianggap babi, @miharame -> haram, vaksin ababil -> babi
#3. hastag contains words but the main twit does not, #beritaislami -> islam, #beritahalal : meski dilarang sejak 2010, vaksinasi fl
#4. misleading and misclassified, aku beli vaksin untuk penyakit ternak babi


############safety/side effects##########################
#tweet contains words below will be classified 1 otherwise 0
search_effect = "autis|autisme|efek samping|aman|bahaya|kematian|kebal|daya tahan"
df$contains_effect <-  as.numeric(!is.na(str_match(df$Tweet, search_effect)))
#findings:
#username named @vaksinasi
#irrelevant words, zaman -> aman, jaman -> aman

############vaccine material##########################
#tweet contains words below will be classified 1 otherwise 0
search_material = "merkuri|mercury|alumunium|aluminium|logam|logam berat|raksa|bahan baku|thimerosal|timerosal|zat kimia|bahan kimia|timbal|nanah|lemak babi|embrio anjing|ginjal kera|racun|lambung babi|ginjal anjing"
df$contains_material <-  as.numeric(!is.na(str_match(df$Tweet, search_material)))
#findings:
#idem dg safety

############konspirasi##########################
#tweet contains words below will be classified 1 otherwise 0
search_conspiracy = "konspirasi|sekongkol"
df$contains_conspiracy <-  as.numeric(!is.na(str_match(df$Tweet, search_conspiracy)))
#findings:
#akun @jkt_imunisasi contains kata2 sekongkol


#3. stemming, please check stemming.R use df2.csv
#search with exact words


#####################issue part 2#####################################
search_religion = "\\bbabi\\b|\\bhalal\\b|\\btoyyib\\b|\\bharam\\b|\\bmengharamkan\\b|\\bmui\\b|\\bagama\\b|\\bislam\\b|\\benzim\\b|\\bmuslim\\b|\\bulama\\b|\\bfatwa\\b|\\bsubhat\\b|\\bmubah\\b"
df$contains_religion <-  as.numeric(!is.na(str_match(df$Tweet_edit, search_religion)))
#findings:
#- haram only, mengharamkan, diharamkan, haramkan tidak masuk. solution is stemming


#aggregate(df$a, by=list(df$my),sum)
library(stringi)
library(stringr)
# filter="vaksin"
# alltwitok$count <- vapply(alltwitok$Tweet,function(x) sum(stri_detect_fixed(x,filter)),1L)
# search <- c("vaksin")
# stri_detect_fixed(df$Tweet,search)
# df$a <- as.numeric(stri_detect_fixed(df$Tweet,search))
# my.df <- mutate(df,Date = as.POSIXct(Date, format="%a %b %d %H:%M:%S %z %Y"))
# df2 <- data.frame("Tweets",strftime(my.df$Date, format = "%Y-%m"))
# 
# 
# rowsum(alltwitok$a,format(alltwitok$Date,'%m-%Y'),sum)
# 
# sum(stri_detect_fixed(alltwitok$Tweet,search))
# alltwitok$n <- rowsum(a,format(Date,))
#2. remove link in tweets

#n.bag-of-words source: https://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/
#http://www.slideshare.net/rdatamining/text-mining-with-r-an-analysis-of-twitter-data

#n. influencer
#who tweeting the most in our data, source http://blog.ouseful.info/2011/11/09/getting-started-with-twitter-analysis-in-r/


#1. Number of vaccination related tweets (keywords: vaccine, vaccination, and immunization) observed per day from 2012 to 2015 


