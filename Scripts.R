library(Rcpp)
load("tweet_index_sorted.Rdata")
tweet_index= y;
sourceCpp("sourceCPP.cpp")
strt = Sys.time()
#Brute Force
ful = getNearest(tweet_index[1:1000],tweet_index[1001:length(tweet_index)])
en = Sys.time()
en-strt

#Advancement
database_index = tweet_index[1001:length(tweet_index)]
len_database = unlist(lapply(database_index,function(row) {length(row)}))
db_list = order(len_database)
sorted_database_len = database_index[db_list]
approx = 0
strt = Sys.time()
li_0 = getNearest_Advancement(tweet_index[1:1000],sorted_database_len,0.50)
en = Sys.time()
en-strt
approx = 0.20
strt = Sys.time()
li_20 = getNearest_Advancement(tweet_index[1:1000],sorted_database_len,0.50)
en = Sys.time()
en-strt
approx = 0.50
strt = Sys.time()
li_50 = getNearest_Advancement(tweet_index[1:1000],sorted_database_len,0.50)
en = Sys.time()
en-strt
#li = db_list[li]+1001
#getDistance_1(tweet_index[[6]],tweet_index[[270099]])
#getNearest(tweet_index[6],tweet_index[1001:length(tweet_index)])
