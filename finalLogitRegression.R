#TODO:
#logit(rt/imp)=NB+accumulative+trend

library(tidyverse)
library(lubridate)
library(aod)
library(olsrr)

#Pre-processing (national account)
#-----------------------
#Import red cross data and turn into one data set May 3 - Aug 1
RCdataMay = read.csv("/Users/home/Dropbox/Social Media and Humanitarian Operations/Data from RC/tweet_activity_metrics_redcrosscanada_20160503_20160601_en.csv",header=TRUE)
RCdataJun = read.csv("/Users/home/Dropbox/Social Media and Humanitarian Operations/Data from RC/tweet_activity_metrics_redcrosscanada_20160601_20160701_en.csv",header=TRUE)
RCdataJul = read.csv("/Users/home/Dropbox/Social Media and Humanitarian Operations/Data from RC/tweet_activity_metrics_redcrosscanada_20160701_20160801_en.csv",header=TRUE)
df = dplyr::bind_rows(RCdataMay,RCdataJun)
df = dplyr::bind_rows(df,RCdataJul)
#isolate date
df$date = as.Date(df$time)
df$timeSplit = format(as.POSIXct(df$time) ,format = "%H:%M:%S")
#Group dates into weeks
df$week = as.numeric(df$date-(min(df$date))) %/%7 
#order in ascending order of date and time
df = df[order(df$date,df$time),]
#Calculate cumulative variables based on date and week 
df = df %>% group_by(week) %>% mutate(week_RTs_cumsum = cumsum(retweets))
df = df %>% group_by(week) %>% mutate(week_likes_cumsum = cumsum(likes))
df = df %>% group_by(week) %>% mutate(week_replies_cumsum = cumsum(replies))
df = df %>% group_by(date) %>% mutate(daily_RTs_cumsum = cumsum(retweets))
df = df %>% group_by(date) %>% mutate(daily_likes_cumsum = cumsum(likes))
df = df %>% group_by(date) %>% mutate(daily_replies_cumsum = cumsum(replies))
df$engageRatio = df$retweets/df$impressions

#Import file with Naive Bayes scores on tweets
NBtweets_allaccounts = read.csv("/Users/home/Dropbox/Social Media and Humanitarian Operations/_Students Deliverables/processedDONE2.csv",header=TRUE)
NBtweets = dplyr::filter(NBtweets_allaccounts, NBtweets_allaccounts$username == "redcrosscanada")
#Merge Naive Bayes file with RCdata
#lose 5/1051 tweets here -> I was fine with this 
df2 = dplyr::inner_join(df,NBtweets,by = c("Tweet.permalink"="permalink"))

#Import file with Google Trends data
GTrends = read.csv("/Users/home/Dropbox/Social Media and Humanitarian Operations/_Students Deliverables/googletrends_rc/canadianredcross_canadaredcross.csv",header=TRUE)
GTrends$Canada.Red.Cross...Canada. = NULL #remove extra column
GTrends$trendValue =  GTrends$Canadian.red.cross...Canada.
GTrends$Canadian.red.cross...Canada.= NULL
GTrends$Day = as.Date(GTrends$Day)
df3 = dplyr::inner_join(df2,GTrends,by = c("date.x"="Day"))

#create models
model_RT_weekly = glm(engageRatio ~ prob_score+week_RTs_cumsum+trendValue, data = df3, family = quasibinomial)
model_RT_daily = glm(engageRatio ~ prob_score+daily_RTs_cumsum+trendValue, data = df3, family = quasibinomial) 
model_likes_weekly = glm(engageRatio ~ prob_score+week_likes_cumsum+trendValue, data = df3, family = quasibinomial)
model_likes_daily = glm(engageRatio ~ prob_score+daily_likes_cumsum+trendValue, data = df3, family = quasibinomial)
model_replies_weekly = glm(engageRatio ~ prob_score+week_replies_cumsum+trendValue, data = df3, family = quasibinomial)
model_replies_daily = glm(engageRatio ~ prob_score+daily_replies_cumsum+trendValue, data = df3, family = quasibinomial)

#models w/o GT
noGT_model_RT_weekly = glm(engageRatio ~ prob_score+week_RTs_cumsum, data = df3, family = quasibinomial)
noGT_model_RT_daily = glm(engageRatio ~ prob_score+daily_RTs_cumsum, data = df3, family = quasibinomial) 
noGT_model_likes_weekly = glm(engageRatio ~ prob_score+week_likes_cumsum, data = df3, family = quasibinomial)
noGT_model_likes_daily = glm(engageRatio ~ prob_score+daily_likes_cumsum, data = df3, family = quasibinomial)
noGT_model_replies_weekly = glm(engageRatio ~ prob_score+week_replies_cumsum, data = df3, family = quasibinomial)
noGT_model_replies_daily = glm(engageRatio ~ prob_score+daily_replies_cumsum, data = df3, family = quasibinomial)


#-----------------------
#Provincial accounts (just AB - no data for SK)
#-----------------------
#interpolate followers for AB from wayback
ABfollowers = read.csv("/Users/home/Dropbox/Social Media and Humanitarian Operations/_Students Deliverables/rcABfollowers.csv",header = TRUE)
ABfollowers$date = as.Date(ABfollowers$date)
#fill in missing dates 
ABfollowers = ABfollowers %>%  complete(date = seq.Date(min(date), max(date), by="day"))
ABfollowers$day_num = as.numeric(ABfollowers$date-(min(ABfollowers$date)))
#interpolate data
df_ = data.frame()
for (i in 0:max(ABfollowers$day_num)){
  bro = data.frame(i,approx(ABfollowers$day_num,ABfollowers$followers,xout=i))
  df_ = rbind(df_, bro)
}
df_$y=round(df_$y, digits=0) #round # of followers
ABfollowers = dplyr::inner_join(ABfollowers,df_,by = c ("day_num"="i"))
ABfollowers = subset(ABfollowers, select = -c(followers,x)) #remove extra columns

#Get only Alberta Tweets with NB scores
ABtweets = dplyr::filter(NBtweets_allaccounts, NBtweets_allaccounts$username == "RedCrossAB")
ABtweets <- ABtweets %>%
  mutate(date = as.character(as.Date(date, "%m/%d/%Y"),"%Y-%m-%d"))
ABtweets$date=as.Date(ABtweets$date)
#Group dates into weeks
ABtweets$week = as.numeric(ABtweets$date-(min(ABtweets$date))) %/%7 
ABtweets = ABtweets[order(ABtweets$date,ABtweets$time),]
ABtweets = ABtweets %>% group_by(week) %>% mutate(week_RTs_cumsum = cumsum(retweets))
ABtweets = ABtweets %>% group_by(week) %>% mutate(week_likes_cumsum = cumsum(favorites))
ABtweets = ABtweets %>% group_by(week) %>% mutate(week_replies_cumsum = cumsum(replies))
ABtweets = ABtweets %>% group_by(date) %>% mutate(daily_RTs_cumsum = cumsum(retweets))
ABtweets = ABtweets %>% group_by(date) %>% mutate(daily_likes_cumsum = cumsum(favorites))
ABtweets = ABtweets %>% group_by(date) %>% mutate(daily_replies_cumsum = cumsum(replies))

#Import Google trends data for AB and join with tweets
GTrendsAB = read.csv("/Users/home/Dropbox/Social Media and Humanitarian Operations/_Students Deliverables/googletrends_rc/albertaredcross.csv",header=TRUE)
GTrendsAB$Day = as.Date(GTrendsAB$Day)
GTrendsAB$trendValue = GTrendsAB$Alberta.Red.Cross...Canada.
GTrendsAB$Alberta.Red.Cross...Canada.= NULL
ABtweets = dplyr::inner_join(ABtweets,GTrendsAB, by = c("date"="Day"))
#join with interpolated follower numbers
ABtweets = dplyr::inner_join(ABtweets,ABfollowers, by = "date")

#create dependent variable
ABtweets$engageRatio = ABtweets$retweets/ABtweets$y

#models for AB tweets
ABmodel_RT_weekly = glm(engageRatio ~ prob_score+week_RTs_cumsum+trendValue, data = ABtweets, family = quasibinomial)
ABmodel_RT_daily = glm(engageRatio ~ prob_score+daily_RTs_cumsum+trendValue, data = ABtweets, family = quasibinomial) 
ABmodel_likes_weekly = glm(engageRatio ~ prob_score+week_likes_cumsum+trendValue, data = ABtweets, family = quasibinomial)
ABmodel_likes_daily = glm(engageRatio ~ prob_score+daily_likes_cumsum+trendValue, data = ABtweets, family = quasibinomial)
ABmodel_replies_weekly = glm(engageRatio ~ prob_score+week_replies_cumsum+trendValue, data = ABtweets, family = quasibinomial)
ABmodel_replies_daily = glm(engageRatio ~ prob_score+daily_replies_cumsum+trendValue, data = ABtweets, family = quasibinomial)

#models w/o GT
noGT_ABmodel_RT_weekly = glm(engageRatio ~ prob_score+week_RTs_cumsum, data = ABtweets, family = quasibinomial)
noGT_ABmodel_RT_daily = glm(engageRatio ~ prob_score+daily_RTs_cumsum, data = ABtweets, family = quasibinomial) 
noGT_ABmodel_likes_weekly = glm(engageRatio ~ prob_score+week_likes_cumsum, data = ABtweets, family = quasibinomial)
noGT_ABmodel_likes_daily = glm(engageRatio ~ prob_score+daily_likes_cumsum, data = ABtweets, family = quasibinomial)
noGT_ABmodel_replies_weekly = glm(engageRatio ~ prob_score+week_replies_cumsum, data = ABtweets, family = quasibinomial)
noGT_ABmodel_replies_daily = glm(engageRatio ~ prob_score+daily_replies_cumsum, data = ABtweets, family = quasibinomial)




