library(dplyr)
library(lubridate)
library(aod)
library(ggplot2)
library(olsrr)

x = rcCanada_followersbydate$day_num
y = rcCanada_followersbydate$followers

df = data.frame()
for (val in 0:176){
  data_frame <- data.frame(val,approx(x,y,xout=val))
  df = rbind(df, data_frame)
}

df = dplyr::full_join(df,rcCanada_followersbydate,by= c("val" = "day_num"))
df$followers=round(df$y, digits=0)
df = subset(df, select = -c(x,y))
df$date =  as.Date("2016-03-22") + df$val
df$date = as.Date(df$date)
#filter(between(df$date, as.Date("2016-05-01"), as.Date("2016-08-02")))
df = df[ !(df$val %in% c(0:39)), ]
df = df[ !(df$val %in% c(135:176)), ]

rctweets_CANADA$date2 = as.Date(substr(rctweets_CANADA$date,1,10))

merged = dplyr::left_join(rctweets_CANADA, df, by = c("date2" = "date"))
merged = dplyr::filter(merged,merged$username == "redcrosscanada")
#filter out replies
mergedReplies = dplyr::filter(merged,merged$reply == 0)
merged2=subset(mergedReplies, select = c(date2, username, replies, retweets, favorites, followers,actionable,reply,retweet))
merged2$replies_ratio = (merged2$replies/merged2$followers)
merged2$RT_ratio = (merged2$retweets/merged2$followers)
merged2$favorites_ratio = (merged2$favorites/merged2$followers)
#merged2$replies_ratio_log = log10(merged2$replies_ratio+1)
#merged2$RT_ratio_log = log10(merged2$RT_ratio+1)
#merged2$favorites_ratio_log = log10(merged2$favorites_ratio+1)

#-------------------------#
#Regressions on org characteristics (followers)
modelRT = glm(RT_ratio ~ followers, data = merged2, family = quasibinomial)
plot(modelRT)
#plot(predict(modelRT),residuals(modelRT))
summary(modelRT)

modelReps = glm(replies_ratio ~ followers, data = merged2, family = quasibinomial)
plot(modelReps)
#plot(predict(modelReps),residuals(modelReps))
summary(modelReps)

modelFavs = glm(favorites_ratio ~ followers, data = merged2, family = quasibinomial)
plot(modelFavs)
#plot(predict(modelFavs),residuals(modelFavs))
summary(modelFavs)

#-------------------------#
#Regression on actionable comments
modelBinaryRT = glm(RT_ratio ~ actionable, data = merged2, family = quasibinomial)
#plot(modelBinaryRT)
summary(modelBinaryRT)

modelBinaryReps = glm(replies_ratio ~ actionable, data = merged2, family = quasibinomial)
summary(modelBinaryReps)

modelBinaryFavs = glm(favorites_ratio ~ actionable, data = merged2, family = quasibinomial)
summary(modelBinaryFavs)





