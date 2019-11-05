library(dplyr)
library(lubridate)

x = rcCanada_followersbydate$day_num
y = rcCanada_followersbydate$followers
approx(x,y,xout=22)

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
filter(between(df$date, as.Date("2016-05-01"), as.Date("2016-08-02")))
df = df[ !(df$val %in% c(0:39)), ]
df = df[ !(df$val %in% c(135:176)), ]

merged = dplyr::left_join(allAccountsCombined, df, by = c("date" = "date"))
merged = dplyr::filter(merged,merged$username == "redcrosscanada")
merged2=subset(merged, select = c(date, username, replies, retweets, favorites, followers))
merged2$replies_ratio = (merged2$replies/merged2$followers)
merged2$RT_ratio = (merged2$retweets/merged2$followers)
merged2$favorites_ratio = (merged2$favorites/merged2$followers)
merged2$replies_ratio_log = log10(merged2$replies_ratio+1)
merged2$RT_ratio_log = log10(merged2$RT_ratio+1)
merged2$favorites_ratio_log = log10(merged2$favorites_ratio+1)








