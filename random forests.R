## all comments are for the previous user, not this one


## I realized I wasn't really aligning things correctly, with teh previous model
## mood is being regressed on sleep from 2 days again, and activity from 2 days ago
## should be current day's sleep, activity, mood. But then other things will get misaligned 
## since things aren't happening contemporaneously. Fixing that here. Need two different fits
## the fit I had previous could be used for sleep, but not for activity and mood
## it might be easier to just do lm stuff straight up, maybe not if picking best lag

setwd('/Users/tnecamp/Box Sync/Intern-Health-Study-2014-15-cohort/Tim/All_2015_daily_data/2015 daily data/')
load("TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")


library(vars)
library(randomForest)

cur_data = full_2015_data[full_2015_data$userid == 110152, ]
dim(cur_data)
xx = 0
for(i in 1:(nrow(cur_data)-1)){
  print(as.numeric(cur_data$date[i] - cur_data$date[i+1]))
  xx = xx + (as.numeric(cur_data$date[i] - cur_data$date[i+1]) == -1)
}
xx == (nrow(cur_data) - 1)
## so this being true indicates that they are all ordered and I have no date gaps. I think I can turn it into a time series now

x1 = ts(cur_data[cur_data$days_intern >= 0, 'mood'])
x2 = ts(cur_data[cur_data$days_intern >= 0, 'TotalMinutesAsleep'], start = 0)
x_new = ts.intersect(x1,x2)

cur_data$sqrtDist = sqrt(cur_data$TotalDistance)
x3 = ts(cur_data[cur_data$days_intern >= 0, 'sqrtDist'], start = 0)
x_new = ts.intersect(x_new, x3)
cur_ts = x_new
colnames(cur_ts) = c('mood', 'TotalMinutesAsleep', 'sqrtDist')

plot(cur_ts)
## looking at the missingness, it looks to be mostly in sleep, I could just impute the sleep with the overall average
## I could then just impute the missing mood in a couple ways (check literature). I could use the previous days mood, or I could use the overall mood average, or use full imputation
## If I'm eventually doing prediction, I will definitely want to make sure not to use future data when imputing

sum(is.na(cur_ts[,'mood']))/(nrow(cur_ts)) ## only 5% missing for mood
sum(is.na(cur_ts[,'sqrtDist']))/(nrow(cur_ts)) ## 13% of distance scores are missing, kind of a lot but whatever

## square root makes the most sense, ths matches with what I found before
#cur_ts2 = ts.union(cur_ts, sqrt(cur_ts[,'TotalDistance']))
#colnames(cur_ts2)[5] = 'sqrtDist'
# it's easier to transform it earlier in the data set

## advice: just look at the cur ts and its missingness, should hint at what to do.

## I like the idea of putting in the previous days mood, but is a little too complex and probably
## won't affect things too much, so I'm just going to impute averages for everything and see what happens
## I maybe should use a movie averge, because if I use the overall average I actually am using future data
## but for now, screw it

cur_ts[is.na(cur_ts[,'mood']), 'mood'] = mean(cur_ts[,'mood'], na.rm = TRUE)
cur_ts[is.na(cur_ts[,'sqrtDist']), 'sqrtDist'] = mean(cur_ts[,'sqrtDist'], na.rm = TRUE)
cur_ts[is.na(cur_ts[,'TotalMinutesAsleep']), 'TotalMinutesAsleep'] = mean(cur_ts[,'TotalMinutesAsleep'], na.rm = TRUE)

cur_fit = VAR(cur_ts[,c('mood', 'TotalMinutesAsleep', 'sqrtDist')], type = 'both', lag.max = 5) ## you don't need to have both days intern and trend, one or the other
summary(cur_fit)

## Give me a matrix which can be used for regression, with the correct number of lags
new_dat = cur_fit$varresult$mood$model
names(new_dat)

new_dat = cur_fit$varresult$mood$model
lm_fit = lm(y ~ ., data = new_dat)   ## for some reason here, when I put in the acutual formula used '-1 + .' then things get messed 
## up. I'm not sure why
summary(lm_fit)

new_dat1 = cur_fit$datamat
lm_fit1 = lm(mood ~ mood.l1 + TotalMinutesAsleep.l1 + sqrtDist.l1 + trend, data = new_dat1)
summary(lm_fit1)
## I literally get the exact same fit, so no big.

forest = randomForest(y ~ -1 + ., data = new_dat, importance = TRUE)
## doesn't help, but I also haven't messed with any paramters at all

#lm_fit = lm(y ~ -1 + ., data = new_dat) ## for some reason when I do this, my Rsquared because super messed up. I don't understand why?
lm_fit = lm(y ~ ., data = new_dat) ## for some reason when I do this, my Rsquared because super messed up. I don't understand why?


## Try with much larger data set
cur_fit_big = VAR(cur_ts[,c('mood', 'TotalMinutesAsleep', 'sqrtDist')], type = 'both', p = 15) ## you don't need to have both days intern and trend, one or the other
summary(cur_fit_big)

## Give me a matrix which can be used for regression, with the correct number of lags
new_dat_big = cur_fit_big$varresult$mood$model
names(new_dat_big)

lm_fit_big = lm(y ~ ., data = new_dat_big)
summary(lm_fit_big)

forest_big =randomForest(y ~ -1 + ., data = new_dat_big, importance = TRUE)
summary(forest_big)
importance(forest_big)[order(importance(forest_big)[,1], decreasing = TRUE),]

## I read some stuff online about importance, it seems like incmse is the best, it's the percent of mean squared error that increases 
## when you drop that variable. So, large %incmse means larger importance. I need to read up more into this

## for this person, the variable selection is nearly the same, except random forest doesn't like previous days mood as much.

## the random forest doesn't really seem to be outperforming my regression. The mean squared of residuals is the about the same, so non-linearities
## don't really help
## how do I control any tuning parameters. Random forest is trying not to overfit, but can I loosen this restirction?

## three tuning parameters mtry, increase this to allow more paramters to make decisions at two nodes
## ntree - I think this is just for computation, want enough to converge
## nodesize - the smoller this is, the longer the trees. It's the number of observations at teh bottom of each node

forest_big1 =randomForest(y ~ -1 + ., data = new_dat_big, mtry = 300, nodesize = 5, importance = TRUE)
forest_big1
forest_big

## I can't really get it to overfit, not sure why, maybe that means there just isn't enough signal in the data

## ultimately, I should probably read more about random forest before I make any major conclusions about it's effectiveness.

## it's basically a first attempt at understanding non-linearities, and I see, for this user, the benefit is none

## it's kind of nice that my R squared stays constant form little to big, this is the control for overfitting that randomforest has