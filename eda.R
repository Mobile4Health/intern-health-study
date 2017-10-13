library(ggplot2)
getwd()
setwd('/Users/tnecamp/Box Sync/Intern-Health-Study-2014-15-cohort/data')
load("TN_merged_activity_sleep_mood_2015.Rdata")



## make some initial plots

## mood
plot1 = ggplot(data = mood_activity_sleep, aes(x = date, y = mood, group = USERID))
plot1 + geom_line(aes(color = USERID))
## too many users, plots are a mess
all_users = unique(mood_activity_sleep$USERID)
users_subset = sample(all_users,2)
data_subset = mood_activity_sleep[(mood_activity_sleep$USERID %in% users_subset),]
plot2 = ggplot(data = data_subset, aes(x = date, y = mood, group = USERID))
plot2 + geom_line(aes(color = USERID))

## make overall histogram
plot3 = ggplot(data = data_subset, aes(x = mood, group = USERID))
plot3 + geom_histogram()

## do users have separate mean moods
aggregate(mood~USERID, data = mood_activity_sleep, FUN = mean)
# yes, definitely! Suggest using a user specific intercept for sure!

# make overall histogram
plot3 = ggplot(data = mood_activity_sleep, aes(x = mood, group = USERID))
plot3 + geom_histogram()

## sleep
## the original data set had no missing values, the missingness came from the merge!
mood_activity_sleep$TotalMinutesAsleep
min(mood_activity_sleep$TotalMinutesAsleep, na.rm = TRUE)
sum(mood_activity_sleep$TotalMinutesAsleep < 20, na.rm = TRUE)
## missing values are coded as NA, but someone has a value of 0 sleep, kind of odd, 
## some people also have sleep values that are less than 20
plot1 = ggplot(data = mood_activity_sleep, aes(x = date, y = TotalMinutesAsleep/60, group = USERID))
plot1 + geom_line(aes(color = USERID))
## too many users, plots are a mess
all_users = unique(mood_activity_sleep$USERID)
users_subset = sample(all_users,2)
data_subset = mood_activity_sleep[(mood_activity_sleep$USERID %in% users_subset),]
plot2 = ggplot(data = data_subset, aes(x = date, y = TotalMinutesAsleep/60, group = USERID))
plot2 + geom_line(aes(color = USERID))

# make overall histogram
plot3 = ggplot(data = mood_activity_sleep, aes(x = TotalMinutesAsleep/60, group = USERID))
plot3 + geom_histogram()

## look at means
aggregate(TotalMinutesAsleep/60~USERID, data = mood_activity_sleep, FUN = mean)

## see if average sleep time is related to average mood
plot(aggregate(TotalMinutesAsleep/60~USERID, data = mood_activity_sleep, FUN = mean)[,2], aggregate(mood~USERID, data = mood_activity_sleep, FUN = mean)[,2])

## activity
mood_activity_sleep$TotalSteps
## I first need to understand missingness, does no total steps imply no total distance (and 0's for everything else)
## if so, should probably code as NA's not 0's!
## should I use steps or distance.
## the NA's that are already there came from the merge

## steps
mood_activity_sleep$TotalSteps
min(mood_activity_sleep$TotalSteps, na.rm = TRUE)
sum(mood_activity_sleep$TotalSteps < 20, na.rm = TRUE)
## missing values are coded as NA, but someone has a value of 0 sleep, kind of odd, 
## some people also have sleep values that are less than 20
plot1 = ggplot(data = mood_activity_sleep, aes(x = date, y = TotalSteps, group = USERID))
plot1 + geom_line(aes(color = USERID))
## too many users, plots are a mess
all_users = unique(mood_activity_sleep$USERID)
users_subset = sample(all_users,2)
data_subset = mood_activity_sleep[(mood_activity_sleep$USERID %in% users_subset),]
plot2 = ggplot(data = data_subset, aes(x = date, y = TotalSteps, group = USERID))
plot2 + geom_line(aes(color = USERID))

# make overall histogram
plot3 = ggplot(data = mood_activity_sleep, aes(x = TotalSteps, group = USERID))
plot3 + geom_histogram()
# heavy tail per usual

## look at means
aggregate(TotalSteps~USERID, data = mood_activity_sleep, FUN = mean)

## distance
mood_activity_sleep$TotalDistance
min(mood_activity_sleep$TotalDistance, na.rm = TRUE)
sum(mood_activity_sleep$TotalDistance < 20, na.rm = TRUE)
## missing values are coded as NA, but someone has a value of 0 sleep, kind of odd, 
## some people also have sleep values that are less than 20
plot1 = ggplot(data = mood_activity_sleep, aes(x = date, y = TotalDistance, group = USERID))
plot1 + geom_line(aes(color = USERID))
## too many users, plots are a mess
all_users = unique(mood_activity_sleep$USERID)
users_subset = sample(all_users,2)
data_subset = mood_activity_sleep[(mood_activity_sleep$USERID %in% users_subset),]
plot2 = ggplot(data = data_subset, aes(x = date, y = TotalDistance, group = USERID))
plot2 + geom_line(aes(color = USERID))

# make overall histogram
plot3 = ggplot(data = mood_activity_sleep, aes(x = TotalDistance, group = USERID))
plot3 + geom_histogram()
## distance is also heavy tailed, what is their correlation (nearly one?)

## look at means
mean(mood_activity_sleep)
aggregate(TotalDistance~USERID, data = mood_activity_sleep, FUN = mean)

## correlation between steps and distance
plot4 = ggplot(data = mood_activity_sleep, aes(x = TotalDistance, y = TotalSteps, group = USERID))
plot4 + geom_point(aes(colour = USERID))
all_users = unique(mood_activity_sleep$USERID)
users_subset = sample(all_users,1)
data_subset = mood_activity_sleep[(mood_activity_sleep$USERID %in% users_subset),]
plot5 = ggplot(data = data_subset, aes(x = TotalDistance, y = TotalSteps, group = USERID))
plot5 + geom_point(aes(colour = USERID))
## it seems to be basicially linear
## look at the correlation
mat = cbind(mood_activity_sleep$TotalDistance, mood_activity_sleep$TotalSteps)
mat = mat[complete.cases(mat),]
cor(mat[,1], mat[,2])

## check if 0's aligned
mat = cbind(mood_activity_sleep$TotalDistance, mood_activity_sleep$TotalSteps)
sum(which(is.na(mat[,1])) ==which(is.na(mat[,2])))
sum(is.na(mat[,1]))
## so na's match up, what about 0's?
mat = cbind(mood_activity_sleep$TotalDistance, mood_activity_sleep$TotalSteps)
mat = mat[complete.cases(mat),]
dim(mat)[1] - sum((mat[,1] == 0) == (mat[,2] == 0))
## so 6 entries don't align
## here is what they look like!
mood_activity_sleep_no_NA = mood_activity_sleep[complete.cases(cbind(mood_activity_sleep$TotalDistance, mood_activity_sleep$TotalSteps)),]
mood_activity_sleep_no_NA[!(mood_activity_sleep_no_NA$TotalDistance == 0) == (mood_activity_sleep_no_NA$TotalSteps == 0),]
## so nothing crazy happened if steps are 0, distance is super low, 
## and vice verse
## I would assume that they just didn't wear the device at all

## what about other activity measures
mood_activity_sleep_no_NA$activity_sum = rowSums(mood_activity_sleep_no_NA[,6:14])
mood_activity_sleep_no_NA$activity_total_sum = rowSums(mood_activity_sleep_no_NA[,4:5])

mat = cbind(mood_activity_sleep_no_NA$activity_total_sum, mood_activity_sleep_no_NA$activity_sum)
dim(mat)[1] - sum((mat[,1] == 0) == (mat[,2] == 0))
mat[!(mat[,1] == 0) == (mat[,2] == 0),]
## so there are 6 rows where there is something in the specific activity variables, but nothing in the tota
## there is one where the opposite is true
## so 7 entries don't align
## here is what they look like!
mood_activity_sleep_no_NA[!(mood_activity_sleep_no_NA$activity_total_sum == 0) == (mood_activity_sleep_no_NA$activity_sum == 0),]
## so nothing crazy happened if total distance/steps is 0, then light active min was 1 or 2 min
## and vice-versa
## I would assume that they just didn't wear the device at all

## overall I think we are safe to say that if total distance or total steps is 0, 
## then the activity is missing for the day! Now I will encode NA's as such


indices_with_0 = (mood_activity_sleep$TotalSteps == 0 | mood_activity_sleep$TotalDistance == 0)
indices_with_0[is.na(indices_with_0)] = FALSE
mood_activity_sleep[indices_with_0,]
# now set them all as missing
mood_activity_sleep[indices_with_0, 4:14] = NA

## look at correlations
plot4 = ggplot(data = mood_activity_sleep, aes(x = TotalDistance, y = mood, group = USERID))
plot4 + geom_point(aes(colour = USERID))

plot4 = ggplot(data = mood_activity_sleep, aes(x = TotalDistance, y = TotalMinutesAsleep/60, group = USERID))
plot4 + geom_point(aes(colour = USERID))

plot4 = ggplot(data = mood_activity_sleep, aes(x = TotalDistance, y = mood, group = USERID))
plot4 + geom_point(aes(colour = USERID))

##here
cor(mood_activity_sleep$TotalDistance, mood_activity_sleep$TotalMinutesAsleep, 


    