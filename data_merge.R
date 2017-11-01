## this code does some initial exploration of the sleep, mood, and activity
## data for 2015 for the entire study. 
## It then compiles the 3 data sets into one data set.
## it then combines baseline information and PHQ9s
## this is how I created the final data set for the hackathon


##### Explore then merge sleep, mood, and activity ########
getwd()
setwd('/Users/tnecamp/Box Sync/Intern-Health-Study-2014-15-cohort/Tim/All_2015_daily_data/2015 daily data')
# check out mood data and missingness
mood = read.csv("Mood_2015_Final.csv")
is(mood[,1])
unique(mood)
head(mood[,1:5])
xx = mood[1:23,2:dim(mood)[2]]
as.vector(xx[,1])
level1 = NULL
for(i in 1:(dim(mood)[2]-1)){
  level1 = c(level1, levels(xx[,i]))}
unique(level1)
## so all missing are coded as "null" or "null "

mood = read.csv("Mood_2015_Final.csv", na.strings = c("null", "null "))

sleep = read.csv("sleepDay_merged.csv")
activity = read.csv("dailyActivity_merged.csv")
unique(mood$userid)
unique(sleep$Id)
unique(activity$Id)

unique(mood$userid) == unique(sleep$Id)
unique(sleep$Id)==unique(activity$Id)
length(unique(mood$userid))
## So the users in these 3 data sets are the exact same, there are 23 of them


## Right now, I'm thinking I should combine all data sets into one giant matrix like
## heartsteps. This might make life a lot easier.

## making it all into one data set

## first check each data set for duplicates
# duplicated tests for row level duplication

## Check the time
kk = duplicated(sleep[,1:2])
sleep[kk,]
sum(kk)
## there are duplicates, not sure why
## check to make sure the duplicates also have the same sleep values
kk2 = duplicated(sleep)
sleep[kk2,]
sum(kk2)
## So there is one entry that is duplicated in user and time but has different sleep

kk3 = (kk2 != kk)
ind_val = which(kk3)
sleep[ind_val,]
sleep[(ind_val-1),]
## So there is a duplicated value that doesn't have the same minutes asleep and mintues in bed
## it seems like there was a 3rd entry which added on more sleep. Not sure what to do
## looking at the data online with minute by minute step count,
## it seems like this 3rd entry is made up, not sure where it comes from


## now remove fully duplicated values
remove_list = !kk2
sleep_no_dup =  sleep[remove_list,]
# now get rid of the not fully duplicated one, keeping the smaller sleep time
kk4 = duplicated(sleep_no_dup[,1:2])
sleep_no_dup = sleep_no_dup[!kk4,]

## This shows that all of the times in the sleep data set are midnight
abc = levels(sleep$SleepDay)
new_vec = NULL
for(x in 1:length(abc)){
  new_vec[x] = substring(abc[x], nchar(abc[x])-10)
}
sum(duplicated(new_vec)) == length(new_vec) - 1

## so now sleep_no_dup is okay for merging EXCEPT need to change date and time to match

## now looking at activity
## I now have 4 extra columns I didn't have before, let's see what's in them
names(activity)[16:19]
sum(is.na(activity[,16:19]))
4*dim(activity)[1]
## shows that most, but not all are missing
activity[(rowSums(is.na(activity[,16:19])) < 4), c(1,2,3, 16:19)]
new_dat = activity[(rowSums(is.na(activity[,16:19])) < 4), ]
unique(new_dat$Id)
## only 2 users have such data, but why, is it self-reported?
## I actually have no idea wher this came from and don't know why I only
## see it for two users. How did they calculate CaloriesBMR or resting heart rate??
## to get floors, they needed to have a special fitbit, maybe that's what these
## two users have? not sure, it didn't seem to be the case online though

kk = duplicated(activity[,1:2])
activity[kk,]
sum(kk)
## there are no duplicates, can ignor code below: ##
# 
# ## check to make sure the duplicates also have the same activity values
# kk2 = duplicated(activity)
# activity[kk2,]
# sum(kk2)
# ## so all but 2 of the duplicates have full duplicates I'm going to remove those
# remove_list = !kk2
# activity_no_dup =  activity[remove_list,]
# ## Now check out the 2 who weren't full duplicates
# kk = duplicated(activity_no_dup[,1:2])
# activity_no_dup[kk,]
# ind_val = which(kk)
# ## check out first duplicate
# activity_no_dup[ind_val[1],]
# activity_no_dup[(ind_val[1]-1),]
# ## check out second duplicate
# activity_no_dup[ind_val[2],]
# activity_no_dup[(ind_val[2]-1),]
# ## the first duplicate is kind of odd, because everything is same (missing), but the total calories, seems like no real difference
# ## the second one has missing data for the frist entry, but non missing for the second
# ## based off of this, we will use the second values (the duplicated values)
# ## remove the first values
# xx = kk[2:length(kk)]
# xx = c(xx, FALSE)
# activity_no_dup[xx,]
# activity_no_dup = activity_no_dup[!xx,]
## so now activity has no duplications
##                                           ##

## let's do the same thing for mood

#check on dates 
dates = names(mood)[2:dim(mood)[2]]
dates
date1= dates[1]

date1 = substring(date1, 2)

dates1 <- as.Date(dates, "X%m.%d.%Y")

for(i in 1:(length(dates1)-1)){
  if((dates1[i+1]-dates1[i]) != 1){
    print(i)}
}
## This shows that every date is equally spaced by one day!
## Also, there aren't any duplicated dates!

## first need to reshape it
mood_long = reshape(mood, varying = colnames(mood)[2:(dim(mood)[2])], 
                    v.names = "mood", 
                    direction = "long", 
                    timevar = "date", 
                    idvar = "userid",
                    ids = "userid",
                    times = colnames(mood)[2:(dim(mood)[2])])

## so now mood_long is long version, should have no gaps between dates, no duplicates
sum(duplicated(mood_long[,1:2]))

## now the 3 data sets are good to go, I just have to combine them all into one, first match up date then use
## merge function! match on date and user, probably can use mood as the one containing all values, but
## need to check the date is in the right range

# sort mood long so it's sorted the same way as the other matrices
mood_long2 = mood_long[order(mood_long$userid),]

# turn mood dates into date format 
dates = mood_long2$date
dates1 <- as.Date(dates, "X%m.%d.%Y")
mood_long2$date = dates1

# turn activity dates into date format
activity_no_dup = activity
dates = activity_no_dup$ActivityDate
dates1 <- as.Date(dates, "%m/%d/%Y")
activity_no_dup$ActivityDate = dates1

# turn sleep dates in to date format
dates = sleep_no_dup$SleepDay
## I ignore time here because all times are midnight (see above)
dates1 <- as.Date(dates, "%m/%d/%Y")
sleep_no_dup$SleepDay = dates1

## so I think things are ready to merge!
## Let's see where the overlap is
min(sleep_no_dup$SleepDay)
min(activity_no_dup$ActivityDate)
min(mood_long2$date)

max(sleep_no_dup$SleepDay)
max(activity_no_dup$ActivityDate)
max(mood_long2$date)

sum(activity_no_dup$ActivityDate < "2015-04-01")
dim(activity_no_dup)
activity_no_dup[activity_no_dup$ActivityDate < "2015-04-01",c(1,2,3,4, 14,15)]
# so all data before April seems to be missing completely
# now try to find first overall date with either positive step counts or positive distance
pos_steps_entries = activity_no_dup$TotalSteps > 0
pos_dist_entries = activity_no_dup$TotalSteps > 0
pos_dist_or_step = pos_dist_entries + pos_steps_entries > 0
activity_no_dup_pos = activity_no_dup[pos_dist_or_step,]
min(activity_no_dup_pos$ActivityDate)
## So April 2 is the first recorded positive activity
activity_no_dup[activity_no_dup$ActivityDate < "2015-04-03",c(1,2,3,4, 14,15)]
## so the max is the same, the min is different, but we can start from 4/1/15 because of what is said above
## mood starts on 4/1/15, but our first activity date is 3/31/15, but the activity dates
## are all zero on 3/31/15, so we will just start our data from 4/1/15
## also, the first mood measurement is on 4/8/15, but we will still start at 4/1/15 anyway
## so mood_long2 has all entries we want, we can merge on it

## check to make sure your user, date set is all encompassing i.e. covers sleep and activity
xx1 = unique(mood_long2[,1:2])
#xx2 = unique(activity_no_dup[,1:2]) # I only caer about the ones after 4/1 
xx2 = unique(activity_no_dup[activity_no_dup$ActivityDate >= "2015-04-01",c(1,2)])
xx3 = unique(sleep_no_dup[,1:2])

xx1[,2] = as.character(xx1[,2])
xx11 = paste(xx1[,1],xx1[,2])

xx2[,2] = as.character(xx2[,2])
xx21 = paste(xx2[,1],xx2[,2])

xx3[,2] = as.character(xx3[,2])
xx31 = paste(xx3[,1],xx3[,2])

## so mood_long2 has all the dates in activity from 4-1-15 and after
all(xx21 %in% xx11)
which(!(xx21 %in% xx11))

## so mood_long2 has all the dates in sleep
all(xx31 %in% xx11)

## Time to merge them all
## merge mood and activity
dim(mood_long2)
dim(activity_no_dup)
mood_activity = merge(mood_long2, activity_no_dup, by.x = c("userid", "date"), by.y = c("Id", "ActivityDate"), all.x = TRUE)


## IS SLEEP DATA FOR THE NIGHT BEFORE OR THE NIGHT AFTER?? -- It's the night before, it counts all sleep that 
## ended on a given date as sleep for that date
dim(sleep_no_dup)
mood_activity_sleep = merge(mood_activity, sleep_no_dup, by.x = c("userid", "date"), by.y = c("Id", "SleepDay"), all.x = TRUE)

dim(mood_activity_sleep)

## did random checking and this data set seems good!
# save
#save(mood_activity_sleep, file = "TN_merged_activity_sleep_mood_2015_ALL.Rdata")
#write.csv(mood_activity_sleep, file = "TN_merged_activity_sleep_mood_2015_ALL.csv")


######### Now merge with the baseline data ###############

setwd('/Users/tnecamp/Box Sync/Intern-Health-Study-2014-15-cohort/Tim/')
# check out mood data and missingness
baseline141516 = read.csv("IHSdata_141516_Biomarkers_10132017.csv")
## This is good, NAs for blank columns, except when it's a string column (like dates of surveys)

head(mood_activity_sleep)

## Check the overlap of users
unique(mood_activity_sleep$userid) == unique((baseline141516$UserID)[baseline141516$Year == 2015])
## so users are the same, we can merge on users!

## this does what I want, repeats the baseline for every user entry in mood_activity_sleep
full_2015_data = merge(mood_activity_sleep, baseline141516, by.x = c("userid"), by.y = c("UserID"))

## save
#save(full_2015_data, file = "TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")
#write.csv(full_2015_data, file = "TN_merged_baseline_activity_sleep_mood_2015_ALL.csv")


########## Turn activity that is 0 into missing   ########################
## check if na's and 0's aligned
mat = cbind(full_2015_data$TotalDistance, full_2015_data$TotalSteps)
sum(which(is.na(mat[,1])) == which(is.na(mat[,2])))
sum(is.na(mat[,1]))
sum(is.na(mat[,2]))
## so na's match up, what about 0's?
mat = cbind(full_2015_data$TotalDistance, full_2015_data$TotalSteps)
mat = mat[complete.cases(mat),]
dim(mat)[1] - sum((mat[,1] == 0) == (mat[,2] == 0))
## so 10 entries don't align
## here is what they look like!
full_2015_data_no_NA = full_2015_data[complete.cases(cbind(full_2015_data$TotalDistance, full_2015_data$TotalSteps)),]
full_2015_data_no_NA[!(full_2015_data_no_NA$TotalDistance == 0) == (full_2015_data_no_NA$TotalSteps == 0),1:25]
## so nothing crazy happened if steps are 0, distance is super low, 
## and vice verse
## I would assume that they just didn't wear the device at all

## what about other activity measures
full_2015_data_no_NA$activity_sum = rowSums(full_2015_data_no_NA[,6:14])
full_2015_data_no_NA$activity_total_sum = rowSums(full_2015_data_no_NA[,4:5])

mat = cbind(full_2015_data_no_NA$activity_total_sum, full_2015_data_no_NA$activity_sum)
dim(mat)[1] - sum((mat[,1] == 0) == (mat[,2] == 0))
mat[!(mat[,1] == 0) == (mat[,2] == 0),]
## so there are 8 rows where there is something in the specific activity variables, but nothing in the tota
## there are 3 where the opposite is true
## so 11 entries don't align
## here is what they look like!
full_2015_data_no_NA[!(full_2015_data_no_NA$activity_total_sum == 0) == (full_2015_data_no_NA$activity_sum == 0),1:15]
## so nothing crazy happened if total distance/steps is 0, then light active min was 1 or 2 min
## and vice-versa
## I would assume that they just didn't wear the device at all

## overall I think we are safe to say that if total distance OR total steps is 0, 
## then the activity is missing for the day! Now I will encode NA's as such

indices_with_0 = (full_2015_data$TotalSteps == 0 | full_2015_data$TotalDistance == 0)
indices_with_0[is.na(indices_with_0)] = FALSE
full_2015_data[indices_with_0,1:20]
# now set them all as missing
full_2015_data[indices_with_0, 4:14] = NA

## save
#save(full_2015_data, file = "TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")
#write.csv(full_2015_data, file = "TN_merged_baseline_activity_sleep_mood_2015_ALL.csv")

############## Add in the two extra PHQ9's  ##################

setwd('/Users/tnecamp/Box Sync/Intern-Health-Study-2014-15-cohort/Tim/')
PHQ.BS1.2015 = read.csv("2015BioShort1.csv")
PHQ.BS2.2015 = read.csv("2015BioShort2.csv")
sort(unique(PHQ.BS1.2015$USERID)) %in% sort(unique(PHQ.BS2.2015$USERID)) ## They don't overlap :(

sort(unique(PHQ.BS2.2015$USERID)) %in% unique(full_2015_data$userid)
sort(unique(PHQ.BS1.2015$USERID)) %in% unique(full_2015_data$userid)
## so not identical, but they are subsets of the overall data, so that's good, we can merge on users

## check overlap of column names
names(PHQ.BS1.2015) %in% names(full_2015_data)
names(PHQ.BS2.2015) %in% names(full_2015_data)
## so no names overlap
## but names overlap here
names(PHQ.BS1.2015) %in% names(PHQ.BS2.2015)
## solution-rename
colnames(PHQ.BS1.2015) <- paste(colnames(PHQ.BS1.2015),"BS1",sep="_")
colnames(PHQ.BS2.2015) <- paste(colnames(PHQ.BS2.2015),"BS2",sep="_")

## fix dates variables
PHQ.BS1.2015$PHQdate_BS1
dates = PHQ.BS1.2015$PHQdate_BS1
dates1 <- as.Date(dates, "%m/%d/%Y")
PHQ.BS1.2015$PHQdate_BS1 = dates1

PHQ.BS1.2015$StartDate_BS1
dates = PHQ.BS1.2015$StartDate_BS1
dates1 <- as.Date(dates, "%m/%d/%Y")
PHQ.BS1.2015$StartDate_BS1 = dates1

PHQ.BS2.2015$PHQdate_BS2
dates = PHQ.BS2.2015$PHQdate_BS2
dates1 <- as.Date(dates, "%m/%d/%Y")
PHQ.BS2.2015$PHQdate_BS2 = dates1

## now I can merge!
## this does what I want, repeats the baseline for every user entry in mood_activity_sleep
PHQ.total.2015 = merge(PHQ.BS1.2015, PHQ.BS2.2015, by.x = c("USERID_BS1"), by.y = c("USERID_BS2"), all = TRUE)
full_2015_data = merge(full_2015_data, PHQ.total.2015, by.x = c("userid"), by.y = c("USERID_BS1"), all.x = TRUE)

########## last minute checks then save! ###################

## Check that data is sorted by dates
for(user_i in unique(full_2015_data$userid)){
  data_subset = full_2015_data[full_2015_data$userid ==user_i,]
  print(sum(sort(data_subset[,2]) != data_subset[,2]))
}
#it is

## add variable 
full_2015_data$days_intern = full_2015_data$date - full_2015_data$StartDate_BS1

## save
setwd('/Users/tnecamp/Box Sync/Intern-Health-Study-2014-15-cohort/Tim/All_2015_daily_data/2015 daily data/')
save(full_2015_data, file = "TN_merged_baseline_activity_sleep_mood_2015_ALL.Rdata")
write.csv(full_2015_data, file = "TN_merged_baseline_activity_sleep_mood_2015_ALL.csv")