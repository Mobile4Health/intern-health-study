## this code does some initial exploration of the sleep, mood, and activity
## data for 2015. It then compiles the 3 data sets into one data set.

getwd()
setwd('/Users/tnecamp/Box Sync/Intern-Health-Study-2014-15-cohort/data')

## check out mood data and missingness
# mood = read.csv("Mood_2015_all.csv")
# is(mood[,1])
# unique(mood)
# head(mood[,1:5])
# xx = mood[1:23,2:269]
# as.vector(xx[,1])
# for(i in 1:268){
# level1 = c(level1, levels(xx[,i]))}
# ## so all missing are coded as null

mood = read.csv("Mood_2015_all.csv", na.strings = "null")

sleep = read.csv("sleepDay_merged_2015cohort.csv")
activity = read.csv("dailyActivity_merged_2015cohort.csv")
unique(mood$USERID)
unique(sleep$Id)
unique(activity$Id)

unique(mood$USERID) == unique(sleep$Id)
unique(sleep$Id)==unique(activity$Id)
length(unique(mood$USERID))
 ## So the users in these 3 data sets are the exact same, there are 23 of them

#check on dates 
dates = names(mood)[2:269]
dates
date1= dates[1]

date1 = substring(date1, 2)

dates1 <- as.Date(dates, "X%m.%d.%Y")

for(i in 1:(length(dates1)-1)){
  if((dates1[i+1]-dates1[i]) != 1){
    print(i)}
}
## This shows that every date is equally spaced by one day!
yy = (as.vector(t(xx)))
ts1 = ts(yy)

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
## probably should go with the great one, as for the other duplicated
##sleep values, we can just remove them since they are fully duplicated

## now remove fully duplicated values
remove_list = !kk2
sleep_no_dup =  sleep[remove_list,]
# now get rid of the not fully duplicated one, keeping the larger sleep time
kk4 = duplicated(sleep_no_dup[,1:2])
sleep_no_dup = sleep_no_dup[!kk4,]

## This shows that all of the times in the sleep data set are midnight
abc = levels(sleep$SleepDay)
for(x in 1:261){
  new_vec[x] = substring(abc[x], nchar(abc[x])-10)
}
sum(duplicated(new_vec))

## so now sleep_no_dup is okay for merging EXCEPT need to change date and time to match

## now looking at activity
kk = duplicated(activity[,1:2])
activity[kk,]
sum(kk)
## there are duplicates, not sure why
## check to make sure the duplicates also have the same sleep values
kk2 = duplicated(activity)
activity[kk2,]
sum(kk2)
## so all but 2 of the duplicates have full duplicates I'm going to remove those
remove_list = !kk2
activity_no_dup =  activity[remove_list,]
## Now check out the 2 who weren't full duplicates
kk = duplicated(activity_no_dup[,1:2])
activity_no_dup[kk,]
ind_val = which(kk)
## check out first duplicate
activity_no_dup[ind_val[1],]
activity_no_dup[(ind_val[1]-1),]
## check out second duplicate
activity_no_dup[ind_val[2],]
activity_no_dup[(ind_val[2]-1),]
## the first duplicate is kind of odd, because everything is same (missing), but the total calories, seems like no real difference
## the second one has missing data for the frist entry, but non missing for the second
## based off of this, we will use the second values (the duplicated values)
## remove the first values
xx = kk[2:length(kk)]
xx = c(xx, FALSE)
activity_no_dup[xx,]
activity_no_dup = activity_no_dup[!xx,]

## so now activity has no duplications

## let's do the same thing for mood
## first need to reshape it
mood_long = reshape(mood, varying = colnames(mood)[2:(dim(mood)[2])], 
                v.names = "mood", 
                direction = "long", 
                timevar = "date", 
                idvar = "USERID",
                ids = "USERID",
                times = colnames(mood)[2:(dim(mood)[2])])

## so now mood_long is long version, should have no gaps between dates, no duplicates

## now the 3 data sets are good to go, I just have to combine them all into one, first match up date then use
## merge function! match on date and user, probably can use mood as the one containing all values, but
## need to check the date is in the right range

# sort mood long so it's sorted the same way as the other matrices
mood_long2 = mood_long[order(mood_long$USERID),]

# turn mood dates into date format 
dates = mood_long2$date
dates1 <- as.Date(dates, "X%m.%d.%Y")
mood_long2$date = dates1

# turn activity dates into date format
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

## so activity starts jan 1st, the other ones start in April
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
## so let's make things square, having data or missing values for all users from April 2nd onward
sum(mood_long2$date == "2015-04-08")
max(mood_long2$date)
# so mood goes from april 8th to dec 31

new_mat = mood_long2[0,]
users = unique(mood_long2$USERID)
dates_of_interest = seq(as.Date("2015/4/2"), as.Date("2015/4/7"), by = "day")
for(x in seq_along(dates_of_interest)){
  temp_mat = data.frame(users, dates_of_interest[x], NA)
  new_mat = rbind(new_mat, temp_mat)
}
names(new_mat) = names(mood_long2)
mood_long3 = rbind(mood_long2, new_mat)
## so mood_long3 has all entries we want, we can merge on it

## check to make sure your user, date set is all encompassing i.e. covers sleep and activity
xx1 = unique(mood_long3[,1:2])
#xx2 = unique(activity_no_dup[,1:2]) # I only caer about the ones after 4/1 
xx2 = unique(activity_no_dup[activity_no_dup$ActivityDate > "2015-04-01",c(1,2)])
xx3 = unique(sleep_no_dup[,1:2])

xx1[,2] = as.character(xx1[,2])
xx11 = paste(xx1[,1],xx1[,2])

xx2[,2] = as.character(xx2[,2])
xx21 = paste(xx2[,1],xx2[,2])

xx3[,2] = as.character(xx3[,2])
xx31 = paste(xx3[,1],xx3[,2])

## so mood_long2 has all the dates in activity after 4-1
all(xx21 %in% xx11)
which(!(xx21 %in% xx11))

## so mood_long2 has all the dates in sleep
all(xx31 %in% xx11)

## Time to merge them all
## merge mood and activity
dim(mood_long3)
dim(activity_no_dup)
mood_activity = merge(mood_long3, activity_no_dup, by.x = c("USERID", "date"), by.y = c("Id", "ActivityDate"), all.x = TRUE)


## IS SLEEP DATA FOR THE NIGHT BEFORE OR THE NIGHT AFTER??
dim(sleep_no_dup)
mood_activity_sleep = merge(mood_activity, sleep_no_dup, by.x = c("USERID", "date"), by.y = c("Id", "SleepDay"), all.x = TRUE)

dim(mood_activity_sleep)
## should be 6302   19

## need to do some other random checks, but this seems to work!

#?save
#ave(mood_activity_sleep, file = "TN_merged_activity_sleep_mood_2015.Rdata")
#write.csv(mood_activity_sleep, file = "TN_merged_activity_sleep_mood_2015.csv")

## Next steps: check on which day aligns with sleep
## clean up this code to run fluently and upload on git (should match saved data sets)
## check values in data frame with orginal data as a double check
