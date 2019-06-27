# U1-As1-An Analytical Detective

setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-lesson/U1-As1-An Analytical Detective")
mvt = read.csv("mvtWeek1.csv")
View(mvt)

str(mvt)
# How many rows of data (observations) are in this dataset?
# How many variables are in this dataset?

# what is the maximum value of the variable "ID"?
max(mvt$ID)
# [1] 9181151

# What is the minimum value of the variable "Beat"?
min(mvt$Beat)
# [1] 111

# How many observations have value TRUE in the Arrest variable
summary(mvt$Arrest)
#    Mode   FALSE    TRUE 
# logical  176105   15536 

# How many observations have a LocationDescription value of ALLEY?
nrow(subset(mvt, mvt$LocationDescription == "ALLEY"))
# [1] 2308

# for working with dates in a original format
Sys.setlocale("LC_ALL", "C")

# In what format are the entries in the variable Date?
mvt$Date[1]
# [1] 12/31/12 23:15

# This converts the variable "Date" into a Date object in R.
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

# What is the month and year of the median date in our dataset? 
summary(DateConvert)

# extract the month and the day and add these variables to our data frame mvt.
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

# In which month did the fewest motor vehicle thefts occur?
sort(table(mvt$Month))
# February     April     March      June       May   January September  November  December 
#     13511     15280     15758     16002     16035     16047     16060     16063     16426 
#    August      July   October 
#     16572     16801     17086 

# On which weekday did the most motor vehicle thefts occur?
sort(table(mvt$Weekday))
#    Sunday   Tuesday  Saturday  Thursday    Monday Wednesday    Friday 
#     26316     26791     27118     27319     27397     27416     29284 

#  Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Month,mvt$Arrest)

#  let's make a histogram of the variable Date. 
hist(mvt$Date, breaks = 100)

# how arrests have changed over time. Create a boxplot of the "Date", sorted by the "Arrest" 
boxplot(mvt$Date ~ mvt$Arrest)
# Note that the time period is from 2001 to 2012, so the middle of the time period is the 
# beginning of 2007. If you look at the boxplot, the one for Arrest=TRUE is definitely skewed 
# towards the bottom of the plot, meaning that there were more crimes for which arrests were 
# made in the first half of the time period.

# For what proportion of motor vehicle thefts in 2001 was an arrest made?
table(mvt$Arrest, mvt$Year)
# 1212/(1212+13068) = 0.08487395

# find the top five locations where motor vehicle thefts occur
sort(table(mvt$LocationDescription))

# Create a subset of your data, which only happened in one of these five locations
Top5 = subset(mvt, LocationDescription=="STREET" | 
LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | 
LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | 
LocationDescription=="DRIVEWAY - RESIDENTIAL")

# or creating a subset in R by using %n%
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", 
"DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)

# One of the locations has a much higher arrest rate than the other locations. Which is it? 
table(Top5$LocationDescription , Top5$Arrest)

# On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$LocationDescription, Top5$Weekday)
# or
aaa = subset(Top5, LocationDescription == "GAS STATION" )
table(aaa$Weekday)
