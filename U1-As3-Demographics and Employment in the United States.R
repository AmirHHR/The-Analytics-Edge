# U1-As3-DemographicsEmpUniSta

setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As3-Demographics and Employment in the United States")
CPS <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As3-Demographics and Employment in the United States/CPSData.csv")
View(CPS)
str(CPS)

# what is the most common industry of employment?
sort(summary(CPS$Industry))

# Which state has the fewest interviewees?
sort(table(CPS$State))

# What proportion of interviewees are citizens of the United States?
table(CPS$Race,CPS$Hispanic)>250                
#                       0     1
#   American Indian  TRUE  TRUE
#   Asian            TRUE FALSE
#   Black            TRUE  TRUE
#   Multiracial      TRUE  TRUE
#   Pacific Islander TRUE FALSE
#   White            TRUE  TRUE

# Often when evaluating a new dataset, we try to identify if there is a pattern in the missing 
# values in the dataset. We will try to determine if there is a pattern in the missing values of 
# the Married variable. 
table(is.na(CPS$Married))
#  FALSE   TRUE 
# 105964  25338

# We can test the relationship between these four variable values and whether the Married 
# variable is missing with the following commands:

table(CPS$Region, is.na(CPS$Married))
#             FALSE  TRUE
#   Midwest   24609  6075
#   Northeast 21432  4507
#   South     33535  7967
#   West      26388  6789
table(CPS$Sex, is.na(CPS$Married))
#          FALSE  TRUE
#   Female 55264 12217
#   Male   50700 13121
table(CPS$Age, is.na(CPS$Married))
#     FALSE TRUE
#   0      0 1283
#   1      0 1559
#   2      0 1574
#   3      0 1693
#   4      0 1695
#   5      0 1795
#   6      0 1721
#   7      0 1681
#   8      0 1729
#   9      0 1748
#   10     0 1750
#   11     0 1721
#   12     0 1797
#   13     0 1802
#   14     0 1790
#   15  1795    0
#   16  1751    0
#   17  1764    0
#   18  1596    0
#   19  1517    0
#   20  1398    0
#   21  1525    0
#   22  1536    0
#   23  1638    0
# ...
table(CPS$Citizenship, is.na(CPS$Married))
#                        FALSE  TRUE
#   Citizen, Native      91956 24683
#   Citizen, Naturalized  6910   163
#   Non-Citizen           7098   492

# For each possible value of Region, Sex, and Citizenship, there are both interviewees with 
# missing and non-missing Married values. However, Married is missing for all interviewees Aged 
# 0-14 and is present for all interviewees aged 15 and older. This is because the CPS does not 
# ask about marriage status for interviewees 14 and younger.

# The breakdown of missing MetroAreaCode by State can be obtained with 
table(CPS$State, is.na(CPS$MetroAreaCode))

# Which region has the largest proportion of interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))
#             FALSE  TRUE
#   Midwest   20010 10674
#   Northeast 20330  5609
#   South     31631  9871
#   West      25093  8084

# The mean() function, which takes the average of the values passed to it, will treat TRUE as 1 
# and FALSE as 0, meaning it returns the proportion of values that are true. For instance, 
# mean(c(TRUE, FALSE, TRUE, TRUE)) returns 0.75.

# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
# Which state has the largest proportion of non-metropolitan interviewees, ignoring states 
# where all interviewees were non-metropolitan?
sort(tapply(is.na(CPSData$MetroAreaCode), CPSData$State, mean))

# When analyzing a variable stored by a numeric code, we will often want to convert it into the 
# values the codes represent. To do this, we will use a dictionary, which maps the the code to 
# the actual value of the variable.
MetroAreaMap <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As3-Demographics and Employment in the United States/MetroAreaCodes.csv")
View(MetroAreaMap)
CountryMap <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As3-Demographics and Employment in the United States/CountryCodes.csv")
View(CountryMap)

# To merge in the metropolitan areas, we want to connect the field MetroAreaCode from the CPS 
# data frame with the field Code in MetroAreaMap. The following command merges the two data 
# frames on these columns, overwriting the CPS data frame with the result
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
# The first two arguments determine the data frames to be merged (they are called "x" and "y", 
# respectively, in the subsequent parameters to the merge function). by.x="MetroAreaCode" means 
# we're matching on the MetroAreaCode variable from the "x" data frame (CPS), while by.y="Code" 
# means we're matching on the Code variable from the "y" data frame (MetroAreaMap). Finally, 
# all.x=TRUE means we want to keep all rows from the "x" data frame (CPS), even if some of the 
# rows' MetroAreaCode doesn't match any codes in MetroAreaMap

# What is the name of the variable that was added to the data frame
# MetroArea

# How many interviewees have a missing value for the new metropolitan area variable? Note that 
# all of these interviewees would have been removed from the merged data frame if we did not 
# include the all.x=TRUE parameter.
summary(CPS$MetroArea)
# 34238

# Which of the areas has the largest number of interviewees?
sort(table(CPS$MetroArea))
# New York-Northern New Jersey-Long Island, NY-NJ-PA 
#                                               5409 

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
# Laredo, TX 
# 0.966292135

# determine the number of metropolitan areas from which at least 20% of interviewees are Asian.
sum(tapply(CPS$Race == "Asian", CPS$MetroArea, mean) > 0.2)
# [1] NA
sum(tapply(CPS$Race == "Asian", CPS$MetroArea, mean) > 0.2 , na.rm = TRUE)
# [1] 4

# which metropolitan area has the smallest proportion of interviewees who have received no 
# high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

# merge in the country of birth information from the CountryMap data frame, replacing the CPS 
# data frame with the result.
CountryCodes <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As3-Demographics and Employment in the United States/CountryCodes.csv")
View(CountryCodes)
CPS = merge(CPS, CountryCodes, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

# What is the name of the variable added to the CPS data frame by this merge operation?
# Country
# How many interviewees have a missing value for the new country of birth variable?
# 176

# Among all interviewees born outside of North America, which country was the most common 
# place of birth?
sort(table(CPS$Country))
# Philippines 
# 839 

# What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" 
# metropolitan area have a country of birth that is not the United States?
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")       
#         FALSE  TRUE
#   FALSE 78757 12744
#   TRUE   3736  1668
#   1668/(1668+3736)=0.309

# Which metropolitan area has the largest number of interviewees with a country of birth in India?
sort(tapply( CPS$Country == "India",CPS$MetroArea, sum, na.rm = TRUE))
# New York-Northern New Jersey-Long Island, NY-NJ-PA 
#                                                 96 
max(sort(tapply( CPS$Country == "India",CPS$MetroArea, sum, na.rm = TRUE)))
# [1] 96
which.max(sort(tapply( CPS$Country == "India",CPS$MetroArea, sum, na.rm = TRUE)))
# New York-Northern New Jersey-Long Island, NY-NJ-PA 
which.max(tapply( CPS$Country == "India",CPS$MetroArea, sum, na.rm = TRUE))
# New York-Northern New Jersey-Long Island, NY-NJ-PA 
