# U1 - L - Linear Regression

# Basic Calculations
8*6
2^16

# Functions
sqrt(2)
abs(-65)

# get help ?function name
?sqrt

# assign a value to the variable = or <- 
SquareRoot2 = sqrt(2)
HoursYear <- 365*24

# list of all the variables
ls()


# Vectors

# create a vector
c(2,3,5,8,13)
Country = c("Brazil", "China", "India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)

# extracting the first element of a vector
Country[1]
LifeExpectancy[3]

# sequence of numbers from 0 to 100 with increament step of 2
Sequence = seq(0,100,2)
Sequence

# Data Frames

# building a dataFrame
CountryData = data.frame(Country, LifeExpectancy)

# link the new data into the data frame.
CountryData$Population = c(199000,1390000,1240000,7997,318000)

Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy, Population)

# combine data frames by stacking the rows
AllCountryData = rbind(CountryData, NewCountryData)



# VIDEO 4

# Loading csv files
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)

# Subsetting
# second argument is the criteria for which observations of WHO should belong
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)

# Writing csv files
write.csv(WHO_Europe, "WHO_Europe.csv")

# Removing variables
rm(WHO_Europe)


# VIDEO 5

# Basic data analysis 

mean(WHO$Under15)

# standard deviation 
sd(WHO$Under15)
summary(WHO$Under15)

# index of a min observation
which.min(WHO$Under15)
WHO$Country[86]

which.max(WHO$Under15)
WHO$Country[124]

# Scatterplot x-axis, y-axis
plot(WHO$GNI, WHO$FertilityRate)

# Subsetting

# two conditions
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)

# number of rows in "outliers"
nrow(Outliers)

# specific features of a dataFrame
Outliers[c("Country","GNI","FertilityRate")]


# VIDEO 6

# Histograms
# useful for understanding the distribution(the most frequent value) 
hist(WHO$CellularSubscribers)

# Boxplot
# useful for understanding the statistical range of a variable.
# LifeExpectancy sorted by Region
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countries by Region")

# Summary Tables

# counts the values
table(WHO$Region)

# splits the data by the second argument,
# then applies the third argument function to first argument.
tapply(WHO$Over60, WHO$Region, mean)

tapply(WHO$LiteracyRate, WHO$Region, min)
#    Africa              Americas Eastern Mediterranean 
#        NA                    NA                    NA 
#    Europe       South-East Asia       Western Pacific 
#        NA                    NA                    NA 
                   
# NA because we have some missing values in literacy rate.
# just remove the missing values when doing the computation. 

tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)
#    Africa              Americas Eastern Mediterranean 
#      31.1                  75.2                  63.9 
#    Europe       South-East Asia       Western Pacific 
#      95.2                  56.8                  60.6
