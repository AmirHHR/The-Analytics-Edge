# U1 - R - Introduction

# Video 2 - Reading in the Dataset

# Get the current directory
getwd()
# setting the working directory
setwd("address")
# Read the csv file
USDA = read.csv("USDA.csv")
# Structure of the dataset
str(USDA)
# Statistical summary
summary(USDA)

# for working with dates in English format
Sys.setlocale("LC_ALL", "C")


# Video 3 - Basic Data Analysis

# Vector notation
USDA$Sodium
# Finding the index of the food with highest sodium levels
which.max(USDA$Sodium)
# Get names of variables in the dataset
names(USDA)
# Get the name of the food with highest sodium levels
USDA$Description[265]
# Create a subset of the foods with sodium content above 10,000mg
HighSodium = subset(USDA, Sodium>10000)

highSodium2c = highSodium[c("Description" , "Sodium")]
highSodium2c
#                                                  Description Sodium
# 265                                               SALT,TABLE  38758
# 922                        SOUP,BF BROTH OR BOUILLON,PDR,DRY  26000
# 923                                SOUP,BEEF BROTH,CUBED,DRY  24000
# 925                         SOUP,CHICK BROTH OR BOUILLON,DRY  23875
# 926                               SOUP,CHICK BROTH CUBES,DRY  24000
# 938                                         GRAVY,AU JUS,DRY  11588
# 1303                                            ADOBO FRESCO  17152
# 5321 LEAVENING AGENTS,BAKING PDR,DOUBLE-ACTING,NA AL SULFATE  10600
# 5324                            LEAVENING AGENTS,BAKING SODA  27360
# 5698                         DESSERTS,RENNIN,TABLETS,UNSWTND  26050

sortedHighSodium = highSodium2c[with(highSodium2c, order(-Sodium)),]
sortedHighSodium
#                                                  Description Sodium
# 265                                               SALT,TABLE  38758
# 5324                            LEAVENING AGENTS,BAKING SODA  27360
# 5698                         DESSERTS,RENNIN,TABLETS,UNSWTND  26050
# 922                        SOUP,BF BROTH OR BOUILLON,PDR,DRY  26000
# 923                                SOUP,BEEF BROTH,CUBED,DRY  24000
# 926                               SOUP,CHICK BROTH CUBES,DRY  24000
# 925                         SOUP,CHICK BROTH OR BOUILLON,DRY  23875
# 1303                                            ADOBO FRESCO  17152
# 938                                         GRAVY,AU JUS,DRY  11588
# 5321 LEAVENING AGENTS,BAKING PDR,DOUBLE-ACTING,NA AL SULFATE  10600


# Count the number of rows, or observations
nrow(HighSodium)
# Output names of the foods with high sodium content
HighSodium$Description
# Finding the index of CAVIAR in the dataset
match("CAVIAR", USDA$Description)
# Find amount of sodium in caviar
USDA$Sodium[4154]
# Doing it in one command!
USDA$Sodium[match("CAVIAR", USDA$Description)]
# Summary function over Sodium vector
summary(USDA$Sodium)

sd(usda$Sodium)
# NA
sd(usda$Sodium, na.rm = TRUE)
# 1045.417
  
# for comparing a number with a other numbers in a column we should compute the mean and sd of 
# that column and then compare that number with mean + sd of that column.
  
# Video 4 - Plots

# Visualization is a crucial step for initial data exploration 
# for discovering relationships like patterns and outliers.

# Scatter Plots
plot(USDA$Protein, USDA$TotalFat)
# Add xlabel, ylabel and title
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")
# Creating a histogram
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C")
# Add limits to x-axis
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100))
# Specify breaks of histogram - the interval of breaks: divide a little bit more than max of a 
# column by number of breaks 
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100), breaks=100)
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim = c(0,100), breaks=2000)
# Boxplots
boxplot(USDA$Sugar, ylab = "Sugar (g)", main = "Boxplot of Sugar")


# Video 5 - Adding a variable

# Creating a variable that takes value 1 if the food has higher sodium than average, 0 otherwise
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
str(HighSodium)
# Adding the variable to the dataset
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
# Similarly for HighProtein, HigCarbs, HighFat
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))


# Video 6 - Summary Tables

# How many foods have higher sodium level than average?
table(USDA$HighSodium)
# How many foods have both high sodium and high fat?
table(USDA$HighSodium, USDA$HighFat)
#        0    1
#   0 3529 1355
#   1 1378  712

# selects first 4 rows of a data set
head(USDA , 4)
# selects last 4 rows of a data set
tail(USDA,4)

# The rows belong to the first input, which is HighSodium, and the columns correspond to the 
# second input, which is HighFat.

# Average amount of iron sorted by high and low protein?
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)
# Maximum level of Vitamin C in hfoods with high and low carbs?
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
# is it true that foods that are high in carbs have generally high vitamin C content?
# Using summary function with tapply
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)
