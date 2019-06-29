# U1-As2-Stock Dynamics

Sys.setlocale("LC_ALL","C")
# [1] "C"

setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As1-Stock Dynamics")
ProcterGamble <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As1-Stock Dynamics/ProcterGambleStock.csv")
CocaCola <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As1-Stock Dynamics/CocaColaStock.csv")
GE <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As1-Stock Dynamics/GEStock.csv")
IBM <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As1-Stock Dynamics/IBMStock.csv")
Boeing <- read.csv("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U1/U1-As1-Stock Dynamics/BoeingStock.csv")

# Before working with these data sets, we need to convert the dates into a format that R can 
# understand. Take a look at the structure of one of the datasets using the str function. Right 
# now, the date variable is stored as a factor. 

IBM$Date[1:10]
#  [1] 1/1/70  2/1/70  3/1/70  4/1/70  5/1/70  6/1/70  7/1/70  8/1/70  9/1/70  10/1/70

# We can convert this to a "Date" object in R 
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
# The first argument to the as.Date function is the variable we want to convert, and the second 
# argument is the format of the Date variable. We can just overwrite the original Date variable 
# values with the output of this function

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

#  How many observations are there in each data set?
str(IBM)
# 'data.frame':	480 obs. of  2 variables:
#  $ Date      : Date, format: "1970-01-01" "1970-02-01" ...
#  $ StockPrice: num  360 347 327 320 270 ...

# What is the earliest year in our datasets?
summary(IBM)
#       Date              StockPrice    
#  Min.   :1970-01-01   Min.   : 43.40  
#  1st Qu.:1979-12-24   1st Qu.: 88.34  
#  Median :1989-12-16   Median :112.11  
#  Mean   :1989-12-15   Mean   :144.38  
#  3rd Qu.:1999-12-08   3rd Qu.:165.41  
#  Max.   :2009-12-01   Max.   :438.90

# What is the standard deviation of the stock price of Procter & Gamble over this time period?
sd(ProcterGamble$StockPrice)
# [1] 18.19414

# Let's plot the stock prices for CocaCola during time 
plot(CocaCola$Date, CocaCola$StockPrice)

# Around what year did Coca-Cola has its highest stock price in this time period?
which.max(CocaCola$StockPrice)
# [1] 37
CocaCola$Date[37]
# [1] "1973-01-01"

# Around what year did Coca-Cola has its lowest stock price in this time period?
CocaCola$Date[which.min(CocaCola$StockPrice)]
# [1] "1980-03-01"

# see a line instead, since this is a continuous time period. 
plot(CocaCola$Date, CocaCola$StockPrice, type = 'l',col = "Red")

# Now, let's add the line for Procter & Gamble too.
lines(ProcterGamble$Date , ProcterGamble$StockPrice , col = "blue")

# lty=2 line dashed.
plot(CocaCola$Date, CocaCola$StockPrice, type = 'l',col = "Red")
lines(ProcterGamble$Date , ProcterGamble$StockPrice , lty = 2)

# draw a vertical line at March 1, 2000.a certain date.
# The argument lwd=2 makes the line a little thicker.
abline(v=as.Date(c("2000-03-01")), lwd=2)

# draw a horizental line at 80.
abline(h=80, lwd=2)

# Let's take a look at how the stock prices changed from 1995-2005 for all companies
# this intervention is useful if we want to compare the trends between all companies

# Which stock fell the most right after the technology bubble burst in March 2000?
# Which stock reaches the highest value in the time period 1995-2005?
# Comparing September 1997 to November 1997, which companies saw a decreasing trend in their 
# stock price? (Select all that apply.)
# In the last two years of this time period (2004 and 2005) which stock seems to be performing 
# the best, in terms of increasing stock price?
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type = 'l',col = "Blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], type = 'l',col = "Orange")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type = 'l',col = "Green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type = 'l',col = "Purple")
# This will plot the stock prices from 1995 through 2005, which are from 301 to 432. 

# If you prefer to change the type of the line instead of the color, here are some options 
# lty=2 dashed, lty=3 dotted, lty=4 alternate between dashes and dots, and lty=5 long-dashed

# To see all of the color options in R, type colors() in your R console.

# finding the row number of specific date in df
match(as.Date(c("2000-03-01")), IBM$Date)
# [1] 363

# In which months has IBM historically had a higher stock price than average?

sort(tapply(IBM$StockPrice, months(IBM$Date), mean))
# October  November      July September      June    August  December 
#  137.3466  138.0187  139.0670  139.0885  139.0907  140.1455  140.7593 
#   January       May     April     March  February 
#  150.2384  151.5022  152.1168  152.4327  152.6940 

sort(tapply(IBM$StockPrice, IBM$Month, mean)) > mean(IBM$StockPrice)
#   October  November      July September      June    August  December 
#     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE 
#   January       May     April     March  February 
#      TRUE      TRUE      TRUE      TRUE      TRUE

