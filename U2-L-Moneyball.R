# U2-L-Moneyball

# W -> RD -> RS,RA -> RS -> OBP,SLG
#                  -> RA -> OOBP, OSLG

# Read in data
setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U2/U2-moneyball")
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# we want to build a linear regression equation to predict wins using the difference between 
# runs scored and runs allowed. To make this a little easier, let's start by creating a new 
# variable. We'll call it moneyball$RD, for run difference, and set it equal to moneyball$RS, 
# runs scored, minus moneyball$RA, runs allowed.

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# before we build the linear regression equation, let's visually check to see if there's a 
# linear relationship between Run Difference and Wins.

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)
cor(moneyball$RD,moneyball$W)
# [1] 0.938515

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 80.881375   0.131157  616.67   <2e-16 ***
# RD           0.105766   0.001297   81.55   <2e-16 ***

# let's see if we can use this model to confirm the claim made in Moneyball that a team needs to 
# score at least 135 more runs than they allow to win at least 95 games.

# W = 80.881375 + 0.105766 * RD 

# We want Wins to be greater than or equal to 95 so that the A's make it to the playoffs. (based
# on the diagram of the playoffs teams)

# W >= 95 -> RD = (95 - 80.881375)/0.105766 = 133.4 ~ 135

# Let's see if we can use linear regression in R, to verify which baseball statistics are 
# important for predicting runs scored.
runsReg = lm(RS ~ OBP + SLG + BA , data = moneyball)
summary(RunsReg)

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -788.46      19.70 -40.029  < 2e-16 ***
# OBP          2917.42     110.47  26.410  < 2e-16 ***
# SLG          1637.93      45.99  35.612  < 2e-16 ***
# BA           -368.97     130.58  -2.826  0.00482 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 24.69 on 898 degrees of freedom
# Multiple R-squared:  0.9302,	Adjusted R-squared:   0.93 
# F-statistic:  3989 on 3 and 898 DF,  p-value: < 2.2e-16 

# if we look at our coefficients, we can see that the coefficient for batting average is 
# negative. This implies that a team with a lower batting average will score more runs, which is 
# a little counterintuitive. What's going on here is a case of multicollinearity.These three 
# hitting statistics are highly correlated, so it's hard to interpret the coefficients of our 
# model. Let's try removing batting average, the variable with the least significance, to see 
# what happens to our model.

runsReg = lm(RS ~ OBP + SLG , data = moneyball)
summary(RunsReg)

# We can see that our independent variables are still very significant, the coefficients are 
# both positive as we expect, and our R-squared is still about 0.93. So this model is simpler, 
# with only two independent variables, and has about the same R-squared. Overall a better model.

# Residuals:
#     Min      1Q  Median      3Q     Max 
# -70.838 -17.174  -1.108  16.770  90.036 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -804.63      18.92  -42.53   <2e-16 ***
# OBP          2737.77      90.68   30.19   <2e-16 ***
# SLG          1584.91      42.16   37.60   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 24.79 on 899 degrees of freedom
# Multiple R-squared:  0.9296,	Adjusted R-squared:  0.9294 
# F-statistic:  5934 on 2 and 899 DF,  p-value: < 2.2e-16

# If we look at the coefficients of our model, we can see that on-base percentage has a larger 
# coefficient than slugging percentage. Since these variables are on about the same scale, this 
# tells us that on-base percentage is probably worth more than slugging percentage.


# Using our regression models, we would like to predict before the season starts how many games 
# the 2002 Oakland A's will win. To do this, we first have to predict how many runs the team will 
# score and how many runs they will allow. These models use team statistics. However, when we are 
# predicting for the 2002 Oakland A's before the season has occurred, the team is probably 
# different than it was the year before. So we don't know the team statistics. But we can estimate 
# these statistics using past player performance. This approach assumes that past performance 
# correlates with future performance and that there will be few injuries during the season. Using 
# this approach, we can estimate the team statistics for 2002 by using the 2001 player statistics. 
# Let's start by making a prediction for runs scored.

# we can predict RS in 2002 based on OBP, SLG in 2001
# we can predict RA in 2002 based on OOBP, OSLG in 2001
# we can predict W in 2002 based on predicted RS and RA which computed for 2002