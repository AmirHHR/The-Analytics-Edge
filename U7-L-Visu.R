# U7-L-Visualization 

# in ggplot, the mapping of data properties to visual properties is done by just adding layers 
# to the plot. This makes it much easier to create sophisticated plots and to add to existing 
# plots.

# All ggplot graphics consist of three elements. The first is data, in a data frame. The second 
# is an aesthetic mapping, which describes how variables in the data frame are mapped to graphical 
# attributes. This is where we'll define which variables are on the x- and y-axes, whether or 
# not points should be colored or shaped by certain attributes, etc. The third element is which 
# geometric objects we want to determine how the data values are rendered graphically. This is 
# where we indicate if the plot should have points, lines, bars, boxes, etc.

setwd("C:/Users/Administrator/Desktop/R Projects/The-Analytics-Edge/U7/lesson")
WHO = read.csv("WHO.csv")
str(WHO)
plot(WHO$GNI, WHO$FertilityRate)

# Let's redo this using ggplot 

# aesthetic mapping we have to decide what we want on the x-axis and what we want on the y-axis.
# we need to tell ggplot what geometric objects to put in the plot. We could use bars, lines, 
# points, or something else. You can build different types of graphs by using the same ggplot 
# object.


# Install and load the ggplot2 library:
install.packages("ggplot2")
library(ggplot2)

# Create the ggplot object with the data and the aesthetic mapping:
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))

# Add the geom_point geometry
scatterplot + geom_point()

# Make a line graph instead:
scatterplot + geom_line()

# Redo the plot with blue triangles instead of circles:
scatterplot + geom_point(color = "blue", size = 3, shape = 17) 

# Another option:
scatterplot + geom_point(color = "darkred", size = 3, shape = 8) 

# Add a title to the plot:
scatterplot + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")

# Save our plot:
fertilityGNIplot = scatterplot + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")

# let's create a file we want to save our plot to. We can do that with the pdf function. And 
# type the name you want your file to have.
pdf("MyPlot.pdf")

# let's just print our plot to that file
print(fertilityGNIplot)

# type dev.off() to close the file.
dev.off()

# If you want to see all of the available colors in R, type in your R console:

colors()

# shapes in R -- http://www.cookbook-r.com/Graphs/Shapes_and_line_types/

# how to color our points by region and how to add a linear regression line to our plot. 

# Color the points by region: 
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

# Color the points according to life expectancy:
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()

# we were coloring by a factor variable, Region and we had exactly seven different colors 
# corresponding to the seven different regions. Here, we're coloring by LifeExpectancy instead, 
# which is a numerical variable, so we get a gradient of colors.

# Is the fertility rate was a good predictor of the percentage of the under 15?
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()

# Let's try a log transformation:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# Simple linear regression model to predict under 15, using log fertility rate:
mod = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(mod)
# Multiple R-squared:  0.9391
# If we instead had just used the FertilityRate, the R-squared would have been 0.87.

# Visualization was a great way for us to realize that the log transformation would be better.

# Add regression line to our plot: a 95% confidence interval shaded around the line by default
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")

# 99% confidence interval
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

# No confidence interval in the plot
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE)

# Change the color of the regression line:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE, colour = "orange")

# for more complex data first Visualization, second table and third heat map is used.

# A heatmap would be useful for the visualize one variable relative to two variables.

# In a heat map, we can pick different color schemes based on the type of data to convey 
# different messages. In crime, a yellow to red color scheme might be appropriate because it 
# can highlight some of the more dangerous areas in red. Your eye is naturally drawn to the red 
# areas of the plot. In other applications, both high and low values are meaningful, so having a 
# more varied color scheme might be useful. And in other applications, you might only want to see 
# cells with high values, so you could use a gray scale to make the cells with low values white.

# create a basic line plot to visualize crime trends

# We'll add the argument stringsAsFactors = FALSE, since we have a text field, and we want to 
# make sure it's read in properly.

# Load our data:
mvt = read.csv("mvt.csv", stringsAsFactors=FALSE)
# We'll add the argument stringsAsFactors = FALSE, since we have a text field, and we want to 
# make sure it's read in properly.
str(mvt)
Sys.setlocale("LC_ALL", "C")

# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")

# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

# Create a simple line plot - need the total number of crimes on each day of the week.
table(mvt$Weekday)

# Save this table as a data frame so we can pass it to ggplot
WeekdayCounts = as.data.frame(table(mvt$Weekday))
str(WeekdayCounts) 


# Load the ggplot2 library:
library(ggplot2)

# Create our plot
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  
# This just groups all of our data into one line, since we want one line in our plot.

# What ggplot did was it put the days of the week in alphabetical order.But we actually want the 
# days of the week in chronological orderto make this plot a bit easier to read.
# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))

# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# make the line dashed and makes the line lighter in color
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2, alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# we'll add the hour of the day to our line plot, and then create an alternative visualization 
# using a heat map.

# Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour)
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
str(DayHourCounts)

# Convert a factor variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))

# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))
# we want the aesthetic to have the group equal to Var1, which is the day of the week.

# Change the colors for each day of the week
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=1)

# While we can get some information from this plot, it's still quite hard to interpret. Seven 
# lines is a lot. Let's instead visualize the same information with a heat map.

# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")

# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2) 
  
# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2, alpha=0.5) 


# Fix the order of the days:
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
  
# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())


# we'll plot crime on a map of Chicago.

# Install and load two new packages:
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)

# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11)

# Look at the map
ggmap(chicago)

# Plot the first 100 motor vehicle thefts:
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame 
# for each area because We're interested in whether or not an area has a high amount of crime,
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))
# This gives us the total crimes at every point on a grid.

str(LatLonCounts)

# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

# plot these points on our map, making the size and color of the points depend on the total 
# number of motor vehicle thefts.

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) + scale_colour_gradient(low="yellow", high="red")

# use the geom_tile geometry  to make something that looks more like a traditional heat map.
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

# alpha: This will define how to scale the colors on the heat map according to the crime counts.

# VIDEO 6 - Geographical Map on US
# Load our data:
murders = read.csv("murders.csv")
str(murders)

# Load the map of the US
statesMap = map_data("state")
str(statesMap)

# group: This is the variable defining how to draw the United States into groups by state.
# Plot the map:
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

# we need to make sure that the "state" names are the same in the "murders" data frame and in the 
# statesMap data frame. So let's create a new variable called region in our murders data frame 
# to match the state name variable in the statesMap data frame.

# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Now we can join the statesMap data frame with the murders data frame by using the merge 
# function, which matches rows of a data frame based on a shared identifier. We just defined the 
# variable region, which exists in both data frames. So we'll call our new data frame murderMap, 
# and we'll use the merge function, where the first argument is our first data frame, statesMap, 
# the second argument is our second data frame, murders, and the third argument is by="region". 
# This is the identifier to use to merge the rows.

# Join the statesMap data and the murders data into one dataframe:
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

View(murders)
View(statesMap)
View(murderMap)

# we'll add one more argument this time, which is fill=Murders so that the states will be 
# colored according to the Murders variable.
# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# it looks like California and Texas have the largest number of murders. But is that just 
# because they're the most populous states?
# geom_polygon to outline the states in black

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# There aren't really any red states. Why? It turns out that Washington, DC is an outlier with 
# a very high murder rate,but it's such a small region on the map that we can't even see it.So 
# let's redo our plot, removing any observations with murder rates above 10, 

# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

# Keep in mind that when interpreting and explaining the resulting plot, you should always note 
# what you did to create it and always consider outliers.
