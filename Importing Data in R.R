####Importing Data in R

####Import CSV
cces_sample <- read.csv("Sample_data.csv")

####Write CSV
write.csv(cces_sample,"test.csv")

####type in your directory path in setwd() or use the Session-->Set Working Directory menu options

getwd()

setwd("D:/dropbox/Dropbox/data_viz_coursera/")

#### Don't need the whole file path now
cces_sample <- read.csv("cces_sample_coursera.csv")

class(cces_sample)

median(cces_sample$pew_religimp,na.rm=T)

table(cces_sample$race)

#** Data visualization
my_data <- data.frame("var1"=runif(50,0,100),"var2"=runif(50,0,100))
dim(my_data)
plot(my_data$var1, my_data$var2,
     main = "My first Scatter plot",
     xlab = "Variable 1",
     ylab = "Variable 2",
     ylim = c(0,120), 
     xlim = c(0,120)
     )

#-- Histogram and density plot

# Some sample data: these two vectors contain 200 data points each:

set.seed(1234)
rating  <- rnorm(200)
head(rating)

rating2 <- rnorm(200, mean=.8)
head(rating2)

# When plotting multiple groups of data, some graphing routines require a data frame with one column for the grouping variable and one for the measure variable.
## Make a column to indicate which group each value is in
cond <- factor( rep(c("A","B"), each=200) )

data <- data.frame(cond, rating = c(rating,rating2))
head(data)
View(data)
dim(data)

# Histogram
hist(rating)

# Use 8 bins (this is only approximate - it places boundaries on nice round numbers)
# Make it light blue #CCCCFF
# Instead of showing count, make area sum to 1, (freq=FALSE)
hist(rating, breaks=8, col="#CCCCFF", freq=FALSE)

# Put breaks at every 0.6
boundaries <- seq(-3, 3.6, by=.6)
boundaries

hist(rating, breaks=boundaries)


# Kernel density plot
plot(density(rating))

# Multiple groups with kernel density plots.
plot.multi.dens <- function(s)
{
  junk.x = NULL
  junk.y = NULL
  for(i in 1:length(s)) {
    junk.x = c(junk.x, density(s[[i]])$x)
    junk.y = c(junk.y, density(s[[i]])$y)
  }
  xr <- range(junk.x)
  yr <- range(junk.y)
  plot(density(s[[1]]), xlim = xr, ylim = yr, main = "")
  for(i in 1:length(s)) {
    lines(density(s[[i]]), xlim = xr, ylim = yr, col = i)
  }
}

# the input of the following function MUST be a numeric list
plot.multi.dens( list(rating, rating2))

# The sm package also includes a way of doing multiple density plots. The data must be in a data frame.
install.packages("sm")
library(sm)
sm.density.compare(data$rating, data$cond)
# Add a legend (the color numbers start from 2 and go up)
legend("topright", levels(data$cond), fill=2+(0:nlevels(data$cond)))


## ___ Box plot
# The examples here will use the ToothGrowth data set, which has two independent variables, and one dependent variable.
head(ToothGrowth)

# Simple box plots of len against supp and dose:
boxplot(len ~ supp, data=ToothGrowth)

# Even though `dose` is a numeric variable, `boxplot` will convert it to a factor
boxplot(len ~ dose, data=ToothGrowth)

# A boxplot of len against supp and dose together.
boxplot(len ~ interaction(dose,supp), data=ToothGrowth)

plot(len ~ interaction(dose,supp), data=ToothGrowth)

#___ Scatter plot
set.seed(955)
# Make some noisily increasing data
dat <- data.frame(xvar = 1:20 + rnorm(20,sd=3),
                  yvar = 1:20 + rnorm(20,sd=3),
                  zvar = 1:20 + rnorm(20,sd=3))

head(dat)

# ---- Basic scatterplots
# Plot the points using the vectors xvar and yvar
plot(dat$xvar, dat$yvar)

# Same as previous, but with formula interface
plot(yvar ~ xvar, dat)

# Add a regression line
fitline <- lm(dat$yvar ~ dat$xvar)
abline(fitline)

# -- Scatterplot matrices
# It is also possible to make a matrix of scatterplots if you would like to compare several variables.
# See this for a way to make a scatterplot matrix with r values.
# A scatterplot matrix
plot(dat[,1:3])

# Another way of making a scatterplot matrix, with regression lines
# and histogram/boxplot/density/qqplot/none along the diagonal
install.packages("car")
library(car)
scatterplotMatrix(dat[,1:3],
                  diagonal="histogram",
                  smooth=FALSE)


## Week 1 Peer Review Solution Key
#Problem 1
# Create a data frame that includes two columns, one named “Animals” and the other named “Foods”.
# The first column should be this vector (note the intentional repeated values):
data <- data.frame("Animals"= c("Dog", "Cat", "Fish","Fish","Lizard"),
                   "Foods" = c("Breads", "Orange", "Chocolate","carrots","Milk"))
head(data)

# Problem 2
# Using the data frame created in Problem 1, use the table() command to create a frequency table for the
# column called “Animals”.

table(data$Animals)

# Problem 3
# Use read.csv() to import the survey data included in this assignment. Using that data, make a histogram of
# the column called “pid7”
cces_sample <- read.csv("Sample_data.csv")
hist(cces_sample$pid7)

### Module 2: tidyverse
vignette("tidy-data") # or visit tidyverse.org
install.packages("tidyverse")
library(tidyverse)

data(mtcars) # example data
dim(mtcars)
head(mtcars)
str(mtcars)

#*** use tidyverse
 cces <- read_csv ("Sample_data.csv")

 # switch between tibble or data_frame
 
 cces_dataframe <- as.data.frame(cces)
 cces_tibble <- as_tibble(cces_dataframe)
 
# Drop rows with missing data in any cell
 cces <- drop_na(cces)
 
# use the filter function to select only women respondents
 table(cces$gender)
 women <- filter(cces,gender==2)
