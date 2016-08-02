## ---- economics
library(dplyr)
data(economics, package = "ggplot2")
# data frames are essentially a list of vectors
glimpse(economics)
head(economics)

## ---- gapminder
library(gapminder)
glimpse(gapminder)
help(gapminder) # the data set gapminder documentation
?gapminder # equivalent to help(gapminder)

## ---- pedestrian
library(readr)
ped <- read_csv("http://dicook.github.io/Statistical_Thinking/data/Pedestrian_Counts.csv")
glimpse(ped)

## ---- basics1
set.seed(1000) # specify a seed to make sure the results reproducible
x <- rnorm(6) # randomly generate 6 numbers from the standard normal distribution
typeof(x) # type of x
x # print x on the screen
sum(x + 10) # add 10 to each element in x and sum them up
x[1] # extract the 1st element in x
x[1:4] # extract the first four elements in x
x[c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)]

## ---- basics2
# create a list with two named elements "a" and "b" and the old x being overwritten
x <- list(   
  a = 10,
  b = c(1, "2")
)
x$a # extract named element of "a" from the list 'x' 
x[["a"]] # equivalent to x$a
x["a"] # notice the difference from x[["a"]]?
class(x[["a"]]); class(x["a"]) # so what's the difference?
str(x) # show the structure of the object "x"

## ---- basics3
x <- c(50, 12, NA, 20) # NA means Not Available in R
mean(x) # calculate the average of x
mean(x, na.rm = TRUE) # na.rm is an argument in the mean() function

## ---- student
download.file("http://dicook.github.io/Statistical_Thinking/data/student_sub.rds",
              destfile = "../data/student_sub.rds") # the path can be changed
student2012.sub <- readRDS("../data/student_sub.rds")

## ---- solution
table(student2012.sub$CNT)
australia <- student2012.sub[student2012.sub$CNT=="AUS",]
shanghai <- student2012.sub[student2012.sub$CNT=="QCN",]
mean(australia$PV1READ)
sd(australia$PV1READ)
mean(shanghai$PV1READ)
sd(shanghai$PV1READ)
