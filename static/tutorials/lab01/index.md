---
title: "ETC2420/5242 <br> Introduction to R, RStudio & RMarkdown"
date: Lab 01
author: "http://st.netlify.com/tutorials/lab01"
---



# Have fun with R
## US economic time series from ggplot2 

* `date`: month of data collection
* `pce`: personal consumption expenditures
* `pop`: total population
* `psavert`: personal saving rate
* `uempmed`: median duration of unemployment
* `unemploy`: number of unemployed
* Read its [documentation](http://docs.ggplot2.org/current/economics.html) for
  more details


```r
library(dplyr)
data(economics, package = "ggplot2")
# data frames are essentially a list of vectors
glimpse(economics)
```

```
## Observations: 574
## Variables: 6
## $ date     <date> 1967-07-01, 1967-08-01, 1967-09-01, 1967-10-01, 1967...
## $ pce      <dbl> 507.4, 510.5, 516.3, 512.9, 518.1, 525.8, 531.5, 534....
## $ pop      <int> 198712, 198911, 199113, 199311, 199498, 199657, 19980...
## $ psavert  <dbl> 12.5, 12.5, 11.7, 12.5, 12.5, 12.1, 11.7, 12.2, 11.6,...
## $ uempmed  <dbl> 4.5, 4.7, 4.6, 4.9, 4.7, 4.8, 5.1, 4.5, 4.1, 4.6, 4.4...
## $ unemploy <int> 2944, 2945, 2958, 3143, 3066, 3018, 2878, 3001, 2877,...
```

```r
head(economics)
```

```
## # A tibble: 6 x 6
##         date   pce    pop psavert uempmed unemploy
##       <date> <dbl>  <int>   <dbl>   <dbl>    <int>
## 1 1967-07-01 507.4 198712    12.5     4.5     2944
## 2 1967-08-01 510.5 198911    12.5     4.7     2945
## 3 1967-09-01 516.3 199113    11.7     4.6     2958
## 4 1967-10-01 512.9 199311    12.5     4.9     3143
## 5 1967-11-01 518.1 199498    12.5     4.7     3066
## 6 1967-12-01 525.8 199657    12.1     4.8     3018
```
Can you think of two questions you could answer using these variables?

# Have fun with R
## Gapminder data


```r
library(gapminder)
glimpse(gapminder)
```

```
## Observations: 1,704
## Variables: 6
## $ country   <fctr> Afghanistan, Afghanistan, Afghanistan, Afghanistan,...
## $ continent <fctr> Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asia, Asi...
## $ year      <int> 1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992...
## $ lifeExp   <dbl> 28.801, 30.332, 31.997, 34.020, 36.088, 38.438, 39.8...
## $ pop       <int> 8425333, 9240934, 10267083, 11537966, 13079460, 1488...
## $ gdpPercap <dbl> 779.4453, 820.8530, 853.1007, 836.1971, 739.9811, 78...
```

```r
help(gapminder) # the data set gapminder documentation
?gapminder # equivalent to help(gapminder)
```

Can you think of two questions you could answer using these variables?

# Have fun with R
## Pedestrian counts in the city of Melbourne


```r
library(readr)
ped <- read_csv("https://st.netlify.com/data/Pedestrian_Counts.csv")
glimpse(ped)
```

```
## Observations: 1,392,618
## Variables: 4
## $ Date_Time     <chr> "01-MAY-2009 00:00", "01-MAY-2009 00:00", "01-MA...
## $ Sensor_ID     <int> 4, 17, 18, 16, 2, 1, 13, 15, 9, 10, 12, 11, 5, 6...
## $ Sensor_Name   <chr> "Town Hall (West)", "Collins Place (South)", "Co...
## $ Hourly_Counts <int> 209, 28, 36, 22, 52, 53, 17, 124, 5, 8, 2, 5, 15...
```

Can you think of two questions you could answer using these variables?

# Have fun with R
## R packages


```r
install.package("dplyr")
library(dplyr) # load the package
```

* For example, `dplyr` is an R package written by [Hadley Wickham](http://hadley.nz/).
It is a fast, consistent tool for working with data frame.
* There are more than 8000 packages available on R CRAN now.

# Have fun with R
## R basics

* Example 1: a vector of numerics

```r
set.seed(1000) # specify a seed to make sure the results reproducible
x <- rnorm(6) # randomly generate 6 numbers from the standard normal distribution
typeof(x) # type of x
x # print x on the screen
sum(x + 10) # add 10 to each element in x and sum them up
x[1] # extract the 1st element in x
x[1:4] # extract the first four elements in x
x[c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)]
```

* Example 2: a list

```r
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
```

* Example 3: compute statistics on a vector with missing values

```r
x <- c(50, 12, NA, 20) # NA means Not Available in R
mean(x) # calculate the average of x
mean(x, na.rm = TRUE) # na.rm is an argument in the mean() function
```

# What is markdown?

> An **easy-to-read** and **easy-to-write** plain text format.

* Check out the [markdown homepage](https://daringfireball.net/projects/markdown/)
  for its philosophy
* Examples of basic syntax
    + Emphasis
        - `**bold**` ===> **bold**
        - `*italics*` ===> *italics*
    * Lists

```
* Examples of basic syntax
    + Emphasis
        - **bold**
        - *italics*
    + Lists
```

* [Markdown cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)
* This slides is written in **R** + **Markdown** = **[RMarkdown](http://rmarkdown.rstudio.com/)**!!

# Write report in RMarkdown

1. Start RStudio
2. Create a new project (`ETC2420`) for this unit
    + File -> New Project -> Existing Directory -> Empty Project
3. Open a new RMarkdown document and save it as `MYLab1.Rmd` 
    * File -> New File -> R Markdown -> OK -> Knit HTML
4. Look through this sample Rmd document
    + Meta information starting and ending with `---`
    + Embed R code like 
        - One line of R code
<pre>```{r cars} 
summary(cars)
```</pre>
        - Multiple lines of R code
<pre>```{r economics} 
library(dplyr) 
data(economics, package = "ggplot2")
glimpse(economics)
```</pre>
    * Write outside the R chunk but in markdown way
5. Knit the file to a pdf/word document

# Your turn
## Get things done in R and RMarkdown

1. Read in the OECD PISA data

```r
download.file("https://st.netlify.com/data/student_sub.rds",
              destfile = "../data/student_sub.rds") # the path can be changed
student2012.sub <- readRDS("../data/student_sub.rds")
```
2. Tabulate the countries (`CNT`)
3. Extract the values for Australia (`AUS`) and Shanghai (`QCN`)
4. Compute the average and standard deviation of the reading scores (`PV1READ`), for each country
5. Write a few sentences explaining what you learn about reading scores in these two countries

<meta name="copyright" content="LICENSE: CC BY-NC 3.0 US" />
