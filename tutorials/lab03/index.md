---
title: "ETC2420/5242 <br> Testing hypotheses using permutation"
author: Earo Wang <br> & <br> Nathaniel Tomasetti 
date: Lab 03
---



# Data structures --- data frame

* Vector
* Matrix
* Array
* List
* **Data frame**
    + `economics`, `gapminder` and `pedestrian` datasets that you looked at in the
      first week are all data frames.
    * More on data structures in R, please read one of the chapters in [Advanced R](http://adv-r.had.co.nz/Data-structures.html).

## Create a data frame


```r
library(dplyr)
library(tidyr)
# Create a data frame
yawn_expt <- data_frame(group = c(rep("treatment", 34), rep("control", 16)),
                        yawn = c(rep("yes", 10), rep("no", 24),
                                 rep("yes", 4), rep("no", 12)))
```


```r
# Ways to take a look at the data frame
head(yawn_expt) # the first rows to display
tail(yawn_expt) # the last rows to display
glimpse(yawn_expt)
```

# Make a contingency table



<center>

|group     | no| yes| total|
|:---------|--:|---:|-----:|
|control   | 12|   4|    16|
|treatment | 24|  10|    34|
</center>


```r
# Make a contigency table 
long_yawn <- tally(group_by(yawn_expt, group, yawn))
wide_yawn <- spread(long_yawn, yawn, n)
tbl_yawn <- mutate(wide_yawn, total = sum(no, yes))
tbl_yawn
```

```
## Source: local data frame [2 x 4]
## Groups: group [2]
## 
##       group    no   yes total
##       <chr> <int> <int> <int>
## 1   control    12     4    16
## 2 treatment    24    10    34
```

* However, the R code above is NOT readable, since it needs reading from right to left.
* "Piping" with `%>%` has been introduced to make code more readable.


```r
# Make a contigency table with pipe %>%
yawn_expt %>%
  group_by(group, yawn) %>% 
  tally() %>%
  spread(yawn, n) %>% 
  group_by(group) %>%
  mutate(total = sum(no, yes))
```

```
## Source: local data frame [2 x 4]
## Groups: group [2]
## 
##       group    no   yes total
##       <chr> <int> <int> <int>
## 1   control    12     4    16
## 2 treatment    24    10    34
```

# Permutation test


```r
prop_dif <- function(dat) {
  permute_yawn <- dat %>% 
    mutate(yawn = sample(yawn)) # permutate yawn variable
  # You're expected to write the rest of the function
}
```



* Run the function 10000 times
* Set seed to make results reproducible


```r
set.seed(1234) # give a seed to randomly sample from 1:10
sample(1:10)
```

```
##  [1]  2  6  5  8  9  4  1  7 10  3
```

```r
set.seed(1234) # retrieve the last random sample by using the same seed
sample(1:10)
```

```
##  [1]  2  6  5  8  9  4  1  7 10  3
```


```r
set.seed(2016) 
diff_prop <- numeric(length = 10000)
for (i in 1:10000) {
  # Writing a for-loop is easy peasy for you ;)
}
```



* Make a histogram


```r
library(ggplot2)
pdif_df <- as_data_frame(diff_prop) # convert numerics to data frame
ggplot(data = pdif_df, aes(x = value)) + 
  geom_histogram(binwidth = 0.025)
```

<img src="figure/histogram-1.png" title="plot of chunk histogram" alt="plot of chunk histogram" style="display: block; margin: auto;" />

* Draw a vertical line on the plot that represents the difference for the actual data

<img src="figure/vline-1.png" title="plot of chunk vline" alt="plot of chunk vline" style="display: block; margin: auto;" />

# Misc

## Online R resources

* [Data wrangling cheet sheet](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)
  about `dplyr` and `tidyr`
* [Data visualization cheet sheet](https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)
  about `ggplot2`
* [R programming course](https://www.coursera.org/learn/r-programming) on Coursera starts this week!

## Consultation hours

* Earo Wang
    * **Time**: 13:30 to 15:00 on Thursday
    * **Venue**: Level 11/W1106, Menzies Building
* Nathaniel Tomasetti
    * **Time**: 14:00 to 15:00 on Tuesday
    * **Venue**: Level 11/W1106, Menzies Building

<meta name="copyright" content="LICENSE: CC BY-NC 3.0 US" />
