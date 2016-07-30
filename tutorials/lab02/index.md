---
title: "ETC2420/5242 <br> Write functions in R: <br> using the Monty Hall example"
author: Earo Wang <br> <earo.wang@gmail.com>
date: Lab 02
---



# Consultation time

* **Time**: 13:30 to 14:30 on Thursday
* **Venue**: Level 11/W1106, Menzies Building

# Play the game Monty Hall

* Go to the web site [InterActivate's Simple Monty Hall](http://www.shodor.org/interactivate/activities/SimpleMontyHall/). Each member of the group should do this themselves.
    1. Play the game 21 times with the strategy ''Do not switch doors''. Note the proportion of times you win the jackpot.
    2. Play the game 21 times with the strategy ''Switch doors''. Note the proportion of times you win the jackpot.
* Discuss with your group how the two strategies change your chances of winning.

## My stats

* Strategy 1:
    + Games stayed: 21
    + Games stayed and won: 9
    + Experimental probability to win: 42.86%
+ Strategy 2:
    + Games switched: 21
    + Games switched and won: 15
    + Experimental probability to win: 71.43%

# The algorithm of the game

1. Randomly assign `{"pig", "pig", "car"}` to door `{1, 2, 3}`
2. Player selects a door
3. Check if door selected matches a `{pig}`, and if so show the other `{pig}`. If it is the `{car}`, randomly select one of the two other doors to reveal
4. Enable the player choose their next door, either to `switch` or `stay`
5. Print result

## You're going to write your first game in R!!!

# Start writing a simple function in R

## What does `sample()` do?


```r
sample(c("pig", "pig", "car"), 1)
?sample
```

* Try to run this line 10 times and track the outputs

## Your first function: `ppc_sample()`


```r
ppc_sample <- function() {
  sample(c("pig", "pig", "car"), 1)
}
ppc_sample() 
```

```
## [1] "pig"
```

* Run this function 20 times

# What is a loop? How to write a for-loop in R?

* Execute code repeatedly
* Here's a simple example of for-loop


```r
for (i in 1:20) {
  print(i)
}
```

* Use `ppc_sample()` in a for-loop


```r
for (i in 1:20) {
  ppc_sample()
}
```

* Report the proportion of times that `car` is selected. (You could put this in a loop and compute the proportion of times `car` shows.)


```r
# My version
results <- vector(mode = "character", length = 20)
for (i in 1:20) {
  results[i] <- ppc_sample()
}
ncar <- sum(results == "car")
ncar / 20
```


```r
# Di's version
ncar <- 0
for (i in 1:20) {
  if (ppc_sample() == "car") {
    ncar <- ncar + 1
  }
}
ncar / 20
```

The code above does the same thing. Which piece of R code is more efficient?

* How many times would you expect `car` to be selected out of 20 runs?
* Modify the function so that it takes one argument, which will be the player's initial choice of door. 


```r
ppc_choice <- function(choice = 1) {
  doors <- sample(c("pig", "pig", "car"))
  doors[choice]
}
ppc_choice(choice = 2)
```

```
## [1] "car"
```

# If-else statement

<center><img src="figure/monty.png" alt="ifelse"/></center>

## In English

* If the player's choice is `pig`, the host opens the door for the other `pig`. (`if`)
* If the player's choice is `car`, the host randomly opens either one of the doors for `pig`. (`else`)

## In R


```r
if (condition) { # if the condition is satisfied, then excute
  # R code
  if (condition) { # nested if-else
    # R code
  } else {
    # R code
  }
} else {
  # R code
}
```

## Implemented in the game


```r
choice <- 2
doors <- sample(c("pig", "pig", "car"))
notchosen <- c(1:3)[-choice]
if (doors[choice] == "pig") { # if the player's choice is pig
  if (doors[notchosen[1]] == "pig") {
    host_choice <- paste(notchosen[1], doors[notchosen[1]], sep = "-")
  } else {
    host_choice <- paste(notchosen[2], doors[notchosen[2]], sep = "-")
  }
} else { # if the player's choice is car
  d <- sample(notchosen, 1)
  host_choice <- paste(d, doors[d],  sep = "-")
}
host_choice
```

```
## [1] "1-pig"
```

* Wrap the code above into a function `ppc_host()`


```r
ppc_host <- function(choice = 1) {
  # R code
}
```

# Your turn


```r
ppc_play <- function(choice = 1, strategy = "switch") {
  # Leaving the fun bit to you to figure out :)
}
```

## Turn in 

* Your `.Rmd` file
* Your pdf (or word) file that results from knitting the Rmd
* Make sure your group members are listed as authors, one person per group will turn in the report
* **DUE**: by 7:30am in the following Wednesday , loaded into moodle

# View this slides in your browser

<center>
<http://rawgit.com/earowang/Statistical_Thinking/gh-pages/tutorials/lab02/index.html>
</center>

## Manual control
* **Left/Right arrow**: page up/down; or click to page down
* **C**: toggle table of content
* **A**: display current or all slides
* **S**: make fonts smaller
* **B**: make fonts bigger

<meta name="copyright" content="LICENSE: CC BY-NC 3.0 US" />
