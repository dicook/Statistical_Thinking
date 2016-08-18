## ---- sample
sample(c("pig", "pig", "car"), 1)
?sample

## ---- fun-ppc-sample
ppc_sample <- function() {
  sample(c("pig", "pig", "car"), 1)
}
ppc_sample() 

## ---- for-loop
for (i in 1:20) {
  print(i)
}

## ---- for-loop-eg
for (i in 1:20) {
  print(ppc_sample())
}

## ---- 20-props-cars-1
# My version
results <- vector(mode = "character", length = 20)
for (i in 1:20) {
  results[i] <- ppc_sample()
}
ncar <- sum(results == "car")
ncar / 20

## ---- 20-props-cars-2
# Di's version
ncar <- 0
for (i in 1:20) {
  if (ppc_sample() == "car") {
    ncar <- ncar + 1
  }
}
ncar / 20

## ---- ppc-choice
ppc_choice <- function(choice = 1) {
  doors <- sample(c("pig", "pig", "car"))
  doors[choice]
}
ppc_choice(choice = 2)

## ---- if-else
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

## ---- ppc-host-1
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

## ---- ppc-host-2
ppc_host <- function(choice = 1) {
  # R code
}

## ---- ppc-play
ppc_play <- function(choice = 1, strategy = "switch") {
  # Leaving the fun bit to you to figure out :)
}
