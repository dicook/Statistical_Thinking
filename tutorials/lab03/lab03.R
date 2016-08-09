## ---- df
library(dplyr)
library(tidyr)
# Create a data frame
yawn_expt <- data_frame(group = c(rep("treatment", 34), rep("control", 16)),
                        yawn = c(rep("yes", 10), rep("no", 24),
                                 rep("yes", 4), rep("no", 12)))

## ---- glimpse
# Ways to take a look at the data frame
head(yawn_expt) # the first rows to display
tail(yawn_expt) # the last rows to display
glimpse(yawn_expt)

## ---- table1
# Make a contigency table 
long_yawn <- tally(group_by(yawn_expt, group, yawn))
wide_yawn <- spread(long_yawn, yawn, n)
tbl_yawn <- mutate(wide_yawn, total = sum(no, yes))
tbl_yawn

## ---- kable
kable(tbl_yawn)

## ---- table2
# Make a contigency table with pipe %>%
yawn_expt %>%
  group_by(group, yawn) %>% 
  tally() %>%
  spread(yawn, n) %>% 
  group_by(group) %>%
  mutate(total = sum(no, yes))

## ---- prop_dif
prop_dif <- function(dat) {
  permute_yawn <- dat %>% 
    mutate(yawn = sample(yawn)) # permutate yawn variable
  # You're expected to write the rest of the function
}

## ---- prop_dif2
prop_dif <- function(dat) {
  permute_yawn <- dat %>% 
    mutate(yawn = sample(yawn))
  total <- permute_yawn %>% 
    group_by(group) %>% 
    summarize(count = sum(n()))
  yes <- permute_yawn %>% 
    filter(yawn == "yes") %>% 
    group_by(group) %>% 
    summarize(count = sum(n()))
  diff_prop <- diff(yes$count / total$count)
  return(diff_prop)
}

## ---- seed
set.seed(1234) # give a seed to randomly sample from 1:10
sample(1:10)
set.seed(1234) # retrieve the last random sample by using the same seed
sample(1:10)

## ----- diff_prop
set.seed(2016) 
diff_prop <- numeric(length = 10000)
for (i in 1:10000) {
  # Writing a for-loop is easy peasy for you ;)
}

## ---- diff_prop1
set.seed(2016) # set a seed to make the results reproducible
diff_prop <- numeric(length = 10000)
for (i in 1:10000) {
  diff_prop[i] <- prop_dif(yawn_expt)
}

## ---- histogram
library(ggplot2)
pdif_df <- as_data_frame(diff_prop) # convert numerics to data frame
ggplot(data = pdif_df, aes(x = value)) + 
  geom_histogram(binwidth = 0.025)

## ---- vline
actual_diff <- 10 / 34 - 4 / 16
ggplot(dat = pdif_df, aes(x = value)) + 
  geom_histogram(binwidth = 0.025) +
  geom_vline(xintercept = actual_diff, colour = "red")
