################################################################################
###
### Working with Dates and Times in R
###
### Sara Gottlieb-Cohen, StatLab Manager
### Marx Library
### Yale University
###
################################################################################

# Load necessary packages

library(tidyverse)
library(lubridate)

############## DATES ####################

# Great resource: lubridate cheatsheet
# https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_lubridate.pdf

# When working with dates in R, we have "dates" and "date-times."

# A date example is June 21st, 1988. 
# A date-time example is 1988-06-21 05:22:30.

# To format dates, lubridate provides a series of functions that are a
# permutation of the letters "m" "d" and "y" to represent the ordering of
# month, day and year. 

mdy("June 21, 1988")
mdy("6-21-1988")
mdy("06/21/1988")
mdy("6/21/1902")

dmy("21-June-1988")
dmy("21 June 1988")

ymd("1988 6 21")
ymd("1988-06-21")

bday <- mdy("June 21st, 1988")
bday

# What kind of variable is bday?

class(bday)

# It prints like character data, but it actually  represents the number of days since
# 1970-01-01. Dates prior to 1970-01-01 are negative numbers. Use as.numeric()
# to verify.

as.numeric(bday)

# lubridate can handle date-times, too, even if they include AM/PM times.

bday2 <- "June 21st, 1988 17:22:30"
mdy_hms(bday2)

bday3 <- "June 21st, 1988 5:22:30PM"
mdy_hms(bday3)

# You can also calculate durations between dates using "%--%.

today <- "October 26th, 2020"
today <- mdy(today)
my_age <- bday %--% today

as.numeric(my_age)
as.duration(my_age)

time_length(my_age, unit = "months")
time_length(my_age, unit = "years")
time_length(my_age, unit = "days")
time_length(my_age, unit = "minutes")

# We can also read in times without dates using the functions ms, hm, or hms,
# where again "h", "m", and "s" stand for "hours", "minutes", and "seconds".
# Here are a few examples.

time1 <- c("1:13", "0:58", "1:01")
time1 <- ms(time1)

time2 <- c("12:23:11", "09:45:31", "12:05:22")
time2 <- hms(time2)

time3 <- c("2:14", "2:16", "3:35")
time3 <- hm(time3)

# But don't forget that these are not character strings! These times are 
# actually stored as seconds. 

as.numeric(time1)
as.numeric(time2)
as.numeric(time3)

## Exercise 1: When did Trump tweet the most in 2016?

# Read in the data. 

trump_tweets <- read_csv("https://raw.githubusercontent.com/mkearney/trumptweets/master/data/trumptweets-1515775693.tweets.csv")
head(trump_tweets)
str(trump_tweets)
nrow(trump_tweets)

# 1. "created_at" is our date-time variable; the first step is to treat it as such.

trump_tweets <- trump_tweets %>%
  mutate(created_at = ymd_hms(created_at))

str(trump_tweets)
head(trump_tweets$created_at)
as.numeric(head(trump_tweets$created_at))

# 2. Create a new column called "date" that contains the date but ignores the time.
# Hint: use the date() command.

trump_tweets <- trump_tweets %>%
  mutate(date = date(created_at))

# 3. Create a new column that stores just the month of the tweet. Make sure the month is
# labeled but abbreviated. Hint: use the month() command. Typing ?month will help you out.

trump_tweets <- trump_tweets %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE))

# 4. Create a new column that stores just the year of the tweet. Hint: the command will
# similar to the command for month.

trump_tweets <- trump_tweets %>%
  mutate(year = year(date))

# 5. Use a pipe to count the number of tweets in each month of 2016. Call this new 
# data frame "monthly_counts." Hint: filter the data to only include observations 
# from 2016, group the data by month, and then use the count() command.

monthly_counts <- trump_tweets %>%
  filter(year == 2016) %>%
  group_by(month) %>%
  count()

# 6. Plot the data as a line graph, with month on the x-axis and number of tweets
# on the y-axis. 

ggplot(monthly_counts, aes(x = month, y = n, group = 1)) +
  geom_line() +
  labs(title = "Number of tweets by month",
       y = "# of tweets")

## Exercise 2: Which month/year has the shortest intervals between tweets?

# 1. Use a pipe to calculate the interval and duration (in minutes) between tweets.
# Start by creating a new "lag" column, that has the date/time of the previous tweet.
# Then create an "interval" column that stores the interval data between the two dates.
# Finally, create an "interval_minutes" column, which transforms that duration into minute
# units. Hint: make sure your data is arranged in ascending order, starting with the 
# oldest date, before creating the lag!

trump_tweets <- trump_tweets %>%
  arrange(created_at) %>%
  mutate(previous_tweet = lag(created_at),
         interval = previous_tweet %--% created_at,
         interval_minutes = time_length(interval, unit = "minutes"))

# 3. Use a pipe to find the tweet with the shortest amount of time, on average, between
# tweets. Hint: group the data by year and month, use the summarize command to calculate the
# average time between tweets (in minutes), and then present the data in ascending order.

av_durations <- trump_tweets %>%
  group_by(year, month) %>%
  summarize(average = mean(interval_minutes, na.rm = T)) %>%
  arrange(average)

############## STRINGS ####################

# Great resource: stringr cheatsheet
# https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf

# Most commands take at least two inputs; the first input is always the character string, or vector of strings.

# str_length() will return the length of a string

str_length("Sara")

# str_sub() will return certain letters of a string, based on their position.

str_sub("Sara", 1, 1)
str_sub("Sara", 1, 2)
str_sub("Sara", 2, 4)
str_sub("Sara", -1, -1)

# str_sub() can also be used to replace certain characters in a string. The character string
# must be saved as an object first.

name <- "Sara"
str_sub(name, 1, 1) <- "X"
name

# str_to_upper(), str_to_lower(), and str_to_title() change cases.

strings <- c("This is a sentence.", "This is also a sentence.")

str_to_lower(strings)
str_to_upper(strings)
str_to_title(strings)

# We can extract words with word(), based on position

word(strings, 1, 3)

# str_count() will count the number of times some pattern occurs. By counting the number of spaces,
# we can count the number of words.

str_count(strings, pattern = " ")

# We can split strings into their component words.

str_split(strings, pattern = " ")
words <- str_split(strings, pattern = " ", simplify = TRUE)
words

# We can use the base R function table() to count how many times each word appears.

table(words)

# stringr provides many commands related to pattern matching.

emails <- c("sara@yale.edu", "sara@gmail.com", "sara@aol.com", "sara@harvard.edu")

# We to detect, locate, extract, replace, and split strings. 
# Each function takes two arguments: a vector of strings, and a pattern to match.

# str_detect() detects presence or absence of a pattern and returns T/F. 
# str_subset() returns the elements that contain that pattern.

str_detect(emails, pattern = ".edu")
str_subset(emails, pattern = ".edu")

# str_count() tallies the number of matches in each element.

str_count(emails, pattern = "a")

# We can find the number of times a string occurs in a vector using sum().

sum(str_detect(emails, pattern = "edu"))
sum(str_count(emails, pattern = "a"))

# str_locate() locates the first position of a pattern and returns a numeric matrix
# with columns start and end. str_locate_all() locates all matches, and returns a list of 
# numeric matrices.

str_locate(emails, pattern = "edu")

starts <- str_locate(emails, pattern = "edu")[, 1]
starts 

# str_replace() and str_replace_all() will replace a matched pattern.
# str_replace() will replace only the first instance of a pattern within each element;
# in this case, we get the same result using both because "edu" never appears more than
# once in a character string.

str_replace(emails, pattern = "a", "X")
str_replace_all(emails, pattern = "a", "X")

## Exercise 1: How many tweets contained the word "Hillary" appear during each month of 2016?

# 1. Create a new column called "contains_Hillary" that indicates whether a tweet contained "Hillary."
# But first make sure to make all tweets lower case!

trump_tweets <- trump_tweets %>%
  mutate(text = str_to_lower(text),
         contains_Hillary = str_detect(text, "hillary"))

# 2. Use a pipe to only look at data from 2016. Then summarize the data to calculate the number of times
# the tweets containing "Hillary" for each month of 2016. Call this new data frame "Hillary_counts."

Hillary_counts <- trump_tweets %>%
  filter(year == 2016) %>%
  group_by(month) %>%
  summarize(num_tweets = sum(contains_Hillary))

# 3. Plot the data as a line graph, with month on the x-axis and number of tweets on the y-axis.

ggplot(Hillary_counts, aes(x = month, y = num_tweets, group = 1)) +
  geom_line() +
  labs(title = "Number of tweets by month",
       y = "# of tweets")
