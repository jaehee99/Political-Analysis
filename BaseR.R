# POL3010 
# September 16, 2021 
# Jaehee Lee 
# Problem Set 1 

# File last edited on 210920 by YK
# File last run on 210920 by YK

# YK: Very good! Don't forget to check out the answer key for different ways of coding.
# Total: 20/20

# 1. 
# (a) 
stat.tool <- c("R", "Python", "Stata", "SAS", "SPSS")
stat.tool
# (b) 
calendar <- matrix(5:18, byrow = TRUE, nrow = 2)
calendar
# (c) 
kids <- c("girl", "boy", "boy", "boy", "girl")
factor_kids <- factor(kids)
factor_kids         # YK: Your code works, but the object name is different.
# (d) 
grade <- c("A", "D", "B", "C", "B", "A", "B", "B", "C", "F")
factor_grade <- factor(grade, ordered = TRUE, levels = c("F", "D", "C", "B", "A"))
factor_grade        # YK: Again, a different object name

# 2. 
# (a) 
country <- c("Cambodia", "China", "Iceland", "Kazakhstan", "Saudi Arabia", "Singapore", 
             "South Africa", "South Korea", "UK", "US")
status <- c("partly free", "not free", "free", "not free", "not free", "partly free", "free", 
            "partly free", "free", "free")
access <- c(12, 8, 25, 10, 12, 19, 16, 22, 23, 21)
content <- c(18, 2, 34, 11, 9, 17, 29, 24, 30, 30)
userright <- c(13, 0, 36, 11, 5, 18, 25, 20, 25, 25)
InternetFreedom <- data.frame(country, status, access, content, userright)
InternetFreedom

# (b) 
## Using base R 
### First way
InternetFreedom2 <- InternetFreedom
InternetFreedom2$total <- InternetFreedom2$access + InternetFreedom2$content + InternetFreedom2$userright
InternetFreedom2

## Using tidyverse
library(tidyverse)
### Second way
InternetFreedom %>%
  group_by(country) %>%
  mutate(total = access + content + userright) %>%
  ungroup() -> InternetFreedom2_2way
InternetFreedom2_2way
### Third way  
InternetFreedom %>%  
  add_column(total = access + content + userright) -> InternetFreedom2_3way
InternetFreedom2_3way

# (c) 
partly_free_data <- subset(InternetFreedom2, subset = status == "partly free", select = c("country", "status"))
partly_free_data
# (d) 
userright_less_20 <- subset(InternetFreedom2, subset = userright < 20) 
userright_less_20
# (e)
total_high_low <- InternetFreedom2[order(-InternetFreedom2$total), ]
total_high_low

# 3. 
NewList <- list(stat.tool,calendar, kids, grade, InternetFreedom, InternetFreedom2)
NewList      # YK: Your code is correct, but some components have different values because of different names used in earlier questions.

##########################################################

# POL3010 
# October 14, 2021
# Jaehee Lee 
# Problem Set 2

# File last edited on 201021 by YK
# File last run on 201021 by YK

# Total: 19.8/20
# YK: Good job! Please see the comments below and review the solution. I like how you tried to write code to have the exact answers as the final output though, it's perfectly fine to run simple code and get the answers, using the information in the output (e.g. Q1(b) and Q1(c)). 

library(poliscidata) # load poliscidata

# Question 1 
str(gss) 
help(gss) 
gss$science_quiz # check science_quiz 
gss$wordsum # check wordsum
# YK: Don't print out the entire values of var in this size of dataset. Call head(), tail(), or other relevant functions instead.

# Question 1-(a)
# Use weights argument because gss dataset is a survey data
mean_science_quiz <- wtd.mean(gss$science_quiz, w=gss$wtss) 
mean_science_quiz

median_science_quiz <- wtd.median(gss$science_quiz, w=gss$wtss) 
median_science_quiz

mode_science_quiz <- wtd.mode(gss$science_quiz, w=gss$wtss) 
mode_science_quiz

sd_science_quiz <- wtd.sd(gss$science_quiz, w=gss$wtss) 
sd_science_quiz

mean_wordsum <- wtd.mean(gss$wordsum, w=gss$wtss) 
mean_wordsum

median_wordsum <- wtd.median(gss$wordsum, w=gss$wtss) 
median_wordsum

mode_wordsum <- wtd.mode(gss$wordsum, w=gss$wtss) 
mode_wordsum

sd_wordsum <- wtd.sd(gss$wordsum, w=gss$wtss) 
sd_wordsum

# Question 1-(b)
# Answer:
# "The public knows more about words than about science": FALSE
# Reasoning: 
print(mean_science_quiz > mean_wordsum)
# This result is TRUE, indicating that the mean of science_quiz is higher than wordsum,


# The average of the public knowledge on science is higher than the average of the public knowledge on word.
print(median_science_quiz == median_wordsum)
# The median of these are same.
print(mode_science_quiz > mode_wordsum)
# This result is TRUE, indicating that the most common value in science_quiz is higher than wordsum.
# Based on the results above, the public knows more about science than words, therefore, the statement in the question 1-(b) is FALSE

#Question 1-(c)
# "science_quiz has a greater degree of dispersion than word_sum": TRUE
print(sd_science_quiz > sd_wordsum)
# This is TRUE, so standard deviation of science_quiz is higher than the standard deviation of wordsum.
# If the standard deviation is higher then, it is more spread out, indicating that science_quiz has a greater degree of dispersion than word_sum

#Question 1-(d)
science_quiz_percent <- freqC(gss$science_quiz, w=gss$wtss)
# Including the NA's the percentage is this(Percent):
science_percent_w_NA <- science_quiz_percent[10, 'Percent'] + science_quiz_percent[11, 'Percent'] 
# Ignoring the NA's the percentage is this(Valid Percent):
science_percent_n_NA <- science_quiz_percent[10, 'Valid Percent'] + science_quiz_percent[11, 'Valid Percent'] 

wordsum_percent <- freqC(gss$wordsum, w=gss$wtss)
# Including the NA's the percentage is this(Percent):
wordsum_percent_w_NA <- wordsum_percent[10, 'Percent'] + wordsum_percent[11, 'Percent'] 
# Ignoring the NA's the percentage is this(Valid Percent):
wordsum_percent_n_NA <- wordsum_percent[10, 'Valid Percent'] + wordsum_percent[11, 'Valid Percent'] 

# Result: 
# Respondents would receive a grade of A on science quiz with NA included is: 
science_percent_w_NA
# Respondents would receive a grade of A on science quiz without NA is: 
science_percent_n_NA
# Respondents would receive a grade of A on wordsum with NA included is: 
wordsum_percent_w_NA 
# Respondents would receive a grade of A on wordsum without NA is: 
wordsum_percent_n_NA

# YK: As we discussed in class, NA's mean respondents didn't answer the questions and therefore, should be removed to calculate the valid percentage. Or think about it this way: You treated NA's as if they are 'higher' than 10's, which doesn't make sense. Also, NA's in R don't have a size. Even though you have two versions of answers, I'm giving a little penalty for your attention next time. (1.8/2pts)

# Question 2 
# Question 2-(a)
gss$attend #check the attend variable 
class(gss$attend) 
levels(gss$attend) # check levels 

# reclassify the variables as ordered factors and check
attend_ord <- factor(gss$attend, 
                     levels=c("Never", "<Once/yr",  "Once/yr",  "Sev times/yr",
                              "Once/mo",  "2-3 times /mo",  "Nrly evry wk",
                              "Every wk", ">Once/wk"),
                     ordered=TRUE)  
class(attend_ord) # check attend_ord

# Question 2-(b)
# mode of attend_ord 
wtd.mode(attend_ord, w=gss$wtss)
# median of attend_ord
wtd.median(attend_ord, w=gss$wtss)

# We can double check above results using the freqC() function
freqC(attend_ord, w=gss$wtss, plot = FALSE) 
# We can also check mode and median in this freqC() 
# We can say that the mode is "Never" because the Valid Percent of "Never" is the highest. 
# When the Cum Percent is 54.08 is "Sev times/yr", so this includes 50 percent, therefore, "Sev times/yr" is the median of this attend_ord

# Question 2-(c)
freqC(attend_ord, w=gss$wtss) # Also, show the plot and see the dispersion. 

# Answer: Low dispersion!
# Reason: 
# If attend_ord had high variation, the frequency distribution would be almost equal or similar, but it isn't.
# If attend_ord had no dispersion, all cases would go into one category, but it isn't 
# Therefore, based on the above reasoning and also when we see the bar chart, it seems that attend_ord is a relatively low degree of dispersion.

# Question 3
?world 
str(world)
world$hdi
# Question 3 -(a)
sum(is.na(world$hdi))
# There are nine missing values in hdi

# Question 3- (b)
hdi_mean <- mean(world$hdi, na.rm = TRUE)
hdi_median <- median(world$hdi, na.rm = TRUE)

print(hdi_mean < hdi_median) 
# hdi has a negative skew because, when comparing hdi_mean and hdi_median, I got the 
# result that hdi_median is greater than hdi_mean
# When median is greater than mean, 
# most values are clustered in the right side and there is few outliers that pulls the mean down, 
# so there is a long tail in the negative direction so it is left skewed/negatively skewed.


library(moments) # load moments library to check the skewness 
skewness(world$hdi, na.rm = TRUE) 
# Also checked the skewness using moments library, and the result is negative. 

# Question 3- (c)
world_country_hdi <- world[ , c("country", "hdi")] # select "country" and "hdi" variables and assign it to a world_country_hdi
ordered_hdi <- world_country_hdi[order(world_country_hdi$hdi, decreasing = TRUE), ] # order hdi and assign it to ordered_hdi 
top_5 <- ordered_hdi[1:5,] # since we already made it into the order, we can just select the first 5 rows and assign it to top_5
top_5['country'] # show the five countries that have highest values of HDI

# Double check using sortC function which is only in poliscidata, in one line
sortC(world, country, hdi, descending = TRUE)[1:5, ]['country']
