# libraries 
library(poliscidata) # load poliscidata
library(tidyverse)
library(moments) # load moments library to check the skewness 

# 
stat.tool <- c("R", "Python", "Stata", "SAS", "SPSS")
stat.tool
#  
calendar <- matrix(5:18, byrow = TRUE, nrow = 2)
calendar
#  
kids <- c("girl", "boy", "boy", "boy", "girl")
factor_kids <- factor(kids)
factor_kids         
# 
grade <- c("A", "D", "B", "C", "B", "A", "B", "B", "C", "F")
factor_grade <- factor(grade, ordered = TRUE, levels = c("F", "D", "C", "B", "A"))
factor_grade        

# 
country <- c("Cambodia", "China", "Iceland", "Kazakhstan", "Saudi Arabia", "Singapore", 
             "South Africa", "South Korea", "UK", "US")
status <- c("partly free", "not free", "free", "not free", "not free", "partly free", "free", 
            "partly free", "free", "free")
access <- c(12, 8, 25, 10, 12, 19, 16, 22, 23, 21)
content <- c(18, 2, 34, 11, 9, 17, 29, 24, 30, 30)
userright <- c(13, 0, 36, 11, 5, 18, 25, 20, 25, 25)
InternetFreedom <- data.frame(country, status, access, content, userright)
InternetFreedom

# 
InternetFreedom2 <- InternetFreedom
InternetFreedom2$total <- InternetFreedom2$access + InternetFreedom2$content + InternetFreedom2$userright
InternetFreedom2

#
InternetFreedom %>%
  group_by(country) %>%
  mutate(total = access + content + userright) %>%
  ungroup() -> InternetFreedom2_2way
InternetFreedom2_2way

#
InternetFreedom %>%  
  add_column(total = access + content + userright) -> InternetFreedom2_3way
InternetFreedom2_3way

# 
partly_free_data <- subset(InternetFreedom2, subset = status == "partly free", select = c("country", "status"))
partly_free_data
#
userright_less_20 <- subset(InternetFreedom2, subset = userright < 20) 
userright_less_20
# 
total_high_low <- InternetFreedom2[order(-InternetFreedom2$total), ]
total_high_low

# 
NewList <- list(stat.tool,calendar, kids, grade, InternetFreedom, InternetFreedom2)
NewList      

# 
str(gss) 
help(gss) 
gss$science_quiz # check science_quiz 
gss$wordsum # check wordsum

# 
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

# 
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

#
# "science_quiz has a greater degree of dispersion than word_sum": TRUE
print(sd_science_quiz > sd_wordsum)
# This is TRUE, so standard deviation of science_quiz is higher than the standard deviation of wordsum.
# If the standard deviation is higher then, it is more spread out, indicating that science_quiz has a greater degree of dispersion than word_sum

#
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

#  
# Respondents would receive a grade of A on science quiz with NA included is: 
science_percent_w_NA
# Respondents would receive a grade of A on science quiz without NA is: 
science_percent_n_NA
# Respondents would receive a grade of A on wordsum with NA included is: 
wordsum_percent_w_NA 
# Respondents would receive a grade of A on wordsum without NA is: 
wordsum_percent_n_NA

# 
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

# 
# mode of attend_ord 
wtd.mode(attend_ord, w=gss$wtss)
# median of attend_ord
wtd.median(attend_ord, w=gss$wtss)

# We can double check above results using the freqC() function
freqC(attend_ord, w=gss$wtss, plot = FALSE) 
# We can also check mode and median in this freqC() 
# We can say that the mode is "Never" because the Valid Percent of "Never" is the highest. 
# When the Cum Percent is 54.08 is "Sev times/yr", so this includes 50 percent, therefore, "Sev times/yr" is the median of this attend_ord

# 
freqC(attend_ord, w=gss$wtss) # Also, show the plot and see the dispersion. 

# Answer: Low dispersion!
# Reason: 
# If attend_ord had high variation, the frequency distribution would be almost equal or similar, but it isn't.
# If attend_ord had no dispersion, all cases would go into one category, but it isn't 
# Therefore, based on the above reasoning and also when we see the bar chart, it seems that attend_ord is a relatively low degree of dispersion.

# 
?world 
str(world)
world$hdi
# 
sum(is.na(world$hdi))
# There are nine missing values in hdi

# 
hdi_mean <- mean(world$hdi, na.rm = TRUE)
hdi_median <- median(world$hdi, na.rm = TRUE)

print(hdi_mean < hdi_median) 
# hdi has a negative skew because, when comparing hdi_mean and hdi_median, I got the 
# result that hdi_median is greater than hdi_mean
# When median is greater than mean, 
# most values are clustered in the right side and there is few outliers that pulls the mean down, 
# so there is a long tail in the negative direction so it is left skewed/negatively skewed.

skewness(world$hdi, na.rm = TRUE) 
# Also checked the skewness using moments library, and the result is negative. 

# 
world_country_hdi <- world[ , c("country", "hdi")] # select "country" and "hdi" variables and assign it to a world_country_hdi
ordered_hdi <- world_country_hdi[order(world_country_hdi$hdi, decreasing = TRUE), ] # order hdi and assign it to ordered_hdi 
top_5 <- ordered_hdi[1:5,] # since we already made it into the order, we can just select the first 5 rows and assign it to top_5
top_5['country'] # show the five countries that have highest values of HDI

# Double check using sortC function which is only in poliscidata, in one line
sortC(world, country, hdi, descending = TRUE)[1:5, ]['country']

# 
str(world) # see the data 
# 'world' dataset description: variables about countries in the world.
# 
# world$women13: "Percent Women in lower house of legislature, democracies only, 2013 (Inter-Parliamentary Union)"
# Step 1: see the data
str(world$women13) 
# Step 2: find the NA using logic
is.na(world$women13)
# Step 3: Figure out how many missing values world$women13 have. 
sum(is.na(world$women13))

# 
mean(world$women13, na.rm = TRUE)
median(world$women13, na.rm = TRUE)
# Answer: Mean is 21.10 and Median is 20.80
summary(world$women13) # double check using summary() function

# 
hist(world$women13, 
     main = "HISTOGRAM \n Percent Women in lower house of legislature \n (Democracies,2013)",
     xlab = " 'world' data, 'women13' variable",
     border="blue", 
     col= "light blue", 
     xlim = c(0, 50))

#  
# The standard deviation can tell how spread out from the mean/median.
# So, I find out the standard deviation, because it can tell the dispersion of the distribution
sd(world$women13, na.rm = TRUE) # I get the result of 10.6408 
# Standard deviation is measuring how far the typical values are from the mean. 
# So, the typical values are 10.6408 away from the mean 21.10
# The smaller the s.d., the closer values are to the mean. 
# Here I get the result of 10.6408 and the smallest value of s.d. is 0, so I can say that the values are 
# not that close to the mean. So, it is high dispersion. 

#  
world$women13.centered <- scale(world$women13, center=TRUE, scale=FALSE)
summary(world$women13.centered) # check results

# 
world$women13.standard <- scale(world$women13, center = TRUE, scale = TRUE)
# when scale is TRUE, then argument will divide the resulting difference by s.d. 
summary(world$women13.standard)

#
nes
levels(nes$budget_deficit_x)
freqC(nes$budget_deficit_x, nes$nesw)

#  
nes$budget_deficit_x.n <- as.numeric(nes$budget_deficit_x)
head(nes$budget_deficit_x, 10)
head(nes$budget_deficit_x.n, 10) 
# Answer: 
# On nes$budget deficit x.n, those who favor reducing the federal budget
# deficit have the following three numeric codes: 1, 2, and 3.
# On nes$budget deficit x.n, those who neither favor nor oppose reducing the
# federal budget deficit have the following numeric code: 4.
# On nes$budget deficit x.n, those who oppose reducing the federal budget
# deficit have the following numeric codes: 5, 6, and 7.

# 
nes$budget_deficit_x.3cat <- cut2(nes$budget_deficit_x.n, cuts = c(4,5,7))

#  
freqC(nes$budget_deficit_x.3cat, w=nes$nesw)

# 
levels(nes$budget_deficit_x.3cat) <- c("Favor", "Neither", "Oppose")
levels(nes$budget_deficit_x.3cat)
freqC(nes$budget_deficit_x.3cat, w=nes$nesw)
table(nes$budget_deficit_x, nes$budget_deficit_x.3cat)

#  
# nes description: The American National Election Survey polls individuals about their political beliefs and behavior
# nes$ftgr cons: Feeling thermometer(Conservatives)
# nes$ftgr liberals: Feeling thermometer(Liberals)
nes$ftgr_conservatism.index <- nes$ftgr_cons - nes$ftgr_liberals # create a new variable 

# 
min(nes$ftgr_conservatism.index, na.rm = TRUE)
max(nes$ftgr_conservatism.index, na.rm = TRUE)
summary(nes$ftgr_conservatism.index, na.rm = TRUE)
# Answer: 
# The new variable nes$ftgr conservatism.index ranges -100 between and 100.

# 
cutpoints = c(-90,-80,-70,-60,-50,-40,-30,-20,-10,0,1,11,21,31,41,51,61,71,81,91,100)
nes$conservatism.index.cat <- cut2(nes$ftgr_conservatism.index, cuts=cutpoints)
head(nes$conservatism.index.cat)

# 
class(nes$conservatism.index.cat) # this is factor 
nes$conservatism.index.cat <- as.ordered(nes$conservatism.index.cat) # make it as an ordered factor
class(nes$conservatism.index.cat)

# 
levels(nes$conservatism.index.cat) <- c("-100 ~ -91", "-90 ~ -81", "-80 ~ -71", 
                                        "-70 ~ -61", "-60 ~ -51", "-50 ~ -41", "-40 ~ -31","-30 ~ -21", "-20 ~ -11", "-10 ~ 1", "0", "1~10", "11~20" ,"21~30","31~40", "41~50", "51~60",
                                        "61~70", "71~80", "81~90","91~100")
levels(nes$conservatism.index.cat)

#  
freqC(nes$conservatism.index.cat, w=nes$wt) 
# Describe its distribution: 
# 1) Almost Symmetric Distribution: The shape to the left and the right of value "0" line are similar
# 2) Unimodal Distribution: There is a peak in this distribution at the value "0"
# Most of the values are concentrated in the 0, which means that nes$ftgr_cons and nes$ftgr_liberals are same. 
# So, most of the people who did this survey would rate 50 marks toward both conservatives and liberals.
# Indicating that these people have no feeling at all, for both conservatives and liberals.


# 
# Step 1: Examine the data 
str(gss$grass)
# Step 2: Create an indicator variable
gss$grass.yes <- as.numeric(gss$grass == "LEGAL")
# Step 3: Check if LEGAL is 1, if NOT LEGAL is 0
head(gss$grass, 10)
head(gss$grass.yes, 10)
# Final Step: Double check final output using table function
table(gss$grass, gss$grass.yes)

# 
str(states$demstate13) # check the data 
hist(states$demstate13, 
     main="Partisan Composition of State Legislatures in the U.S.", 
     xlab = "Percent State Legislature Democrats in 2013", 
     xlim=c(0,100), 
     ylim = c(0,12), 
     col = "light blue", 
     border = "white", 
     font.lab = 4)

# 
world$dem_level4 #Regime type (Economist 2014)
str(world$dem_level4) #Factor #Four levels 
class(world$dem_level4)

plot(world$dem_level4, 
     main = "Countries Regime Type", 
     ylab = "Counts", 
     xlab = "Regime type in 2014")

# 
str(world$oecd)
str(world$gender_equal3)
oecd_gender <- table(world$oecd, world$gender_equal3)
barplot(oecd_gender, 
        main = "Gender Empowerment Measure \n in OECD vs. Non−OECD Member States", 
        xlab = "Gender Empowerment Measure",
        ylab = "# of Countries",
        legend = rownames(oecd_gender),
        ylim = c(0,25),
        beside = TRUE)

# 
states$south <- factor(states$south , levels=c("South", "Nonsouth"))

boxplot(states$blkpct10 ~ states$south, 
        main = "% of Blacks in Southern and Non−Southern States (2010)",  
        ylim = c(0,40),
        names = c("South","Non-south"),
        ylab = "%", 
        xlab = "")

# 
# The age hypothesis: Older people are more likely to turn out to vote than younger people
# check variables 
str(nes$voted2012) # Did Respondent vote in 2012?
str(nes$dem_age3) # Demographic: Age Group 
levels(nes$voted2012) # check the levels of voted2012
levels(nes$dem_age3) # check the levels of dem_age3 

xtp(nes, y = voted2012, x = dem_age3)

# 
# Answer: Yes, it appears that the age hypothesis is correct! 
# 68.1% of respondents who are 17-39 years old voted in 2012, and this figure increase to 82.3%. 
# 82.3 % of respondents who are 40-59 years old voted in 2012, and further this figure increase to 88.57%.
# 88.6% of respondents who are more than 60 years old voted in 2012. 

# 
str(nes$cses_closepty) # cses_closepty: Close to any political party? 
nesD <- svydesign(id = ~ 1, data = nes, weights = ~wt) # Update nesD 
# cross tab with a control variable 
xtabC(~voted2012 + dem_age3 + cses_closepty , data = nesD)

# 
# Additive. Based on the controlled cross-tab, 
# both when 'closeness to political party' is 'yes' and 'no', 
# shows that older people are more likely to turnout to vote than younger people. 
# Therefore, the direction of the relationship between the independent variable and the dependent variable
# the same at all values of the control variable. 

# Compare between closeness of political party 
# We can see that 
# Age group 17-39: 78.33% vs. 55.59%
# Age group 40-59: 87.60% vs. 70.05%
# Age group 60-older: 94.06% vs. 79.96%
# The strength of the relationship between the independent variable and the dependent variable are similar at 
# all values of the control variable. 

# check with using line graph
df <- data.frame(
  age_group = c("17-39", "40-59", "60-older"),
  close_pp = c(78.33, 87.60, 94.06),
  not_close_pp = c(55.59, 70.05, 79.96)
)
df

plot(df$close_pp,            
     type = "b", lwd = 2, xaxt = "n", col = "blue",       
     xlab = "Age Group", 
     ylab = "% Respondents Turnout (2012)", 
     main = "% Respondents Turnout Decision (2012) \nby 'Age group' and 'Closeness to Party'",
     ylim = c(0, 100))       
lines(df$not_close_pp,           
      type = "b",     
      lwd = 2, 
      col = "red")
axis(1, at = 1:length(df$age_group), labels = df$age_group)        
legend("bottomright",               
       legend = c("Close to Political Party", "Not Close to Political Party"), 
       lty = 1, lwd = 2, pch = 21,   
       bty = "n",                   
       col = c("blue", "red"),     
       text.col = c("blue", "red"))
# When we see this line graph, we can say that red line and blue line are almost parallel. 
# So it is additive relationship

# 
# check variables
str(nes$ftcasi_illegal)
str(nes$libcon3)
# Create a mean comparison table to check 2-(a) statement
compmeans(x = nes$ftcasi_illegal, f = nes$libcon3, w = nes$wt, plot = FALSE)

# 
# Yes, the political ideology explanation is supported! 
# Liberals are more likely to have warmer feelings toward illegal immigrants than conservatives. 
# The mean rating of those who are liberals is 46.75.
# Yet, it goes down to 37.74 and then 31.81 for those who are moderate, conservative, respectively.

#  
# Check variable
str(nes$dem_educ3)
levels(nes$dem_educ3)
# mean comparison table with a control var
imeansC(~ftcasi_illegal, ~libcon3+ dem_educ3, data = nesD)

# 
# Interactive: 
# Although there is a negative relationship between political 
# ideology and feeling ratings toward illegal immigrants with all levels of education level.
# the strength of this relationship varies with education level.

# The difference in average feeling toward illegal immigrants between liberals and conservatives 
# are 44.24-32.16 = 12.08 in the 'HS or less' category. 
# In the 'Some coll' group, there's a more difference, 45.38-31.43 =13.95. 
# Among those with 'Coll+' group, the difference is bigger which is 50.27-31.86 =18.41. 

# The relationship between education level and feeling thermometer is different across different political ideology. 
# (Liberals: 50.27 - 44.24 = 6.03; Moderate: 40.45-35.93=4.52; Conservative: 31.86-32.16= -0.3)


# Line graph
df2 <- data.frame(
  ideology = c("Liberal", "Moderate", "Conservative"),
  HS_or_less = c(44.24, 35.93, 32.16),
  Some_coll = c(45.38, 39.14, 31.43),
  Coll__ = c(50.27, 40.45, 31.86)
)
df2

plot(df2$HS_or_less,             
     type = "b", lwd = 2, xaxt = "n", col = "blue", 
     xlab = "Political Ideology", 
     ylab = "Feeling Thermometer Scale for the illegal immigrants", 
     ylim = c(0, 100),  
     yaxs = "i")       
lines(df2$Some_coll,           
      type = "b",     
      lwd = 2, 
      col = "green")
lines(df2$Coll__,           
      type = "b",     
      lwd = 2, 
      col = "red")
axis(1, at = 1:length(df2$ideology), labels = df2$ideology)        
legend("topright",  
       title.col = "black",
       legend = c("HS_or_less", "Some_coll", "Coll_"), 
       lty = 1, lwd = 2, pch = 21, cex = .7,   
       bty = "n",                   
       col = c("blue", "green", "red"),     
       text.col = c("blue", "green", "red"))

#We can see in the graph that the relationship between political ideology and feelings toward illegal immigrants,
# controlling for education level, is interactive. 



