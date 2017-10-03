#####
# Week 8
#####

# Import Stata data into R
library("foreign")
week8 <- read.dta("https://github.com/hylee87/surveyclass/raw/master/data/week8data.dta")
detach("package:foreign", unload = TRUE)

#-----------------------------
# Let's look at data
#-----------------------------

library("summarytools")

# Variable information
dfSummary(week8)

# Descriptive statistics for nominal variables
freq(week8$marital)
descr(as.numeric(week8$marital)) # as.numeric() -> transform factors to integers

# Descriptive statiscs for ordinal variables
freq(week8$degree)
descr(as.numeric(week8$degree))

# Descriptive statistics for interval variables
freq(week8$prestg80)
descr(week8$prestg80)

## Percentile values
# .10: 10%; .25: 25%; .50: 50%; .75: 75%; .90: 90%
# na.rm = TRUE : exclude missing values (NA)
quantile(week8$prestg80, c(.10, .25, .50, .75, .90), na.rm = TRUE)

# Descriptive statistics for subgroups
descr(week8[week8$sex=="male",]$prestg80)
descr(week8[week8$sex=="female",]$prestg80)

#-----------------------------
# Graphics
#-----------------------------

## Bar plot
# <exclude = "na">: Exclude "na" category; if you want to exclude multiple categories, use <exclude = c("iap", other", "dk", "na")> for instance.
barplot(table(week8$marital, exclude = "na"), main = "Marital Status") 

## Pie graph
pie(table(week8$marital, exclude = "na"), main = "Marital Status")

## Histogram
hist(week8$prestg80, main = "Distribution of Occupational Prestige",
     col = "black", border = "white")

## Box plot
# Simple box plot
boxplot(week8$prestg80, main = "Distribution of Occupational Prestige") 

# Box plot for subgroups; use <variable~subgroup>; data = "your data name"
boxplot(prestg80~sex, data = week8, main = "Occupational Prestige by Gender",
        xlab = "Gender", ylab = "Occupational Prestige")
