######
# R codes for Week 9
# 10/10/2017
######

#-------------------------------------------------
# This week we will import SPSS file into R.
# After importing it, You will see some warning message. 
# Don't worry about that. It does not affect the data.
# The record type 7 is used to store features to make newer SPSS software 
# able to read new data, but does not affect data.
# I have used this numerous times and data is not lost.
#-------------------------------------------------

# Import SPSS data
# Do not change the following code. I saved the file on the web.
# Running the code will import the data into your RStudio.
# As in the week 8, we will use "foreign" package.

library("foreign")
mydata <- read.spss("https://github.com/hylee87/surveyclass/raw/master/data/GSS2008.sav", to.data.frame = TRUE)

# Alternatively, you can download the data using the link.
# Then, put the file in your working directory
# And run "mydata <-read.spss ("GSS2008.sav", to.data.frame = TRUE)"


# Check the data in the "Environment" tab.
# The data should include 2,023 observations and 42 variables.

# Now, let's look at the data

# Load "summarytools" package
library("summarytools")

# We will look at the variables and how they are coded
# You can check the assigned labels for each value in the variable
dfSummary(mydata)

#-------------------------------------------------
#-------------------------------------------------
# The following codes are necessary for Q 1

# A: Check the normality of the distribution
descr(mydata$prestg80) # Prestige score of Occupations
descr(mydata$age) # Hours worked last week

# If mean > median : Right-skewed
# If mean < median : Left-skewed

# Then, check "Skewness".
# If Skewness > 0 : Right-skewed
# If Skewness < 0 : Left-skewed

# If -0.5 < Skewness < 0.5, we can think of the distribution as normal.

#-------------------------------------------------
# B: Standardize an interval variable
mydata$z.age <- scale(mydata$age, center = TRUE, scale = TRUE) # Note that "z.hrs1" is a new variable name which is a z-score of "hrs1"

head(mydata) # It will show the data for the first six cases.
# But we are seeking for information only on "hrs1" and "z.hrs1". For achieving this, run the following code.
head(mydata[,c("id", "age", "z.age")]) # Be careful at the location of commas(,).

#-------------------------------------------------
# C: Finding Z-score for a specific value and its associated probability
# I made a function for Q1. C, which lets you to obtain the necessary information.
# Please run the following code. You need not understand what the code means.
# It is the beyond the scope of this course.
# Run this code without changing anything.

getZ <- function(v1, v2){
  score <- (v1 -mean(v2, na.rm = TRUE))/sd(v2, na.rm = TRUE)
  prob <- pnorm(abs(round(score, digits = 2)))
  cat(" 1. Z-score for", v1, ":", round(score, digits = 2))
  cat("\n 2. The area between mean and Z-score (B)", ":", round(prob-0.5, digits = 4))
  cat("\n 3. The area beyond Z-score (C)", ":", 0.5-round(prob-0.5, digits = 4))
}
#-------------------------------------------------

# Now, we are ready to use this function.

getZ(75, mydata$age) 

# This will show the Z-score for 75 in "hrs1"age" and its associated probability. 
# getZ(your value, your variable), then it will show the z-score of your value in the distribution of your variable.
# The area between mean and Z-score corresponds to B area in the Standard Normal Table that I posted in Week 8.
# The area beyond Z-score corresponds to C area in the Standard Normal Table.

#-------------------------------------------------
# D: Create the frequency table
freq(mydata$age)

#-------------------------------------------------
# F: Create the histogram
hist(mydata$age, main = "Distribution of Age",
     col = "black", border = "white", xlab = "Age")
hist(mydata$z.age, main = "Distribution of Standardized Age",
     col = "black", border = "white", xlab = "Z-score for Age")

#-------------------------------------------------
#-------------------------------------------------
# The following codes are necessary for Q 2
# For answering Q 2, we need another package called "gmodels"
# Install and load it

library("gmodels")

#-------------------------------------------------
# A: Construct 95% confidence intervals for Age
ci(mydata$age, confidence = 0.95, na.rm = TRUE)

# Use "confidence = 0.90" if you want to get 90% confidence intervals.

#-------------------------------------------------
# B: Construct 95% confidence intervals for the proportion of those who think homosexuality is not wrong at all.

#-------------------------------------------------
# First, we need to recode homosex variable into one in which 1 = "not wrong at all" and 0 for other categories
# Before doing it, please check the labels for homosex, using dfSummary(mydata).
mydata$d.homosex[mydata$homosex=="NOT WRONG AT ALL"] <- 1
mydata$d.homosex[mydata$homosex=="ALWAYS WRONG"] <- 0
mydata$d.homosex[mydata$homosex=="ALMST ALWAYS WRG"] <- 0
mydata$d.homosex[mydata$homosex=="SOMETIMES WRONG"] <- 0
mydata$d.homosex[mydata$homosex=="OTHER"] <- NA

# Then, check whether you generate the new variable correctly.
# Compare "NOT WRONG AT ALL" in homosex with "1" in d.homosex. The number should be the same.
freq(mydata$homosex)
freq(mydata$d.homosex)

# The mean in the output is the proportion of those who think homosexuality is not wrong at all.
descr(mydata$d.homosex)

# Construct 95% confidence intervals for the proportion of those who think homosexuality is not wrong at all.
ci(mydata$d.homosex, confidence = 0.95, na.rm = TRUE)

#-------------------------------------------------
#-------------------------------------------------
# The following codes are necessary for Q 3

#-------------------------------------------------
# A: Calculate 90% CI of age for different classes
ci(mydata$age[mydata$class=="LOWER CLASS"], confidence = 0.9, na.rm = TRUE)
ci(mydata$age[mydata$class=="WORKING CLASS"], confidence = 0.9, na.rm = TRUE)
ci(mydata$age[mydata$class=="MIDDLE CLASS"], confidence = 0.9, na.rm = TRUE)
ci(mydata$age[mydata$class=="UPPER CLASS"], confidence = 0.9, na.rm = TRUE)

#-------------------------------------------------
# B: We will visualize the results of Q3 A.
# For visualizing it, we need the package called "gplots"
# Install and load it.

library("gplots")
plotmeans(age ~ class, data = mydata, mean.labels = TRUE, ylab = "Age by class",
          connect = FALSE)

#-------------------------------------------------
# C. Describe the relationship between age and class
# 1. Compare means first. For instance, working class is younger than other classes.
# 2. Check whether this difference is statistically meaningful.
#    If confidence intervals do not overlap with each other, you can state
#    "the difference is statistically meaningful".


# Please feel free to contact me if you are stuck in running this code.
# Thanks.