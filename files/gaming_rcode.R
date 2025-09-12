###############################################################################
# TURTLE GAMES - CUSTOMER METRICS AND REVIEWS ANALYSIS
###############################################################################

# The aim of the R-Analysis is to complement the python analysis by providing 
# additional exploratory and descriptive statistics on the data. 
# The R-Analysis also provides improved models of the data.

###############################################################################

# 1. Import the necessary libraries
library(tidyverse)
library(skimr)
library(DataExplorer)
#library(dplyr)

# Import the psych library
library(psych)

# Breusch pagan test 
#install.packages("lmtest")   # Run this only once
library(lmtest)

# Import rpart for Recurssive partitioning and Regression trees
#install.packages("rpart")
#install.packages("rpart.plot")  
library(rpart)
library(rpart.plot)

# Install plotly package
#install.packages("plotly")
library(plotly)

# Suppress warnings
options(warn=-1)
###############################################################################

# Please add the path to the 'turtle_reviews_updated.csv' file
# 2. Set the working directory
setwd('/Users/alexandraakaoui/Desktop/LSE - Data Analytics /2 Course 3/Assignment/Akaoui_Alexandra_DA301_Assignment')

###############################################################################

# 3. Import the file
# The imported file contains the clean dataset, prepared after all the cleaning
# steps performed in python

df <- read.csv('turtle_reviews_updated.csv')

# Display the first 5 rows of the DataFrame
head(df,100)

###############################################################################

#------------------------------------------------------------------------------
# V. Further Descriptive and Exploratory Data Analysis
#------------------------------------------------------------------------------


# 4. Keep one row per customer_id and drop the review and summary column
df_clean <- df %>%
 select(-review, -summary,-product) %>%
  distinct(customer_id, .keep_all = TRUE)

# Print the size of df_clean to confirm it contains 782 rows
dim(df_clean)

###############################################################################

# 5. Explore the data

# The data columns are all in the correct format. 

# The customer_id column ranges between 1 and 782. It was concluded in the python
# analysis that there 782 unique customers that could be grouped based on their
# age, education, annual remuneration, spending score and loyalty points. 

# The age data shows that most customers are adults with a concentration between 
# ~30-50. 

# Annual remuneration shows that the customers' income concentration is between 
# 23 and 63 thousand pounds. This is consistent with average annual incomes  
# according to the Office of national statistic (ONS).
# source: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/bulletins/annualsurveyofhoursandearnings/2024

# Spending score shows a concentration in the range between 29 to 73. There seems
# to be extreme values with a min of 1 and a max of 99. This will be confirmed through 
# visualizations.

# Loyalty points also seem to be concentrated on the lower bracket. The highest 
# concentration lies between 700 and 1650. There also seem to be outliers with a 
# a min of 25 and a max of 6847. This will be confirmed by visualizations. 

# Using the tibble function
as_tibble(df_clean)

# Using the glimpse function
glimpse(df_clean)

# Using the summary function
summary(df_clean)

###############################################################################

# 6. Frequency tables for gender and education

# The customer base consists mostly of Female customers
# The split is 
## - Female=435
## - Male=347
table(df_clean$gender)

# The customer base consists mostly of Graduate customers
# The split is 
## - Graduate=347 
## - Basic=20 
## - Diploma=75
## - Phd=182 
## - Postgraduate=158
table(df_clean$education)

###############################################################################

# 7. Exploratory analysis visualizations

# Violin plots

# Violin plot: Customer age distribution by gender
# In addition to the insights of the descriptive statistics, the violin plots show
# that the Male customer base is larger on both extremities of the age spectrum. 
# The dominance of Female customers is mainly between the age of 30-50. 

ggplot(df_clean,aes(x=gender,y=age,fill=gender))+geom_violin()+
  labs(fill= 'Gender',
    title='Turtle Games',
  subtitle ='Customer Age Distribution by Gender',
       x='Gender',
       y='Age')+
  theme_classic()

#Violin Plot: Customer age distribution by education
# The data is showing very young customers with Graduate, Phd and postgraduate 
# degrees. This seems questionable and would be highlighted to Turtle games as 
# a data quality issue

ggplot(df_clean,aes(x=education,y=age,fill=education))+geom_violin()+
  labs(fill= 'Education',
       title='Turtle Games',
       subtitle ='Customer Age Distribution by Education',
       x='Education',
       y='Age')+
  theme_classic()

# Violin Plot: Customer Annual remuneration by gender
# The variation between genders in annual income is not significant. Both genders
# follow more or less the same pattern.

ggplot(df_clean,aes(x=gender,y=remuneration_k,fill=gender))+geom_violin()+
  labs(fill= 'Gender',
       title='Turtle Games',
       subtitle ='Customer Annual Remuneration k (GBP) by Gender',
       x='Gender',
       y='Annual Remuneration k (GBP) ')+
  theme_classic()


# Violin Plot: Customer Annual remuneration by education
# Basic education seem to have the largest customer base in the high annual 
# remuneration bracket (>60 k (GBP)).
# All other educational levels seem to have their largest customer base in the 
# lower annual remuneration brackets (<60 k (GBP)).
# This means that high-earners with higher-education don't form a great percentage
# of the current customer base. 

ggplot(df_clean,aes(x=education,y=remuneration_k,fill=education))+geom_violin()+
  labs(fill= 'Education',
       title='Turtle Games',
       subtitle ='Customer Annual Remuneration Distribution by Education',
       x='Education',
       y='Annual Remuneration k (GBP)')+
  scale_y_continuous(limits = c(10, 115)) +  # sets limits and removes outside data
  theme_classic()
 
# Violin Plot: Customer Spending Score by Gender
# Both male and female customers follow more or less the same spending pattern. 
ggplot(df_clean,aes(x=gender,y=spending_score,fill=gender))+geom_violin()+
  labs(fill= 'Gender',
       title='Turtle Games',
       subtitle ='Customer Spending Score Distribution by Gender',
       x='Gender',
       y='Spending Score (0-100)')+
  scale_y_continuous(limits = c(10, 100)) +  # sets limits and removes outside data
  theme_classic()

# Violin Plot: Customer Spending Score by Education
# A big proportion of the higher-education customers have higher spending brackets
# compared to customers having a basic educational level. 

ggplot(df_clean,aes(x=education,y=spending_score,fill=education))+geom_violin()+
  labs(fill= 'Education',
       title='Turtle Games',
       subtitle ='Customer Spending Score Distribution by Education',
       x='Education',
       y='Spending Score (0-100)')+
  scale_y_continuous(limits = c(10, 100)) +  # sets limits and removes outside data
  theme_classic()

###############################################################################

# Boxplots:

# Boxplot: Customer Age distribution by Education
# The boxplot is showing outliers for the Phd education level only. 
# From python analysis, it was noticed that there are 22 customers with age between
# 17-18 with higher-education degrees. These would be highlighted to Turtle games
# as unusual, but they're not statistical outliers. This was also checked by calculating
# IQR, lower and upped bounds. 
ggplot(df_clean,aes(x=education,y=age,fill=education))+
  geom_boxplot()+
  labs(fill= 'Education',
       title='Turtle Games',
       subtitle ='Customer Age Distribution by Education',
       x='Education',
       y='Age')+
  theme_classic()

# Calculating the Interquartile range and the lower and upped bound of age.
df_clean %>%
  group_by(education) %>%
  summarise(
    Q1 = quantile(age, 0.25),
    Q3 = quantile(age, 0.75),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    outliers = sum(age < lower | age > upper)
  )

# Boxplot: Customer Age Distribution by Gender
# Use the ggplotly to transform the original ggplot and make the boxplot 
# interactive.
# Male customers have a slightly larger age range and their median age slightly
# exceeds female customers. 
ggplotly(
  ggplot(df_clean,aes(x=gender,y=age,fill=gender))+
  geom_boxplot()+
  labs(fill= 'Gender',
       title='Turtle Games',
       subtitle ='Customer Age Distribution by Gender',
       x='Gender',
       y='Age')+
  theme_classic())

# Boxplot: Customer Annual Remuneration Distribution by Gender 
# Male annual remuneration has a slightly higher median. Other than that both 
# male and female customers have a very similar annual remuneration distribution
ggplotly(
  ggplot(df_clean,aes(x=gender,y=remuneration_k,fill=gender))+
  geom_boxplot()+
  labs(fill= 'Gender',
       title='Turtle Games',
       subtitle ='Customer Annual Remuneration k (GBP) Distribution by Gender',
       x='Gender',
       y='Annual Remuneration k (GBP)')+
  theme_classic())

# Boxplot: Customer Annual Remuneration Distribution by Education 
# There is only one outlier under the diploma educational level. The outlier was kept
# in the data, as it is not highly unlikely for diploma holders to be high-earners.
# source: https://www.gov.uk/government/publications/labour-market-value-of-higher-and-further-education-qualifications-a-summary-report/labour-market-value-of-higher-and-further-education-qualifications-a-summary-report#:~:text=such%20as%20gender).-,Summary,%5Bfootnote%201%5D
ggplotly(
  ggplot(df_clean,aes(x=education,y=remuneration_k,fill=education))+
  geom_boxplot()+
  labs(fill= 'Education',
       title='Turtle Games',
       subtitle ='Customer Annual Remuneration k (GBP) Distribution by Education',
       x='Education',
       y='Annual Remuneration k (GBP)')+
  theme_classic())

# Boxplot: Customer Spending Score Distribution by Gender
# There is no visible difference between Male and Female customers when it comes
# to spending scores.
ggplotly(
  ggplot(df_clean,aes(x=gender,y=spending_score,fill=gender))+
  geom_boxplot()+
  labs(fill= 'Gender',
       title='Turtle Games',
       subtitle ='Customer Spending Score (1-100) Distribution by Gender',
       x='Gender',
       y='Spending Score (1-100)')+
  theme_classic())

# Boxplot: Customer Spending Score Distribution by Education
# Graduate customers have the highest median spending score followed by Postgraduate
# and Phd customers. 
# Graduate customers constitutes a bigger share of the customer base and this definitely 
# influenced the median. 
# holders. 
ggplotly(
  ggplot(df_clean,aes(x=education,y=spending_score,fill=education))+
  geom_boxplot()+
  labs(fill= 'Education',
       title='Turtle Games',
       subtitle ='Customer Spending Score (1-100) Distribution by Education',
       x='Education',
       y='Spending Score (1-100)')+
  theme_classic())

# Boxplot: Customer Loyalty points Distribution by Gender
# Both female an male customers seem to have a concentration of loyalty points
# <~1620. Female customers have a larger range. 
ggplotly(
  ggplot(df_clean,aes(x=gender,y=loyalty_points,fill=gender))+
  geom_boxplot()+
  labs(fill= 'Gender',
       title='Turtle Games',
       subtitle ='Customer Loyalty Points Distribution by Gender',
       x='Gender',
       y='Loyalty Points')+
  theme_classic())


# Boxplot: Customer Loyalty points Distribution by Education
# The concentration of the loyalty points for all educational levels is <1620 points. 
# The only exception to this is for the basic educational level. 
# All educational levels are showing outliers with high loyalty points. 

ggplotly(
  ggplot(df_clean,aes(x=education,y=loyalty_points,fill=education))+
  geom_boxplot()+
  labs(fill= 'Education',
       title='Turtle Games',
       subtitle ='Customer Loyalty Points Distribution by Education',
       x='Education',
       y='Loyalty Points')+
  theme_classic())

###############################################################################

# Histograms:

# Histogram: Loyalty Points

binwidth_val <- (max(df_clean$loyalty_points) - min(df_clean$loyalty_points)) / 30

ggplot(df_clean, aes(x = loyalty_points)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count.. * binwidth_val), color = 'blue', size = 1, adjust = 1) +
  labs(
    title = 'Turtle Games',
    subtitle = 'Customer Loyalty Points Distribution',
    x = 'Loyalty Points',
    y = 'Count'
  ) +
  theme_classic()


# Histogram: Annual remuneration

binwidth_remu <- (max(df_clean$remuneration_k) - min(df_clean$remuneration_k)) / 30

ggplot(df_clean, aes(x = remuneration_k)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = 'purple', color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count.. * binwidth_remu), color = 'blue', size = 1, adjust = 1) +
  labs(
    title = 'Turtle Games',
    subtitle = 'Customer Annual Remuneration k (GBP) Distribution',
    x = 'Annual Remuneration k (GBP)',
    y = 'Count'
  ) +
  theme_classic()

# Histogram: Spending Score

binwidth_spend <- (max(df_clean$spending_score) - min(df_clean$spending_score)) / 30

ggplot(df_clean, aes(x = spending_score)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = 'green', color = "black", alpha = 0.7) +
  geom_density(aes(y = ..count.. * binwidth_spend), color = 'blue', size = 1, adjust = 1) +
  labs(
    title = 'Turtle Games',
    subtitle = 'Customer Spending Score (1-100) Distribution',
    x = 'Spending Score (1-100)',
    y = 'Count'
  ) +
  theme_classic()


###############################################################################

# 8. Creating loyalty points brackets

df_clean$loyalty_bracket <- cut(df_clean$loyalty_points,
                                breaks = c(0, 2000, 4000, 6000, 7000),
                                labels = c("0-2k", "2k-4k", "4k-6k", "6k+"),
                                include.lowest = TRUE, right = FALSE
)


# Bar Chart: Number of customers by loyalty points bracket
# There are almost equal number of customers in the 2k-4k and 4k-6k bracket.
# The big majority of customers are in the 0-2k loyalty points bracket
# Only 6 Customers have over 6k+

  ggplot(df_clean, aes(x = loyalty_bracket)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", 
            aes(label = scales::percent((..count..)/sum(..count..))), 
            vjust = -0.5) +
  labs(title='Turtle Games',
    subtitle = "Customer Count by Loyalty Points Bracket",
       x = "Loyalty Points Bracket",
       y = "Percentage of Customers") +
  theme_classic()

# Customers with 6k+ loyalty points
# Filtering to get the information of the 6 customers in the 6k+ loyalty points bracket
high_loyalty <- df_clean %>%
  filter(loyalty_points >= 6000)
high_loyalty

###############################################################################

# 9. Spending Score Brackets
# Customers having high spending scores belong to the mid to high loyalty brackets.
# The biggest majority of the customers have a spending score between 40-60. 
# Create spending score brackets
df_clean$spending_bracket <- cut(df_clean$spending_score,
                                breaks = c(0, 20, 40, 60, 80, 100),
                                labels = c("0-20", "20-40", "40-60", "60-80","80-100"),
                                include.lowest = TRUE, right = FALSE
)

ggplot(df_clean, aes(x = spending_bracket,fill=loyalty_bracket)) +
  geom_bar() +
  geom_text(stat = "count", 
            aes(label = scales::percent((..count..)/sum(..count..))), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 3) +
  labs(title='Turtle Games',
       subtitle = "Customer Count by Spending Score Bracket",
       x = "Spending Score Bracket",
       y = "Pecentage of Customers") +
  theme_classic()

# Customers with high spending score and low loyalty points
high_spend_low_loyalty <- df_clean %>%
  filter(spending_score>=90 & loyalty_points<1000)
dim(high_spend_low_loyalty)
high_spend_low_loyalty


###############################################################################

# 10. Scatter plot spending score vs loyalty points

ggplot(df_clean, aes(x = spending_score, y = loyalty_points)) +
  geom_point(aes(color = loyalty_bracket), alpha = 0.6) +   # only mapping color inside aes()
    labs(title = 'Turtle Games',
       subtitle = "Loyalty Points vs Spending Score",
       x = "Spending Score (1-100)",
       y = "Loyalty Points",
       color = "Loyalty Bracket") +
  theme_classic()



###############################################################################

# 11. Scatter plot Annual remuneration k (GBP) vs loyalty points

ggplot(df_clean, aes(x = remuneration_k, y = loyalty_points)) +
  geom_point(aes(color = loyalty_bracket), alpha = 0.6) +   # only mapping color inside aes()
  labs(title = 'Turtle Games',
       subtitle = "Loyalty Points vs Annual Remuneration k (GBP)",
       x = "Annual Remuneration k (GBP)",
       y = "Loyalty Points",
       color = "Loyalty Bracket") +
  theme_classic()


###############################################################################

# 12. Breakup the age column in brackets 

df_clean <- df_clean %>%
  mutate(age_bracket = case_when(
    age >= 17 & age < 25 ~ "17–24",
    age >= 25 & age < 35 ~ "25–34",
    age >= 35 & age < 45 ~ "35–44",
    age >= 45 & age < 55 ~ "45–54",
    age >= 55 & age < 65 ~ "55–64",
    age >= 65 ~ "65–72"
  ))

# Barplot: Number of customers by loyalty points bracket
# Main insight to draw from this:
## - Loyalty points between 2k - 6k mostly belong to customers between the age 
## - of 25-54. 
# Faceted by age brackets
ggplotly(
  ggplot(df_clean, aes(x = loyalty_bracket)) +
  geom_bar(fill = "steelblue") +
  facet_wrap(~age_bracket) +
  labs(title = 'Turtle Games',
       subtitle = "Customer Count by Loyalty Points (Faceted by Age Bracket)",
       x = "Loyalty Points Bracket",
       y = "Number of Customers") +
  theme_classic())

# Barplot: Customer count by loyalty points and age bracket
# Age brackets between 25-34 and 35-44 dominate all loyalty points brackets. 
# These are probably families' with kids and they should be targeted by the loyalty 
# program
# Older age brackets are still there, but their contribution to the loyalty program is 
# minimal. 
# source: https://askcaddle.com/resources/blog/loyalty-programs-are-there-generational-preferences/#:~:text=Baby%20Boomers%20might%20find%20the,loyalty%20programs%20and%20generational%20preferences.
# source: https://action.deloitte.com/insight/4083/new-generation-of-consumers-brings-new-expectations-around-loyalty

ggplotly(
  ggplot(df_clean, aes(x = loyalty_bracket, fill = age_bracket)) +
  geom_bar(position = "dodge") +
  labs(title = 'Turtle Games',
       subtitle = "Customer Count by Loyalty Points and Age Bracket",
       x = "Loyalty Points Bracket",
       y = "Number of Customers",
       fill = "Age Bracket") +
  theme_classic())

# Barplot of loyalty points by age bracket with a gender facet
# Oberservations:
# The youngest age bracket is dominated by male customers. This may be due to a
# higher male affinity for video games. This insight could be used to inform the
# marketing team. 
# 6K+ loyalty points bracket is dominated by senior female customers aged 45+. 
# In contrast, male customers in this high-loyalty bracket tend to be younger. 
# This could be influenced by product preferences and differing engagement with 
# the program.

ggplotly(
  ggplot(df_clean, aes(x = loyalty_bracket, fill = age_bracket)) +
  geom_bar(position = "dodge") +
  labs(title = 'Turtle Games',
       subtitle = "Customer Count by Loyalty Points and Age Bracket",
       x = "Loyalty Points Bracket",
       y = "Number of Customers",
       fill = "Age Bracket") +
  facet_wrap(~gender)+
  theme_classic())

###############################################################################

# 11. Linear Regression Model

# Scatter plot: Loyalty point vs spending score with a line of best fit
# The line of best fit drawn on to the scatter plot shows a linear positive 
# relation between spending score and loyalty points
# Notice how the higher loyalty points brackets are further away from the regression
# line. This can be further investigated in this section
ggplot(df_clean, aes(x = spending_score, y = loyalty_points)) +
  geom_point(aes(color = loyalty_bracket), alpha = 0.6) + # only mapping color inside aes()
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = 'Turtle Games',
       subtitle = "Loyalty Points vs Spending Score",
       x = "Spending Score (1-100)",
       y = "Loyalty Points",
       color = "Loyalty Bracket") +
  theme_classic()


# Scatter plot: Loyalty points vs annual remuneration with a line of best fit
# The line of best fit shows a positive linear relation between loyalty points 
# and annual remuneration of the customers.
# Notice how the higher loyalty points brackets are further away from the regression
# line. This can be further investigated in this section
ggplot(df_clean, aes(x = remuneration_k, y = loyalty_points)) +
  geom_point(aes(color = loyalty_bracket), alpha = 0.6) +   # only mapping color inside aes()
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = 'Turtle Games',
       subtitle = "Loyalty Points vs Annual Remuneration k (GBP)",
       x = "Annual Remuneration k (GBP)",
       y = "Loyalty Points",
       color = "Loyalty Bracket") +
  theme_classic()

# Filter the df_clean to only keep the numerical columns
num_col <- c('age', 'remuneration_k', 'spending_score', 'loyalty_points')
df_filtered <- df_clean[, num_col]

# Determine the correlation between the variables
cor(df_filtered)

# Use the corplot() function
# Specify the data frame (df_filtered) and set
# character size (cex=2)
# The correlation plot clearly shows the positive correlation between 
## - Loyalty points
## - Spending Score
## - Remuneration
# Investigations into including the age, despite its low correlation was carried
# out in python and will not be repeated here.
corPlot(df_filtered,cex=3)


#------------------------------------------------------------------------------
# VI. Additional Predictive Models 
#------------------------------------------------------------------------------

###############################################################################
###############################################################################
# Multiple linear regression in Python only considered transformation of the 
# loyalty points (by taking the log and the square root of the loyalty points).
# No scaling was applied in Python and it will be implemented here to see if it 
# would positively reflect on the performance of the model. 
###############################################################################
###############################################################################

# Scale the data
# Scaling helps with model stability and numerical performance since the independent
# variables are on different scales
# The scaling mainly does the following:
## - Centers the data by subtracting the mean
## - Scale it by dividing by the standard deviation

df_scaled <- df_filtered %>%
  mutate(remuneration_scaled = as.numeric(scale(remuneration_k)),
         spending_scaled = as.numeric(scale(spending_score))
         )

df_scaled

# Create Test and Train data 
set.seed(42)

train_indices <- sample(nrow(df_scaled),size=0.7*nrow(df_scaled))

train_data <- df_scaled[train_indices,]
test_data <- df_scaled[-train_indices,]

# Create a new object for the multiple linear regression
model_mlr = lm(loyalty_points~remuneration_scaled+spending_scaled,data=train_data)

# Model Summary: the summary of the model shows that the spending score 
# and the annual remuneration can explain almost 80% of the variations in the
# loyalty points. The p-values are below <0.05 which shows that both remuneration_k
# and spending score are strong predictors of the loyalty points. 
# These results are, however, unreliable since loyalty points do not follow 
# a normal distribution and since heteroscedasticity is detected in the residuals
summary(model_mlr)

# Extract residuals
residuals <- residuals(model_mlr)

# Plotting the qq plot of the residuals to check whether or not the residuals follow
# a normal distribution
qqnorm(residuals(model_mlr),main = "Multiple linear regression model:\nQQ Plot of Loyalty points vs Scaled Spending Score and Annual Remuneration")
qqline(residuals(model_mlr), col = "blue")

# Shapiro-Wilk test for normality of residuals
# Since p<0.05 the residuals of modela fail the normality test of the residuals
# This is a violation of one of the linear regression models assumptions
shapiro_result <- shapiro.test(residuals)
print(shapiro_result)

# Breusch-Pagan test for heteroscedasticity
# p<0.05 which suggests that there is strong heteroscedasticity in the models'
# residuals
bp_result <- bptest(model_mlr)
print(bp_result)

# Residuals vs fitted values for model_mlr1
# The plot also confirms heteroscedasticity
plot(model_mlr$fitted.values, resid(model_mlr),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Multiple linear regression model:\n Residuals vs Fitted (Check for Heteroscedasticity)",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red", lwd = 2)

# Conclusion
# The regression model violates multiple key assumptions, including normality of
# the residuals and homoscedasticity, as indicated by the Shapiro-Wilk test and
# the Breusch-Pagan test. The model's predictions is then deemed unreliable. 
# Model adjustments or alternative models will be considered in this case. 
# No further evaluations will be performed on the above model such as (MAE, MSE..etc)
# also no predictions will be caculated as the results will be unreliable and misleading.

###############################################################################

# 12. Alternative 1: Filtering out the following loyalty brackets:
## - 4k-6k
## - 6k+
# These brackets were the furthest away from the line of best fit.
# They are also clearly outliers in the data (Check Boxplots for the loyalty points)

# Filter out rows with loyalty points >=4000
low_midloyalty <- df_scaled[df_scaled$loyalty_points<=4000,]

# Recalculate the correlation table with the scaled variables
cor(low_midloyalty)

# Model using the filtered DataFrame including only low to mid loyalty points customers
model_lowmid = lm(loyalty_points~remuneration_scaled+spending_scaled,data=low_midloyalty)

# Generate the summary of the model
summary(model_lowmid)

# Plotting the qq plot of the residuals to check whether or not the residuals follow
# a normal distribution
qqnorm(residuals(model_lowmid),main = "Multiple linear regression model:\n Filtered Loyalty points vs Scaled Spending Score and Annual Remuneration")
qqline(residuals(model_lowmid), col = "blue")

# Print the Shapiro Wilk Test for normality of the residuals
# p<0.05 which suggests that the residuals don't follow a normal distribution
shapiro_result <- shapiro.test(residuals(model_lowmid))
print(shapiro_result)

# Print the Breusch-Pagan test results
# p<0.05 which strongly suggests heteroscedasticity of the residuals
bp_result <- bptest(model_lowmid)
print(bp_result)

# Residuals vs fitted values for model_mlr1
# The plot also confirms heteroscedasticity
plot(model_lowmid$fitted.values, resid(model_lowmid),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Multiple linear regression model using Low to Mid loyalty points:\nResiduals vs Fitted (Check for Heteroscedasticity)",
     pch = 19, col = "darkblue")
abline(h = 0, col = "red", lwd = 2)

# Conclusion
# Removing the high loyalty brackets didn't help improve the performance of the model
# Key linear regression assumptions are still violated, which suggests that 
# the model's prediction would be unreliable. 

###############################################################################

# Alternative 2: Cluster as feature Regression
# source: https://hogonext.com/how-to-combine-clustering-with-regression-for-prediction/
# To get the clusters' labels here in R first a k-means clustering will be applied on the scaled data to get the 
# different customer clusters. k=5 will be used based on the thorough analysis applied
# in Python (Elbow and silhouette method also confirmed by the hierarchical clustering). 
# After having the clusters, a clusters column will be added to the df_scaled
# DataFrame and a model will be built based on the annual remuneration, spending score 
# and clusters as an independent variable in the model. 
# The clusters are in this case required as they help capture the different behaviors 
# of the customers' segments and how they reflect on the loyalty points accumulation.
# The power of this model is that it doesn't assume uniform behavior amongst the
# the different clusters.
# The downside is that clustering is an unsupervised model and the regression model in this case,
# will have to rely on the classification model (built in Python) to get the clusters of the new customers. 

colnames(df_scaled)

# Select the clustering data 
clustering_data <- df_scaled%>%
  select('remuneration_scaled','spending_scaled')

set.seed(42)
kmeans_result <- kmeans(clustering_data,center =5,nstart=25)

# Add cluster labels to the df_scaled DataFrame
df_scaled$clusters <- as.factor(kmeans_result$cluster)

# Replace cluster numbers with meaningful names
df_scaled$cluster_name <- factor(df_scaled$clusters,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("Low Spend Low Income", 
                                            "Medium Spend Medium Income", 
                                            "High Spend Low Income", 
                                            "Low Spend High Income", 
                                            "High Spend High Income"))


# Plot the clusters
ggplotly(
  ggplot(df_scaled, aes(x = remuneration_k, y = spending_score, color = cluster_name)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title="Customer Segments Based on K-Means Clustering",
    x = "Annual Remuneration k (GBP)",
    y = "Spending Score (1-100)",
    color = "Clusters"
  ) +
  theme_classic())

# Use the train_data & test_data 
train_data <- df_scaled[train_indices,]
test_data <- df_scaled[-train_indices,]

# Build regression tree using unscaled variables + cluster as an independent variable
tree_model <- rpart(
  loyalty_points ~ remuneration_k + spending_score + cluster_name,
  data = train_data,
  method = "anova",
  control = rpart.control(cp = 0.01, minsplit = 20)
)

# Prune tree to avoid overfitting
# This is done by determining the optimal complexity parameter that returns
# the lowest cross-validation error ie: the complexity parameter of the best 
# performing tree
optimal_cp <- tree_model$cptable[which.min(tree_model$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(tree_model, cp = optimal_cp)

# Plot pruned regression tree 
rpart.plot(
  pruned_tree,
  main = "Cluster as Feature Regression",
  type = 2,               # Show split labels below the node
  extra = 101,            # Show fitted value and % of observations
  under = TRUE,           # Show text under the box
  faclen = 0,             # Show full names for factor levels
  tweak = 1.2,            # Adjust overall size
  fallen.leaves = TRUE,   # Leaves aligned at the bottom
  shadow.col = "gray",    # Add shadow to boxes
  box.palette = "Blues")  # Use a color palette

# Predict on test set
pred <- predict(pruned_tree, newdata = test_data)

# Actual vs Predicted Plot
ggplot(data.frame(actual = test_data$loyalty_points, predicted = pred), aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Turtle Games",
       subtitle = "Actual vs Predicted Loyalty Points",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

# Evaluate the model by calculating the Mean average error, Mean square error and 
# R-squared
mae <- mean(abs(test_data$loyalty_points - pred))
rmse <- sqrt(mean((test_data$loyalty_points - pred)^2))

# Calculate R-squared manually
# Sum of Squared Errors
SSE <- sum((test_data$loyalty_points - pred)^2)   
# Total Sum of Squares
SST <- sum((test_data$loyalty_points - mean(test_data$loyalty_points))^2)  
# Calculate the R-squared
R2 <- 1 - (SSE / SST)
cat("R-squared:", round(R2, 4), "\n")
cat("MAE:", round(mae, 2), "\n")
cat("RMSE:", round(rmse, 2), "\n")


###############################################################################

# The regression model that includes cluster membership as a feature has shown 
# the promising results. This is because it accounts for distinct characteristics
# of each cluster, rather than assuming uniform behavior across all customers.
# Adding clusters as a feature, the model accounts for unique patterns in each cluster. 
# In this case, a pruned decision tree regressor was used as it handles non-linear
# relationships and outliers well. 
# R-squared 93.71% MAE 276.59 and RMSE 365.69.
# This means that having the spending score, annual remuneration and the cluster we can predict
# 93.71% of the variability in the data. This performance obviously doesn't take into 
# account the error dissipation of the classification model which has an overall accuracy of 85%.

