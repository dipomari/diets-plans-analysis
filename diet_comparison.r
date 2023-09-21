# Importing libraries
library(ggplot2)
library(corrplot)
library(car)  
library(psych)
library(dplyr)

# Loading the Dataset
df <- read.csv('/Users/diegoportillaamarillas/Downloads/Diet_R.csv')

describe(df)
summary(df)

#Data Cleaning and Pre-Processing
df$Diet <- as.factor(df$Diet)

df <- df %>%
  mutate(gender = gsub("0", "Male", gender))
df <- df %>%
  mutate(gender = gsub("1", "Female", gender))

df$BMI.Before <- df$pre.weight / (df$Height/100)^2

df$gender <- as.factor(df$gender)

df$Weight.Loss <- df$pre.weight - df$weight6weeks
head(df)

describe(df)
summary(df)


# Descriptive Statistics
####################################################################################################
####################################################################################################
####################################################################################################
# -Research Question 1:
#  - Does factors such as Gender and Age play a role in the outcome of a Diet?
#Correlation Matrix
numeric_columns <- df[sapply(df, is.numeric)]

corrplot.mixed(cor(numeric_columns),
 order= 'AOE',
 title = "Correlation Plot",
 cl.cex = 2,
 number.cex = 2,
 tl.cex = 1.4)

#Histogram Weight Loss
ggplot(df, aes(x = Weight.Loss)) +
  geom_histogram(binwidth = 0.5, fill = "#0e81df", color = "black") +
  xlab("Weight Loss") +
  ylab("Frequency") +
  ggtitle("Histogram of Weight Loss")+
  theme(plot.title = element_text(size = 30, hjust = 0.4),
  axis.text = element_text(size = 25),
  axis.title = element_text(size=25,face = "bold"))

# AGE

age_summary <- summary(df$Age)
age_description <- describe(df$Age)
age_summary
age_description

# Age vs Weight Loss Scatter Plot
ggplot(df, aes(x = Age, y = Weight.Loss)) +
  geom_point(shape = 19, color = "#3300ff") +
  labs(x = "Age", y = "Weight Loss", title = "Scatterplot of Age vs. Weight Loss") +
  theme(plot.title = element_text(size = 30, hjust = 0.4),
  axis.text = element_text(size = 25),
  axis.title=element_text(size=25,face="bold"))

plot(age_mean_df, 
     xlab = "Age Group", 
     ylab = "Mean Weight Loss", 
     main = "Mean Weight Loss by Age Group", 
     type = "o",
     col = "#013762"
)

correlation <- cor(df$Age, df$Weight.Loss)
correlation

# Calculate mean and standard deviation of weight loss by gender
mean_by_gender <- tapply(df$Weight.Loss, df$gender, mean)
sd_by_gender <- tapply(df$Weight.Loss, df$gender, sd)

# Create a data frame to store the results
gender_summary <- data.frame(Gender = c("Female", "Male"), Mean_WL = mean_by_gender, SD_WL = sd_by_gender)
gender_summary

# Descriptive statistics for each diet group within each gender
df %>%
  group_by(gender, Diet) %>%
  summarise(
    Mean_WL = mean(Weight.Loss),
    Median_WL = median(Weight.Loss),
    SD_WL = sd(Weight.Loss),
    Q1 = quantile(Weight.Loss, 0.25),
    Q3 = quantile(Weight.Loss, 0.75)
  ) %>%
  ungroup()

# Create a boxplot with filled boxes by gender
ggplot(df, aes(x = factor(gender), y = Weight.Loss, fill = factor(gender))) +
  geom_boxplot() +
  xlab("Gender") +
  ylab("Weight Loss") +
  ggtitle("Weight Loss by Gender")+
  theme(plot.title = element_text(size = 50, hjust = 0.4), legend.position='none',
  axis.text = element_text(size = 30))

# Boxplot Diet x Gender
ggplot(df, aes(x=Diet, y=Weight.Loss, fill=gender)) +
  geom_boxplot(position=position_dodge(1))+
  ggtitle("Weight Loss by Diet X Gender")+
  theme(plot.title = element_text(size = 30, hjust = 0.4),
  axis.text = element_text(size = 30), 
  axis.title=element_text(size=20,face="bold"),
  legend.key.size = unit(2, 'cm'),
  legend.text = element_text(size=20))

  #BoxPlot Diets x Weight Loss
ggplot(df, aes(x = Diet, y = Weight.Loss, fill = Diet)) +
  geom_boxplot() +
  xlab("Diet Plan") +
  ylab("Weight Loss") +
  ggtitle("Weight Loss by Diet Plan")+
  theme(plot.title = element_text(size = 50, hjust = 0.4), legend.position='none',
  axis.text = element_text(size = 30), 
  axis.title=element_text(size=20,face="bold"))

# Diets Std Dev Viz

std_deviation <- df %>%
  group_by(Diet) %>%
  summarize(SD = sd(Weight.Loss))

ggplot(std_deviation, aes(x = Diet, y = SD, fill = Diet)) +
  geom_bar(stat = "identity", alpha = 0.7) + 
  labs(x = "Diet Plan", y = "Standard Deviation", title = "Standard Deviation of Weight Loss by Diet Plan")+
  theme(plot.title = element_text(size = 34, hjust = 0.4), legend.position='none',
  axis.text = element_text(size = 30), 
  axis.title=element_text(size=20,face="bold"))


####################################################################################################
####################################################################################################
####################################################################################################


#Inferential Statistics

#RQ1 (Age x Weight Loss)

#Assumption NORMALITY
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 4, fill = "#0e81df", color = "black") +
  xlab("Age") +
  ylab("Frequency") +
  ggtitle("Histogram of Age")+
  theme(plot.title = element_text(size = 30, hjust = 0.4),
  axis.text = element_text(size = 25),
  axis.title = element_text(size=25,face = "bold"))

#Assumption LINEARITY
plot(df$Age, df$Weight.Loss, 
     xlab = "Age", 
     ylab = "Weight Loss", 
     main = "Scatterplot of Age vs. Weight Loss",
     pch = 19,
     col = "#3300ff")

# Assumption Hogeneneity of Variances

ggplot(data = df, aes(x = Diet, y = Age)) +
  geom_boxplot(fill = "#0e81df", color = "black") +
  labs(title = "Boxplot of Age by Diet", x = "Diet", y = "Age") +
  theme(plot.title = element_text(size = 50, hjust = 0.4), legend.position='none',
  axis.text = element_text(size = 30), 
  axis.title=element_text(size=20,face="bold"))

bartlett.test(Age ~ Diet, data = df)

# Model
lm_model <- lm(Weight.Loss ~ Age, data = df)
summary(lm_model)

plot(lm_model, 1)
title(main = "Residual vs Fitted", line = 2,  cex.main = 2)
par(
  cex.lab = 1.7,      
  col.lab = "black",  
  col.main = "black" 
)

#RQ2 (gender x Weight Loss)

#Assumptions t-Test
female_data <- df[df$gender == "Female", ]
ggplot(data = female_data, aes(x = Weight.Loss)) +
  geom_histogram(binwidth = 2, fill = "#e90fb6b0", color = "black") +
  labs(title = "Female Weight Loss Histogram", x = "Weight Loss", y = "Frequency") +
  theme(plot.title = element_text(size = 50, hjust = 0.4), legend.position='none',
  axis.text = element_text(size = 30), 
  axis.title=element_text(size=20,face="bold"))

male_data <- df[df$gender == "Male", ]
ggplot(data = male_data, aes(x = Weight.Loss)) +
  geom_histogram(binwidth = 1.8, fill = "#0fd7e9", color = "black") +
  labs(title = "Male Weight Loss Histogram", x = "Weight Loss", y = "Frequency")+
  theme(plot.title = element_text(size = 50, hjust = 0.4), legend.position='none',
  axis.text = element_text(size = 30), 
  axis.title=element_text(size=20,face="bold"))

#Homogeniety of Variance

bartlett.test(Weight.Loss ~ gender, data = df)

#TEST
t.test(Weight.Loss ~ gender,var.equal=TRUE, data = df)


# RQ2 (Diet x Weight Loss) One-way Anova
#ANOVA Assumptions 

    #NORMALITY
qqnorm(df$Weight.Loss[df$Diet == "1"])
title(main = "Q-Q Plot of Weight Loss Data (DIET 1)", line = 3,  cex.main = 1.8)
qqline(df$Weight.Loss[df$Diet == "1"])

qqnorm(df$Weight.Loss[df$Diet == "2"])
title(main = "Q-Q Plot of Weight Loss Data (DIET 2)", line = 3,  cex.main = 1.8)
qqline(df$Weight.Loss[df$Diet == "2"])

qqnorm(df$Weight.Loss[df$Diet == "3"])
title(main = "Q-Q Plot of Weight Loss Data (DIET 3)", line = 3,  cex.main = 1.8)
qqline(df$Weight.Loss[df$Diet == "3"])

    #Equal Variances
bartlett.test(Weight.Loss ~ Diet, data = df)

#TEST
anova_result <- aov(Weight.Loss ~ Diet, data = df)
summary(anova_result)

posthoc <- TukeyHSD(anova_result)

print(posthoc)

# RQ3 (Gender_Diet 3 x Weight loss)
#Assumptions were checked previously
# t-Test

subset_data <- df[df$Diet == 3, ]

t_test_result <- t.test(Weight.Loss ~ gender, var.equal=TRUE, data = subset_data)
t_test_result

#RQ5 (BMI x Weight Loss)

#Assumption NORMALITY
ggplot(df, aes(x = BMI.Before)) +
  geom_histogram(binwidth = 1.7, fill = "#0e81df", color = "black") +
  xlab("BMI Before Diet") +
  ylab("Frequency") +
  ggtitle("Histogram of BMI before diet")+
  theme(plot.title = element_text(size = 30, hjust = 0.4),
  axis.text = element_text(size = 25),
  axis.title = element_text(size=25,face = "bold"))

# Q-Q plot for BMI
qqnorm(df$BMI.Before)
qqline(df$BMI.Before)


#Assumption LINEARITY
ggplot(df, aes(x = BMI.Before, y = Weight.Loss)) +
  geom_point(shape = 19, color = "#3300ff") +
  labs(x = "BMI Before Diet", y = "Weight Loss", title = "Scatterplot of BMI vs. Weight Loss") +
  ggtitle("Histogram of BMI before diet")+
  theme(plot.title = element_text(size = 30, hjust = 0.4),
  axis.text = element_text(size = 25),
  axis.title = element_text(size=25,face = "bold"))

# Assumption Hogeneneity of Variances

ggplot(data = df, aes(x = Diet, y = Age)) +
  geom_boxplot(fill = "#0e81df", color = "black") +
  labs(title = "Boxplot of Age by Diet", x = "Diet", y = "Age") +
  theme(plot.title = element_text(size = 50, hjust = 0.4), legend.position='none',
  axis.text = element_text(size = 30), 
  axis.title=element_text(size=20,face="bold"))

bartlett.test(Age ~ Diet, data = df)

# Model
lm_model <- lm(Weight.Loss ~ BMI.Before, data = df)
summary(lm_model)

plot(lm_model, 1)
title(main = "Residual vs Fitted", line = 2,  cex.main = 2)
par(
  cex.lab = 1.7,      
  col.lab = "black",  
  col.main = "black" 
)
