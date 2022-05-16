#' SDM A1: Credit Rating

setwd("C:/Users/abhatt/Desktop/SDM/Data")
library(readxl)
df <- read_excel("CreditRating.xlsx", sheet="Data")
str(df)
View(df)

#' Data Preprocessing

df$ID <- NULL
df$Gender <- factor(df$Gender)
df$Gender <- relevel(df$Gender, "Female")
df$Student <- factor(df$Student)
df$Student <- relevel(df$Student, "No")
df$Married <- factor(df$Married)
df$Married <- relevel(df$Married, "No")
df$Ethnicity <- factor(df$Ethnicity)
df$Ethnicity <- relevel(df$Ethnicity, "Caucasian")

which(! complete.cases(df)) 
summary(df)

#' Data Visualization

hist(df$Rating, prob=T)
den <- density(df$Rating)                    
lines(den, col="red")

library(corrplot)
m <- cor(cbind(df[, 1:6], df[, 11]))
corrplot(m, method="circle")

library(PerformanceAnalytics)
chart.Correlation(m)

#' OLS Models

m1 <- lm(Rating ~ Income + Limit + Cards + Balance + Age + Student + Married, data=df) 
m2 <- lm(Rating ~ Income + Limit + Cards + Balance + Age + Student + Married + Gender + Ethnicity, data=df)

library(stargazer)
stargazer(m1, m2, type="text", single.row=TRUE)

#' Comparison of nested model

anova(m1, m2)
