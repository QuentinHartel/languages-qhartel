#Quentin Hartel
#Final project

library(dplyr)
library(moments)
library(car)

setwd("C:/Users/quent/OneDrive/Desktop/Correlation and Regression")
getwd()
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

########################################################################################################
#Histograms for dependent Variables
par(mfrow=c(2,2))
hist(peloton$QOLPhyMean)
hist(peloton$QOLPsyMean)
hist(peloton$QOLEnvMean)
hist(peloton$QOLSocMean)

par(mfrow=c(2,2))
hist(peloton$PSETotal)
hist(peloton$SnGTotal)
hist(peloton$SDHSTotal)

########################################################################################################
#Summary statistics for dependent variables
pelotonDVs <- select(peloton, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal)
sumStatsDV <- data.frame(row.names = c("sampleSize", "mean", "median", "stdDev", "min", "max", "Q1", "Q3", "skewness"))

for(column in colnames(pelotonDVs))
{
  stats = c(length(pelotonDVs[[column]]), mean(pelotonDVs[[column]]), median(pelotonDVs[[column]]), 
            sd(pelotonDVs[[column]]), min(pelotonDVs[[column]]), max(pelotonDVs[[column]]), 
            quantile(pelotonDVs[[column]], 0.25), quantile(pelotonDVs[[column]], 0.75), 
            skewness(pelotonDVs[[column]]))
  sumStatsDV[[column]] = stats
}
sumStatsDV

########################################################################################################
#Histograms for Income and Education level
par(mfrow=c(2,2))
hist(peloton$Income_RECODE, xlab="Income", ylab="Count", main="Income Histogram")
hist(peloton$LevEdu_RECODE, xlab="Education Level", ylab="Count", main="Education Level Histogram")

########################################################################################################
#Histograms for PSTotal and AvgWorkoutsPerMonth
par(mfrow=c(2,2))
hist(peloton$PSTotal)
hist(peloton$AvgWorkoutsPerMonth)

########################################################################################################
#Summary statistics for independent variables
pelotonIVs <- select(peloton, PSTotal, AvgWorkoutsPerMonth)
sumStatsIV <- data.frame(row.names = c("sampleSize", "mean", "median", "stdDev", "min", "max", "Q1", "Q3", "skewness"))

for(column in colnames(pelotonIVs))
{
  stats = c(length(pelotonIVs[[column]]), mean(pelotonIVs[[column]]), median(pelotonIVs[[column]]), 
            sd(pelotonIVs[[column]]), min(pelotonIVs[[column]]), max(pelotonIVs[[column]]), 
            quantile(pelotonIVs[[column]], 0.25), quantile(pelotonIVs[[column]], 0.75), 
            skewness(pelotonIVs[[column]]))
  sumStatsIV[[column]] = stats
}
sumStatsIV

########################################################################################################
#Models
########################################################################################################
#PSETotal Model
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

model <- lm(data=peloton, PSETotal ~ Income_RECODE+LevEdu_RECODE+Marital_Status+PSTotal+AvgWorkoutsPerMonth)
summary(model)

#lets take out the non significant variables
model <- lm(data=peloton, PSETotal ~ PSTotal)
summary(model)

#Remove High leverage points 
high_cooks_points <- which(cooks.distance(model) > 0.04)
peloton_clean_data <- peloton[-high_cooks_points, ]
model <- lm(data=peloton_clean_data, PSETotal ~ PSTotal)
summary(model)

inverseResponsePlot(model, main="Inverse Response Plot", xlab="Predicted Values", ylab="Transformed Response", col="blue")

par(mfrow=c(2,2))
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=5)
par(mfrow=c(1, 1))
plot(model, which = 4)

########################################################################################################
#SnGTotal Model
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

model <- lm(data=peloton, SnGTotal ~ Income_RECODE+LevEdu_RECODE+Marital_Status+PSTotal+AvgWorkoutsPerMonth)
summary(model)

#lets take out the non significant variables
model <- lm(data=peloton, SnGTotal ~ PSTotal)
summary(model)

inverseResponsePlot(model, main="Inverse Response Plot", xlab="Predicted Values", ylab="Transformed Response", col="blue")

#Model performance plots
par(mfrow=c(2,2))
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=5)
par(mfrow=c(1, 1))
plot(model, which = 4)


########################################################################################################
#SDHSTotal Model
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

model <- lm(data=peloton, SDHSTotal ~ Income_RECODE+LevEdu_RECODE+Marital_Status+PSTotal+AvgWorkoutsPerMonth)
summary(model)

#lets take out the non significant variables
model <- lm(data=peloton, SDHSTotal ~ PSTotal+as.factor(Income_RECODE))
summary(model)

high_cooks_points <- which(cooks.distance(model) > 0.2)
peloton_clean_data <- peloton[-high_cooks_points, ]
model <- lm(data=peloton_clean_data, SDHSTotal ~ PSTotal+as.factor(Income_RECODE))
summary(model)

high_leverage_points <- which(hatvalues(model) > 0.5)
peloton_clean_data <- peloton_clean_data[-high_leverage_points, ]
model <- lm(data=peloton_clean_data, SDHSTotal ~ PSTotal+as.factor(Income_RECODE))
summary(model)

inverseResponsePlot(model, main="Inverse Response Plot", xlab="Predicted Values", ylab="Transformed Response", col="blue")

#Model performance plots
par(mfrow=c(2,2))
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=5)
par(mfrow=c(1, 1))
plot(model, which = 4)


########################################################################################################
#QOLPhyMean Model
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

model <- lm(data=peloton, QOLPhyMean ~ Income_RECODE+LevEdu_RECODE+Marital_Status+PSTotal+AvgWorkoutsPerMonth)
summary(model)

#lets take out the non significant variables
model <- lm(data=peloton, QOLPhyMean ~ PSTotal+as.factor(Income_RECODE))
summary(model)

#Remove super high cook's distance points
remove <- which(cooks.distance(model) > 0.15)
peloton_clean_data <- peloton[-remove,]
model <- lm(data=peloton_clean_data, QOLPhyMean ~ PSTotal+as.factor(Income_RECODE))
summary(model)

#remove super high leverage points
remove <- which(hatvalues(model) > 0.5)
peloton_clean_data = peloton_clean_data[-remove, ]
model <- lm(data=peloton_clean_data, QOLPhyMean ~ PSTotal+as.factor(Income_RECODE))
summary(model)

#remove IV b/c the outlier points it explained got removed
model <- lm(data=peloton_clean_data, QOLPhyMean ~ PSTotal)
summary(model)

inverseResponsePlot(model, main="Inverse Response Plot", xlab="Predicted Values", ylab="Transformed Response", col="blue")

par(mfrow=c(2,2))
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=5)
par(mfrow=c(1, 1))
plot(model, which = 4)


########################################################################################################
#QOLPsyMean Model
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

model <- lm(data=peloton, QOLPsyMean ~ Income_RECODE+LevEdu_RECODE+Marital_Status+PSTotal+AvgWorkoutsPerMonth)
summary(model)

#lets take out the non significant variables
model <- lm(data=peloton, QOLPsyMean ~ PSTotal+as.factor(Income_RECODE)+as.factor(LevEdu_RECODE))
summary(model)

remove <- which(cooks.distance(model) > 0.2)
peloton_clean_data <- peloton[-remove,]
model <- lm(data=peloton_clean_data, QOLPsyMean ~ PSTotal+as.factor(Income_RECODE)+as.factor(LevEdu_RECODE))
summary(model)

remove <- which(hatvalues(model) > 0.5)
peloton_clean_data <- peloton_clean_data[-remove,]
model <- lm(data=peloton_clean_data, QOLPsyMean ~ PSTotal+as.factor(Income_RECODE)+as.factor(LevEdu_RECODE))
summary(model)

#Income no longer helps
model <- lm(data=peloton_clean_data, QOLPsyMean ~ PSTotal+as.factor(LevEdu_RECODE))
summary(model)

inverseResponsePlot(model, main="Inverse Response Plot", xlab="Predicted Values", ylab="Transformed Response", col="blue")
peloton_clean_data$QOLPsyMeanTransform <- ((bcPower(peloton_clean_data$QOLPsyMean, 3)-1)/3)
model <- lm(data=peloton_clean_data, QOLPsyMeanTransform ~ PSTotal+as.factor(LevEdu_RECODE))
summary(model)

par(mfrow=c(2,2))
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=5)
par(mfrow=c(1, 1))
plot(model, which = 4)


########################################################################################################
#QOLEnvMean Model
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

model <- lm(data=peloton, QOLEnvMean ~ Income_RECODE+LevEdu_RECODE+Marital_Status+PSTotal+AvgWorkoutsPerMonth)
summary(model)

#lets take out the non significant variables
model <- lm(data=peloton, QOLEnvMean ~ PSTotal+as.factor(Income_RECODE)+as.factor(Marital_Status))
summary(model)

remove <- which(cooks.distance(model) > 0.1)
peloton_clean_data <- peloton[-remove,]
model <- lm(data=peloton_clean_data, QOLEnvMean ~ PSTotal+as.factor(Income_RECODE)+as.factor(Marital_Status))
summary(model)

remove <- which(cooks.distance(model) > 0.1)
peloton_clean_data <- peloton[-remove,]
model <- lm(data=peloton_clean_data, QOLEnvMean ~ PSTotal+as.factor(Income_RECODE)+as.factor(Marital_Status))
summary(model)

peloton_clean_data <- peloton_clean_data[-55,]
model <- lm(data=peloton_clean_data, QOLEnvMean ~ PSTotal+as.factor(Income_RECODE)+as.factor(Marital_Status))
summary(model)

inverseResponsePlot(model, main="Inverse Response Plot", xlab="Predicted Values", ylab="Transformed Response", col="blue")

peloton_clean_data$QOLEnvMeanTransform <- (bcPower(peloton_clean_data$QOLEnvMean, 5.5)-1/5.5)
model <- lm(data=peloton_clean_data, QOLEnvMeanTransform ~ PSTotal+as.factor(Income_RECODE)+as.factor(Marital_Status))
summary(model)

par(mfrow=c(2,2))
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=5)
par(mfrow=c(1, 1))
plot(model, which = 4)



########################################################################################################
#QOLSocMean Model
data <- read.csv("PelotonData.csv")
peloton <- select(data, QOLPhyMean, QOLPsyMean, QOLEnvMean, QOLSocMean, PSETotal, SnGTotal, SDHSTotal,
                  Income_RECODE, LevEdu_RECODE, Marital_Status, PSTotal, AvgWorkoutsPerMonth)
peloton <- na.omit(peloton)

model <- lm(data=peloton, QOLSocMean ~ Income_RECODE+LevEdu_RECODE+Marital_Status+PSTotal+AvgWorkoutsPerMonth)
summary(model)

#lets take out the non significant variables
model <- lm(data=peloton, QOLSocMean ~ PSTotal+as.factor(Income_RECODE))
summary(model)

remove <- which(cooks.distance(model) > 0.1)
peloton_clean_data <- peloton[-remove,]
model <- lm(data=peloton_clean_data, QOLSocMean ~ PSTotal+as.factor(Income_RECODE))
summary(model)

remove <- which(hatvalues(model) > 0.5)
peloton_clean_data <- peloton_clean_data[-remove,]
model <- lm(data=peloton_clean_data, QOLSocMean ~ PSTotal+as.factor(Income_RECODE))
summary(model)

inverseResponsePlot(model, main="Inverse Response Plot", xlab="Predicted Values", ylab="Transformed Response", col="blue")

model <- lm(data=peloton_clean_data, QOLSocMean ~ PSTotal)
summary(model)

par(mfrow=c(2,2))
plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=5)
par(mfrow=c(1, 1))
plot(model, which = 4)





