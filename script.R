setwd("/home/anderson/Documents/Kangle/titanicR")

library(tidyverse) 
library(rpart) 
library(randomForest)
library(modelr)

titanic.train <- read_csv(file = "train.csv")
titanic.test <- read_csv(file = "test.csv")

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA

titanic.full <- rbind(titanic.train, titanic.test)

titanic.full$Embarked[which(is.na(titanic.full$Embarked) | titanic.full$Embarked=="")] <- 'S'

age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

#CROSS VALIDATION whith problems (factor values)
#splitData <- resample_partition(titanic.train, c(test = 0.3, train = 0.7))
#lapply(splitData, dim)

#titanic.model2 <- randomForest(formula = survived.formula, data = splitData$train)
#mae(model = titanic.model2, data = splitData$test)

Survived <- predict(titanic.model, newdata = titanic.test)
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df, file = "subimission.csv", row.names = FALSE)
