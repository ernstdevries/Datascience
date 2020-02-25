library(foreign)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071)
library(readr)

chol1 = read.spss("voorbeeld7_1.sav", to.data.frame=TRUE)
ggplot(chol1, aes(x = chol, y = leeftijd)) +
  geom_point() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1)
fit1 <- lm(formula = chol ~ leeftijd, data = chol1)
summary(fit1)
fit2 <- lm(formula = chol ~ leeftijd + bmi + sekse +alcohol, data = chol1)
summary(fit2)
chol1$residuals <- residuals(fit2)
ggplot(chol1, aes(x = residuals)) +
  geom_histogram()

births <-read_delim(file="births.csv", delim=",", col_names = TRUE, col_types = NULL)
head(births)
births <- births %>%
  mutate(home = factor(if_else(child_birth %in% "first line child birth, at home", "home", "not_home"), levels = c("home","not_home"))) %>%
  mutate(pari = if_else(parity > 1, "multi", "primi")) %>%
  mutate(etni = if_else(etnicity %in% "Dutch", "Dutch", "Non_Dutch"))

set.seed(100)
mydat <- births
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(mydat$home, p=0.8, list=FALSE)
# Step 2: Create the training dataset
trainData <- mydat[trainRowNumbers,c(2,4,8,9,10)]
# Step 3: Create the test dataset
testData <- mydat[-trainRowNumbers,c(2,4,8,9,10)]

fit_logreg <- train(home ~ pari + age_cat + etni + urban, data = trainData,
                    method="glm", family="binomial")
summary(fit_logreg)
predicted <- predict(fit_logreg, testData)
confusionMatrix(reference = testData$home, data = predicted)


# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(mydat$home, p=0.8, list=FALSE)
# Step 2: Create the training dataset
trainData <- mydat[trainRowNumbers,c(2,4,8,9,10)]
# Step 3: Create the test dataset
testData <- mydat[-trainRowNumbers,c(2,4,8,9,10)]
fit_rpart <- train(home ~ pari + age_cat + etni + urban, data = trainData,
                   method="rpart")
plot(fit_rpart)
predicted2 <- predict(fit_rpart, testData)
confusionMatrix(reference = testData$home, data = predicted2)