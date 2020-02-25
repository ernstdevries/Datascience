
## installing libraries
## kan ook via menu
install.packages("mice")
library(mice)

search()

3+4

little_sequence <- 1:10

little_sequence <- 3*little_sequence

rnorm(10)

help(rnorm)

rnorm(n = 10, mean = 100, sd = 5)

y <- rnorm(100)


###########################################
###########################################

library(tibble) # package
library(ggplot2) # package

# create a table in R and name it "auto"
# Here I can put some comments

car <- tibble(
  speed = c(33.0, 33.0, 49.1, 65.2, 78.5, 93.0),
  distance = c(4.69, 4.05, 10.3, 22.3, 34.4, 43.5),
  country = c("USA","USA","GE","FR","SW","JP"))

# plot the data and draw a line 
ggplot(data = car, aes(x = speed, y = distance)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

model <- lm(formula = distance ~ speed , data = car)
model

lm(distance ~ speed , data = car)
summary(model)
