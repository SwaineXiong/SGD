# MA 676: SGD example, logistic regression

library(tidyverse)
library(readxl)
dev_logistic <- function (formula, data) {
  y <- model.response(model.frame(formula, data))
  x <- model.matrix(formula, data)
  fam <- binomial()
  function (beta) {
    eta <- drop(x %*% beta)
    sum(fam$dev.resids(y, fam$linkinv(eta), rep(1, length(y))))
  }
}


# [0] Load data from file `CustomerData.xlsx` into a tibble `customer`
df <- read_excel("CustomerData.xlsx") %>%
  as_tibble()
View(df)
# [1] Our main task here is to find customer features that explain ownership of
# a mobile device. Explore a bit, then create a new tibble with the following
# predictors:
#
# - `ownsmobile`: indicator if customer owns mobile phone
# - `female`: indicator if customer is female
# - `age`: years since 18 years of age
# - `education`: years of education
# - `income`: household income in 1000 of dollars
ownsmobile <- ifelse(df$OwnsMobileDevice == "Yes", 1, 0)
female <- ifelse(df$Gender == "Female", 1, 0)
age <- df$Age - 18
education <- df$EducationYears
income <- df$HHIncome / 1000
customer <- tibble(
  ownsmobile,
  female,
  age,
  education,
  income)

# [2] Fit a logistic regression and store your estimated coefficients in
# `beta`. Check that `dev_cust(beta)` matches the residual deviance of your
# fitted model.
dev_cust <- dev_logistic(ownsmobile ~ female + age + education + income,
                         data = customer)
model <- glm(ownsmobile ~ female + age + education + income, data = customer,family = "binomial")
beta <- coef(model)
null_model <- glm(ownsmobile ~ 1, data = customer, family = "binomial")
beta_null <- coef(null_model)
dev <- dev_cust(beta)
null_dev <- dev_cust(c(beta_null,0,0,0,0))
# [3] Suppose that you receive a new batch of data; how would you update your
# gradient descent
n <- 5
lr <- 1e-5
for (b in seq_len(B)){
  batch_index <- (b - 1) * n + seq_len(n)
  x_batch <- x[batch_index, ]
  eta <- drop(x_batch %*% beta)
  mu <-plogis(eta)
  grad <- drop(t(x_batch) %*% (y[batch_index] - mu))
  beta <- beta + lr * grad
  dev2 <- dev_cust(beta)
}


# [4] Now, work in batches, updating your coefficients as you get a new batch.
# The code below assigns the response `y` and design `x`, sets the batch size
# and the number of batches, and an initial random value for the coefficients.
form <- ownsmobile ~ female + age + education + income
y <- model.response(model.frame(form, data = customer))
x <- model.matrix(form, data = customer)

N <- nrow(x)
n <- 5 # batch size
B <- floor(N / n) # number of batches

# init
set.seed(676)
beta <- glm(ownsmobile ~ female + age + education + income,
            family = binomial, data = customer,
            subset = sample.int(N, 20)) |> coef()
dev_cust(beta)

lr <- 1e-5 # learning rate
for (b in seq_len(B)) {
  batch_index <- (b - 1) * n + seq_len(n) # batch rows
  # [ update here ]
}

