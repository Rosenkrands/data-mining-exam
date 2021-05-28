source('setup.R')

# Loaging the library
library(glmnet)

data = scale(mtcars)

# Getting the independent variable
x_var <- data.matrix(data[, c("hp", "wt", "drat", "cyl", "qsec", "gear")])

# Getting the dependent variable
y_var <- mtcars[, "mpg"]

# Setting the range of lambda values
lambda_seq <- 10^seq(4, -2, by = -.1)

# Using glmnet function to build the ridge regression in r
ridge.fit <- glmnet(x_var, y_var, alpha = 0, lambda  = lambda_seq)
ridge.data <- tibble(as.data.frame(t(as.matrix(coef(ridge.fit)))[,-1])) %>%
  mutate(lambda = lambda_seq) %>%
  pivot_longer(cols = !lambda)

ridge.data %>%
  ggplot(aes(x = lambda, y = value, linetype = name, color=name)) +
    geom_line() +
    scale_x_log10() +
    labs(y="Standardized coefficients", color = "Variable", linetype = "Variable")
ggsave('output/ridge_coefficient.pdf', width=8,height=4)

# Using glmnet function to build the lasso regression in r
lambda_seq <- 10^seq(4, -2, by = -.1)
lasso.fit <- glmnet(x_var, y_var, alpha = 1, lambda  = lambda_seq)
lasso.data <- tibble(as.data.frame(t(as.matrix(coef(lasso.fit)))[,-1])) %>%
  mutate(lambda = lambda_seq) %>%
  pivot_longer(cols = !lambda)

lasso.data %>%
  ggplot(aes(x = lambda, y = value, color=name, linetype = name)) +
  geom_line(data=ridge.data, aes(x=lambda,y=value,group=name),color='gray') +
  geom_line() +
  scale_x_log10() +
  labs(y="Standardized coefficients", color = "Variable", linetype = "Variable")
ggsave('output/lasso_coefficient.pdf', width=8,height=4)
