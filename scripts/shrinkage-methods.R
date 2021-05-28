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

tibble(as.data.frame(t(as.matrix(coef(ridge.fit)))[,-1])) %>%
  mutate(lambda = lambda_seq) %>%
  pivot_longer(cols = !lambda) %>%
  ggplot(aes(x = lambda, y = value, linetype = name, color=name)) +
    geom_line() +
    scale_x_log10() +
    labs(y="Standardized coefficients")
ggsave('output/ridge_coefficient.pdf', width=8,height=4)
