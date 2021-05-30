source('setup.R')
library(tree)

tree.fit.1 <- tree(mpg ~ hp, data=mtcars)
plot(tree.fit.1)
text(tree.fit.1)

tree.fit.2 <- tree(mpg ~ hp + wt, data=mtcars)
plot(tree.fit.2)
text(tree.fit.2)
