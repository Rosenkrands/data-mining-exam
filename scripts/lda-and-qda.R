source('setup.R')
library(MASS)

fig_width = 6
fig_height = 4

lda_iris = lda(Species ~ Sepal.Length + Sepal.Width, data=iris)
lda_iris.fitted = predict(lda_iris, iris)

# caret::confusionMatrix(lda_iris.fitted$class, iris$Species)$table

qda_iris = qda(Species ~ Sepal.Length + Sepal.Width, data=iris)
qda_iris.fitted = predict(qda_iris, iris)

# caret::confusionMatrix(qda_iris.fitted$class, iris$Species)$table

x_start = min(iris$Sepal.Length)
x_end = max(iris$Sepal.Length)

y_start = min(iris$Sepal.Width)
y_end = max(iris$Sepal.Width)

x = seq(x_start,x_end,.05)
y = seq(y_start,y_end,.05)

grid = expand.grid(Sepal.Length = x, Sepal.Width = y)
grid.predictions = predict(lda_iris, grid)
grid$class = grid.predictions$class

ggplot(NULL) +
  geom_point(data=grid, aes(x=Sepal.Length, y=Sepal.Width, color=class), shape=3) +
  geom_point(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  # ggtitle('LDA classification regions') +
  theme(legend.position = "none")
ggsave('output/lda_regions.pdf', width=fig_width, height=fig_height)

grid = expand.grid(Sepal.Length = x, Sepal.Width = y)
grid.predictions = predict(qda_iris, grid)
grid$class = grid.predictions$class

ggplot(NULL) +
  geom_point(data=grid, aes(x=Sepal.Length, y=Sepal.Width, color=class), shape=3) +
  geom_point(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  # ggtitle('QDA classification regions') +
  theme(legend.position = "none")
ggsave('output/qda_regions.pdf', width=fig_width, height=fig_height)
