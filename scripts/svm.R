source('setup.R')
set.seed(123)
library(e1071)

n_obs <- 50 * 2
obs <- seq_len(n_obs)
means <- list(c(10,5), c(5,10))
x1 <- c()
x2 <- c()
group <- c()
for (i in seq_along(means)) {
  x1 <- c(x1, rnorm(n_obs / 2, mean = means[[i]][1], sd = 1.5))
  x2 <- c(x2, rnorm(n_obs / 2, mean = means[[i]][2], sd = 1.5))
  group <- c(group,rep(i,n_obs / 2))
}
data <- tibble(obs=obs, x1=x1, x2=x2, Group=factor(group)) %>%
  mutate(Group = factor(ifelse(Group==1, -1, 1)))

ggplot(data, aes(x=x1, y=x2, shape=Group, color=Group)) +
  geom_point()
ggsave('output/osh_data.pdf', width = 8, height = 4)

svm.fit <- svm(
  factor(group) ~ x1 + x2, 
  data=data, 
  kernel='linear',
  cost=10
)
plot(svm.fit, data, x2~x1, fill=TRUE, grid = 100, symbolPalette = c('#F8766D', '#619CFF'), col=c("white","gray"))

n_obs <- 50 * 2
obs <- seq_len(n_obs)
means <- list(c(10,5), c(5,10))
x1 <- c()
x2 <- c()
group <- c()
for (i in seq_along(means)) {
  x1 <- c(x1, rnorm(n_obs / 2, mean = means[[i]][1], sd = 2.5))
  x2 <- c(x2, rnorm(n_obs / 2, mean = means[[i]][2], sd = 2.5))
  group <- c(group,rep(i,n_obs / 2))
}
data <- tibble(obs=obs, x1=x1, x2=x2, Group=factor(group)) %>%
  mutate(Group = factor(ifelse(Group==1, -1, 1)))

ggplot(data, aes(x=x1, y=x2, shape=Group, color=Group)) +
  geom_point()
ggsave('output/osh_data_overlap.pdf', width = 8, height = 4)

svm.fit <- svm(
  factor(group) ~ x1 + x2, 
  data=data, 
  kernel='linear',
  cost=.1
)
plot(svm.fit, data, x2~x1, fill=TRUE, grid = 100, symbolPalette = c('#F8766D', '#619CFF'), col=c("white","gray"))
summary(svm.fit)

svm.fit.polynomial <- svm(
  factor(group) ~ x1 + x2, 
  data=data, 
  kernel='polynomial'
)
plot(svm.fit.polynomial, data, x2~x1, fill=TRUE, grid = 100, symbolPalette = c('#F8766D', '#619CFF'), col=c("white","gray"))

svm.fit.radial <- svm(
  factor(group) ~ x1 + x2, 
  data=data, 
  kernel='radial'
)
plot(svm.fit.radial, data, x2~x1, fill=TRUE, grid = 100, symbolPalette = c('#F8766D', '#619CFF'), col=c("white","gray"))

svm.fit.sigmoid <- svm(
  factor(group) ~ x1 + x2, 
  data=data, 
  kernel='sigmoid'
)
plot(svm.fit.sigmoid, data, x2~x1, fill=TRUE, grid = 100, symbolPalette = c('#F8766D', '#619CFF'), col=c("white","gray"))
