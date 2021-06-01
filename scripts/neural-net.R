source('setup.R')
set.seed(123)

data <- readr::read_csv('bike-sharing/day.csv') %>%
  dplyr::select(cnt, atemp, hum, windspeed)

max <- apply(data, 2, max)
min <- apply(data, 2, min)
data <- as_tibble(scale(data, center = min, scale = max - min)) 

test_idx <- sample(1:nrow(data), round(.2*nrow(data)))

train <- data[-test_idx, ]
test <- data[test_idx, ]

nn <- neuralnet::neuralnet(
  cnt ~ atemp + hum + windspeed,
  data = train,
  hidden = c(5, 4), 
  act.fct = "logistic"
)

lin <- lm(
  cnt ~ atemp + hum + windspeed,
  data=train
)

# this is saved as neural_network.pdf
plot(nn)

nn.rmse <- modelr::rmse(nn, test)
lin.rmse <- modelr::rmse(lin, test)
cat('nn rmse:', round(nn.rmse, 3), 'lm rmse:', round(lin.rmse, 3), '\n')

pred.nn <- predict(nn, test)
pred.lm <- predict(lin, test)
nn.test <- tibble(real = test$cnt, pred = pred.nn, model = rep('Neural Network', nrow(test)))
lm.test <- tibble(real = test$cnt, pred = pred.lm, model = rep('Linear Regression', nrow(test)))

bind_rows(nn.test, lm.test) %>%
  ggplot(aes(x = real, y = pred)) +
  geom_abline(size = 1) +
  geom_point(aes(color = model)) +
  facet_wrap(~model) +
  labs(x='Actual values', y='Predicted values') +
  theme(legend.position = 'none')
ggsave('output/neural_network_compare.pdf', width=8, height=4)
