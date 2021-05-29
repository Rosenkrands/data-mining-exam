source('setup.R')
library(MASS)

auto_data = ISLR::Auto
auto_data$mpg01 = factor(ifelse(auto_data$mpg > median(auto_data$mpg), 1, 0))
auto_data = dplyr::select(auto_data, c(displacement, weight, year, horsepower, mpg01))

train = sample(1:nrow(auto_data), round(.5*nrow(auto_data)), replace = FALSE)
train_data = auto_data[train,]
test_data = auto_data[-train,]

lda_auto = lda(mpg01 ~ ., data=train_data)
lda_pred = predict(lda_auto, test_data)
caret::confusionMatrix(test_data$mpg01, lda_pred$class)

qda_auto = qda(mpg01 ~ ., data=train_data)
qda_pred = predict(qda_auto, test_data)
caret::confusionMatrix(test_data$mpg01, qda_pred$class)

logit_auto = glm(mpg01 ~ ., family = binomial(link='logit'), data=train_data)
logit_pred = predict(logit_auto, test_data, type='response')
caret::confusionMatrix(test_data$mpg01, factor(ifelse(logit_pred > .5, 1, 0)))

roc_plot_data = function(posterior) {
  alpha = seq(0,1,.01)
  rates = list()
  for (i in alpha) {
    predictions = ifelse(posterior > i, 1, 0)
    confusion = caret::confusionMatrix(
      factor(predictions, levels=c(0,1)), 
      factor(test_data$mpg01, levels=c(0,1))
    )
    rates[[paste0(i)]] = tibble(alpha = i,
                                fpr = 1 - confusion$byClass['Specificity'],
                                tpr = confusion$byClass['Sensitivity'])
  }
  rates = do.call(rbind, rates)
  return(rates)
}

lda_roc <- roc_plot_data(lda_pred$posterior[,2])
lda_roc$Model = 'LDA'

qda_roc <- roc_plot_data(qda_pred$posterior[,2])
qda_roc$Model = 'QDA'

logit_roc <- roc_plot_data(logit_pred)
logit_roc$Model = 'logit'

plot_data = rbind(lda_roc, qda_roc, logit_roc)

ggplot(plot_data, aes(x=fpr, y=tpr, color=Model)) +
  geom_line() +
  geom_abline(linetype = 'dashed') +
  labs(x='False Positive Rate', y='True Positive Rate')
ggsave('output/mpg_roc.pdf', width=6, height=4)
