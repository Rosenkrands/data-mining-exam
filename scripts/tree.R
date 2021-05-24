source('setup.R')

library(parallel)

num_cores <- detectCores(logical = F)
cl <- makeCluster(num_cores)

data <- list()
for (i in 1:1000) {data[[i]] <- i}

# TODO: could be nice to repeat for many different seeds and take an average.
compare_binary <- function(i) {
  print(i)
  library(tree)
  library(dplyr)
  library(tidyr)
  
  x1 <- rnorm(500)
  x2 <- rnorm(500)
  eps <- rnorm(500)/2
  
  y1 <- 1 + 2*x1 + 3*x2 + eps
  
  data1 <- data.frame(y1, x1, x2)
  
  lm_model <- lm(y1 ~ ., data = data1)
  lm_pred <- predict(lm_model, data = data1)
  
  tree_model <- tree::tree(y1 ~ ., data1)
  tree_pred <- predict(tree_model, data1)
  
  data1$lm <- lm_pred
  data1$tree <- tree_pred
  
  rslt1 <- data1 %>%
    dplyr::select(-c(x1, x2)) %>%
    pivot_longer(cols = c(lm, tree)) %>%
    group_by(name) %>%
    summarise(rmse = sqrt(mean((y1 - value)^2))) %>%
    mutate(data = 'non-binary')
  
  x11 <- as.numeric(x1 > 0)
  x22 <- as.numeric(x2 > 0)
  y11 <- 1 + 2*x11 + 3*x22 + eps
  
  data2 <- tibble(y11, x11, x22)
  
  lm_model <- lm(y11 ~ ., data = data2)
  lm_pred <- predict(lm_model, newdata = data2)
  
  tree_model <- tree(y11 ~ ., data = data2)
  tree_pred <- predict(tree_model, newdata = data2)
  
  data2$lm <- lm_pred
  data2$tree <- tree_pred
  
  rslt2 <- data2 %>%
    dplyr::select(-x11, -x22) %>%
    tidyr::pivot_longer(cols = c(lm, tree)) %>%
    group_by(name) %>%
    summarise(rmse = sqrt(mean((y11 - value)^2))) %>%
    mutate(data = 'binary')
  
  return(rbind(rslt1, rslt2))
}


rslt <- parLapply(cl, data, compare_binary)

stopCluster(cl)

tibble(rslt) %>%
  unnest(cols = c(rslt)) %>%
  ggplot(aes(x = name, y = rmse, fill = name)) +
  geom_violin() +
  facet_wrap(~data, scales = 'free_y') +
  labs(x = '') +
  theme(legend.position = 'none')

ggsave('output/lm_tree_compare.pdf', width = 8, height = 4)
