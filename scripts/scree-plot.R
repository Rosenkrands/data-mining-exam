source('setup.R')
set.seed(2)

n_obs <- 50 * 3
obs <- seq_len(n_obs)
means <- list(c(1,1), c(10,5), c(5,10))
x1 <- c()
x2 <- c()
for (i in seq_along(means)) {
  x1 <- c(x1, rnorm(n_obs / 3, mean = means[[i]][1], sd = 1.5))
  x2 <- c(x2, rnorm(n_obs / 3, mean = means[[i]][2], sd = 1.5))
}
data <- tibble(obs=obs, x1=x1, x2=x2)

withinss <- function(centers){
  k.out = kmeans(data%>%select(-obs), centers, nstart=10)
  return(1-k.out$tot.withinss/k.out$totss)
}

centers.list = list(); for (i in 1:10) {centers.list[[i]] <- i}
scree_data <- lapply(centers.list, withinss)

tibble(scree_data) %>%
  unnest(cols = 'scree_data') %>%
  mutate(centers=row_number()) %>%
  ggplot(aes(x=centers, y=scree_data)) +
    geom_line(color='gray', linetype='dashed') +
    geom_point(size=3, shape=1) +
    scale_x_continuous(breaks = seq(1,10)) +
    labs(x='Number of centers',
         y='PVE')

ggsave('output/scree_plot.pdf', width = 8, height = 4)
