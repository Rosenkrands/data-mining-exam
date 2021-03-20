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

calculate_cents <- function(data, K) {
  cent_list <- list()
  for (i in 1:K) {
    cent_list[[i]] <- data %>%
      filter(cluster == i) %>%
      select(cluster, x1, x2) %>%
      mutate(x1 = mean(x1),
             x2 = mean(x2)) %>%
      filter(row_number() == 1)
  }
  cents <- tibble(cent_list) %>%
    unnest(cols = cent_list)
  
  return(cents)
}

dist_fun <- function(coord1, coord2) {
  sqrt(sum((coord1 - coord2)^2))
}

update_cluster <- function(data, cents) {
  data <- data %>%
    select(-cluster) %>%
    full_join(cents, by = character(), suffix = c("", ".cent")) %>%
    rowwise() %>%
    mutate(dist = dist_fun(c(x1,x2), c(x1.cent,x2.cent))) %>%
    ungroup() %>%
    group_by(obs) %>%
    filter(dist == min(dist)) %>%
    select(-x1.cent, -x2.cent, -dist) %>%
    ungroup()
  
  return(data)
}

plot_w_cents <- function(data, cents, size, cent_size = 10) {
  ggplot(data, aes(x=x1,y=x2,color=cluster)) +
    geom_point(size = size) +
    geom_point(data = cents, aes(x=x1,y=x2,color=cluster), shape = 13, size = cent_size) +
    theme(legend.position = 'none')
}

my_kmeans <- function(data, K, animate = F, size = 3) {
  initial_plot <- ggplot(data, aes(x = x1, y = x2)) + geom_point(size = size)
  if (animate == T) print(initial_plot)
  
  data$cluster <- as.factor(sample(c(1:K), size = nrow(data), replace = T))
  
  initial_clusters <- ggplot(data, aes(x=x1,y=x2,color=cluster)) + 
    geom_point(size = size) +
    theme(legend.position = 'none')
  if (animate == T) print(initial_clusters)
  
  initial_cents <- calculate_cents(data, K)
  
  data_list <- list()
  data_list[[1]] <- data
  
  if (animate == T) print(plot_w_cents(data_list[[1]], initial_cents, size = size))
  
  data_list[[2]] <- update_cluster(data, initial_cents)
  
  if (animate == T) print(plot_w_cents(data_list[[2]], initial_cents, size = size))
  
  i = 1
  
  while (!identical(data_list[[i]]$cluster, data_list[[i + 1]]$cluster)) {
    cents <- calculate_cents(data_list[[i + 1]], K)
    if (animate == T) print(plot_w_cents(data_list[[i + 1]], cents, size = size))
    
    data_list[[i + 2]] <- update_cluster(data_list[[i + 1]], cents)
    if (animate == T) print(plot_w_cents(data_list[[i + 2]], cents, size = size))
    i = i + 1
  }
  
  cents <- calculate_cents(data_list[[i + 1]], K)
  if (animate == T) print(plot_w_cents(data_list[[i + 1]], cents, size = size))
}

animation::saveGIF(
  my_kmeans(data, 3, animate = T),
  interval = 0.2,
  outdir = getwd(),
  ani.width = 480,
  ani.height = 480
)
