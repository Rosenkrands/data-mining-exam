source('setup.R')

x <- seq(0, 2*pi, .01)

plot_data <- tibble(
  x = x,
  sin = sin(x),
  cos = cos(x)
)

plot_data %>%
  pivot_longer(cols = c('sin', 'cos')) %>%
  ggplot(aes(x = x, y = value, color = name)) +
    geom_line()

ggsave('output/test_plot.pdf', width = 10, height = 4)
