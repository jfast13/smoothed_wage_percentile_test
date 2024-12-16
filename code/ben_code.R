install.packages('epidatatools', repos = c('https://economic.r-universe.dev', 'https://cloud.r-project.org'))
library(epidatatools)
## Calculating 10th and 50th percentile from 1973-2023 

p = c(0.10, 0.50, 0.90)

ben_method <- wage_master %>%  reframe(
  p,
  averaged_value = averaged_quantile(wage, w = orgwgt, probs = p),
  interpolated_value = interpolated_quantile(wage, w = orgwgt, bin_size = 0.25, probs = p),
  classical_value = MetricsWeighted::weighted_quantile(wage, w = orgwgt, probs = p)
)

