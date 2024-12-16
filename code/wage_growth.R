## Wage growth chart ##

wage_growth <- data.frame()
for (percentile in seq(6, 94, by = 1)) {
  new_method <- wage_master %>%
    filter(year %in% c(2010, 2015)) %>%
    group_by(year) %>%
    summarize(
      p46 = weighted_quantile(adj_wage, finalwgt, probs = (percentile - 4) / 100),
      p47 = weighted_quantile(adj_wage, finalwgt, probs = (percentile - 3) / 100),
      p48 = weighted_quantile(adj_wage, finalwgt, probs = (percentile - 2) / 100),
      p49 = weighted_quantile(adj_wage, finalwgt, probs = (percentile - 1) / 100),
      p50 = weighted_quantile(adj_wage, finalwgt, probs = percentile / 100),
      p51 = weighted_quantile(adj_wage, finalwgt, probs = (percentile + 1) / 100),
      p52 = weighted_quantile(adj_wage, finalwgt, probs = (percentile + 2) / 100),
      p53 = weighted_quantile(adj_wage, finalwgt, probs = (percentile + 3) / 100),
      p54 = weighted_quantile(adj_wage, finalwgt, probs = (percentile + 4) / 100)
    ) %>%
    rowwise() %>%
    mutate(
      weighted_avg_p = sum(
        c(p46, p47, p48, p49, p50, p51, p52, p53, p54) * 
          c(1/25, 2/25, 3/25, 4/25, 5/25, 4/25, 3/25, 2/25, 1/25), 
        na.rm = TRUE)
    ) %>%
    select(year, p50, weighted_avg_p)
  
  current_method <- wage_master %>%
    binipolate( var = wage, p = percentile, .by = "year", 0.25, w = finalwgt) %>% 
    left_join(cpi, by = "year") %>%
    mutate(adjwage = wage * (cpi_base/cpiurs)) %>% 
    filter(year %in% c(2010, 2015)) %>% 
    select(year,adjwage)
  
  methods <- new_method %>% 
    left_join(current_method, by= "year")
  
  # Append results for each percentile to the results dataframe
  wage_growth <- bind_rows(wage_growth, methods)
}


wage_growth <- wage_growth %>%
  mutate(
    standard = if_else(
      row_number() %% 2 == 0 & row_number() <= 178,
      (p50 / lag(p50)) - 1,
      NA_real_
    ),
    weighted_average = if_else(
      row_number() %% 2 == 0 & row_number() <= 178,
      (weighted_avg_p / lag(weighted_avg_p)) - 1,
      NA_real_
    ),
    binipolate = if_else(
      row_number() %% 2 == 0 & row_number() <= 178,
      (adjwage/lag(adjwage)) - 1,
      NA_real_
    )
  )

wage_growth <- wage_growth %>%
  filter(year == 2015) %>%          
  rename(percentile = year) %>%        
  select(percentile, standard, weighted_average, binipolate) %>%  
  mutate(percentile = row_number() + 5) %>% 
  mutate(
    weighted_average = if_else(percentile >= 10 & percentile <= 90, weighted_average, NA_real_),
    binipolate = if_else(percentile >= 10 & percentile <= 90, binipolate, NA_real_)
  )


long_wage_growth <- wage_growth %>%
  pivot_longer(
    cols = c(standard, weighted_average, binipolate), 
    names_to = "series", 
    values_to = "values"
  )

wage_growth_plot <- ggplot(long_wage_growth, aes(x = percentile, y = values, color = series, group = series)) +
  geom_line(size = 1.3, alpha =0.7) +                     # Increase line size
  geom_point(size = 3, alpha =0.7 ) + 
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +  # Distinct colors
  scale_shape_manual(values = c(16, 17, 18)) + # Increase point size
  labs(
    title = "Real Wage Growth 2010-2015 by Percentile Smoothing Method",
    x = "Percentile",
    y = "2019-2023 Real Wage Growth (2023$)",
    color = "Methods:"
  ) +                                         # Add labels
  theme_light(base_size = 14) +               # Use a light theme with larger base font size
  theme(
    plot.background = element_rect(fill = "white", color = NA),   # Light background
    panel.background = element_rect(fill = "white"),              # Light panel
    text = element_text(color = "black"),                         # Black text color
    plot.title = element_text(hjust = 0.5),                       # Center title
    legend.position = "top",                                      # Position legend at the top
    panel.grid.major = element_line(color = "grey80"), 
    panel.grid.minor = element_blank()                            # Remove minor gridlines
  ) +
  scale_y_continuous(breaks = seq(floor(-0.1), ceiling(0.16), by = 0.02), limits = c(-.1,0.16), labels = label_percent())+
  scale_x_continuous(breaks = seq(floor(5), ceiling(95), by = 5))+
  labs(y = NULL)# Use a color palette

# Save the plot
ggsave("output/wage_growth.png", plot = wage_growth_plot, width = 10, height = 6, dpi = 300)
