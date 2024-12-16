
## Visualize ## 
long_data_3 <- combined %>%
  pivot_longer(cols = c(binipolate, avg_p, weighted_avg_p), 
               names_to = "series", 
               values_to = "value")

p_3 <- ggplot(long_data_3, aes(x = year, y = value, color = series, group = series)) +
  geom_line(size = 1.5) +                    # Increase line size
  geom_point(size = 3) +                     # Increase point size
  labs(title = "Real 90th Percentile Non Imputed Wage by Smoothing Method",
       x = "Year",
       y = "90th percentile Wage (2023$)",
       color = "Methods") +                   # Add labels
  theme_light(base_size = 14) +              # Use a light theme with larger base font size
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Light background
    panel.background = element_rect(fill = "white"),              # Light panel
    text = element_text(color = "black"),  # Change text color to black for visibility
    plot.title = element_text(hjust = 0.5), # Center title
    legend.position = "top",                 # Position legend at the top
    panel.grid.major = element_line(color = "grey80"), 
    panel.grid.minor = element_blank()       # Remove minor gridlines
  ) +
  scale_color_brewer(palette = "Set1")      # Use a color palette
p_3 <- ggsave("output/90th_wage_plot_NI.png", plot = p_3, width = 10, height = 6, dpi = 300)


long_data_2 <- combined %>%
  pivot_longer(cols = c(avg_p, weighted_avg_p), 
               names_to = "series", 
               values_to = "value")

p_2 <- ggplot(long_data_2, aes(x = year, y = value, color = series, group = series)) +
  geom_line(size = 1.5) +                    # Increase line size
  geom_point(size = 3) +                     # Increase point size
  labs(title = "Real 90th Percentile Non Imputed Wage by Smoothing Method",
       x = "Year",
       y = "90th percentile Wage (2023$)",
       color = "Methods") +                   # Add labels
  theme_light(base_size = 14) +              # Use a light theme with larger base font size
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Light background
    panel.background = element_rect(fill = "white"),              # Light panel
    text = element_text(color = "black"),  # Change text color to black for visibility
    plot.title = element_text(hjust = 0.5), # Center title
    legend.position = "top",                 # Position legend at the top
    panel.grid.major = element_line(color = "grey80"), 
    panel.grid.minor = element_blank()       # Remove minor gridlines
  ) +
  scale_color_brewer(palette = "Set1")      # Use a color palette

ggsave("output/nobin_90th_wage_plot_NI.png", plot = p_2, width = 10, height = 6, dpi = 300)

## COMPARING NON IMPUTED VS IMPUTED
long_data_bini_NI <- super_combined_NI %>%
  pivot_longer(cols = c(binipolate,binipolate_NI), 
               names_to = "series", 
               values_to = "value")

p_binipolate <- ggplot(long_data_bini_NI, aes(x = year, y = value, color = series, group = series)) +
  geom_line(size = 1.5) +                    # Increase line size
  geom_point(size = 3) + 
  labs(title = "Real 90th Percentile by Non-Imputed or Imputed",
       x = "Year",
       y = "90th percentile Wage (2023$)",
       color = "Methods") +                   # Add labels
  theme_light(base_size = 14) +              # Use a light theme with larger base font size
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Light background
    panel.background = element_rect(fill = "white"),              # Light panel
    text = element_text(color = "black"),  # Change text color to black for visibility
    plot.title = element_text(hjust = 0.5), # Center title
    legend.position = "top",                 # Position legend at the top
    panel.grid.major = element_line(color = "grey80"), 
    panel.grid.minor = element_blank()       # Remove minor gridlines
  ) +
  scale_color_brewer(palette = "Set1") + 
  scale_y_continuous(breaks = seq(floor(38), ceiling(66), by = 2), limits = c(38,66))+
  scale_x_continuous(breaks = seq(floor(1970), ceiling(2025), by = 5))      # Use a color palette

ggsave("output/90th_binipolate_NI.png", plot = p_binipolate, width = 10, height = 6, dpi = 300)

long_data_wavg_NI <- super_combined_NI %>%
  pivot_longer(cols = c(weighted_avg_p,weighted_avg_p_NI), 
               names_to = "series", 
               values_to = "value")

p_wavg <- ggplot(long_data_wavg_NI, aes(x = year, y = value, color = series, group = series)) +
  geom_line(size = 1.5) +                    # Increase line size
  geom_point(size = 3) + # Increase point size
  labs(title = "Real 90th Percentile by Non-Imputed or Imputed",
       x = "Year",
       y = "90th percentile Wage (2023$)",
       color = "Methods") +                   # Add labels
  theme_light(base_size = 14) +              # Use a light theme with larger base font size
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Light background
    panel.background = element_rect(fill = "white"),              # Light panel
    text = element_text(color = "black"),  # Change text color to black for visibility
    plot.title = element_text(hjust = 0.5), # Center title
    legend.position = "top",                 # Position legend at the top
    panel.grid.major = element_line(color = "grey80"), 
    panel.grid.minor = element_blank()       # Remove minor gridlines
  ) +
  scale_color_brewer(palette = "Set1") +     # Use a color palette
  scale_y_continuous(breaks = seq(floor(38), ceiling(66), by = 2), limits = c(38,66)) +
scale_x_continuous(breaks = seq(floor(1970), ceiling(2025), by = 5))

ggsave("output/90th_w_avg_NI.png", plot = p_wavg, width = 10, height = 6, dpi = 300)

p_wavg + p_binipolate

### TABLE ###
growth_table <- long_data %>%
  group_by(series) %>%
  summarize(
    growth_1973_2023 = (value[year == 2023] - value[year == 1973]) / value[year == 1973] * 100,
    growth_2019_2023 = (value[year == 2023] - value[year == 2019]) / value[year == 2019] * 100
  )

# Transpose the growth table
growth_by_method <- as.data.frame(t(growth_table))
colnames(growth_by_method) <- growth_by_method[1, ]
growth_by_method <- growth_by_method[-1, ]

write.csv(growth_by_method, "output/growth_by_method.csv", row.names = TRUE)

