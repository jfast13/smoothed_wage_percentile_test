## Median wages by Race 
source(here("code/function.R"), echo = TRUE)
source(here("code/binopolate.R"), echo = TRUE)

## setting what wew want to look at 
percentile <- 50
p_4 <- (percentile-4)/100
p_3 <- (percentile-3)/100
p_2 <- (percentile-2)/100
p_1 <- (percentile-1)/100
p__1 <- (percentile+1)/100
p__2<- (percentile+2)/100
p__3<- (percentile+3)/100
p__4<- (percentile+4)/100

current_method_race <- binipolate(wage_master, var = wage, p = percentile, .by = c(year, wbho), 0.25, w = finalwgt) %>% 
  left_join(cpi, by = "year") %>%
  mutate(adjwage = wage * (cpi_base/cpiurs)) %>% 
  select(year,adjwage,wbho)

current_method_race_NI <- binipolate(wage_master_no_imputed, var = wage, p = percentile, .by = c(year, wbho), 0.25, w = finalwgt) %>% 
  left_join(cpi, by = "year") %>%
  mutate(adjwage = wage * (cpi_base/cpiurs)) %>% 
  select(year,adjwage, wbho)

## NEW METHOD ##                           
new_method_race <- wage_master %>%
  group_by(year,wbho) %>%
  summarise(
    p46 = weighted_quantile(adj_wage, finalwgt, probs = p_4),
    p47 = weighted_quantile(adj_wage, finalwgt, probs = p_3),
    p48 = weighted_quantile(adj_wage, finalwgt, probs = p_2),
    p49 = weighted_quantile(adj_wage, finalwgt, probs = p_1),
    p50 = weighted_quantile(adj_wage, finalwgt, probs = percentile/100),
    p51 = weighted_quantile(adj_wage, finalwgt, probs = p__1),
    p52 = weighted_quantile(adj_wage, finalwgt, probs = p__2),
    p53 = weighted_quantile(adj_wage, finalwgt, probs = p__3),
    p54 = weighted_quantile(adj_wage, finalwgt, probs = p__4)
  ) %>%
  rowwise() %>%
  mutate(
    avg_p = mean(c(p46, p47, p48, p49, p50, p51, p52, p53, p54), na.rm = TRUE),
    weighted_avg_p = sum(
      c(p46, p47, p48, p49, p50, p51, p52, p53, p54) * 
        c(1/25, 2/25, 3/25, 4/25, 5/25, 4/25, 3/25, 2/25, 1/25), 
      na.rm = TRUE)
  ) %>% mutate(wbho = case_when(
    wbho == 1 ~ "White",
    wbho == 2 ~ "Black",
    wbho == 3 ~ "Hispanic",
    wbho == 4 ~ "Other"
  )) %>%
  select(year, avg_p, weighted_avg_p,wbho)

new_method_race_NI <- wage_master_no_imputed %>%
  group_by(year,wbho) %>%
  summarize(
    p46 = weighted_quantile(adj_wage, finalwgt, probs = p_4),
    p47 = weighted_quantile(adj_wage, finalwgt, probs = p_3),
    p48 = weighted_quantile(adj_wage, finalwgt, probs = p_2),
    p49 = weighted_quantile(adj_wage, finalwgt, probs = p_1),
    p50 = weighted_quantile(adj_wage, finalwgt, probs = percentile/100),
    p51 = weighted_quantile(adj_wage, finalwgt, probs = p__1),
    p52 = weighted_quantile(adj_wage, finalwgt, probs = p__2),
    p53 = weighted_quantile(adj_wage, finalwgt, probs = p__3),
    p54 = weighted_quantile(adj_wage, finalwgt, probs = p__4)
  ) %>%
  rowwise() %>%
  mutate(
    avg_p_NI = mean(c(p46, p47, p48, p49, p50, p51, p52, p53, p54), na.rm = TRUE),
    weighted_avg_p_NI = sum(
      c(p46, p47, p48, p49, p50, p51, p52, p53, p54) * 
        c(1/25, 2/25, 3/25, 4/25, 5/25, 4/25, 3/25, 2/25, 1/25), 
      na.rm = TRUE)
  ) %>% mutate(wbho = case_when(
    wbho == 1 ~ "White",
    wbho == 2 ~ "Black",
    wbho == 3 ~ "Hispanic",
    wbho == 4 ~ "Other"
  )) %>% select(year, avg_p_NI, weighted_avg_p_NI,wbho) 

# All three methods put together
combined_race <- current_method_race %>%
  left_join(new_method_race, by = c("year", "wbho")) %>% 
  rename(binipolate = adjwage)

super_combined_race_NI <- combined_race %>% 
  left_join(new_method_race_NI, by = c("year", "wbho")) %>%
  left_join(current_method_race_NI, by = c("year", "wbho")) %>%
  rename(binipolate_NI = adjwage) 


black_white <- super_combined_race_NI %>% 
  mutate(black_binipolate = ifelse(wbho == "Black", binipolate, NA),
         black_binipolate_NI = ifelse(wbho == "Black", binipolate_NI, NA),
         white_binipolate = ifelse(wbho == "White", binipolate, NA),
         white_binipolate_NI = ifelse(wbho == "White", binipolate_NI, NA),
         black_w_avg_p = ifelse(wbho == "Black", weighted_avg_p, NA),
         black_w_avg_p_NI = ifelse(wbho == "Black", weighted_avg_p_NI, NA),
         white_w_avg_p = ifelse(wbho == "White", weighted_avg_p, NA),
         white_w_avg_p_NI = ifelse(wbho == "White", weighted_avg_p_NI, NA)) %>% 
    select(year, black_binipolate, black_binipolate_NI, white_binipolate, white_binipolate_NI,
           black_w_avg_p, black_w_avg_p_NI, white_w_avg_p, white_w_avg_p_NI) %>%
  group_by(year) %>%
  summarize(
    black_binipolate = sum(black_binipolate, na.rm = TRUE),
    black_binipolate_NI = sum(black_binipolate_NI, na.rm = TRUE),
    white_binipolate = sum(white_binipolate, na.rm = TRUE),
    white_binipolate_NI = sum(white_binipolate_NI, na.rm = TRUE),
    black_w_avg_p = sum(black_w_avg_p, na.rm = TRUE),
    black_w_avg_p_NI = sum(black_w_avg_p_NI, na.rm = TRUE),
    white_w_avg_p = sum(white_w_avg_p, na.rm = TRUE),
    white_w_avg_p_NI = sum(white_w_avg_p_NI, na.rm = TRUE)
  ) %>% 
  mutate(
    diff_binipolate = (black_binipolate/white_binipolate -1)/-1,
    diff_binipolate_NI = (black_binipolate_NI/white_binipolate_NI -1)/-1 ,
    diff_w_avg_p = (black_w_avg_p/white_w_avg_p - 1)/-1,
    diff_w_avg_p_NI = (black_w_avg_p_NI/white_w_avg_p_NI - 1)/-1
  )


black_white_p <- black_white %>%
  pivot_longer(cols = c(diff_binipolate, diff_w_avg_p), 
               names_to = "series", 
               values_to = "value")

black_white_p <- ggplot(black_white_p, aes(x = year, y = value, color = series, group = series)) +
  geom_line(size = 1.5) +                    # Increase line size
  geom_point(size = 3) +                     # Increase point size
  labs(title = "Black-White Median Wage Gap Full Sample",
       x = "Year",
       y = "Median Wage Gap (%)",
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
  scale_y_continuous(breaks = seq(floor(0.15), ceiling(0.28), by = 0.01), limits = c(.15,.28))# Use a color palette

ggsave("output/black_white.png", plot = black_white_p , width = 10, height = 6, dpi = 300)

black_white_p_NI <- black_white %>%
  pivot_longer(cols = c(diff_binipolate_NI, diff_w_avg_p_NI), 
               names_to = "series", 
               values_to = "value")

black_white_p_NI <- ggplot(black_white_p_NI, aes(x = year, y = value, color = series, group = series)) +
  geom_line(size = 1.5) +                    # Increase line size
  geom_point(size = 3) +                     # Increase point size
  labs(title = "Black-White Median Wage Gap Non-Imputed Wages Only",
       x = "Year",
       y = "Median Wage Gap (%)",
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
  scale_y_continuous(breaks = seq(floor(0.15), ceiling(0.28), by = 0.01), limits = c(.15,.28))# Use a color palette

ggsave("output/black_white_NI.png", plot = black_white_p_NI , width = 10, height = 6, dpi = 300)