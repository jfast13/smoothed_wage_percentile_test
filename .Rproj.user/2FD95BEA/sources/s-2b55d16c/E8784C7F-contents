
## CALCULATING MEDIAN WAGES USING 3 methods ##

source(here("code/function.R"), echo = TRUE)
source(here("code/binopolate.R"), echo = TRUE)

## setting what wew want to look at 
percentile <- 15
p_4 <- (percentile-4)/100
p_3 <- (percentile-3)/100
p_2 <- (percentile-2)/100
p_1 <- (percentile-1)/100
p__1 <- (percentile+1)/100
p__2<- (percentile+2)/100
p__3<- (percentile+3)/100
p__4<- (percentile+4)/100


#Adjusting before binning 
#current_method <- binipolate(wage_master, var = adj_wage, p = 50, .by = "year", 0.25, w = finalwgt) %>% 
  
#Bin in nominal wages first
current_method <- binipolate(wage_master, var = wage, p = percentile, .by = "year", 0.25, w = finalwgt) %>% 
                   left_join(cpi, by = "year") %>%
                            mutate(adjwage = wage * (cpi_base/cpiurs)) %>% 
                                  select(year,adjwage)

current_method_NI <- binipolate(wage_master_no_imputed, var = wage, p = percentile, .by = "year", 0.25, w = finalwgt) %>% 
  left_join(cpi, by = "year") %>%
  mutate(adjwage = wage * (cpi_base/cpiurs)) %>% 
  select(year,adjwage)

## NEW METHOD ##                           
new_method <- wage_master %>%
  group_by(year) %>%
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
    avg_p = mean(c(p46, p47, p48, p49, p50, p51, p52, p53, p54), na.rm = TRUE),
    weighted_avg_p = sum(
      c(p46, p47, p48, p49, p50, p51, p52, p53, p54) * 
        c(1/25, 2/25, 3/25, 4/25, 5/25, 4/25, 3/25, 2/25, 1/25), 
      na.rm = TRUE)
  ) %>%
  select(year, avg_p, weighted_avg_p)


new_method_NI <- wage_master_no_imputed %>%
  group_by(year) %>%
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
  ) %>%
  select(year, avg_p_NI, weighted_avg_p_NI) 

# All three methods put together
combined <- current_method %>%
  left_join(new_method, by = "year") %>% 
  rename(binipolate = adjwage)

super_combined_NI <- combined %>% 
     left_join(new_method_NI, by = "year") %>%
       left_join(current_method_NI, by = "year") %>%
        rename(binipolate_NI = adjwage)

#write.csv(super_combined_NI, "output/super_combined_NI.csv", row.names = FALSE)

