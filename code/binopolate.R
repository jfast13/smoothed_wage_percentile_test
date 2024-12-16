# binipolate: binned interpolated percentiles
binipolate <- function(data, var, p = 50, .by = NULL, binsize, w = 1, classical = FALSE) {
  
  
  # Check binsize and throw an error if it's less than or equal to 0
  if (binsize <= 0) {
    stop("binsize must be greater than 0.")
  }
  
  # Check data for observations
  if(nrow(data) == 0) {
    stop("no observations, please check sample")
  }
  
  # calculate binned interpolated percentiles
  if(classical == FALSE) {
    # cumulative distribution function
    #note: CDF will be used to approximate closest bins around desired percentile
    CDF <- data %>%
      filter(!is.na({{ var }})) %>%
      # calculate bin values across groups
      mutate(binvalue = floor(({{ var }} - binsize/2)/binsize) * binsize + binsize + binsize/2) %>%
      # sum of weights by grouping and bin
      summarise(sum = sum({{ w }}, na.rm = TRUE), .by = c({{ .by }}, binvalue)) %>%
      # running sum of weights by grouping and bin
      arrange(across(c({{ .by }}, binvalue)))  %>% mutate(runningsum = cumsum(sum), .by = {{ .by }}) %>%
      mutate(totalsum = sum(sum, na.rm = TRUE), cdf = runningsum/totalsum, .by = {{ .by }}) %>%
      # count number of bins by grouping
      mutate(binnumber = seq_along(binvalue), .by = {{ .by }})
    
    #return(CDF)
    
    # identify maximum number of bins within given percentile breaks
    #note: example, p = 50 ~ max(binnumber) == 90
    map(p, ~ CDF %>% filter(cdf <= .x / 100) %>%
          summarise(below = max(binnumber, na.rm = TRUE), .by = {{ .by }}) %>%
          mutate(p = .x)) %>%
      reduce(bind_rows) %>%
      # merge with binned data
      left_join(CDF, by = names(select(., {{ .by }})), relationship = "many-to-many") %>%
      # within each group, filter out to the bin break or bin break + 1
      filter(binnumber == below | binnumber == (below + 1), .by = c({{ .by }}, p)) %>%
      # calculate percentiles
      mutate("{{ var }}" :=  binvalue + (lead(binvalue, 1) - binvalue) * ((p/100 - cdf) / (lead(cdf, 1) - cdf)), .by = c({{ .by }}, p)) %>%
      filter(!is.na({{ var }})) %>%
      select({{ .by }}, {{ var }}, p) %>%
      mutate(across(.cols = where(~ sjlabelled::is_labelled(.) && !is.character(.)), ~ as.character(as_factor(.x)))) %>%
      arrange(across(c({{ .by }}, p)))
  }
  
  # calculate classical weighted quantile
  else {
    reframe(data,
            p = p,
            "{{ var }}" := MetricsWeighted::weighted_quantile({{ var }}, probs = c(p/100), w = {{ w }}), .by = {{ .by }}) %>%
      # select relevant variables
      select({{ .by }}, {{var}}, p) %>%
      # arrange variables
      arrange(across(c({{ .by }}, p)))
  }
  
  
}
