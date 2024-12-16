# Define a custom function to calculate weighted quantiles without interpolation
weighted_quantile <- function(values, weights, probs) {
  # Order values and weights based on the values
  order <- order(values)
  values <- values[order]
  weights <- weights[order]
  
  # Calculate cumulative weights
  cum_weights <- cumsum(weights) / sum(weights)
  
  # What is the percentile we are looking for 
  percentiles <- numeric(length(probs))
  
  # Find the percentile
  for (i in seq_along(probs)) {
    # Identify the first value where cumulative weight exceeds the percentile
    percentiles[i] <- values[which(cum_weights >= probs[i])[1]]
  }
  
  return(percentiles)
}
