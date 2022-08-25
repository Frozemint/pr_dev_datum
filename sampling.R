

categorically_similar <- function(subgraph_1_value, subgraph_2_value) {
  subgraph_1_value <- tolower(subgraph_1_value)
  subgraph_2_value <- tolower(subgraph_2_value)
  result <- subgraph_1_value == subgraph_2_value
  return(result)
}

numerically_similar <- function(subgraph_1_value, subgraph_2_value, threshold) {
  difference <- log10(subgraph_1_value) - log10(subgraph_2_value)
  return(abs(difference) <= 0.5)
}
