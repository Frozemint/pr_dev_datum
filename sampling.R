

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

component_is_similar(component_1, component_2, all_dimensions) {
  component_are_similar = TRUE
  for (dimension in all_dimensions) {
    similarity = compare_component_dimension_similarity(component_1, component_2, 0.1)
    if (similarity == FALSE) {
      return(FALSE)
    }
  }
  return(TRUE)
}

compare_component_dimension_similarity(component_1, component_2, dimension, threshold=0.5) {
  numeric <- is.numeric(component_1.dimension)
  if (numeric) {
    difference <- component_1.dimension - component_2.dimension
    if (difference == 0) {
      return(TRUE)
    }
    return(log10(abs(difference)) <= threshold)
  } else {
    return(component_1.dimension == component_2.dimension)
  }
}

find_similar_components(component, component_universe, dimensions) {
  similar_components <- vector()
  for (component_in_universe in component_universe) {
    if (component_is_similar(component, component_universe, dimensions)) {
      similar_components <- append(component)
    }
  }
  return(similar_components)
}

score_components <- function(components, universe, space, configuration=NA) {
  coverage <- vector()
  for (component in sample) {
    similar_components = find_similar_components(component, universe, configuration)
    for (item in similar_components) {
      if (!is.element(item, coverage)) {
        coverage <- append(item)
      }
    }
  }
  score <- length(coverage) / length(universe)
}

next_components(K, preselected, universe, dimensions, config) {
  result <- vector()
  candidates <- vector()
  for (item in universe) {
    if (!is.element(item, preselected)) {
      candidates <- append(item)
    }
  }
  c_space = preselected
  for (i in 1:K) {
    c_best = vector()
    p_best = NA
    for (candidate in candidates) {
      components_covered_by_candidate = find_similar_components(candidate, candidates, dimensions)
      if (length(components_covered_by_candidate) > length(c_best)) {
        c_best = components_covered_by_candidate
        p_best = candidate
      }
    }
    if (p_best == NA) {
      break
    }
    result <- append(p_best)
    candidates <- candidates[!(candidates %in% p_best)]
    c_space <- append(c_best)
  }
  return(result)
}
