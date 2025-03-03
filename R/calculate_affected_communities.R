calculate_affected_communities <- function(total_communities, affected_communities) {
  percentage <- (affected_communities / total_communities) * 100
  return(round(percentage, 2))
}