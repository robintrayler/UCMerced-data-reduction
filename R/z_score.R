z_score <- function(x) {
  abs(x - mean(x)) / sd(x)
}