
relabel <- function(x) {
  labs <- attr(x, "labels")
  
  # code missing if they have no labels
  x[!x %in% labs[!is.na(labs)]] <- NA
  y <- factor(x, labels = names(labs[!is.na(labs)]))
  
  y
}
