## function to describe variables
##

desc_tab <- function(x, lab_warn = FALSE) {
  freq_tab <- cbind(table(x, useNA = "always"))
  prop_tab <- round(prop.table(freq_tab), 3)
  
  # make label coloumn
  lab_tab <- row.names(freq_tab)
  
  tab <- data.frame(
    Code = lab_tab,
    Freq. = as.numeric(freq_tab),
    Perc. = as.numeric(prop_tab) * 100
  )
  
  # prin quesiton if it's present
  if (!is.null(attr(x, "label", exact = TRUE))) {
      print(attr(x, "label"))
  }
  
  # if it has labels then relabel
  if (!is.null(attr(x, "labels"))) {
    if (lab_warn)
      print("Has label attribute")
    
    # make Code numeric for merging 
    tab$Code <- as.double(levels(tab$Code)[tab$Code])
    
    # get rid of missing labels
    attribute_lab <- attr(x, "labels")
    labs <- as.data.frame(na.omit(attribute_lab))
    labs$Labs <- row.names(labs)
    names(labs)[1] <- "Code"  
    
    tab <- suppressWarnings(left_join(tab, labs, by = "Code"))
    tab <- tab[, c(4, 1:3)]
  }
  
  print(tab)
}
