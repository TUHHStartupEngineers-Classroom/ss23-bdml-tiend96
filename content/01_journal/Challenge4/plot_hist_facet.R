plot_hist_facet <- function(data, fct_reorder = FALSE, fct_rev = FALSE, 
                            bins = 10, fill = "#2dc6d6", color = "white", 
                            ncol = 5, scale = "free") {
  
  data_factored <- data %>%
    
    # Convert input to make the function fail safe 
    # (if other content might be provided)
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    
    # Data must be in long format to make facets
    pivot_longer(cols = everything(),
                 names_to = "key",
                 values_to = "value",
                 # set key = factor() to keep the order
                 names_transform = list(key = forcats::fct_inorder)) 
  
  if (fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if (fct_rev) {
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~ key, ncol = ncol, scale = scale)
  
  return(g)
  
}
