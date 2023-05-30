get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)
  
  data_cor <- data %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_expr) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)
  
  if (fct_reorder) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }
  
  if (fct_rev) {
    data_cor <- data_cor %>% 
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }
  
  return(data_cor)
  
}