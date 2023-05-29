count_to_pct <- function(data, ..., col = n) {
  
  # capture the dots
  grouping_vars_expr <- quos(...)
  col_expr <- enquo(col)
  
  ret <- data %>%
    group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr) / sum(!! col_expr)) %>%
    ungroup()
  
  return(ret)
  
}