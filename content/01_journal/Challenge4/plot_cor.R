plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                    include_lbl = TRUE, lbl_precision = 2, 
                    lbl_position = "outward",
                    size = 2, line_size = 1, vert_size = 0.5, 
                    color_pos = "#2dc6d6", color_neg = "red") {
  
  feature_expr <- enquo(target)
  
  # Perform correlation analysis
  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())
  
  # Plot analysis
  g <- data_cor %>%
    ggplot(aes(x = !! feature_expr, y = feature, group = feature)) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = "black", size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    scale_color_manual(values = c(color_neg, color_pos)) +
    theme(legend.position = "bottom")
  
  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  
  return(g)
  
}