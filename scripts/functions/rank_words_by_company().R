# Plots the words with the highest value in the column specified by rank_column, faceted by company
rank_words_by_company <- function(word_count_df, company_name, rank_column = "n", num_to_show = 15){
  
  word_count_df %>% 
    filter(company == company_name) %>% 
    slice_max(order_by = get(rank_column), n = num_to_show) %>%  
    ggplot() +
    geom_col(mapping = aes(x = reorder(text, get(rank_column)), y = get(rank_column), fill = company),
             show.legend = FALSE) +
    ggtitle(company_name) +
    xlab(NULL) +
    ylab(NULL) +
    scale_fill_manual(values = company_colors[company_name]) +
    coord_flip() %>% 
    return()
  
}