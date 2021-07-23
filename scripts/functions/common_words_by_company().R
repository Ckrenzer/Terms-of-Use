# Plots the most common words, faceted by company
common_words_by_company <- function(word_count_df, company_name){
  
  word_count_df %>% 
    filter(company == company_name) %>% 
    slice_max(order_by = n, n = 15) %>% 
    ggplot() +
    geom_col(mapping = aes(x = reorder(text, n), y = n, fill = company),
             show.legend = FALSE) +
    ggtitle(company_name) +
    xlab(NULL) +
    ylab("Number of Uses") +
    scale_fill_manual(values = company_colors[company_name]) +
    coord_flip() %>% 
    return()
  
}