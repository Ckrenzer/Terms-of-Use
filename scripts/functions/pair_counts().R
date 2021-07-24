# this is a manual version of widyr::pairwise_count()
pair_counts <- function(df, doc_id = "company", text_column = "text"){
  
  left_join(df, df, by = doc_id) %>% 
    filter(get(paste0(text_column, ".x")) != get(paste0(text_column, ".y"))) %>% 
    distinct() %>% 
    count(get(paste0(text_column, ".x")), get(paste0(text_column, ".y")), sort = FALSE) %>% 
    rename(item1 = `get(paste0(text_column, ".x"))`, item2 = `get(paste0(text_column, ".y"))`) %>% 
    return()
  
}