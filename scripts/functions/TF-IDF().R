tf_idf <- function(text_df, doc_id = "company", text_column = "text", counts_column = "n"){
  
  # TERM FREQUENCY -------------------------------------------------------
  tf <- function(text_df, doc_id = "company", text_column = "text", counts_column = "n"){
    text_df %>% 
      dplyr::group_by(!!!rlang::syms(doc_id)) %>% 
      dplyr::summarize(n_terms = sum(get(counts_column))) %>% 
      dplyr::left_join(x = ., y = text_df, by = doc_id) %>% 
      dplyr::mutate(tf = get(counts_column) / n_terms) %>% 
      dplyr::select(-n_terms) %>% 
      return()
  }
  
  
  
  
  # INVERSE DOCUMENT FREQUENCY ---------------------------------------------
  idf <- function(text_df, doc_id = "company", text_column = "text"){
    # a scalar containing the number of documents
    n_docs <- length(unique(text_df[[doc_id]]))
    
    # the words to find in each document
    words <- unique(text_df[[text_column]])
    
    # yikes! This implementation stinks!
    # speed was thrown out the window in this project...
    n_docs_containing_term <- list()
    for(word in words){
      n_docs_containing_term[word] <- unigram_counts %>% 
        group_by(get(doc_id)) %>% 
        summarize(is_found = any(get(text_column) == word)) %>% 
        pull(is_found) %>% 
        sum()
    }
    # coercing the list to a named vector
    n_docs_containing_term <- unlist(n_docs_containing_term, use.names = TRUE)
    
    # a named vector containing the idf statistic
    idf <- log(n_docs / n_docs_containing_term)
    
    # storing the results in a tibble
    idf_df <- tibble({{text_column}} := names(idf_values), idf = idf_values)
    
    # the input data frame with the idf statistic appended
    return(left_join(x = text_df, y = idf_df, by = text_column))
  }
  
  
  
  
  # TF-IDF ---------------------------------------------------------------
  # calculates the term frequency, then the inverse document frequency,
  # and then multiplies the two columns together to give us the TF-IDF
  tf(text_df = text_df, doc_id = doc_id, text_column = text_column, counts_column = counts_column) %>% 
    idf(text_df = ., doc_id = doc_id, text_column = text_column) %>% 
    mutate(tf_idf = tf * idf) %>% 
    return()
}