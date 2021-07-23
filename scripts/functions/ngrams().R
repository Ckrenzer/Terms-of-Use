# requires a key field--a fake one can be made in the previous call if there
# are no key fields; this is fine for my purposes
ngrams <- function(text_df, key_column = NULL, text_column, n = 1){
  # making unigrams
  words <- str_split(text_df[[text_column]], "\\s+", simplify = FALSE)
  n <- n - 1
  
  # results will be added to this data frame
  ngram_df <- tibble(company = character(0), text = character(0))
  
  # performing this operation for each 'key' in the data frame
  for(element in 1:length(words)){
    # the company name is our key
    company_name <- text_df[[key_column]][element]
    
    # the n-grams are added to this vector
    vec <- character(length(words[[element]]) - n)
    for(i in 1:length(vec)){
      for(j in i:(i + n)){
        vec[i] <- str_c(vec[i], words[[element]][j], sep = " ") 
      }
    }
    vec <- tibble(company = company_name, text = str_remove(vec, "^\\s{1}"))
    ngram_df <- bind_rows(ngram_df, vec) 
  }
  
  return(ngram_df)
}