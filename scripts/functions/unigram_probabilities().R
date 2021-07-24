# Count the number of times a word is found in a document,
# then divide by the number of words in that document
# (not unique words, ALL words).

# assumes the words have already been counted
# and are stored in a column named `n`
unigram_probabilities <- function(df, doc_id = NULL){
  if(is.null(doc_id)){
    df <- df %>% 
      mutate(probability = n / sum(n))
  } else {
    df <- df %>% 
      group_by(get(doc_id)) %>% 
      mutate(probability = n / sum(n)) %>% 
      ungroup()
    
  }
  return(df)
}