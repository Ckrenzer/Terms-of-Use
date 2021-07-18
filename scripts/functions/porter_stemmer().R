# Remember as you read this that replacement operations inside the mutate calls refer to entire vectors of words while the operations inside a case_when() call refers to individual words
porter_stemmer <- function(unigram_df, text_column = "word"){
  # The implementation of the Porter stemmer algorithm as I understand it,
  # as described in:
  # https://vijinimallawaarachchi.com/2017/05/09/porter-stemming-algorithm/
  #
  # and the official website:
  # https://tartarus.org/martin/PorterStemmer/index.html
  
  
  # Removing contractions before Porter stemming
  # Words like "won't", "shan't", "ain't" and "can't" are dealt with
  # directly, and words like "o'clock" are only stripped of their apostrophes
  unigram_df <- unigram_df %>% 
    mutate({{text_column}} := str_replace(get(text_column), "won't", "will") %>% 
             str_replace("shan't", "shall") %>%
             str_replace("ain't", "am") %>% 
             str_replace("can't", "can") %>% 
             str_remove(common_contractions) %>% 
             str_remove("'"))
  
  
  # Step 1a
  unigram_df <- unigram_df %>% 
    mutate(m = calculate_m(get(text_column)),
           {{text_column}} := {case_when(
             str_detect(get(text_column), "\\d") ~ get(text_column),
             str_detect(get(text_column), "sses$") ~ str_replace(get(text_column), "sses$", "ss"),
             str_detect(get(text_column), "ies$") ~ str_replace_all(get(text_column), "ies$", "i"),
             str_detect(get(text_column), "([^s])s{1,1}$") ~ str_remove(get(text_column), "s$"),
             
             TRUE ~ get(text_column)
           )})
  
  
  # Step 1b
  unigram_df <- unigram_df %>% 
    mutate(m = calculate_m(get(text_column)),
           {{text_column}} := {case_when(
             str_detect(get(text_column), "\\d") ~ get(text_column),
             m > 0 & str_detect(get(text_column), "eed$") ~ str_replace(get(text_column), "eed$", "ee"),
             str_detect(get(text_column), "ed$") & str_detect(str_sub(get(text_column), end = -3), "[aeiou]") ~ step_1b_helper(word = get(text_column), suffix = "ed$", m = m),
             str_detect(get(text_column), "ing$") & str_detect(str_sub(get(text_column), end = -4), "[aeiou]") ~ step_1b_helper(word = get(text_column), suffix = "ing$", m = m),
             
             TRUE ~ get(text_column)
           )})
  
  
  # Step 1c
  unigram_df <- unigram_df %>% 
    mutate({{text_column}} := {case_when(str_detect(get(text_column), "\\d") ~ get(text_column),
                                         str_detect(get(text_column), "y$") & str_detect(str_sub(get(text_column), end = -1), "[aeiou]") ~ str_replace(string = get(text_column), pattern = "y$", replacement = "i"),
                                         TRUE ~ get(text_column))})
  
  # Step 2
  unigram_df <- unigram_df %>% 
    mutate(m = calculate_m(get(text_column)),
           {{text_column}} := {case_when(
             str_detect(get(text_column), "\\d") ~ get(text_column),
             m > 0 & str_detect(get(text_column), "ational$") ~ str_replace(string = get(text_column), pattern = "ational$", replacement = "ate"),
             m > 0 & str_detect(get(text_column), "tional$") ~ str_replace(string = get(text_column), pattern = "tional$", replacement = "tion"),
             m > 0 & str_detect(get(text_column), "enci$") ~ str_replace(string = get(text_column), pattern = "enci$", replacement = "ence"),
             m > 0 & str_detect(get(text_column), "anci$") ~ str_replace(string = get(text_column), pattern = "anci$", replacement = "ance"),
             m > 0 & str_detect(get(text_column), "izer$") ~ str_replace(string = get(text_column), pattern = "izer$", replacement = "ize"),
             m > 0 & str_detect(get(text_column), "abli$") ~ str_replace(string = get(text_column), pattern = "abli$", replacement = "able"),
             m > 0 & str_detect(get(text_column), "alli$") ~ str_replace(string = get(text_column), pattern = "alli$", replacement = "al"),
             m > 0 & str_detect(get(text_column), "entli$") ~ str_replace(string = get(text_column), pattern = "entli$", replacement = "ent"),
             m > 0 & str_detect(get(text_column), "eli$") ~ str_replace(string = get(text_column), pattern = "eli$", replacement = "e"),
             m > 0 & str_detect(get(text_column), "ousli$") ~ str_replace(string = get(text_column), pattern = "ousli$", replacement = "ous"),
             m > 0 & str_detect(get(text_column), "ization$") ~ str_replace(string = get(text_column), pattern = "ization$", replacement = "ize"),
             m > 0 & str_detect(get(text_column), "ation$") ~ str_replace(string = get(text_column), pattern = "ation$", replacement = "ate"),
             m > 0 & str_detect(get(text_column), "ator$") ~ str_replace(string = get(text_column), pattern = "ator$", replacement = "ate"),
             m > 0 & str_detect(get(text_column), "alism$") ~ str_replace(string = get(text_column), pattern = "alism$", replacement = "al"),
             m > 0 & str_detect(get(text_column), "iveness$") ~ str_replace(string = get(text_column), pattern = "iveness$", replacement = "ive"),
             m > 0 & str_detect(get(text_column), "fulness$") ~ str_replace(string = get(text_column), pattern = "fulness$", replacement = "ful"),
             m > 0 & str_detect(get(text_column), "ousness$") ~ str_replace(string = get(text_column), pattern = "ousness$", replacement = "ous"),
             m > 0 & str_detect(get(text_column), "aliti$") ~ str_replace(string = get(text_column), pattern = "aliti$", replacement = "al"),
             m > 0 & str_detect(get(text_column), "iviti$") ~ str_replace(string = get(text_column), pattern = "iviti$", replacement = "ive"),
             m > 0 & str_detect(get(text_column), "biliti$") ~ str_replace(string = get(text_column), pattern = "biliti$", replacement = "ble"),
             
             TRUE ~ get(text_column)
           )})
  
  
  
  # Step 3
  unigram_df <- unigram_df %>% 
    mutate(m = calculate_m(get(text_column)),
           {{text_column}} := {case_when(
             str_detect(get(text_column), "\\d") ~ get(text_column),
             m > 0 & str_detect(get(text_column), "icate$") ~ str_replace(string = get(text_column), pattern = "icate$", replacement = "ic"),
             m > 0 & str_detect(get(text_column), "ative$") ~ str_remove(string = get(text_column), pattern = "ative$"),
             m > 0 & str_detect(get(text_column), "alize$") ~ str_replace(string = get(text_column), pattern = "alize$", replacement = "al"),
             m > 0 & str_detect(get(text_column), "iciti$") ~ str_replace(string = get(text_column), pattern = "iciti$", replacement = "ic"),
             m > 0 & str_detect(get(text_column), "ical$") ~ str_replace(string = get(text_column), pattern = "ical$", replacement = "ic"),
             m > 0 & str_detect(get(text_column), "ful$") ~ str_remove(string = get(text_column), pattern = "ful$"),
             m > 0 & str_detect(get(text_column), "ness$") ~ str_remove(string = get(text_column), pattern = "ness$"),
             
             TRUE ~ get(text_column)
           )})
  
  
  
  # Step 4
  unigram_df <- unigram_df %>% 
    mutate(m = calculate_m(get(text_column)),
           {{text_column}} := {case_when(
             str_detect(get(text_column), "\\d") ~ get(text_column),
             m > 1 & str_detect(get(text_column), "al$") ~ str_remove(string = get(text_column), pattern = "al$"),
             m > 1 & str_detect(get(text_column), "ance$") ~ str_remove(string = get(text_column), pattern = "ance$"),
             m > 1 & str_detect(get(text_column), "ence$") ~ str_remove(string = get(text_column), pattern = "ence$"),
             m > 1 & str_detect(get(text_column), "er$") ~ str_remove(string = get(text_column), pattern = "er$"),
             m > 1 & str_detect(get(text_column), "ic$") ~ str_remove(string = get(text_column), pattern = "ic$"),
             m > 1 & str_detect(get(text_column), "able$") ~ str_remove(string = get(text_column), pattern = "able$"),           
             m > 1 & str_detect(get(text_column), "ible$") ~ str_remove(string = get(text_column), pattern = "ible$"),           
             m > 1 & str_detect(get(text_column), "ant$") ~ str_remove(string = get(text_column), pattern = "ant$"),           
             m > 1 & str_detect(get(text_column), "ement$") ~ str_remove(string = get(text_column), pattern = "ement$"),           
             m > 1 & str_detect(get(text_column), "ment$") ~ str_remove(string = get(text_column), pattern = "ment$"), 
             m > 1 & str_detect(get(text_column), "ent$") ~ str_remove(string = get(text_column), pattern = "ent$"), 
             m > 1 & str_detect(get(text_column), "tion$|sion$") ~ str_remove(string = get(text_column), pattern = "ion$"),           
             m > 1 & str_detect(get(text_column), "ou$") ~ str_remove(string = get(text_column), pattern = "ou$"),           
             m > 1 & str_detect(get(text_column), "ism$") ~ str_remove(string = get(text_column), pattern = "ism$"),           
             m > 1 & str_detect(get(text_column), "ate$") ~ str_remove(string = get(text_column), pattern = "ate$"),           
             m > 1 & str_detect(get(text_column), "iti$") ~ str_remove(string = get(text_column), pattern = "iti$"),           
             m > 1 & str_detect(get(text_column), "ous$") ~ str_remove(string = get(text_column), pattern = "ous$"),           
             m > 1 & str_detect(get(text_column), "ive$") ~ str_remove(string = get(text_column), pattern = "ive$"),           
             m > 1 & str_detect(get(text_column), "ize$") ~ str_remove(string = get(text_column), pattern = "ize$"), 
             
             TRUE ~ get(text_column)
           )})
  
  
  
  # Step 5a
  unigram_df <- unigram_df %>% 
    mutate(m = calculate_m(get(text_column)),
           {{text_column}} := {case_when(
             str_detect(get(text_column), "\\d") ~ get(text_column),
             m > 1 & str_detect(get(text_column), "e$") ~ str_remove(get(text_column), "e$"),
             m == 1 & !str_detect(get(text_column), "[^aeiou]{1}[aeiou]{1}[^aeiou]{1}e$") & str_detect(get(text_column), "e$") ~ str_remove(get(text_column), "e$"),
             
             TRUE ~ get(text_column)
             
           )})
  
  
  # Step 5b
  unigram_df <- unigram_df %>% 
    mutate(m = calculate_m(get(text_column)),
           {{text_column}} := {case_when(str_detect(get(text_column), "\\d") ~ get(text_column),
                                         m > 1 & str_detect(get(text_column), "ll$") ~ str_replace(string = get(text_column), pattern = "ll$", replacement = "l"),
                                         TRUE ~ get(text_column)
           )}) %>% 
    dplyr::select(-m)
  
  
  return(unigram_df)
}#end of porter_stemmer()
