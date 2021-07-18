# Remember as you read this that replacement operations inside the mutate calls refer to entire vectors of words while the operations inside a case_when() call refers to individual words
porter_stemmer <- function(unigram_df){
  # The implementation of the Porter stemmer algorithm as I understand it,
  # as described in:
  # https://vijinimallawaarachchi.com/2017/05/09/porter-stemming-algorithm/
  #
  # and the official website:
  # https://tartarus.org/martin/PorterStemmer/index.html
  
  
  # Replaces text if the `(*v*) ed ---> _____` or `(*v*) ing ---> ______` cases of Step 1b execute
  step_1b_helper <- function(word, suffix, m){
    
    # See ?`{` if you do not understand how the {} work--they allow us to use the magrittr dot.
    
    str_remove(word, suffix) %>% 
      {case_when(
        str_detect(string = ., pattern = "at$") ~ str_replace(string = ., pattern = "at$", replacement = "ate"),
        str_detect(string = ., pattern = "bl$") ~ str_replace(string = ., pattern = "bl$", replacement = "ble"),
        str_detect(string = ., pattern = "iz$") ~ str_replace(string = ., pattern = "iz$", replacement = "ize"),
        str_detect(string = ., pattern = "([bcdfghjklmnpqrvwxy]){2}$") ~ str_replace(string = ., pattern = "([bcdfghjklmnpqrvwxy]){2}$", replacement = "\\1"), # *d and not (*L, *S, or *Z)) --> single letter
        m == 1 && str_detect(string = ., pattern = "[^aeiou]{1}[aeiou]{1}[^aeiou]{1}$") ~ str_replace(string = ., pattern = "[^aeiou]{1}[aeiou]{1}[^aeiou]{1}$", replacement = "e"),
        
        TRUE ~ .
      )} %>% 
      return()
    
  }#end of step_1b()
  
  
  
  # Step 1a
  unigram_df <- unigram_df %>% 
    mutate(m = str_count(word, pattern = "[aeiou]+[^aeiou]+"),
           word = case_when(
             str_detect(word, "sses$") ~ str_replace(word, "sses$", "ss"),
             str_detect(word, "ies$") ~ str_replace_all(word, "ies$", "i"),
             str_detect(word, "([^s])s{1,1}$") ~ str_remove(word, "s$"),
             
             TRUE ~ word
           ))
  
  
  # Step 1b
  unigram_df <- unigram_df %>% 
    mutate(word = case_when(
      m > 0 && str_detect(word, "eed$") ~ str_replace(word, "eed$", "ee"),
      str_detect(word, "ed$") && str_detect(str_sub(word, end = -3), "[aeiou]") ~ step_1b_helper(word = word, suffix = "ed$", m = m),
      str_detect(word, "ing$") && str_detect(str_sub(word, end = -4), "[aeiou]") ~ step_1b_helper(word = word, suffix = "ing$", m = m),
      
      TRUE ~ word
    ))
  
  
  # Step 1c
  unigram_df <- unigram_df %>% 
    mutate(word = case_when(str_detect(word, "y$") && str_detect(str_sub(word, end = -1), "[aeiou]") ~ str_replace(string = word, pattern = "y$", replacement = "i"),
                          TRUE ~ word))
  
  # Step 2
  unigram_df <- unigram_df %>% 
    mutate(word = case_when(
      m > 0 && str_detect(word, "ational$") ~ str_replace(string = word, pattern = "ational$", replacement = "ate"),
      m > 0 && str_detect(word, "tional$") ~ str_replace(string = word, pattern = "tional$", replacement = "tion"),
      m > 0 && str_detect(word, "enci$") ~ str_replace(string = word, pattern = "enci$", replacement = "ence"),
      m > 0 && str_detect(word, "anci$") ~ str_replace(string = word, pattern = "anci$", replacement = "ance"),
      m > 0 && str_detect(word, "izer$") ~ str_replace(string = word, pattern = "izer$", replacement = "ize"),
      m > 0 && str_detect(word, "abli$") ~ str_replace(string = word, pattern = "abli$", replacement = "able"),
      m > 0 && str_detect(word, "alli$") ~ str_replace(string = word, pattern = "alli$", replacement = "al"),
      m > 0 && str_detect(word, "entli$") ~ str_replace(string = word, pattern = "entli$", replacement = "ent"),
      m > 0 && str_detect(word, "eli$") ~ str_replace(string = word, pattern = "eli$", replacement = "e"),
      m > 0 && str_detect(word, "ousli$") ~ str_replace(string = word, pattern = "ousli$", replacement = "ous"),
      m > 0 && str_detect(word, "ization$") ~ str_replace(string = word, pattern = "ization$", replacement = "ize"),
      m > 0 && str_detect(word, "ation$") ~ str_replace(string = word, pattern = "ation$", replacement = "ate"),
      m > 0 && str_detect(word, "ator$") ~ str_replace(string = word, pattern = "ator$", replacement = "ate"),
      m > 0 && str_detect(word, "alism$") ~ str_replace(string = word, pattern = "alism$", replacement = "al"),
      m > 0 && str_detect(word, "iveness$") ~ str_replace(string = word, pattern = "iveness$", replacement = "ive"),
      m > 0 && str_detect(word, "fulness$") ~ str_replace(string = word, pattern = "fulness$", replacement = "ful"),
      m > 0 && str_detect(word, "ousness$") ~ str_replace(string = word, pattern = "ousness$", replacement = "ous"),
      m > 0 && str_detect(word, "aliti$") ~ str_replace(string = word, pattern = "aliti$", replacement = "al"),
      m > 0 && str_detect(word, "iviti$") ~ str_replace(string = word, pattern = "iviti$", replacement = "ive"),
      m > 0 && str_detect(word, "biliti$") ~ str_replace(string = word, pattern = "biliti$", replacement = "ble"),
      
      TRUE ~ word
    ))
  
  
  
  # Step 3
  unigram_df <- unigram_df %>% 
    mutate(word = case_when(
      m > 0 && str_detect(word, "icate$") ~ str_replace(string = word, pattern = "icate$", replacement = "ic"),
      m > 0 && str_detect(word, "ative$") ~ str_remove(string = word, pattern = "ative$"),
      m > 0 && str_detect(word, "alize$") ~ str_replace(string = word, pattern = "alize$", replacement = "al"),
      m > 0 && str_detect(word, "iciti$") ~ str_replace(string = word, pattern = "iciti$", replacement = "ic"),
      m > 0 && str_detect(word, "ical$") ~ str_replace(string = word, pattern = "ical$", replacement = "ic"),
      m > 0 && str_detect(word, "ful$") ~ str_remove(string = word, pattern = "ful$"),
      m > 0 && str_detect(word, "ness$") ~ str_remove(string = word, pattern = "ness$"),
      
      TRUE ~ word
    ))
  
  
  
  # Step 4
  unigram_df <- unigram_df %>% 
    mutate(word = case_when(
      m > 1 && str_detect(word, "al$") ~ str_remove(string = word, pattern = "al$"),
      m > 1 && str_detect(word, "ance$") ~ str_remove(string = word, pattern = "ance$"),
      m > 1 && str_detect(word, "ence$") ~ str_remove(string = word, pattern = "ence$"),
      m > 1 && str_detect(word, "er$") ~ str_remove(string = word, pattern = "er$"),
      m > 1 && str_detect(word, "ic$") ~ str_remove(string = word, pattern = "ic$"),
      m > 1 && str_detect(word, "able$") ~ str_remove(string = word, pattern = "able$"),           
      m > 1 && str_detect(word, "ible$") ~ str_remove(string = word, pattern = "ible$"),           
      m > 1 && str_detect(word, "ant$") ~ str_remove(string = word, pattern = "ant$"),           
      m > 1 && str_detect(word, "ement$") ~ str_remove(string = word, pattern = "ement$"),           
      m > 1 && str_detect(word, "ment$") ~ str_remove(string = word, pattern = "ment$"), 
      m > 1 && str_detect(word, "tion$|sion$") ~ str_remove(string = word, pattern = "tion$|sion$"),           
      m > 1 && str_detect(word, "ou$") ~ str_remove(string = word, pattern = "ou$"),           
      m > 1 && str_detect(word, "ism$") ~ str_remove(string = word, pattern = "ism$"),           
      m > 1 && str_detect(word, "ate$") ~ str_remove(string = word, pattern = "ate$"),           
      m > 1 && str_detect(word, "iti$") ~ str_remove(string = word, pattern = "iti$"),           
      m > 1 && str_detect(word, "ous$") ~ str_remove(string = word, pattern = "ous$"),           
      m > 1 && str_detect(word, "ive$") ~ str_remove(string = word, pattern = "ive$"),           
      m > 1 && str_detect(word, "ize$") ~ str_remove(string = word, pattern = "ize$"), 
      
      TRUE ~ word
    ))
  
  
  
  # Step 5a
  unigram_df <- unigram_df %>% 
    mutate(word = case_when(
      m > 1 && str_detect(word, "e$") ~ str_remove(word, "e$"),
      m == 1 && !str_detect(word, "[^aeiou]{1}[aeiou]{1}[^aeiou]{1}e$") && str_detect(word, "e$") ~ str_remove(word, "e$"),
      
      TRUE ~ word
      
    ))
  
  
  # Step 5b
  unigram_df <- unigram_df %>% 
    mutate(word = case_when(m > 1 && str_detect(word, "ll$") ~ str_replace(string = word, pattern = "ll$", replacement = "l"),
                          TRUE ~ word
    ))
  
  
  return(unigram_df)
}#end of porter_stemmer()
