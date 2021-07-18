# We ignore the end of the word if it contains these suffixes
# (used in calculate_m(), but is placed here to avoid running this code repeatedly)
suffixes_to_ignore <- c(
  # Suffixes from Step 2
  "ational$",     
  "tional$",      
  "enci$",        
  "anci$",        
  "izer$",        
  "abli$",        
  "alli$",        
  "entli$",       
  "eli$",        
  "ousli$",       
  "ization$",     
  "ation$",       
  "ator$",        
  "alism$",        
  "iveness$",      
  "fulness$",      
  "ousness$",      
  "aliti$",        
  "iviti$",        
  "biliti$",
  
  # Suffixes from Step 4
  "al$",
  "ance$",
  "ence$",
  "er$",
  "ic$",
  "able$",
  "ible$",
  "ant$",
  "ement$",
  "ment$",
  "ent$" ,
  "tion$|sion$",
  "ou$",
  "ism$",
  "ate$",
  "iti$",
  "ous$",
  "ive$",
  "ize$"
) %>% 
  str_c(collapse = "|")




calculate_m <- function(word){
  
  # The location of the start of the suffix
  end_index <-   if_else(condition = str_detect(word, suffixes_to_ignore),
                         true = as.double(str_locate(word, suffixes_to_ignore)[, 1, drop = FALSE]),
                         false = -1)
  
  # The part of the string for which we want to calculate 'm'
  valid_string <- str_sub(string = word, end = end_index)
  
  # Counts the [C](VC)^m[V] occurrences
  m <- str_count(valid_string, pattern = "[aeiou]+[^aeiou]+")
  
  return(m)
}




# Replaces text if the `(*v*) ed ---> _____` or `(*v*) ing ---> ______` cases of Step 1b execute
step_1b_helper <- function(word, suffix, m){
  
  # See ?`{` if you do not understand how the {} work--they allow us to use the magrittr dot.
  # I added way more than necessary, but this was only to ensure R isn't pulling any tricks
  
  str_remove(word, suffix) %>% 
    {case_when(
      str_detect(string = ., pattern = "at$") ~ str_replace(string = ., pattern = "at$", replacement = "ate"),
      str_detect(string = ., pattern = "bl$") ~ str_replace(string = ., pattern = "bl$", replacement = "ble"),
      str_detect(string = ., pattern = "iz$") ~ str_replace(string = ., pattern = "iz$", replacement = "ize"),
      str_detect(string = ., pattern = "(b{2}|c{2}|d{2}|f{2}|g{2}|h{2}|j{2}|k{2}|l{2}|m{2}|n{2}|p{2}|q{2}|r{2}|v{2}|w{2}|x{2}|y{2})$") ~ str_replace(string = ., pattern = "(b{1}|c{1}|d{1}|f{1}|g{1}|h{1}|j{1}|k{1}|l{1}|m{1}|n{1}|p{1}|q{1}|r{1}|v{1}|w{1}|x{1}|y{1})$", replacement = "\\1"), # *d and not (*L, *S, or *Z)) --> single letter
      m == 1 & str_detect(string = ., pattern = "[^aeiou]{1}[aeiou]{1}[^aeiou]{1}$") ~ str_replace(string = ., pattern = "[^aeiou]{1}[aeiou]{1}[^aeiou]{1}$", replacement = "e"),
      
      TRUE ~ .
    )} %>% 
    return()
  
}#end of step_1b()