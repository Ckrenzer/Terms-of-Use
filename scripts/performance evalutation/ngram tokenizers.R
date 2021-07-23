# Your unnest_tokens() implementation
pacman::p_load(stringr, data.table, tidytext, dplyr, microbenchmark)
# Four implementations--any functions you don't recognize belong to data.table


# The text
"words in a brief sentence"
# The text as a one-column data frame
df <- tibble(value = "words in a brief sentence")



# Fastest unigram tokenizer ----------------------------------------------
# To get unigrams, we need to split the words up into a vector
microbenchmark(
  a = str_split("words in a brief sentence", "\\s+")[[1]],
  
  b = unlist(str_split("words in a brief sentence", "\\s+")),
  
  c = str_split("words in a brief sentence", "\\s+") %>% unlist,
  
  times = 10000, check = "identical"
)

# INDEXING IS FASTER THAN COERCION
#
# (microseconds)
# expr  min   lq     mean median   uq    max neval
#    a 13.4 13.9 14.58253   14.1 14.4  101.3 10000
#    b 14.1 14.7 15.41320   14.9 15.2   77.1 10000
#    c 17.2 17.8 19.17496   18.1 18.5 4196.9 10000






# N-gram Algorithms ------------------------------------------------------
# Works for bigrams only
f1 <- function(words = df$value){
  # the unigrams
  words <- str_split(words, "\\s+")[[1]]
  
  tibble(text = str_c(head(words, -1), tail(words, -1), sep = " ", collapse = NULL))
}



# My loop implementation (no one helped me put this together)
f2 <- function(words = df$value, n = 2){
  # making unigrams
  words <- str_split(words, "\\s+")[[1]]
  n <- n - 1
  
  vec <- character(length(words) - n)
  for(i in 1:length(vec)){
    for(j in i:(i + n)){
      vec[i] <- str_c(vec[i], words[j], sep = " ") 
    }
  }
  vec <- str_remove(vec, "^\\s{1}")
  return(tibble(text = vec))
}



# Does not work for unigrams
f3 <- function(words = df$value, n = 2){
  # the unigrams
  words <- str_split(words, "\\s+")[[1]]
  n <- n - 1
  
  lst1 <- lapply(shift(words, n = 0:n, type = 'lead'), na.omit)
  mn <- min(lengths(lst1))
  grams <- do.call(paste,  lapply(lst1, head, mn))
  
  tibble(text = grams)
}



# tidytext implementation
f4 <- function(data_frame = df){
  data_frame %>% 
    unnest_tokens(input = value, output = text, token = "ngrams", n = 2, drop = TRUE)
}







# PERFORMANCE EVALUTATION ------------------------------------------------
microbenchmark(
  f1(),
  
  f2(),
  
  f3(),
  
  f4(),
  
  times = 1000, check = "identical"
)

# BEFORE REQUIRING ALL FUNCTIONS TO RETURN IDENTICAL OUTPUT
# (f's 1, 2, and 3 returned character vectors)
#
# (microseconds)
# expr   min      lq      mean  median     uq     max neval
# f1()  33.4   45.20   58.3125   53.50   63.3   302.8  1000
# f2() 152.7  172.40  207.4807  191.50  209.1  1085.5  1000
# f3()  75.7   95.15  115.6706  108.25  121.1   926.4  1000
# f4() 970.4 1016.30 1211.6472 1056.65 1214.0 20109.6  1000


# AFTER REQUIRING ALL FUNCTIONS TO REUTRN IDENTICAL OUTPUT
#
# (microseconds)
# expr   min      lq      mean  median      uq     max neval
# f1() 456.4  477.85  603.8260  506.70  582.45 15474.9  1000
# f2() 601.3  636.35  855.5795  675.45  772.50 16002.6  1000
# f3() 496.4  525.05  683.0014  555.25  655.85  5008.8  1000
# f4() 969.8 1086.25 1337.5965 1182.95 1310.70  5609.6  1000






# Mock data containing a document ID field
df <- tibble(company = c("a", "b"), value = c("words in a brief sentence", "green eggs and ham"))


# This version is an expansion to f2() that preserves document ID
f5 <- function(text_df, key_column = "company", text_column = "value", n = 2){
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

microbenchmark(
  f5 = f5(text_df = df, n = 2),
  f4 = f4(),
  
  times = 10000, check = "identical"
)

# TIDYTEXT'S REDEMPTION
#
# (microseconds)
# expr    min     lq     mean  median      uq      max neval
#   f5 2346.2 2473.5 2833.771 2569.15 2757.40  60182.7 10000
#   f4  969.4 1051.4 1245.736 1139.80 1216.95 263826.1 10000


# Preserving document IDs is far more expensive than dropping it,
# and tidytext's implementation is much faster than a for loop
# equivalent when this preservation is kept.
#
# for loops in R are slow...who would have guessed!
# (jk--f5() would benefit from some optimization, though)