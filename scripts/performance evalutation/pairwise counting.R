# Setup ------------------------------------------------------------------
# Packages
pacman::p_load(stringr, dplyr, widyr, readtext, microbenchmark)

# Sources scripts
sourceDir <- function(path, trace = TRUE, ...) {
  op <- options(); on.exit(options(op)) # to reset after each 
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
    options(op)
  }
}

# Loading in helper functions and related objects
sourceDir(path = "scripts/functions/")


# The data frame containing the terms of use
tos <- readtext(file = "data/") %>%
  as_tibble() %>% 
  rename(company = doc_id) %>% 
  mutate(company = str_remove_all(company, "_Terms_of_Service.txt"),
         text = str_to_lower(text),
         text = str_replace_all(text, "[â€œ§™“]", " ")) %>% 
  mutate(text = str_replace_all(text, "[,/\\\\.:;()\"\\[\\]&]|^\\s+-\\s+|\u009d", " ")) %>% 
  ngrams(text_df = ., key_column = "company", text_column = "text", n = 8) %>% 
  mutate(skipgram_id = row_number()) %>% 
  unite(col = id, sep = "_", company, skipgram_id) %>% 
  ngrams(text_df = ., key_column = "id", text_column = "text", n = 1) %>% 
  pair_counts(df = ., doc_id = "company", text_column = "text")




# Tests ------------------------------------------------------------------
microbenchmark::microbenchmark(
  manual = pair_counts(df = pairs, doc_id = "company", text_column = "text"),
  
  professional = widyr::pairwise_count(tbl = pairs, item = text, feature = company),
  
  
  times = 35, unit = "s"
)

# Widyr is almost 25 times faster--and probably more memory-efficient
#
# (seconds)
#         expr       min        lq     mean    median       uq       max neval
#       manual 3.4536420 3.7091412 3.865805 3.8800228 4.039974 4.1866493    35
# professional 0.1387029 0.1484858 0.167379 0.1563218 0.167190 0.4169272    35


