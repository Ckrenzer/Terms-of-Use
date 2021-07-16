if(!require(pacman)) install.packages("pacman")
pacman::p_load(rvest, stringr, dplyr, readr)

# Scraping ---------------------------------------------------------------
# LinkedIn
# Effective Date: August 11, 2020
# This does not include any of the text with lightbulb icons
url <- "https://www.linkedin.com/legal/user-agreement?trk=hb_ft_userag"
linkedin_tos <- read_html(url) %>% 
  html_elements(".global-paragraph-style--p1") %>% 
  html_text() %>% 
  str_c(collapse = "\n")





# Facebook
# Effective Date: October 22, 2020
# Contains everything from 'Terms of Service' through 'Date of Last Revision:'
url <- "https://www.facebook.com/legal/terms"
facebook_tos <- read_html(url) %>% 
  html_elements(xpath = "//*[@id=\"content\"]/div/div/div[2]/div[2]") %>% 
  html_text()





# YouTube
# Effective Date: March 17, 2021
# Pulls from 'Terms of Service' through 'Effective as of'
url <- "https://www.youtube.com/t/terms"
youtube_tos <- read_html(url) %>% 
  html_elements(".title , #main-content div") %>% 
  html_text() %>% 
  str_c(collapse = "\n")





# Amazon
# Effective Date: May 3, 2021
# Pulls from 'Coditions of Use' though the last bullet point at the bottom
url <- "https://www.amazon.com/gp/help/customer/display.html?nodeId=508088&ref_=footer_cou"
amazon_tos <- read_html(url) %>% 
  html_elements(".cs-help-content") %>% 
  html_text() %>% 
  str_split("\n") %>% 
  unlist()

# The first entry of relevant text
first_entry <- min(which(str_detect(amazon_tos, "Conditions of Use")))
# The last entry of relevant text
last_entry <- which(str_detect(amazon_tos, "copyright owner's behalf\\."))

# Keeping only relevant entries, combining into one string, and replacing
# repeated newline characters
amazon_tos <- amazon_tos[first_entry:last_entry] %>% 
  str_c(collapse = "\n") %>% 
  str_replace_all("\n{2,}", "\n")





# Spotify
# Effective Date: February 7th, 2019
# Pulls 'Spotify Terms and Conditions of Use', then skips to 'Hello, and
# welcome...' through the company address
url <- "https://www.spotify.com/us/legal/end-user-agreement/"
spotify_tos <- read_html(url) %>% 
  html_elements(xpath = "//*[@id=\"content-main\"]/div") %>% 
  html_text() %>% 
  str_remove("Effective[\\s\\S]* 25 Contact us ")





# GitHub
# Effective Date: November 16, 2020
# Would this really be a GitHub project if I didn't check their TOS?
# Pulls from 'Thank you for using GitHub!' through section 6 at the end,
# removing the summary table at the beginning of the TOS and beginning with
# 'A. Definitions'
url <- "https://docs.github.com/en/github/site-policy/github-terms-of-service"
github_tos <- read_html(url) %>% 
  html_elements("#article-contents") %>% 
  html_text() %>% 
  str_remove("Summary[\\s\\S]*Effective date: November 16, 2020")





# DuckDuckGo
# April 11th, 2012???
# This is the privacy policy--I couldn't find DuckDuckGo's TOS, but
# DuckDuckGo HAD to be included
# Manually extracts relevant fields. Includes 'DuckDuckGo does not collect...',
# then skips until 'WHY YOU SHOULD CARE', and goes from there through
# 'I (Gabriel Weinberg)'
url <- "https://duckduckgo.com/privacy"
duckduckgo_tos <- read_html(url) %>% 
  html_elements(".blk__text") %>% 
  html_text() %>% 
  str_split("\n") %>%
  unlist() %>% 
  .[c(7, 19:144)]








# Saving Results ---------------------------------------------------------
# The results will be stored in .txt files
write_lines(x = linkedin_tos, file = "data/linkedin_Terms_of_Service.txt", append = FALSE)
write_lines(x = facebook_tos, file = "data/facebook_Terms_of_Service.txt", append = FALSE)
write_lines(x = youtube_tos, file = "data/youtube_Terms_of_Service.txt", append = FALSE)
write_lines(x = amazon_tos, file = "data/amazon_Terms_of_Service.txt", append = FALSE)
write_lines(x = spotify_tos, file = "data/spotify_Terms_of_Service.txt", append = FALSE)
write_lines(x = github_tos, file = "data/github_Terms_of_Service.txt", append = FALSE)
write_lines(x = duckduckgo_tos, file = "data/duckduckgo_Terms_of_Service.txt", append = FALSE)
