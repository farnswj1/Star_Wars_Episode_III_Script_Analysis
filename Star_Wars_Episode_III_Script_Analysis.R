# Justin Farnsworth
# Star Wars Episode III Script Analysis
# August 26, 2020

# Required packages
if (!require(tidyverse)) { install.packages("tidyverse"); library(tidyverse) }
if (!require(tidytext)) { install.packages("tidytext"); library(tidytext) }
if (!require(R.utils)) { install.packages("R.utils"); library(R.utils) }
if (!require(rvest)) { install.packages("rvest"); library(rvest) }


# Loading The Data
####################################################################################################

# Download the script
# Source: Internet Movie Screenplay Database
url <- "https://www.imsdb.com/scripts/Star-Wars-Revenge-of-the-Sith.html"
html_page <- read_html(url) %>% html_nodes("table")

# Extract the script from the page
script <- html_page[13] %>% html_text

# Remove the HTML page variable
rm(html_page)


# Cleaning The Data
####################################################################################################

# Convert the script into a data frame
script <- unlist(str_split(script, "\\n|\\r|\\t")) %>% 
  as_tibble() %>% 
  rename(Line = value)

# Keep only the dialogue and split the name and quotes into separate columns
script <- script %>% 
  filter(!str_detect(Line, "^$")) %>% 
  filter(!str_detect(Line, "STAR WARS EPISODE 3: REVENGE OF THE SITH SCRIPT")) %>% 
  filter(!str_detect(Line, "Star Wars: Revenge of the Sith")) %>% 
  filter(str_detect(Line, ": "))  %>% 
  separate(Line, into = c("Character", "Line"), sep = ": ", extra = "merge")

# Clean up the character names in the script
# Note: Aliases have been changed to the character's actual name
script <- script %>% 
  mutate(Character = str_replace(Character, "ANAKINN|DARTH VADER", "ANAKIN")) %>% 
  mutate(Character = str_replace(Character, "^BAIL$", "BAIL ORGANA")) %>% 
  mutate(Character = str_replace(Character, "^CLONE PILOT$", "CLONE PILOT 1")) %>% 
  mutate(Character = str_replace(Character, "^DA[BR]TH S[Il]DIOUS", "PALPATINE")) %>% 
  mutate(Character = str_replace(Character, "FANGZAR", "FANG ZAR")) %>% 
  mutate(Character = str_replace(Character, "FlRESHIP PILOT", "FIRESHIP PILOT")) %>% 
  mutate(Character = str_replace(Character, "^G-3PO$|^THREEPIO$", "C-3PO")) %>% 
  mutate(Character = str_replace(Character, "G[il]DDEAN DANU", "GIDDEAN DANU")) %>% 
  mutate(Character = str_replace(Character, "^Kl-ADI-MUNDI$", "KI-ADI-MUNDI")) %>% 
  mutate(Character = str_replace(Character, "^MACE$|^MACE W[il]NDU$", "MACE WINDU")) %>% 
  mutate(Character = str_replace(Character, "^OBI-WAN $", "OBI-WAN")) %>% 
  mutate(Character = str_replace(Character, "QUI ?-GON", "QUI-GON")) %>% 
  mutate(Character = str_replace(Character, "SUPER BATTLE DROID 2", "SUPER BATTLE DROID 3")) %>% 
  mutate(Character = str_replace(Character, "SUPER BATTLE DROID 1", "SUPER BATTLE DROID 2")) %>% 
  mutate(Character = str_replace(Character, "^SUPER BATTLE DROID$", "SUPER BATTLE DROID 1")) %>% 
  mutate(Character = str_replace(Character, "TlON MEDON", "TION MEDON")) %>% 
  filter(!str_detect(Character, "^PALPATINE ")) %>% 
  separate_rows(Character, sep = "/")

# Clean up the lines and remove any parantheses along with text inside them
script <- script %>% 
  mutate(Line = str_replace_all(Line, " ?\\([^\\(\\)]*\\) ?", " ")) %>% 
  mutate(Line = str_replace_all(Line, "\u0085", "…")) %>% 
  mutate(Line = str_trim(str_replace_all(Line, "\u0092", "’")))

# Show the script
script

# Show the total number of lines for each character
script %>% 
  count(Character, name = "Total") %>% 
  arrange(desc(Total))


# Sentiment Analysis - Bing
####################################################################################################

# Tokenize the lines, filter out stop words, and merge the Bing sentiments to the script
script_tokenized_bing <- script %>% 
  unnest_tokens(
    Word, 
    Line, 
    token = "regex", 
    pattern = "([^A-Za-z\\d'-]|'(?![A-Za-z\\d]))"
  ) %>% 
  filter(!(Word %in% stop_words$word)) %>% 
  left_join(
    rename(get_sentiments("bing"), Word = word, Sentiment = sentiment), 
    by = "Word"
  ) %>% 
  mutate(Sentiment = capitalize(Sentiment))

# Show the 30 most commonly spoken words in the movie
script_tokenized_bing %>% 
  count(Word, name = "Total") %>% 
  arrange(desc(Total)) %>% 
  slice(1:30) %>% 
  print(n = 30)

# Show the 20 most common positive words in the movie
script_tokenized_bing %>% 
  filter(Sentiment == "Positive") %>% 
  count(Word, name = "Total") %>% 
  arrange(desc(Total)) %>% 
  slice(1:20) %>% 
  print(n = 20)

# Show the 20 most common negative words in the movie
script_tokenized_bing %>% 
  filter(Sentiment == "Negative") %>% 
  count(Word, name = "Total") %>% 
  arrange(desc(Total)) %>% 
  slice(1:20) %>% 
  print(n = 20)

# Show the total number of positive and negative sentiments
script_tokenized_bing %>% 
  filter(!is.na(Sentiment)) %>% 
  count(Sentiment, name = "Total") %>% 
  arrange(Total)

# Plot the total number of positive and negative sentiments
script_tokenized_bing %>% 
  filter(!is.na(Sentiment)) %>% 
  count(Sentiment, name = "Total") %>% 
  ggplot(aes(reorder(Sentiment, Total), Total)) + 
  geom_bar(aes(fill = Sentiment), stat = "identity", show.legend = FALSE) + 
  ggtitle("Frequency of Positive vs. Negative Sentiments (Bing)") + 
  labs(
    x = "Sentiment", 
    y = "Frequency", 
    caption = "Star Wars Episode III - Revenge of the Sith"
  ) + 
  theme_bw()

# Show the number of positive and negative sentiments for each character
script_tokenized_bing %>% 
  count(Character, Sentiment, name = "Total") %>% 
  spread(Sentiment, Total) %>% 
  rename("NA" = `<NA>`) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(Spread = Positive - Negative) %>% 
  arrange(desc(Spread)) %>% 
  print(n = Inf)

# Plot the sentiments of the 9 most important characters
script_tokenized_bing %>% 
  filter(
    Character %in% c(
      "ANAKIN", "OBI-WAN", "PADME", "PALPATINE", "YODA", 
      "MACE WINDU", "C-3PO", "COUNT DOOKU", "GENERAL GRIEVOUS"
    ) & !is.na(Sentiment)
  ) %>% 
  count(Character, Sentiment, name = "Total") %>% 
  ggplot(aes(Sentiment, Total)) + 
  geom_col(aes(fill = Sentiment), show.legend = FALSE) + 
  facet_wrap(~Character, scales = "free_x") + 
  ggtitle("Frequency of Positive vs. Negative Sentiments By Character (Bing)") + 
  labs(y = "Frequency", caption = "Star Wars Episode III - Revenge of the Sith") + 
  coord_flip() + 
  theme_bw()

# Plot the most commonly used words by each of the most important characters
script_tokenized_bing %>% 
  filter(
    Character %in% c(
      "ANAKIN", "OBI-WAN", "PADME", "PALPATINE", "YODA", 
      "MACE WINDU", "C-3PO", "COUNT DOOKU", "GENERAL GRIEVOUS"
    )
  ) %>% 
  count(Character, Word, name = "Total") %>% 
  group_by(Character) %>% 
  arrange(desc(Total)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(
    Word = factor(
      paste(Word, Character, sep = "_"), 
      levels = rev(paste(Word, Character, sep = "_"))
    )
  ) %>% 
  ggplot(aes(Word, Total)) + 
  geom_col(aes(fill = Character), show.legend = FALSE) + 
  facet_wrap(~Character, scales = "free_y") + 
  ggtitle("Most Commonly Spoken Words By Character") + 
  labs(y = "Frequency", caption = "Star Wars Episode III - Revenge of the Sith") + 
  scale_x_discrete(labels = function(word) str_replace(word, "_.+$", "")) + 
  coord_flip() + 
  theme_bw()

# Plot the most relevant words for each of the most important characters
script_tokenized_bing %>% 
  filter(
    Character %in% c(
      "ANAKIN", "OBI-WAN", "PADME", "PALPATINE", "YODA", 
      "MACE WINDU", "C-3PO", "COUNT DOOKU", "GENERAL GRIEVOUS"
    )
  ) %>% 
  count(Character, Word, name = "Total") %>% 
  bind_tf_idf(Word, Character, Total) %>% 
  group_by(Character) %>% 
  arrange(desc(tf_idf)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(
    Word = factor(
      paste(Word, Character, sep = "_"), 
      levels = rev(paste(Word, Character, sep = "_"))
    )
  ) %>% 
  ggplot(aes(Word, tf_idf)) + 
  geom_col(aes(fill = Character), show.legend = FALSE) + 
  facet_wrap(~Character, scales = "free_y") + 
  ggtitle("Most Relevant Words For Each Character (Bing)") + 
  labs(y = "Frequency", caption = "Star Wars Episode III - Revenge of the Sith") + 
  scale_x_discrete(labels = function(word) str_replace(word, "_.+$", "")) + 
  coord_flip() + 
  theme_bw()


# Sentiment Analysis - NRC
####################################################################################################

# Tokenize the lines, filter out stop words, and merge the NRC sentiments to the script
script_tokenized_nrc <- script %>% 
  unnest_tokens(
    Word, 
    Line, 
    token = "regex", 
    pattern = "([^A-Za-z\\d'-]|'(?![A-Za-z\\d]))"
  ) %>% 
  filter(!(Word %in% stop_words$word)) %>% 
  left_join(
    rename(get_sentiments("nrc"), Word = word, Sentiment = sentiment), 
    by = "Word"
  ) %>% 
  mutate(Sentiment = capitalize(Sentiment))

# Show the frequency of each sentiment in the movie
script_tokenized_nrc %>% 
  filter(!is.na(Sentiment)) %>% 
  count(Sentiment, name = "Total") %>% 
  arrange(desc(Total))

# Plot the frequency of each sentiment in the movie
script_tokenized_nrc %>% 
  filter(!is.na(Sentiment)) %>% 
  count(Sentiment, name = "Total") %>% 
  ggplot(aes(reorder(Sentiment, desc(Total)), Total)) + 
  geom_bar(aes(fill = Sentiment), stat = "identity", show.legend = FALSE) + 
  ggtitle("Frequency of Sentiment Types (NRC)") + 
  labs(
    x = "Sentiment", 
    y = "Frequency", 
    caption = "Star Wars Episode III - Revenge of the Sith"
  ) + 
  theme_bw()

# Show the frequency of each sentiments for each character
script_tokenized_nrc %>% 
  count(Character, Sentiment, name = "Total") %>% 
  spread(Sentiment, Total) %>% 
  rename("NA" = `<NA>`) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(
    Spread = Anticipation + Joy + Positive + Surprise + Trust 
             - Anger - Disgust - Fear - Negative - Sadness
  ) %>% 
  arrange(desc(Spread)) %>% 
  print(n = Inf)

# Plot the sentiments of the 9 most important characters
script_tokenized_nrc %>% 
  filter(
    Character %in% c(
      "ANAKIN", "OBI-WAN", "PADME", "PALPATINE", "YODA", 
      "MACE WINDU", "C-3PO", "COUNT DOOKU", "GENERAL GRIEVOUS"
    ) & !is.na(Sentiment)
  ) %>% 
  count(Character, Sentiment, name = "Total") %>% 
  ggplot(aes(reorder(Sentiment, desc(Sentiment)), Total)) + 
  geom_col(aes(fill = Sentiment), show.legend = FALSE) + 
  facet_wrap(~Character, scales = "free_x") + 
  ggtitle("Frequency of Sentiment Types By Character (NRC)") + 
  labs(
    x = "Sentiment", 
    y = "Frequency", 
    caption = "Star Wars Episode III - Revenge of the Sith"
  ) + 
  coord_flip() + 
  theme_bw()

# Plot the most commonly used words by sentiment
script_tokenized_nrc %>% 
  count(Sentiment, Word, name = "Total") %>% 
  group_by(Sentiment) %>% 
  arrange(desc(Total)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(
    Word = factor(
      paste(Word, Sentiment, sep = "_"), 
      levels = rev(paste(Word, Sentiment, sep = "_"))
    )
  ) %>% 
  ggplot(aes(Word, Total)) + 
  geom_col(aes(fill = Sentiment), show.legend = FALSE) + 
  facet_wrap(~Sentiment, scales = "free_y") + 
  ggtitle("Most Commonly Spoken Words By Sentiment (NRC)") + 
  labs(y = "Frequency", caption = "Star Wars Episode III - Revenge of the Sith") + 
  scale_x_discrete(labels = function(word) str_replace(word, "_.+$", "")) + 
  coord_flip() + 
  theme_bw()

# Plot the most relevant words for each of the most important characters
script_tokenized_nrc %>% 
  filter(
    Character %in% c(
      "ANAKIN", "OBI-WAN", "PADME", "PALPATINE", "YODA", 
      "MACE WINDU", "C-3PO", "COUNT DOOKU", "GENERAL GRIEVOUS"
    )
  ) %>% 
  count(Character, Word, name = "Total") %>% 
  bind_tf_idf(Word, Character, Total) %>% 
  group_by(Character) %>% 
  arrange(desc(tf_idf)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(
    Word = factor(
      paste(Word, Character, sep = "_"), 
      levels = rev(paste(Word, Character, sep = "_"))
    )
  ) %>% 
  ggplot(aes(Word, tf_idf)) + 
  geom_col(aes(fill = Character), show.legend = FALSE) + 
  facet_wrap(~Character, scales = "free_y") + 
  ggtitle("Most Relevant Words For Each Character (NRC)") + 
  labs(y = "Frequency", caption = "Star Wars Episode III - Revenge of the Sith") + 
  scale_x_discrete(labels = function(word) str_replace(word, "_.+$", "")) + 
  coord_flip() + 
  theme_bw()
