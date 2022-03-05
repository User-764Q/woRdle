library(tidyverse)

### Data Source
# https://www.keithv.com/software/wlist/ I used the highest quality word list I could find here 

raw_word_list <- read_csv('wlist_match12.txt', col_names = 'Word')

Words_5_letters <- raw_word_list %>%
  filter(str_length(Word) == 5) %>%
  mutate(word = str_to_lower(Word)) %>%
  select(2)

wordle_cheater <- function() {
  
  possibilities <- Words_5_letters
  
  green_letter_pattern <- readline('Enter Green Letters With Dots For The Other Letters,\n e.g. ".pple" or press enter to skip \n: ')
  
  possibilities_green_filtered <- possibilities %>%
    filter(str_detect(word, green_letter_pattern))
  
  possibilities_green_filtered
  
  orange_letters <- readline('Enter all orange letters you have,\n e.g. "abk" or press enter to skip\n: ')
  
  orange_letters_split <- str_split(orange_letters, '')[[1]]
  
  possibilities_orange_filtered <- possibilities_green_filtered
  
  for (l in orange_letters_split) {
    possibilities_orange_filtered <- possibilities_orange_filtered %>%
      filter(str_detect(word, l))
  }
  
  possibilities_grey_filtered <- possibilities_orange_filtered
  
  grey_letters <- readline('Enter all grey letters you have,\n e.g. "abk" or press enter to skip\n: ')
  
  grey_letters_split <- str_split(grey_letters, '')[[1]]
  
  for (l in grey_letters_split) {
    possibilities_grey_filtered <- possibilities_grey_filtered %>%
      filter(!str_detect(word, l))
  }
  
  possibilities_grey_filtered
  
}


