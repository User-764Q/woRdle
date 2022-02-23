library(tidyverse)

# Creating Word List for Wordle

### Data Source

# I found a list of all Enlish words on GitHub (here)[https://github.com/dwyl/english-words/blob/master/words_alpha.txt]. 

raw_word_list <- read_csv('https://github.com/dwyl/english-words/blob/master/words_alpha.txt?raw=true', col_names = 'Word')

Words_x_letters <- raw_word_list %>%
  filter(str_length(Word) == Game_Word_Length)

letter_splitter <- function(word) {unlist(strsplit(word, split = ""))}

solution_word <- sample(Words_x_letters$Word, 1) %>% str_to_lower()

solution_letters <- letter_splitter(solution_word)

### User Entry

guess_word <- readline(prompt = paste0('Enter a ', 
                                      Game_Word_Length, 
                                      ' letter word: ')) %>% str_to_lower()

guess_letters <- letter_splitter(guess_word)

is_a_word <- function(word_entry, Words_x_letters = Words_x_letters) {
  
  word_entry %in% Words_x_letters$Word
  
}

position_and_letter_matcher <- function(guess_letters, solution_letters) 
  {guess_letters == solution_letters}

letter_match <- NULL
letter_matcher <- function(guess_letters, solution_letters) { for (l in guess_letters) 
{if (l %in% solution_letters) {
  letter_match <- c(letter_match, TRUE) } else {letter_match <- c(letter_match, FALSE) }}
  letter_match %>% unlist()
  }

guess_resulter <- function(guess_word, solution_word) {
  
  guess_letters <- letter_splitter(guess_word) 
  
  solution_letters <- letter_splitter(solution_word) 
  
  pos_and_ltr_mtch_rslt <- position_and_letter_matcher(guess_letters, 
                                                       solution_letters)
  
  ltr_mtch_rslt <- letter_matcher(guess_letters, 
                                     solution_letters)
  
  guess_result <- rep('0', Game_Word_Length)
  
  if(sum(pos_and_ltr_mtch_rslt) == Game_Word_Length) 
    {guess_result <- 'You WIN'} else if (sum(pos_and_ltr_mtch_rslt) < Game_Word_Length &
      sum(pos_and_ltr_mtch_rslt) >= 0 ) {
        for (i in 1:Game_Word_Length) {
          if(pos_and_ltr_mtch_rslt[i] == TRUE) {
            guess_result[i] <- '*'} else if (ltr_mtch_rslt[i] == TRUE) {
              guess_result[i] <- '+'} else if (ltr_mtch_rslt[i] == FALSE) {
                guess_result[i] <- '_'} else 
                {guess_result[i] <- paste0("Error Error Does Not Compute 1, ",  
                  ltr_mtch_rslt[i],
                  pos_and_ltr_mtch_rslt[i])
                }}} else {guess_result <- paste0('Error Error Does Not Compute 2, ',
                                                 sum(pos_and_ltr_mtch_rslt))}
  paste0(guess_word, ' ', guess_result %>% paste0(collapse = ''))
}


