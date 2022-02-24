library(tidyverse)

# Creating Word List for Wordle

### Data Source

# I found a list of all Enlish words on GitHub (here)[https://github.com/dwyl/english-words/blob/master/words_alpha.txt]. 

# raw_word_list <- read_csv('https://github.com/dwyl/english-words/blob/master/words_alpha.txt?raw=true', col_names = 'Word')

Game_Word_Length <- 5

Game_Guess_Length <- 10

Words_x_letters <- raw_word_list %>%
  filter(str_length(Word) == Game_Word_Length)

letter_splitter <- function(word) {unlist(strsplit(word, split = ""))}

solution_word <- sample(Words_x_letters$Word, 1) %>% str_to_lower()

solution_letters <- letter_splitter(solution_word)

guess_letters <- letter_splitter(guess_word)

is_a_word <- function(word_entry, Words_x_letters = Words_x_letters) {
  
  word_entry %in% Words_x_letters$Word
  
}

Guesses <- NULL

n_Guesses <- 0

position_and_letter_matcher <- function(guess_letters, solution_letters) 
  {guess_letters == solution_letters}

letter_match <- NULL

letter_matcher <- function(guess_letters, solution_letters) { for (l in guess_letters) 
{if (l %in% solution_letters) {
  letter_match <- c(letter_match, TRUE) } else {letter_match <- c(letter_match, FALSE) }}
  letter_match %>% unlist()
  }

tried_letters <- NULL

untried_letters <- letters

guess_string <- NULL

result_data_frame <- data.frame('Turn: ' = NULL,
           'Guess:' = NULL, 
           'Green_Letters' = NULL, 
           'Grey_Letters' =  NULL)

guess_resulter <- function(guess_word, solution_word) {
  
  n_Guesses <<- n_Guesses + 1
  
  if(n_Guesses > Game_Guess_Length) {
    print(paste0('You Loose, Solution was ', 
                 solution_word))
    break 
    }

  guess_letters <- letter_splitter(guess_word) 
  
  guess_string <- c(guess_string, guess_letters)
  
  solution_letters <- letter_splitter(solution_word) 
  
  pos_and_ltr_mtch_rslt <- position_and_letter_matcher(guess_letters, 
                                                       solution_letters)
  
  ltr_mtch_rslt <- letter_matcher(guess_letters, 
                                     solution_letters)
  
  guess_result <- rep('0', Game_Word_Length)
  
  grey_letters <- NULL
  
  tried_letters <-  c(tried_letters, guess_letters) %>% 
    letter_splitter() %>% 
    unique() %>% 
    sort() %>%
    paste0(collapse = '')
  
  if(sum(pos_and_ltr_mtch_rslt) == Game_Word_Length) 
    {guess_result <- 'You WIN'} else if (sum(pos_and_ltr_mtch_rslt) < Game_Word_Length &
      sum(pos_and_ltr_mtch_rslt) >= 0 ) {
        for (i in 1:Game_Word_Length) {
          if(pos_and_ltr_mtch_rslt[i] == TRUE) {
            guess_result[i] <- str_to_upper(guess_letters[i])} else if (ltr_mtch_rslt[i] == TRUE) {
              guess_result[i] <- '+'
              grey_letters <- c(grey_letters, guess_letters[i]) %>% 
                               letter_splitter() %>% 
                               unique() %>% 
                               sort() %>%
                               paste0(collapse = '')
              } else if (ltr_mtch_rslt[i] == FALSE) {
                guess_result[i] <- '_'} else 
                {guess_result[i] <- paste0("Error Error Does Not Compute 1, ",  
                  ltr_mtch_rslt[i],
                  pos_and_ltr_mtch_rslt[i])
                }}} else {guess_result <- paste0('Error Error Does Not Compute 2, ',
                                                 sum(pos_and_ltr_mtch_rslt))}
  
  result_line <- data.frame('Turn: ' = n_Guesses,
                            'Guess:' = guess_word, 
                            'Green_Letters' = guess_result %>% paste0(collapse = ''), 
                            'Grey_Letters' =  paste0(grey_letters, collapse = ''), 
                            'Un_tried' = paste0(LETTERS[!letters %in% letter_splitter(tried_letters)], 
                                                collapse = ''))
 
  result_line
  
}

take_a_guess <- function() {
  
  guess_word <- readline('Enter_your_guess: ')

  result_data_frame <<- rbind(result_data_frame, 
                           guess_resulter(guess_word, 
                                          solution_word))
  
  result_data_frame[1:4]
}
