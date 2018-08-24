
#**********************************************
# EXPLORING THE TIDYTEXT PACKAGE 
#**********************************************

library("tidytext") 
library("tidyverse")
library("janeaustenr")
library("tokenizers")

# load jane austen's books: 
# ?str_detect
# ?cumsum
# ?regex

orig.books <- austen_books() %>% 
      group_by(book) %>% 
      mutate(linenum = row_number(), 
             chapter = cumsum(str_detect(text, 
                                         regex("^chapter [\\divxlc]", 
                                               ignore_case = TRUE)))) %>% 
      ungroup()

# examine result: 
orig.books



#*********************************************************
# now tidy with tidytext::unnest_tokens()
# ?unnest_tokens

tidybooks <- orig.books %>% 
      unnest_tokens(word, text)

str(tidybooks)

# alternative using tokenizers::tokenize_word_stems(): 
# ?tokenize_word_stems

tidybooks2 <- orig.books$text %>% 
      tokenize_word_stems(simplify = TRUE)

tidybooks2[1:100]
str(tidybooks2)  # List of 73422
