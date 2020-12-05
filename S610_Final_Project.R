## S610 Final Project
library(readr)
dubliners <- read_file("http://www.gutenberg.org/files/2814/old/dblnr11.txt")

next_word <- function(input_word, corpus, length, t_matrix = NULL) {
  corp_vector <- unlist(strsplit(corpus, ## split on spaces and punctuation
                                 ## and turn list into vector
                                 "\\, ?| |\\. |\\.|\\? |\\! |\" | \"|\' | \'| (|) | \\[|\\] |  |  "))
  unique_words <- unique(corp_vector) ## find the unique words
  
  if (is.null(t_matrix) == T){ ## if don't define matrix in yourself, it'll 
    t_matrix <- build_matrix(corp_vector, unique_words) 
    ## create one using build_matrix
  }
  current_word <- input_word ## starts as your input word
  words <- c(input_word) ## make vector of words that contains output
  for(i in 1:length){
    ## cycle through number of words in the output
    in_word_i <- which(unique_words == current_word) ## search out vector of 
    ## unique words for the input word
    if (length(in_word_i) != 0){ ## if put word in that exists in the text
      new_word <- sample(colnames(t_matrix), 1, prob = t_matrix[in_word_i, ])
      ## sample from the columns of
      ## the t_matrix, sample one word according to probabilities of 
      ## the input words row in the transition matrix 
      words <- c(words, new_word) ## add word we sampled to words
      current_word <- new_word ## cycle again with resetting current word to
      ## new word
    } else{ ## don't necessarily have to put a word in that exists in the text
      ## so samples randomly
      new_word <- sample(colnames(t_matrix), 1) 
      words <- c(words, new_word) 
      current_word <- new_word 
    }
  }
  cat(words) ## takes the word vector and prints them out as single string
}

build_matrix <- function(words_vec, unique_words = NULL) {
  if (is.null(unique_words) == T){ ## if don't give it unique words, it'll
    unique_words <- unique(words_vec) 
    ## create unique list of words and build_matrix requires unique words
  }
  tmatrix_unique_words <- data.frame(matrix(nrow = length(unique_words),
                                            ncol = length(unique_words)),
                                     row.names = unique_words) 
  ## make empty data frame based on number of unique words
  colnames(tmatrix_unique_words) <- unique_words 
  for(i in 1:length(unique_words)){
    subsequent_words_i <- which(words_vec == unique_words[i])+1
    ## finding index of every word that comes after every word
    if (is.na(words_vec[subsequent_words_i[length(subsequent_words_i)]]) == T){
      subsequent_words_i <- subsequent_words_i[1:(length(subsequent_words_i)-1)]
      ## since by find the next word by doing word + 1, once get to last word,
      ## there's nothing at that index + 1, so checking for if have last word
      ## if have last word, take it out for making these frequencies
    } 
    subsequent_words <- words_vec[subsequent_words_i] 
    ## vector of subsequent words
    unique_sub_words <- unique(subsequent_words) ## find unique subsequent words
    sub_word_count <- rep(list(0), length(unique_words)) ## empty list that is 
    ## the length of unique subsequent words for each unique word
    for (k in 1:length(unique_sub_words)){
      unique_sub_words_i <- which(unique_words == unique_sub_words[k])
      ## taking index of all subsequent words
      if (length(unique_sub_words_i) != 0){
        sub_word_count[[unique_sub_words_i]] <- sum(subsequent_words == 
                                                      unique_sub_words[k])
        ## count everything that appears in unique subsequent words
      }
    }
    tmatrix_unique_words[i,] <- unlist(sub_word_count)
    ## populate row of each unique word with count of subsequent words
    tmatrix_unique_words[i,] <- tmatrix_unique_words[i,]*
      (1/sum(tmatrix_unique_words[i,]))
    ## divide by total of that row
  }
  return(tmatrix_unique_words) 
}

paragraph <- "We crossed ourselves and came away. In the little room downstairs
we found Eliza seated in his arm-chair in state. I groped my way
towards my usual chair in the corner while Nannie went to the
sideboard and brought out a decanter of sherry and some
wine-glasses. She set these on the table and invited us to take a
little glass of wine. Then, at her sister's bidding, she filled out the
sherry into the glasses and passed them to us. She pressed me to
take some cream crackers also but I declined because I thought I
would make too much noise eating them. She seemed to be
somewhat disappointed at my refusal and went over quietly to the
sofa where she sat down behind her sister. No one spoke: we all
gazed at the empty fireplace."

next_word("and", paragraph, 5)



