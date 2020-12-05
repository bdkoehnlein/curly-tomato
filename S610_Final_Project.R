## S610 Final Project
library(readr)
dubliners <- read_file("http://www.gutenberg.org/files/2814/old/dblnr11.txt")

next_word <- function(input_word, corpus, length, t_matrix = NULL) {
  corp_vector <- unlist(strsplit(corpus, 
                                 "\\, ?| |\\. |\\.|\\? |\\! |\" | \"|\' | \'| (|) | \\[|\\] |  |  "))
  unique_words <- unique(corp_vector)
  
  if (is.null(t_matrix) == T){
    t_matrix <- build_matrix(corp_vector, unique_words)
  }
  
  current_word <- input_word
  words <- c(input_word)
  print(t_matrix)
  for(i in 1:length){
    in_word_i <- which(unique_words == current_word)
    if (length(in_word_i) != 0){
      new_word <- sample(colnames(t_matrix), 1, prob = t_matrix[in_word_i, ])
      words <- c(words, new_word)
      current_word <- new_word
    } else{
      new_word <- sample(colnames(t_matrix), 1)
      words <- c(words, new_word)
      current_word <- new_word
    }
  }
  cat(words)
}

build_matrix <- function(words_vec, unique_words = NULL) {
  if (is.null(unique_words) == T){
    unique_words <- unique(words_vec)
  }
  tmatrix_unique_words <- data.frame(matrix(nrow = length(unique_words),
                                            ncol = length(unique_words)),
                                     row.names = unique_words)
  colnames(tmatrix_unique_words) <- unique_words
  
  for(i in 1:length(unique_words)){
    subsequent_words_i <- which(words_vec == unique_words[i])+1
    if (is.na(words_vec[subsequent_words_i[length(subsequent_words_i)]]) == T){
      subsequent_words_i <- subsequent_words_i[1:(length(subsequent_words_i)-1)]
    }
    subsequent_words <- words_vec[subsequent_words_i]
    unique_sub_words <- unique(subsequent_words)

    sub_word_count <- rep(list(0), length(unique_words))
    
    for (k in 1:length(unique_sub_words)){
      unique_sub_words_i <- which(unique_words == unique_sub_words[k])
      if (length(unique_sub_words_i) != 0){
        sub_word_count[[unique_sub_words_i]] <- sum(subsequent_words == unique_sub_words[k])
      }
    }
    tmatrix_unique_words[i,] <- unlist(sub_word_count)
    tmatrix_unique_words[i,] <- tmatrix_unique_words[i,]*(1/sum(tmatrix_unique_words[i,]))
  }
  return(tmatrix_unique_words)
}

faulkner_sample <- read_file("faulkner.txt")

next_word("send", "one, one, two, one, two, three, four, three, two.", 10)

