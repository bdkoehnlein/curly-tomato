## S610 Final Project

##############################
## Word Generating Function ##
##############################

next_word <- function(input_word, corpus, length, t_matrix = NULL) {
  
  ## Testing the arguments
  if(class(input_word) != "character" && class(corpus) != "character"){
    stop("arguments not correct class (characters)")
  }
  
  ## Change text into a vector
  corp_vector <- unlist(strsplit(corpus,
                                 "\\, ?| |\\. |\\.|\\? |\\! |\" | \"|\' | \'| (|) | \\[|\\] |  |  |: "))
  
  ## find the unique words
  unique_words <- unique(corp_vector) 
  
  ## Check for a given matrix in the arguments, or pass to the matrix function
  if (is.null(t_matrix) == T){
    t_matrix <- build_matrix(corp_vector, unique_words) 
  }
  
  ## Test the matrix
  if(rowSums(t_matrix) != 1 
     && length(unique_words) != nrow(t_matrix)
     && length(unique_words) != ncol(t_matrix)){
    stop("t_matrix is not a transition matrix")
  }
  
  ## Now, sample new words from the matrix
  current_word <- input_word
  words <- c(input_word) 
  for(i in 1:length){
    in_word_i <- which(unique_words == current_word)
    
    ## See if the initial word is in the text
    if (length(in_word_i) != 0){
      new_word <- sample(colnames(t_matrix), 1, prob = t_matrix[in_word_i, ])

      words <- c(words, new_word)
      current_word <- new_word
      
      ## If not sample uniformly from the unique words
    } else{
      new_word <- sample(colnames(t_matrix), 1) 
      words <- c(words, new_word) 
      current_word <- new_word 
    }
  }
  
  ## Print out our new string!
  cat(words)
}

################################
## Matrix Generating Function ##
################################

build_matrix <- function(words_vec, unique_words = NULL) {
  
  ## Check for a given vector of unique words, or make one
  if (is.null(unique_words) == T){ 
    unique_words <- unique(words_vec) 
  }
  
  ## Build the structure of the matrix and name the rows and columns
  tmatrix_unique_words <- data.frame(matrix(nrow = length(unique_words),
                                            ncol = length(unique_words)),
                                     row.names = unique_words)
  colnames(tmatrix_unique_words) <- unique_words 
  
  ## now populate the matrix with relative frequencies of 
    # Subsequent words 
  for(i in 1:length(unique_words)){
    
    ## Get the indecies of unique words
    subsequent_words_i <- which(words_vec == unique_words[i])+1
    
    ## Eliminate the index after the last word
    if (is.na(words_vec[subsequent_words_i[length(subsequent_words_i)]]) == T){
      subsequent_words_i <- subsequent_words_i[1:(length(subsequent_words_i)-1)]
    } 
    
    ## Find subsequent words
    subsequent_words <- words_vec[subsequent_words_i] 
    unique_sub_words <- unique(subsequent_words)
    sub_word_count <- rep(list(0), length(unique_words))
    
    ## And count them up!
    for (k in 1:length(unique_sub_words)){
      unique_sub_words_i <- which(unique_words == unique_sub_words[k])
      if (length(unique_sub_words_i) != 0){
        sub_word_count[[unique_sub_words_i]] <- sum(subsequent_words == 
                                                      unique_sub_words[k])
      }
    }
    
    ## Fill in the rows
    tmatrix_unique_words[i,] <- unlist(sub_word_count)
    
    ## and divide
    tmatrix_unique_words[i,] <- tmatrix_unique_words[i,]*
      (1/sum(tmatrix_unique_words[i,]))
  }
  return(tmatrix_unique_words) 
}





