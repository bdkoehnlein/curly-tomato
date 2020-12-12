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



#########################
## Weighted Next Words ##
#########################

smooth_next_words <- function(input_string, corpus, length, m = 1, t_matrix = NULL){
  
  if(class(input_string) != "character" && class(corpus) != "character"){
    stop("arguments not correct class (characters)")
  }
  
  corp_vector <- unlist(strsplit(corpus,
                                 "\\, ?| |\\. |\\.|\\? |\\! |\" | \"|\' | \'| (|) | \\[|\\] |  |  |: "))
  
  ## Change the input string into a vector and make sure that it's at least as long as m
  input_vec <- unlist(strsplit(input_string, " "))
  
  if (length(input_vec) < m){
    length <- m - length(input_vec)
    extra_words <- next_word(input_vec[length(input_vec)], corpus, length)
    input_vec <- c(input_vec, extra_words)
  }
  
  ## make a vector with every string of m words
  strings_vec <- c()
  for (i in 1:length(corp_vector)){
    current_i <- i+m-1
    strings_vec[i] <- paste(corp_vector[i:current_i], collapse =  " ")
  }
  strings_vec <- strings_vec[-((length(strings_vec)-(m-1)):length(strings_vec))]
  
  ## extract the unique strings
  unique_strings <- unique(strings_vec)
  
  if (is.null(t_matrix) == T){
    t_matrix <- build_matrix(strings_vec, unique_strings) 
  }
  
  if(rowSums(t_matrix) != 1 
     && length(unique_words) != nrow(t_matrix)
     && length(unique_words) != ncol(t_matrix)){
    stop("t_matrix is not a transition matrix")
  }
  
  ## Now, sample new words from the matrix
  current_string <- paste(input_vec, collapse =  " ")
  vec <- c(input_vec) 
  for(i in 1:length){
    in_string_i <- which(unique_strings == current_string)

    ## See if the initial string is in the text
    if (length(in_string_i) != 0){
      new_string <- sample(colnames(t_matrix), 1, prob = t_matrix[in_string_i, ])
      new_vec <- unlist(strsplit(new_string, " "))
      new_word <- new_vec[m]
      vec <- c(vec, new_word)
      current_string <- paste(vec[(i+1):(i+m)], collapse =  " ")
      
      ## If not sample uniformly from the unique words
    } else{
      new_string <- sample(colnames(t_matrix), 1) 
      new_vec <- unlist(strsplit(new_string, " "))
      new_word <- new_vec[m]
      vec <- c(vec, new_word)
      current_string <- paste(vec[(i+1):(i+m)], collapse = " ")
    }
  }
  cat(vec)
}


