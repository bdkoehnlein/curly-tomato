## Tests
source("S610_Final_Project.R")
setwd("/Users/bdkoehnlein/psychic-telegram/curly-tomato")
library(readr)

## test output
paragraph <- "We crossed ourselves and came away. In the little room downstairs
we found Eliza seated in his arm-chair in state. I groped my way towards my 
usual chair in the corner while Nannie went to the sideboard and brought out a 
decanter of sherry and some wine-glasses. She set these on the table and invited 
us to take a little glass of wine. Then, at her sister's bidding, she filled out 
the sherry into the glasses and passed them to us. She pressed me to take some 
cream crackers also but I declined because I thought I would make too much noise 
eating them. She seemed to be somewhat disappointed at my refusal and went over 
quietly to the sofa where she sat down behind her sister. No one spoke: we all 
gazed at the empty fireplace."

## test for transition matrix
next_word("and", paragraph, 5, matrix(c(0, 0, 1,
                                    1, 1, 0), nrow=2))

## test for argument class (correct input)
next_word(5, paragraph, 5, matrix(c(0, 0, 1,
                                      1, 1, 0), nrow=2))

## test for correct output
next_word("and", paragraph, 5)

## test that matrix rows sum to 1
test_vector <- unlist(strsplit(paragraph, ## split on spaces and punctuation
                               ## and turn list into vector
                               "\\, ?| |\\. |\\.|\\? |\\! |\" | \"|\' | \'| (|) | \\[|\\] |  |  |: "))

test_that("Rows Sum to 1", {
  expect_equal(rowSums(build_matrix(test_vector)[1,]), c(1), use.names = FALSE)
})


## Longer Test with "A Rose for Emily" ##

faulkner_sample <- read_file("faulkner.txt") 

next_word("He", faulkner_sample, 20)

smooth_next_words("He walked", faulkner_sample, 20, 2)



