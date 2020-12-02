## S610 Final Project
install.packages("readr")
library(readr)
dubliners <- read_file("http://www.gutenberg.org/files/2814/old/dblnr11.txt")

## split on spaces
## split on , space . space ? space ! space space " " space ' space space ' 
dub_words <- strsplit(dubliners, "\\, | |\\. |\\? |\\! |\" | \"|\' | \'| (|) | [|] |  |  ")
print(dub_words)

## find the unique words
unique_words <- unique(unlist(dub_words))

## create the transition matrix
tmatrix_unique_words <- data.frame(matrix(nrow = length(unique_words),
                                          ncol = length(unique_words)),
                                   row.names = unique_words)
colnames(tmatrix_unique_words) <- unique_words



