# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi 


setwd("D:\\Edu\\Master\\Courses\\Statistical Programming\\Repo\\shakespeare_statprog1") ## comment out of submitted
z <- a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8") ##import text; create z variable just for debug

a.dir <- grep("^\\[.*\\]$", a) ## get direction words coordinate
if(length(a.dir) > 0){ a<-a[-a.dir]} ## remove the direction words if it's found
a.I <- grepl("\\bI\\b", a) ## get all character "I" coordinate
a.A <- grepl("\\bA\\b", a) ## get all character "A" coordinate
a.uc <- (a == toupper(a)) ## get all uppercase coordinate
a.allowed <- !a.uc | a.I | a.A ## get coordinate of vector without all uppercase, but including character I and A
a <- a[a.allowed] ## get the filtered vector using the result of above variable
a <- gsub("_", "", a) ## remove underscores

split_punct <- function(v, punct){
  p <- grep(punct, v) ## get coordinate of vector containing punctuations
  pw <- grep(punct, v, value=TRUE) ## get the word containing punctuations
  
  tl <- rep("",length(p)+length(v)) ## vector to store all words
  tlp <- p+1:length(p) ## coordinate of punctuations
  
  pp <- regexpr(punct, v[p]) ## get coordinate of punctuation in the word
  tl[-tlp] <- gsub(punct, "", v) ## put the punctuations after its word
  tl[tlp] <- substr(v[p], pp, pp) ## put the cleaned words in its original coordinates

  return(tolower(tl)) ##return lowercase version of vector
}

punct <- c(",", ".", ";", "!", ":", "?") ## punctuations variable
# "[[:punct:]]"
a <- split_punct(a, punct) ## run the split_punct function

a.freq <- table(a) ## create frequency table of the text
a.freq.sorted <-sort(a.freq, decreasing=TRUE) ## sort the frequency table
test <- test[1:1000] ## get 1000 most common words

## tabulate option, but doesn't work
# u <-unique(a)
# a.match <-match(a, u)
# a.count <- tabulate(a.match)
# b <- rank(a.count)
##