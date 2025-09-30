# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi 


setwd("D:\\Edu\\Master\\Courses\\Statistical Programming\\Repo\\shakespeare_statprog1") ## comment out of submitted
b <- a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")

b.dir <- grep("^\\[.*\\]$", b) ## get direction words coordinate
if(length(b.dir) > 0){ b<-b[-b.dir]} ## remove the direction words if it's found
b.I <- grepl("\\bI\\b", b) ## get all character "I" coordinate
b.A <- grepl("\\bA\\b", b) ## get all character "A" coordinate
b.uc <- (b == toupper(b)) ## get all uppercase coordinate
b.allowed <- !b.uc | b.I | b.A ## get coordinate of vector without all uppercase, but including character I and A
b <- b[b.allowed] ## get the filtered vector using the result of above variable
b <- gsub("_", "", b) ## remove underscores

punct <- c(",", ".", ";", "!", ":", "?")
# "[[:punct:]]"
split_punct <- function(v, punct){
  p <- grep(punct, v) ## get coordinate of vector containing punctuations
  pw <- grep(punct, v, value=TRUE) ## get the word containing punctuations
  
  tl <- rep("",length(p)+length(v)) ## vector to store all words
  tlp <- p+1:length(p) ## coordinate of punctuations
  
  pp <- regexpr(punct, v[p]) ## get coordinate of punctuation in the word
  tl[-tlp] <- gsub(punct, "", v) ## put the punctuations after its word
  tl[tlp] <- substr(v[p], pp, pp) ## put the cleaned words in its original coordinates

  return(tolower(tl))
}

b <- split_punct(b, punct)
b.freq <- table(b) #create frequency table of the text
test <-sort(b.freq, decreasing=TRUE)
test <- test[1:1000]