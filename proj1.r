# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi 


setwd("D:\\Edu\\Master\\Courses\\Statistical Programming\\Repo\\shakespeare_statprog1") ## comment out of submitted
z <- a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
               fileEncoding="UTF-8") ##import text; create z variable just for debug

a.dir <- grep("^\\[.*\\]$", a) ## get direction words coordinate
if(length(a.dir) > 0){ a<-a[-a.dir]} ## remove the direction words if it's found

a.ob <-grep("[", a, fixed=TRUE);
a.dir <- c()

for (i in a.ob) {
  cb <- grep("]",a[i:i+100],fixed=TRUE)
  if (length(cb)>0){
    icb<-i + cb[1]-1
    a.dir <- c(a.dir, i:icb)
  }
}
a<-a[-a.dir]

a <- gsub("\\d+", "",a) ## remove all arabic numerals
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

punct <- "[,.;!:?]" ## punctuations variable
# "[[:punct:]]"
a <- split_punct(a, punct) ## run the split_punct function

## table option
# a.freq <- table(a) ## create frequency table of the text
# a.freq.sorted <-sort(a.freq, decreasing=TRUE) ## sort the frequency table
# b <- a.freq.sorted[1:1000] ## get 1000 most common words
##

b<-unique(a)
idx <- match(a, b)
word_counts <- tabulate(idx, nbins = length(b))

## 8. Get the top ~1000 most common words
word_rank <- rank(-word_counts, ties.method = "first")  # rank frequencies
top_1000_idx <- which(word_rank <= 1000)

b <- b[top_1000_idx]                  # words
counts_top1000 <- word_counts[top_1000_idx]   # their counts

## 9. (Optional) Sort top words by frequency, descending
ord <- order(counts_top1000, decreasing = TRUE)
b <- b[ord]
counts_top1000_sorted <- counts_top1000[ord]



## Step 6:Create matrix M of word token sequences

# Step 6a: Create token vector for full text using common words in b
# NA if word not in common word list
##text_tokens <- match(a, b)
text_tokens<-match(a,b)

# Step 6b: Create matrix M with shifted token sequences
mlag <- 4  # maximum lag (can be changed)
n <- length(text_tokens)

# Create matrix with n-mlag rows and mlag+1 columns
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)

# Fill each column with progressively shifted token vectors
for (i in 1:(mlag + 1)) {
  M[, i] <- text_tokens[i:(n - mlag + i - 1)]
}


# Step 7: Function to generate next word token
next.word <- function(key, M, M1, w = rep(1, ncol(M) - 1)) {
  # Generate next word token based on key (word sequence)
  # Input: key - vector of tokens for current word sequence
  #        M - matrix of token sequences
  #        M1 - full text token vector
  #        w - mixture weights for different order models
  # Output: single token for next word
  
  mlag <- ncol(M) - 1  # maximum lag from matrix dimensions
  key_len <- length(key)
  
  # Vectors to collect possible next tokens and their probabilities
  all_next <- c()
  all_probs <- c()
  
  # Try matching from full key down to single word
  for (m in mlag:1) {
    # Determine which part of key to use
    if (key_len >= m) {
      # Use last m words of key
      current_key <- key[(key_len - m + 1):key_len]
    } else if (m <= key_len) {
      # Use available key
      current_key <- key
    } else {
      # Key too short for this order
      next
    }
    
    # Match to appropriate columns in M
    mc <- mlag - m + 1  # starting column
    
    # Find matching rows
    ii <- colSums(!(t(M[, mc:mlag, drop=FALSE]) == current_key))
    matching_rows <- which(ii == 0 & is.finite(ii))
    
    # Get next words from matching rows (last column of M)
    if (length(matching_rows) > 0) {
      next_tokens <- M[matching_rows, mlag + 1]
      # Remove NAs
      next_tokens <- next_tokens[!is.na(next_tokens)]
      
      if (length(next_tokens) > 0) {
        # Add to collection with appropriate probability weight
        all_next <- c(all_next, next_tokens)
        all_probs <- c(all_probs, rep(w[m] / length(next_tokens), 
                                      length(next_tokens)))
      }
    }
  }
  
  # If we found some next words, sample from them
  if (length(all_next) > 0) {
    return(sample(all_next, 1, prob = all_probs))
  } else {
    # No matches found - sample random common word from text
    common_tokens <- M1[!is.na(M1)]
    return(sample(common_tokens, 1))
  }
}

sample <-sample(a_clean, 1)  ##take a random single word token from cleaned text
next.word(sample, M, b, w = rep(1, ncol(M) - 1)) ##run the next.word function
