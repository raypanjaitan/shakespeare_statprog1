# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061 - Points 1-5(d)
# Sanjoi Sethi 

## 3
setwd("D:\\Edu\\Master\\Courses\\Statistical Programming\\Repo\\shakespeare_statprog1") ## comment out of submitted
z <- a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
               fileEncoding="UTF-8") ##import text; create z variable just for debug

## 4(a)
ob <-grep("[", a, fixed=TRUE); ## get indices of words with open bracket
dir <- c() ##initiate variable for words 

for (i in ob) { ## loops
  cb <- grep("]",a[i:i+100],fixed=TRUE) ## get indices of 100 words after i that contains char "]"
  if (length(cb)>0){ ## check if it's found
    a<-a[-c(i:cb[1])] ## remove indices from i to the indice where the close bracket is found
    # icb<-i + cb[1] 
    # dir <- c(dir, i:icb[1])
  }
}

## 4(b)
a <- gsub("\\d+", "",a) ## remove all arabic numerals
a.I <- grepl("\\bI\\b", a) ## get all character "I" coordinate
a.A <- grepl("\\bA\\b", a) ## get all character "A" coordinate
a.uc <- (a == toupper(a)) ## get all uppercase coordinate
a.allowed <- !a.uc | a.I | a.A ## get coordinate of vector without all uppercase, but including character I and A
a <- a[a.allowed] ## get the filtered vector using the result of above variable

## 4(c)
a <- gsub("_", "", a) ## remove underscores

## 4(d)
split_punct <- function(v, punct){
  p <- grep(punct, v) ## get coordinate of vector containing punctuations
  pw <- grep(punct, v, value=TRUE) ## get the word containing punctuations
  
  tl <- rep("",length(p)+length(v)) ## vector to store all words
  tlp <- p+1:length(p) ## coordinate of punctuations
  
  pp <- regexpr(punct, v[p]) ## get coordinate of punctuation in the word
  tl[-tlp] <- gsub(punct, "", v) ## put the punctuations after its word
  tl[tlp] <- substr(v[p], pp, pp) ## put the cleaned words in its original coordinates
  
  return(tl) ##return 
}

## 4(e)
punct <- ",|\\.|;|!|:|\\?" ## punctuations variable, escape character by using double backslash
# punct <- "[,.;!:?]" ## punctuations variable option
a <- split_punct(a, punct) ## run the split_punct function

## 4(f)
a <-tolower(a) ##lowercase version of vector

## 5(a)
b<-unique(a) ## create vector b from unique version of a
## 5(b)
idx <- match(a, b) ## find the index which element in b each element of a corresponds to
## 5(c)
word_counts <- tabulate(idx, nbins = length(b)) ## tabulation for the words vector

## 5(d) Get the top ~1000 most common words
word_rank <- rank(-word_counts, ties.method = "first")  ## rank frequencies
top_1000_idx <- which(word_rank <= 1000)

b <- b[top_1000_idx] ## words
counts_top1000 <- word_counts[top_1000_idx] ## their counts

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