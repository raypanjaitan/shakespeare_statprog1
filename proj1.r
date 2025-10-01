# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi 


setwd("C:/Users/adiku/OneDrive/Desktop/edin/project/shakespeare_statprog1")
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")

##Deleting Stage Directions
open_br<-grep("[", a, fixed=TRUE)
del1<-c()
for (i in open_br){
  close_br=grep("]",a[i:i+100],fixed=TRUE)
  if (length(close_br)>0){
    close_index<-i + close_br[1]-1
    del1 <- c(del1, i:close_index)
  }
}
a_clean=a[-del1] 

##Deleting Character Names
a_upper<-toupper(a_clean)==a_clean & !(a_clean %in% c("I","A"))
a_clean1= a_clean[!a_upper]

##Deleting Roman Numerals--->Arabic Numerals
arabic <- grep("[0-9]", a_clean1, value=FALSE)
a_clean2 <- a_clean1[-arabic]

##Removing dashes
a_clean3<- gsub("_", "", a_clean2)
b=unique(a_clean3)

idx <- match(a_clean3, b)

## 7. Word counts using tabulate
word_counts <- tabulate(idx, nbins = length(b))

## 8. Get the top ~1000 most common words
word_rank <- rank(-word_counts, ties.method = "first")  # rank frequencies
top_1000_idx <- which(word_rank <= 1000)

b_top1000 <- b[top_1000_idx]                  # words
counts_top1000 <- word_counts[top_1000_idx]   # their counts

## 9. (Optional) Sort top words by frequency, descending
ord <- order(counts_top1000, decreasing = TRUE)
b_top1000_sorted <- b_top1000[ord]
counts_top1000_sorted <- counts_top1000[ord]



## Step 6:Create matrix M of word token sequences

# Step 6a: Create token vector for full text using common words in b
# NA if word not in common word list
##text_tokens <- match(a, b)
text_tokens<-match(a_clean3,b_top1000_sorted)

# Step 6b: Create matrix M with shifted token sequences
mlag <- 2  # maximum lag (can be changed)
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


### Step 8: Select a starting word (not punctuation)
### Find tokens that correspond to actual words (not punctuation)
punct_tokens <- match(punct_to_split, b)
non_punct <- text_tokens[!(text_tokens %in% punct_tokens) & !is.na(text_tokens)]

##Randomised starting token
start_token<- sample(non_punct, 1)
