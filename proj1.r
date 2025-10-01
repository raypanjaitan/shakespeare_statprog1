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

# Step 5: Find unique words and create common word vocabulary
# Step 5a: Get vector of all unique words in text
unique_words <- unique(a_clean3)

# Step 5b: Create index vector mapping each word in a to unique_words
# Result is same length as a, with values giving position in unique_words
word_index <- match(a, unique_words)

# Step 5c: Count occurrences of each unique word using tabulate
word_counts <- tabulate(word_index)

# Step 5d: Extract the approximately 1000 most common words
# rank gives ranks with highest count = lowest rank number
word_ranks <- rank(-word_counts)  ## negate so highest count gets rank 1
top_n <- 1000
common_words <- unique_words[word_ranks <= top_n]
b <- common_words  ## store in b as specified



## Step 6:Create matrix M of word token sequences

# Step 6a: Create token vector for full text using common words in b
# NA if word not in common word list
##text_tokens <- match(a, b)
text_tokens<-match(a_clean3,b)

# Step 6b: Create matrix M with shifted token sequences
mlag <- 4  # maximum lag (can be changed)
n <- length(text_tokens)

# Create matrix with n-mlag rows and mlag+1 columns
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)

# Fill each column with progressively shifted token vectors
for (col in 1:(mlag + 1)) {
  start_pos <- col
  end_pos <- n - mlag + col - 1
  M[, col] <- text_tokens[start_pos:end_pos]
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
punctuation_marks <- c(",", ".", ";", "!", ":", "?") #punctuation symbols
is_not_punct <- !(b %in% punctuation_marks) #removing the punctuation
valid_starts <- which(is_not_punct) #returns only the TRUE indices in is_not_punct


##Randomised starting token
start_token <- sample(valid_starts, 1)

###Step 9: Generate the sentence until full stop

# Initialize sequence with starting word
sequence <- start_token

## keep generating until we hit a full stop
repeat{
  context_len<-min(length(sequence), mlag) # number of words you can check previously to predict the next word
  start_pos<-length(sequence) - context_len +1 #beginning of the sliding window of order mlag
  context<-start_pos:length(sequence) #the main parameter needed to predict and generate tokens
  
  next_token<-next.word(context, M, text_tokens) #generates next token using next.word function
  sequence<-c(sequence, next_token) #updates sequence
  
  #stop when a full stop is reached
  if(next_token=="."){ 
    break
  }
  
  #it started going in infinite loops, so a threshold is kept
  if(length(sequence)>10){
    break
  }
} 

#the words generated
generated_words <- b[sequence] 

## print result nicely
cat("Generated Shakespeare-like sentence:\n", paste(generated_words, collapse = " "), "\n\n")

