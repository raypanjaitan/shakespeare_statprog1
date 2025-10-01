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
mlag <- 4  # maximum lag (can be changed)
n <- length(text_tokens)

# Create matrix with n-mlag rows and mlag+1 columns
M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)

# Fill each column with progressively shifted token vectors
for (i in 1:(mlag + 1)) {
  M[, i] <- text_tokens[i:(n - mlag + i - 1)]
}

