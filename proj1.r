# Aditya Sreekumar Achary - s2844915 - (Worked on Points 6, 8 & 9)
# Sanjoi Sethi - s2891732 (Worked on Point 7)
# Trisno Raynaldy Panjaitan - s2779061 - (Worked on Points 1-5(d))

##1 Repo Link: https://github.com/raypanjaitan/shakespeare_statprog1.git
## 2 & 3
setwd("D:\\Edu\\Master\\Courses\\Statistical Programming\\Repo\\shakespeare_statprog1") ## comment out of submitted
a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8") ##import text;

## 4(a) Removing the staging directions
stage_dir <- function(a) 
{
  ob <-grep("[", a, fixed=TRUE); ## get indices of words with open bracket
  if(length(ob)==0)
  {
    return(a) #Error-handling
  }
  else
  {
    dir <- c() ##initiate variable for words 
    
    for (i in ob) { ## loops
      searchLength <-i+100
      cb <- grep("]",a[i:searchLength],fixed=TRUE) ## get indices of 100 words after i that contains char "]"
      if (length(cb)>0){ ## check if it's found
        obi <- i+cb[1]-1 ## calculate indice of the closed bracket
        dir <- c(dir, i:obi) ## add the open bracket and closed bracket indices to var dir
      }
    }
    
    a<-a[-dir] ## remove indices from i to the indices taken from var dir
    return(a)
  }
}

a_1=stage_dir(a) #Calling the function to remove stage directions

## 4(b)
upper_numeral=function(a_1)
{
  a_1 <- gsub("\\d+", "",a_1) ## remove all arabic numerals
  a.I <- grepl("\\bI\\b", a_1) ## get all character "I" coordinate
  a.A <- grepl("\\bA\\b", a_1) ## get all character "A" coordinate
  a.uc <- (a_1 == toupper(a_1)) ## get all uppercase coordinate
  a.allowed <- !a.uc | a.I | a.A ## get coordinate of vector without all uppercase, but including character I and A
  a_2 <- a_1[a.allowed] ## get the filtered vector using the result of above variable
  return(a_2)
}

a_2=upper_numeral(a_1)

#4 (c) Removing "-" and "_"
hyphen_underscore <- function(a_2)
{
  a_hyphen <- gsub("-", "", a_2) ## remove hyphens
  a_underscore <- gsub("_", "", a_hyphen) ## remove underscores
  return(a_underscore)
}

a_3=hyphen_underscore(a_2)

#4 (d) Function for detaching the punctuations from the word
split_once <- function(v, punct_one){
  punct=paste0("\\", punct_one)
  if (length(grep(punct, v))>0){
    p <- grep(punct, v) ## get coordinate of vector containing punctuations
    pw <- grep(punct, v, value=TRUE) ## get the word containing punctuations
    
    tl <- rep("",length(p)+length(v)) ## vector to store all words
    tlp <- p+1:length(p) ## coordinate of punctuations
    
    pp <- regexpr(punct, v[p]) ## get coordinate of punctuation in the word
    tl[-tlp] <- gsub(punct, "", v) ## put the punctuations after its word
    tl[tlp] <- substr(v[p], pp, pp) ## put the cleaned words in its original coordinates
  }
  else{
    tl <- v
  }
  return(tl) ##return 
}

#For recursively calling split_punct
split_punct <- function(v, punct){
  temp=v
  for (i in punct){
    temp=split_once(temp,i)
  }
  return(temp)
}

## 4(e) Using the split_punct function
punct <- c(",", ".", ";", "!", ":", "?") ## punctuations variable, escape character by using double backslash
# punct <- "[,.;!:?]" ## punctuations variable option
a_4<- split_punct(a_3, punct) ## run the split_punct function

## 4(f) lowercase version of vector
lowercase_words=function(a_4)
{
  return(tolower(a_4))
}

a_5 <-tolower(a_4)

## 5(a)
unique_words<-unique(a_5) ## create vector b from unique version of a
## 5(b)
idx <- match(a_5, unique_words) ## find the index which element in b each element of a corresponds to
## 5(c)
word_counts <- tabulate(idx, nbins = length(b)) ## tabulation for the words vector

## 5(d) Get the top ~1000 most common words
word_rank <- rank(-word_counts, ties.method = "first")  ## rank frequencies
top_1000_idx <- which(word_rank <= 1000)

b <- unique_words[top_1000_idx] ## words
counts_top1000 <- word_counts[top_1000_idx] ## their counts

#6 (a) Creating the token vector
token_generation=function(a_5,b)
{
  tokens=match(a_5,b) #Creating a vector of indices by matching the most common 1000 words to our dataset
  return(tokens)
}

tokens=token_generation(a_5,b)
cat("Length of data vector is same as the tokens vector:",(length(a_5)), "tokens:", length(tokens)) #Checking if length of data vector a_5 is same as the tokens vector

## Step 6(a): Creating the token vector

# Function to generate token indices from text data
# Maps each word in the cleaned text to its position in the vocabulary
# Words not in vocabulary are assigned NA
token_generation=function(a_5,b)
{
  tokens=match(a_5,b) #Creating a vector of indices by matching the most common 1000 words to our dataset
  return(tokens)
}

tokens=token_generation(a_5,b)
cat("Length of data vector is same as the tokens vector:",(length(a_5)), "tokens:", length(tokens)) #Checking if length of data vector a_5 is same as the tokens vector

# 6(b): Creating matrix M

# Each row represents a sliding window of (mlag+1) consecutive tokens
# Columns 1 to mlag: context words | Column (mlag+1): target word to predict
matrix_creation=function(n, mlag, tokens)
{
  M=matrix(NA, nrow=(n-mlag), ncol=(mlag+1), byrow=TRUE) 
  
  # Fill each column with shifted token sequences
  for(i in 0:mlag)
  {
    M[,i+1]=tokens[(1+i):(n-mlag+i)]
  }
  return(M)
}

M=matrix_creation(length(tokens), mlag=4, tokens)

#7 Generation model
next.word=function(key,M,M1,w=rep(1,ncol(M)-1)) #Pass M1 from outside
{
  #Pre-processing of key
  key_1=stage_dir(key)
  key_2=upper_numeral(key_1)
  key_3=hyphen_underscore(key_2)
  key_4=split_punct(key_3, punct) #Using punct as global
  key_5=lowercase_words(key_4)
  
  #Choosing last mlag words from key
  mlag=ncol(M)-1
  length_check=min(mlag, length(key_5))
  key_6=tail(key_5, length_check)
  
  #Making tokens of key
  token_key=token_generation(key_6,b)
  prob=setNames(rep(0, length(b)),1:length(b))
  
  for(mc in 1:mlag)
  {
    reduced_token=head(token_key[mc:mlag]) #Decreasing the token length as the size of matrix M decreases
    ii=colSums(!(t(M[,mc:mlag,drop=FALSE])==reduced_token)) 
    matching=grep(0, ii, fixed=TRUE) #Counting the number of zeros for all matches
    if(length(matching)>0) 
    {
      u=M[matching, mlag+1] #Finding the corresponding last column entry of M wherever we found a match
      prob_mc=(1/mlag)*table(u)/length(u) #Finding the probability distribution over b
      prob[names(prob_mc)]=prob[names(prob_mc)]+prob_mc
    }
    ###Commented because getting an unresolved error
    # else
    # {
    #   #Sampling a common word, and assigning it highest probability for it to be selected
    #   tab=table(a_5)
    #   frequencies=tab[as.character(b)]
    #   frequencies[is.na(frequencies)] <- 0
    #   probabilities=frequencies/length(a_5)
    #   most_probable_word=sample(x=b,size=1,prob=probabilities)
    #   most_probable_token=grep(most_probable_word,b,fixed=TRUE)
    #   prob_mc=(1/mlag)*c(most_probable_token=1)
    #   prob[names(prob_mc)]=prob[names(prob_mc)]+prob_mc
    # }
  }
  most_probable_tokens=names(prob)[which(prob==max(prob))] #Tokens with highest probability (maybe multiple)
  most_probable_token=as.integer(sample(most_probable_tokens,1)) #Sampling if multiple tokens in the previous step (Introduces randomness)
  most_probable_word=b[most_probable_token] #Converting token to word
  return(most_probable_word) 
}

## Step 8: Select a starting word (not punctuation)
punctuation_marks <- c(",", ".", ";", "!", ":", "?") # Define punctuation marks to exclude from sentence start
is_not_punct <- !(b %in% punctuation_marks) # Identify non-punctuation words in vocabulary
valid_starts <- which(is_not_punct) # Get indices of valid starting words


#Randomly select a starting token from valid options
start_token <- sample(valid_starts, 1)

##Step 9: Generate the sentence until full stop

# Initialize sequence with starting word
sequence <- b[start_token]

# Keep generating until we hit a full stop
repeat{
  context=sequence  #This provides the actual word strings needed by next.word function
  next_token<-next.word(context, M, tokens) #generates next token using next.word function
  sequence<-c(sequence, next_token) #updates sequence
  
  #Stop generating when full stop is generated
  if(next_token=="."){ 
    break
  }
  
  #Sequence limited to 10 tokens to ensure generation completes (it started going in infinite loops)
  if(length(sequence)>9){
    print("Note: Since the process is going into an infinite loop, we have kept a threshold of 10")
    break
  }
} 

#Store the generated token sequence
generated_words=sequence

#Display the generated sequence
cat("Generated Shakespeare-like sentence:\n", paste(generated_words, collapse = " "), "\n\n")