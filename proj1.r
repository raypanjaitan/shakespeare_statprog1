# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi - s2891732

#3 Changing the working directory. Also, reading the text dataset.
setwd("/Users/sanjoisethi/Documents/UoE Notes/Sem 1/Stat Programming/Assignment 1/shakespeare_statprog1")
a <- scan("shakespeare.txt", what="character", skip=83, nlines=196043-83,
          fileEncoding="UTF-8") #Reading the dataset in UTF-8 text encoding, in character type form. We are skipping the first 83 lines of the dataset.

stage_dir=function(a) 
{
  if(length(grep("[", a, fixed=TRUE))==0)
  {
    return(a) #Error-handling
  }
  else
  {
    rem_index=integer(0) #To store indices of the stage words to be removed
    starting_bracket_index=grep("[", a, fixed=TRUE) #Indices of [
    for(i in starting_bracket_index) #For stage direction words
    {
      ending_bracket_counter=min(i+100, length(a)) #Counter to search for ]
      stage_dir_words=a[i:ending_bracket_counter] #Staging direction words between []
      ending_bracket_index=grep("]", stage_dir_words, fixed=TRUE)[1] #Indices of ] 
      if(!is.na(ending_bracket_index)) #Checking that there exists a ]
      {
        j=i+ending_bracket_index-1 #Ending bracker index
      } 
      else 
      {
        j=i #No match for [
      }
      rem_index=c(rem_index, i:j) #All words from []
    }
    return(a[-rem_index]) #Removing the staging directions
  }
}
  

a_1=stage_dir(a) #Calling the function to remove stage directions

#4 (b) Removing character names (Fully Uppercase Words) and Arabic Numerals
upper_numeral=function(a_1)
{
  a_upper=a_1==toupper(a_1) & !(a_1 %in% c("I", "A")) #Fetching the uppercase words except I and A
  a_upper_rem=a_1[!a_upper] #Removing the uppercase words
  a_numeral=gsub("[0-9]", "", a_upper_rem) #Removing Arabic numerals
  return(a_numeral)
}

a_2=upper_numeral(a_1) #Passing the dataset to remove uppercase words and Arabic numerals

#4 (c) Removing "-" and "_'
hyphen_underscore=function(a_2)
{
  a_hyphen=gsub("-", "", a_2) #Removing -
  a_underscore=gsub("_", "", a_hyphen) #reomving _
  return(a_underscore)
}

a_3=hyphen_underscore(a_2) #Passing the dataset to remove - and _

#4 (d) Function for detaching the punctuations from the word
split_punct=function(word_vec, punct_vec)
{
  for(i in punct_vec) 
  {
    new_punct=paste0("\\", i) #Handling for not be read as punctuation
    if(any(grepl(new_punct, word_vec))) #Checking words with punctuations
    {
      new_word_vec=c() #Temp vector
      for(j in word_vec) #For separating the punctuations
      {
        new_word_vec=c(new_word_vec, j) #Appending the jth word
        if (grepl(new_punct, j)) #Checking for punctuation in the word
        {
          new_word=gsub(new_punct, "", j) #Deleting the punctuation attached
          new_word_vec[length(new_word_vec)]=new_word #Replacing the original word with the above
          new_word_vec=c(new_word_vec, i) #Appending the punctuation right after the word
        }
      }
      word_vec=new_word_vec #Temp vec to the main vec
    }
  }
  return(word_vec)
}

#Dry Run of the function
eg_1=c("An", "omnishambles,", "in", "a", "headless", "chicken,", "factory.")
punct_vec=c(",", ".", ";", "!", ":", "?")
eg_1_ans=split_punct(eg_1, punct_vec)
eg_1_ans

#4 (e) Using split_punct for our dataset
a_4=split_punct(a_3[1:10000], punct_vec) #Running over partial dataset as the function is taking too long to run

#4(f) Converting the dataset to lowercase
lowercase_words=function(a_4)
{
  return(tolower(a_4))
}

a_5=lowercase_words(a_4)

#5 (a) Finding the unique words
unique_words=function(a_5)
{
  return(unique(a_5)) 
}

b_1=unique_words(a_5) #Passing the dataset to fetch the unique words

#5 (b) #Fetching the indices of unique words from a_5
match_index=function(a_5, b_1)
{
  return(match(a_5, b_1)) 
}

b_2=match_index(a_5,b_1) #Passing the datasets to get the indices of all occurences of the unique words in b present in a_5
cat("Length of data vector is same as the index vector a_5:",(length(a_5)), "b:", length(b_2)) #Checking if length of data vector a_5 is same as the index vector b

#5 (c) Counting the occurences of unique words
tabulation=function(b_2, b_1)
{
  b_3=tabulate(b_2)
  names(b_3)=b_1
  return(b_3)
}

b_3=tabulation(b_2, b_1) #Passing the index vector to tabulate it

#5 (d) Finding the most common 1000 words
common=function(b_3)
{
  ranks=rank(-b_3, ties.method="first") #Giving ascending rank to words with max occurences
  common_words=names(ranks)[ranks<=1000] #Choosing top 1000 words
  return(common_words)
}

b=common(b_3)

#6 (a) Creating the token vector
token_generation=function(a_5,b)
{
  tokens=match(a_5,b) #Creating a vector of indices by matching the most common 1000 words to our dataset
  return(tokens)
}

tokens=token_generation(a_5,b)
cat("Length of data vector is same as the tokens vector a_5:",(length(a_5)), "tokens:", length(tokens)) #Checking if length of data vector a_5 is same as the tokens vector

#6 (b) Creating matrix M
matrix_creation=function(n, mlag, tokens)
{
  M=matrix(tokens, nrow=(n-mlag), ncol=(mlag+1), byrow=TRUE) #Creating a matrix M using the token vector
  return(M)
}

M=matrix_creation(length(a_5), mlag=4, tokens)

#7 Generation model
next.word=function(key,M,M1,w=rep(1,ncol(M)-1)) #Pass M1 from outside
{
  #Pre-processing of key
  key_1=stage_dir(key)
  key_2=upper_numeral(key_1)
  key_3=hyphen_underscore(key_2)
  key_4=split_punct(key_3, punct_vec) #Using punct_vec as global
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
    # Commented because getting an unresolved error
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

predicted_word=next.word(key=a_5[1:10], M, tokens)

#9 Sentence generation till full-stop
sentence_generation=function(initial){
  sentence=c(initial)
  while (!(tail(sentence,1) %in% c(".",","))) #Waiting for a full-stop takes a long time
  {
    next_word=next.word(key=sentence, M, tokens)
    sentence=c(sentence,next_word)
  }
  return(sentence)
}

raw_sentence=sentence_generation(predicted_word)

processed_output=function(raw_sentence){
  return(paste(raw_sentence, collapse = " "))
}

processed_output(raw_sentence)
