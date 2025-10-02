# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi - s2891732

#3 Changing the working directory. Also, reading the text dataset.
setwd("/Users/sanjoisethi/Documents/UoE Notes/Sem 1/Stat Programming/Assignment 1/shakespeare_statprog1")
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8") #Reading the dataset in UTF-8 text encoding, in character type form. We are skipping the first 83 lines of the dataset.

stage_dir=function(a) 
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

a_1=stage_dir(a) #Calling the function to remove stage directions

#4 (b) Removing character names (Fully Uppercase Words) and Arabic Numerals
upper_numeral=function(a_1)
{
  a_upper=a_1==toupper(a_1) & !(a_1 %in% c("I","A")) #Fetching the uppercase words except I and A
  a_upper_rem=a_1[!a_upper] #Removing the uppercase words
  a_numeral=gsub("[0-9]", "", a_upper_rem) #Removing Arabic numerals
  return(a_numeral)
}

a_2=upper_numeral(a_1) #Passing the dataset without the stage words

#4 (c) Removing "-" and "_'
hyphen_underscore=function(a_2)
{
  a_hyphen=gsub("-", "", a_2)
  a_underscore=gsub("_", "", a_hyphen)
}

a_3=hyphen_underscore(a_2)

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
a_4=split_punct(a_3[1:100000], punct_vec) #Running over partial dataset as the function is taking too long to run
length(a_4)

#4(f) Converting the dataset to lowercase
a_5=tolower(a_4)