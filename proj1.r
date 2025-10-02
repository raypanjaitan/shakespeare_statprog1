# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi - s2891732


#3 Changing the working directory. Also, reading the text dataset.
setwd("/Users/sanjoisethi/Documents/UoE Notes/Sem 1/Stat Programming/Assignment 1/shakespeare_statprog1")
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8") #Reading the dataset in UTF-8 text encoding, in character type form. We are skipping the first 83 lines of the dataset.

#4 (a) Removing the stage directions
# stage_dir=function(starting_bracket_index, a)
# {
#   stage_dir_words=c()
#   for (i in starting_bracket_index)
#   {
#     stage_dir_words=c()
#     counter=0 #For noting the index of ]
#     for (j in 0:100)
#     {
#       #Finding the index of ] and storing it in the counter variable and breaking from the loop as soon as a ] is found
#       if(grepl("]", a[i+j]))
#       {
#         counter=j
#         break
#       }
#     }
#     stage_dir_words=c(stage_dir_words, a[i:(i+counter)])#Extracting the words between []
#   }
#   a_1=a[is.na(match(a,stage_dir_words))] #Deleting the stage words 
#   return(a_1)
# }
# 
# starting_bracket_index=grep("[", a, fixed=TRUE) #Fetching the indices of the opening brackets
# a_1=stage_dir(starting_bracket_index, a)

open_br<-grep("[", a, fixed=TRUE)
del1<-c()
for (i in open_br){
  close_br=grep("]",a[i:i+100],fixed=TRUE)
  if (length(close_br)>0){
    close_index<-i + close_br[1]-1
    del1 <- c(del1, i:close_index)
  }
}
a_1=a[-del1] 

#4 (b) Removing character names (Fully Uppercase Words) and Arabic Numerals
upper_numeral=function(a_1)
{
  a_upper=a==toupper(a) & !(a %in% c('I','A')) #Fetching the uppercase words except I and A
  a_upper_rem=a_upper[!a_upper] #Removing the uppercase words
  a_numeral=gsub("[0-9]", "", a_upper_rem) #Removing Arabic numerals
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
eg_1=split_punct(x, punct_vec)
eg_1
