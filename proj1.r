# Aditya Sreekumar Achary - s2844915
# Trisno Raynaldy Panjaitan - s2779061
# Sanjoi Sethi - s2891732


#3 Changing the working directory. Also, reading the text dataset.
setwd("/Users/sanjoisethi/Documents/UoE Notes/Sem 1/Stat Programming/Assignment 1/shakespeare_statprog1")
a <- scan("shakespeare.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8") #Reading the dataset in UTF-8 text encoding, in character type form. We are skipping the first 83 lines of the dataset.

#4 (a) Removing the stage directions
stage_dir=function(starting_bracket_index, a)
{
  counter=0 #For noting the index of ]
  for (i in 0:100)
  {
    #Finding the index of ] and storing it in the counter variable and breaking from the loop as soon as a ] is found
    if(grepl(']', a[starting_bracket_index+i]))
    {
      counter=i
      break
    }
  }
  stage_dir_words=a[starting_bracket_index:(starting_bracket_index+i)] #Extracting the words between []
  a_1=a[is.na(match(a,stage_dir_words))] #Deleting the stage words 
  return(a_1)
}

starting_bracket_index=grep("[", a, fixed=TRUE) #Fetching the indices of the opening brackets
a_1=stage_dir(a) #Passing it to the function








