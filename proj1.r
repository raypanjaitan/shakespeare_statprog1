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

##Deleting Roman Numerals
roman <- grep("^[IVXLCDM]+$", a_clean1)
a_clean2 <- a_clean1[-roman]

##Removing dashes
a_clean3 <- gsub("-", "", a_clean2)