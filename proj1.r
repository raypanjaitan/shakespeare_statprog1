# Aditya Sreekumar Achary
# Trisno Raynaldy Panjaitan - s2779061
# Shun Wang


setwd("D:\\Edu\\Master\\Courses\\Statistical Programming\\Repo\\shakespeare_statprog1") ## comment out of submitted
a <- scan("pg100.txt",what="character",skip=83,nlines=196043-83,
          fileEncoding="UTF-8")

for (word in a){
  if(nchar(word) > 2){
    if(grepl("\\[.*\\]", word)){
      word<-gsub("[", "",word, fixed=TRUE)
      word<-gsub("]", "",word, fixed=TRUE)
      
    }
    if(grepl("_", word)){
      word<-gsub("_", '',word)
    }
    
    if(toupper(word) == word || grepl("\\d", word)){
      word<-''
    }
  }
}