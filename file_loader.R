fileloader <- function(filetoread){
  i <- filetoread
  library(readxl)
  
  #setting up directory
  maindir <- getwd()
  setwd("input")
  
  #number of input files with names
  file_list <- dir()
  
  read <- read_xlsx(file_list[i])
  
  #go back to main dir
  setwd(maindir)
  
  #read first file
  return(read)
  
}


filenumber <- function(){
  #setting up directory
  maindir <- getwd()
  setwd("input")
  
  #number of input files with names
  file_list <- dir()
  
  #go back to main dir
  setwd(maindir)
  
  return(length(file_list))
}

filenames <- function(number) {
  i <- number
  #setting up directory
  maindir <- getwd()
  setwd("input")
  
  file_list <- dir()
  filename <- file_list[i]
  
  #go back to main dir
  setwd(maindir)
  return(filename)
}


