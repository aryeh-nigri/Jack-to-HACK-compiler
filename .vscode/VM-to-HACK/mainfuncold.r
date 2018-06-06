#region main
################################################# MAIN #################################################
oldWd <- getwd() ## save working directory

passedPath <- commandArgs()
#vectorPath <- strsplit(passedPath[6], "\\\\")#[[1]]

filesPath <- gsub("\\\\", "/", passedPath[6])
#filesPath <- file.path(vectorPath)

if(length(filesPath) == 0){ ## if no arguments were passed
  filesPath <- getwd()      ## use the current directory
}

setwd(filesPath)

## find all files with .vm, from this directory and recursively
files <- list.files(pattern = "\\.vm$", recursive = TRUE)


## iterate throw every file, and write a new file .asm with the correct machine translation
for(currentFile in files){
  
  ##fileName <- basename(currentFile)  ## get only the file name, without path
  currentFileName <- gsub(".vm", "", currentFile)
  outputFileName <- gsub(".vm", ".asm", currentFile) ## name new file with same name, and extension .asm
  outputFile <- file.create(outputFileName) ## creates the file .asm in the directory passed to the script
  currentOutputFile <- file(outputFileName, "w") ## open file to write
  
  myFile <- file(currentFile, "r") ## open file to read
  linesInFile <- readLines(myFile) ## reads every line from the current file
  ## iterate throw every line
  lineNumber <- 1 ## number line to pass when a function creates label
  callCounter <- 0 ## counter to handle multiple function calls
  for(currentLine in linesInFile){

    if(!startsWith(currentLine, "//")){ ## write every command as a comment
      writeLines(paste("//", toupper(currentLine)), currentOutputFile)
      #writeLines(paste("//", currentLine), currentOutputFile)
    }

    wordsInLine <- strsplit(currentLine, " ")[[1]] ## splits the word in the line by 1 white space
    
    switch(wordsInLine[1], ## switch with the first word, and goes to right function
           "function"={
             functionVM(wordsInLine[2], wordsInLine[3], currentOutputFile)
           },
           "call"={
             call(wordsInLine[2], wordsInLine[3], currentOutputFile, callCounter)
             callCounter <- callCounter + 1 ## callCounter++
           },
           "return"={
             returnVM(currentOutputFile)
           },
           "label"={
             label(currentFileName,wordsInLine[2], currentOutputFile)
           },
           "goto"={
             goto(currentFileName, wordsInLine[2] ,currentOutputFile)
           },
           "if-goto"={
             ifGoto(currentFileName, wordsInLine[2], currentOutputFile)
           },
           "push"={
             push(wordsInLine[2], wordsInLine[3], currentOutputFile)
           },
           "pop"={
             pop(wordsInLine[2], wordsInLine[3], currentOutputFile)
           },
           "add"={
             add(currentOutputFile)
           },
           "sub"={
             sub(currentOutputFile)
           },
           "neg"={
             neg(currentOutputFile)
           },
           "not"={
             not(currentOutputFile)
           },
           "eq"={
             eq(lineNumber, currentOutputFile)
           },
           "gt"={
             gt(lineNumber, currentOutputFile)
           },
           "lt"={
             lt(lineNumber, currentOutputFile)
           },
           "and"={
             and(currentOutputFile)
           },
           "or"={
             or(currentOutputFile)
           },
           "//"={ ## optional case, may be removed if wanted
             writeLines(currentLine, currentOutputFile) ## write the comments on the .vm file
           },
           ## default
           {})
    
    lineNumber <- lineNumber + 1 ## lineNumber++
  }
  
  close(myFile);close(currentOutputFile)
  
}

setwd(oldWd)
################################################# MAIN #################################################
#endregion