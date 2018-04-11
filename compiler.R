## Arieh Nigri


#region functions
################################################# FUNCTIONS #################################################

## Initialization. This asm code should appear as the first lines of each asm file.
sysInit <- function(output){
  # Initialize the SP to 256
  writeLines(c("@256", "D=A", "@SP", "M=D"), output)
  # call to the code that implement function call
  # with function name "Sys.init" and number of arguments is 0
  call("Sys.init", "0", output)
}

#region program flow commands
################################################# PROGRAM FLOW COMMANDS #################################################

## A declaration of a label c, inside current file FileName
label <- function(fileName, arg1, output){
  writeLines(c(paste("(", fileName, ".", arg1, ")", sep = "")), output)
}

## Jump to label c that was declared in current file FileName
goto <- function(fileName, arg1, output){
  writeLines(c(paste("@", fileName, ".", arg1, sep = ""), "0;JMP"), output)
}

## pop the topmost stack element and if it is not zero jump to label c in current file FileName
ifGoto <- function(fileName, arg1, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", paste("@", fileName, ".", arg1, sep = ""), "D;JNE"), output)
}

################################################# PROGRAM FLOW COMMANDS #################################################
#endregion

#region function calling commands
################################################# FUNCTION CALLING COMMANDS #################################################

## Here starts a function named f(arg1), which has k(arg2) local variables
functionVM <- function(arg1, arg2, output){
  writeLines(c(
               paste("(", arg1, ")", sep = ""),
               paste("@", arg2, sep = ""),
               "D=A",
               paste("@", arg1, ".END", sep = ""),
               "D;JEQ",
               paste("(", arg1, ".LOOP)", sep = ""),
               "@SP",
               "A=M",
               "M=0",
               "@SP",
               "M=M+1",
               paste("@", arg1, ".LOOP", sep = ""),
               "D=D-1;JNE",
               paste("(", arg1, ".END)", sep = "")
               ), output)
}

## Invoke function g(arg1), after n(arg2) arguments have been pushed onto the stack
callCounter <- 0 ## counter to handle multiple function calls
call <- function(arg1, arg2, output){
  writeLines(c(
               paste("@", arg1, ".RETURN_ADDRESS", callCounter, sep = ""),
               "D=A",
               "@SP",
               "A=M",
               "M=D",
               "@SP",
               "M=M+1",
               "@LCL",
               "D=M",
               "@SP",
               "A=M",
               "M=D",
               "@SP",
               "M=M+1",
               "@ARG",
               "D=M",
               "@SP",
               "A=M",
               "M=D",
               "@SP",
               "M=M+1",
               "@THIS",
               "D=M",
               "@SP",
               "A=M",
               "M=D",
               "@SP",
               "M=M+1",
               "@THAT",
               "D=M",
               "@SP",
               "A=M",
               "M=D",
               "@SP",
               "M=M+1",
               "@SP",
               "D=A",
               paste("@", strtoi(arg2) + 5, sep = ""), ## arg2 + 5
               "D=D+A",
               "@SP",
               "D=M-D",
               "@ARG",
               "M=D",
               "@SP",
               "D=M",
               "@LCL",
               "M=D",
               "@arg1",
               "0;JMP",
               paste("(", arg1, ".RETURN_ADDRESS", callCounter, ")", sep = "")
               ), output)

  callCounter <- callCounter + 1
}

## Terminate execution and return control to the calling function
returnVM <- function(output){

}

################################################# FUNCTION CALLING COMMANDS #################################################
#endregion

#region memory access commands
################################################# MEMORY ACCESS COMMANDS #################################################

## function push to stack
push <- function(arg1, arg2, output){
  switch (arg1,
          "constant"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            #writeLines(c(paste("@", arg2, sep = ""), "D=A", "@0", "M=M+1", "A=M-1", "M=D"), output)
          },
          "local"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@LCL", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            #writeLines(c(paste("@", arg2, sep = ""), "D=A", "@LCL", "A=M", "A=D+A", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
          },
          "argument"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@ARG", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            #writeLines(c(paste("@", arg2, sep = ""), "D=A", "@ARG", "A=M", "A=D+A", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
          },
          "this"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@THIS", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            #writeLines(c(paste("@", arg2, sep = ""), "D=A", "@THIS", "A=M", "A=D+A", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
          },
          "that"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@THAT", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            #writeLines(c(paste("@", arg2, sep = ""), "D=A", "@THAT", "A=M", "A=D+A", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
          },
          "temp"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@5", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            #writeLines(c(paste("@", arg2, sep = ""), "D=A", "@5", "A=D+A", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
          },
          "static"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@16", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            #writeLines(c(paste("@", arg2, sep = ""), "D=A", "@16", "A=D+A", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
          },
          "pointer"={
            if(arg2 == "0"){
              writeLines(c("@THIS", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
              #writeLines(c("@THIS", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
            }
            else if(arg2 == "1"){
              writeLines(c("@THAT", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
              #writeLines(c("@THAT", "D=M", "@0", "M=M+1", "A=M-1", "M=D"), output)
            }
          }
  )
}

## function pop to stack
pop <- function(arg1, arg2, output){
  switch (arg1,
          "local"={
            writeLines(c("@LCL", "D=M", paste("@", arg2, sep = ""), "D=D+A", "@13", "M=D", "@SP", "M=M-1", "A=M", "D=M", "@13", "A=M", "M=D"), output)
          },
          "argument"={
            writeLines(c("@ARG", "D=M", paste("@", arg2, sep = ""), "D=D+A", "@13", "M=D", "@SP", "M=M-1", "A=M", "D=M", "@13", "A=M", "M=D"), output)
          },
          "this"={
            writeLines(c("@THIS", "D=M", paste("@", arg2, sep = ""), "D=D+A", "@13", "M=D", "@SP", "M=M-1", "A=M", "D=M", "@13", "A=M", "M=D"), output)
          },
          "that"={
            writeLines(c("@THAT", "D=M", paste("@", arg2, sep = ""), "D=D+A", "@13", "M=D", "@SP", "M=M-1", "A=M", "D=M", "@13", "A=M", "M=D"), output)
          },
          "temp"={
            writeLines(c("@5", "D=A", paste("@", arg2, sep = ""), "D=D+A", "@13", "M=D", "@SP", "M=M-1", "A=M", "D=M", "@13", "A=M", "M=D"), output)
          },
          "static"={
            writeLines(c("@16", "D=A", paste("@", arg2, sep = ""), "D=D+A", "@13", "M=D", "@SP", "M=M-1", "A=M", "D=M", "@13", "A=M", "M=D"), output)
          },
          "pointer"={
            if(arg2 == "0"){
              writeLines(c("@SP", "M=M-1", "A=M", "D=M", "@THIS", "M=D"), output)
              #writeLines(c("@0", "M=M-1", "A=M", "D=M", "@THIS", "M=D"), output)
            }
            else if(arg2 == "1"){
              writeLines(c("@SP", "M=M-1", "A=M", "D=M", "@THAT", "M=D"), output)
              #writeLines(c("@0", "M=M-1", "A=M", "D=M", "@THAT", "M=D"), output)
            }
          }
  )
}

################################################# MEMORY ACCESS COMMANDS #################################################
#endregion

#region arithmetic/boolean commands
################################################# ARITHMETIC / BOOLEAN COMMANDS #################################################

## function add to stack
add <- function(output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "M=M+D"), output)
  #writeLines(c("@0", "M=M-1", "A=M", "D=M", "A=A-1", "M=M+D"), output)
}

## function sub to stack
sub <- function(output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "M=M-D"), output)
  #writeLines(c("@0", "M=M-1", "A=M", "D=M", "A=A-1", "M=M-D"), output)
}

## function neg to stack
neg <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "M=-M"), output)
  #writeLines(c("@0", "A=M", "A=A-1", "M=-M"), output)
}

## function not to stack
not <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "M=!M"), output)
  #writeLines(c("@0", "A=M", "A=A-1", "M=!M"), output)
}

## function eq to stack
eq <- function(lineNumber, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", 
             paste("@TRUE", lineNumber, sep = ""), "D;JEQ", "@SP", "A=M-1", "M=0", 
             paste("@FALSE", lineNumber, sep = ""), "0;JEQ", 
             paste("(TRUE", lineNumber, ")", sep = ""), "@SP", "A=M-1", "M=-1", 
             paste("(FALSE", lineNumber, ")", sep = "")), output)
  #writeLines(c("@0", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", paste("@TRUE", lineNumber, sep = ""), "D;JEQ", "@0", "A=M-1", "M=0", paste("@FALSE", lineNumber, sep = ""), "0;JEQ", paste("(TRUE", lineNumber, ")", sep = ""), "@0", "A=M-1", "M=-1", paste("(FALSE", lineNumber, ")", sep = "")), output)
}

## function gt to stack
gt <- function(lineNumber, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", 
             paste("@TRUE", lineNumber, sep = ""), "D;JGT", "@SP", "A=M-1", "M=0", 
             paste("@FALSE", lineNumber, sep = ""), "0;JEQ", 
             paste("(TRUE", lineNumber, ")", sep = ""), "@SP", "A=M-1", "M=-1", 
             paste("(FALSE", lineNumber, ")", sep = "")), output)
  #writeLines(c("@0", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", paste("@TRUE", lineNumber, sep = ""), "D;JGT", "@0", "A=M-1", "M=0", paste("@FALSE", lineNumber, sep = ""), "0;JEQ", paste("(TRUE", lineNumber, ")", sep = ""), "@0", "A=M-1", "M=-1", paste("(FALSE", lineNumber, ")", sep = "")), output)
}

## function lt to stack
lt <- function(lineNumber, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", 
             paste("@TRUE", lineNumber, sep = ""), "D;JLT", "@SP", "A=M-1", "M=0", 
             paste("@FALSE", lineNumber, sep = ""), "0;JEQ", 
             paste("(TRUE", lineNumber, ")", sep = ""), "@SP", "A=M-1", "M=-1", 
             paste("(FALSE", lineNumber, ")", sep = "")), output)
  #  writeLines(c("@0", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", paste("@TRUE", lineNumber, sep = ""), "D;JLT", "@0", "A=M-1", "M=0", paste("@FALSE", lineNumber, sep = ""), "0;JEQ", paste("(TRUE", lineNumber, ")", sep = ""), "@0", "A=M-1", "M=-1", paste("(FALSE", lineNumber, ")", sep = "")), output)
}

## function and to stack
and <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "D=M", "A=A-1", "M=M&D", "@SP", "M=M-1"), output)
  #writeLines(c("@0", "A=M", "A=A-1", "D=M", "A=A-1", "M=M&D", "@0", "M=M-1"), output)
}

## function or to stack
or <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "D=M", "A=A-1", "M=M|D", "@SP", "M=M-1"), output)
  #writeLines(c("@0", "A=M", "A=A-1", "D=M", "A=A-1", "M=M|D", "@0", "M=M-1"), output)
}

################################################# ARITHMETIC / BOOLEAN COMMANDS #################################################
#endregion

################################################# FUNCTIONS #################################################
#endregion

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
  for(currentLine in linesInFile){

    if(!startsWith(currentLine, "//")){ ## write every command as a comment
      writeLines(paste("//", currentLine), currentOutputFile)
    }

    wordsInLine <- strsplit(currentLine, " ")[[1]] ## splits the word in the line by 1 white space
    
    switch(wordsInLine[1], ## switch with the first word, and goes to right function
           "function"={
             functionVM(wordsInLine[2], wordsInLine[3], currentOutputFile)
           },
           "call"={
             call(wordsInLine[2], wordsInLine[3], currentOutputFile)
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
           "//"={
             writeLines(currentLine, currentOutputFile)
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