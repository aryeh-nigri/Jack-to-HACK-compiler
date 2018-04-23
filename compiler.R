## Aryeh Nigri


#region functions
################################################# FUNCTIONS #################################################

## Initialization. This asm code should appear as the first lines of each asm file.
sysInit <- function(output, haveSysInit){
  # Initialize the SP to 256
  writeLines(c("@256", "D=A", "@SP", "M=D"), output)
  # call to the code that implement function call
  # with function name "Sys.init" and number of arguments is 0
  if(haveSysInit == TRUE){
    callVM("Sys.init", "0", output, 0)
  }
}

#region program flow commands
################################################# PROGRAM FLOW COMMANDS #################################################

## A declaration of a label c, inside current file FileName
labelVM <- function(fileName, arg1, output){
  writeLines(c(paste("(", fileName, ".", arg1, ")", sep = "")), output)
}

## Jump to label c that was declared in current file FileName
gotoVM <- function(fileName, arg1, output){
  writeLines(c(paste("@", fileName, ".", arg1, sep = ""), "0;JMP"), output)
}

## pop the topmost stack element and if it is not zero jump to label c in current file FileName
ifGotoVM <- function(fileName, arg1, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", paste("@", fileName, ".", arg1, sep = ""), "D;JNE"), output)
}

################################################# PROGRAM FLOW COMMANDS #################################################
#endregion

#region function calling commands
################################################# FUNCTION CALLING COMMANDS #################################################

## Here starts a function named f(arg1), which has k(arg2) local variables
functionVM <- function(arg1, arg2, output){
  writeLines(c(
               "// label f",
               paste("(", arg1, ")", sep = ""),
               "// אתחול המשתנים המקומיים",
               paste("@", arg2, sep = ""), "D=A", paste("@", arg1, ".END", sep = ""), "D;JEQ",
               "// (k != 0) קפיצה אם שקר",
               paste("(", arg1, ".LOOP)", sep = ""), "@SP", "A=M", "M=0", "@SP", "M=M+1", paste("@", arg1, ".LOOP", sep = ""),
               "//(k != 0)קפיצה כל עוד",
               "D=D-1;JNE",
               "// (k == 0)סיום אם אמת",
               paste("(", arg1, ".END)", sep = "")
               ), output)
}

## Invoke function g(arg1), after n(arg2) arguments have been pushed onto the stack
callVM <- function(arg1, arg2, output, counter){
  writeLines(c(
               "// push return-address",
               paste("@", arg1, ".RETURN_ADDRESS", counter, sep = ""), "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1",
               "// push LCL",
               "@LCL", "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1",
               "// push ARG",
               "@ARG", "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1",
               "// push THIS",
               "@THIS", "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1",
               "// push THAT",
               "@THAT", "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1",
               "// ARG = SP-n-5", ## SP - (n+5)
               "@SP", "D=M", paste("@", strtoi(arg2) + 5, sep = ""), "D=D-A", "@ARG", "M=D",
               "// LCL = SP",
               "@SP", "D=M", "@LCL", "M=D",
               "// goto f",
               paste("@", arg1, sep = ""), "0;JMP",
               "// label return-address",
               paste("(", arg1, ".RETURN_ADDRESS", counter, ")", sep = "")
               ), output)
}

## Terminate execution and return control to the calling function
returnVM <- function(output){
  writeLines(c(
               "// FRAME = LCL",
               "@LCL", "D=M",
               "// RET = *(FRAME - 5)",
               "// RAM[13] = (LOCAL - 5)",
               "@5", "A=D-A", "D=M", "@13", "M=D",
               "// *ARG = pop()",
               "@SP", "M=M-1", "A=M", "D=M", "@ARG", "A=M", "M=D",
               "// SP = ARG + 1",
               "@ARG", "D=M", "@SP", "M=D+1",
               "// THAT = *(FRAME - 1)",
               "@LCL", "M=M-1", "A=M", "D=M", "@THAT", "M=D",
               "// THIS = *(FRAME - 2)",
               "@LCL", "M=M-1", "A=M", "D=M", "@THIS", "M=D",
               "// ARG = *(FRAME - 3)",
               "@LCL", "M=M-1", "A=M", "D=M", "@ARG", "M=D",
               "// LCL = *(FRAME - 4)",
               "@LCL", "M=M-1", "A=M", "D=M", "@LCL", "M=D",
               "// goto RET",
               "@13", "A=M", "0;JMP"
              ), output)
}

################################################# FUNCTION CALLING COMMANDS #################################################
#endregion

#region memory access commands
################################################# MEMORY ACCESS COMMANDS #################################################

## function push to stack
pushVM <- function(arg1, arg2, output){
  switch (arg1,
          "constant"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@SP", "M=M+1", "A=M-1", "M=D"), output)
          },
          "local"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@LCL", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
          },
          "argument"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@ARG", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
          },
          "this"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@THIS", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
          },
          "that"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@THAT", "A=M", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
          },
          "temp"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@5", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
          },
          "static"={
            writeLines(c(paste("@", arg2, sep = ""), "D=A", "@16", "A=D+A", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
          },
          "pointer"={
            if(arg2 == "0"){
              writeLines(c("@THIS", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            }
            else if(arg2 == "1"){
              writeLines(c("@THAT", "D=M", "@SP", "M=M+1", "A=M-1", "M=D"), output)
            }
          }
  )
}

## function pop to stack
popVM <- function(arg1, arg2, output){
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
            }
            else if(arg2 == "1"){
              writeLines(c("@SP", "M=M-1", "A=M", "D=M", "@THAT", "M=D"), output)
            }
          }
  )
}

################################################# MEMORY ACCESS COMMANDS #################################################
#endregion

#region arithmetic/boolean commands
################################################# ARITHMETIC / BOOLEAN COMMANDS #################################################

## function add to stack
addVM <- function(output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "M=M+D"), output)
}

## function sub to stack
subVM <- function(output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "M=M-D"), output)
}

## function neg to stack
negVM <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "M=-M"), output)
}

## function not to stack
notVM <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "M=!M"), output)
}

## function eq to stack
eqVM <- function(lineNumber, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", 
             paste("@TRUE", lineNumber, sep = ""), "D;JEQ", "@SP", "A=M-1", "M=0", 
             paste("@FALSE", lineNumber, sep = ""), "0;JEQ", 
             paste("(TRUE", lineNumber, ")", sep = ""), "@SP", "A=M-1", "M=-1", 
             paste("(FALSE", lineNumber, ")", sep = "")), output)
}

## function gt to stack
gtVM <- function(lineNumber, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", 
             paste("@TRUE", lineNumber, sep = ""), "D;JGT", "@SP", "A=M-1", "M=0", 
             paste("@FALSE", lineNumber, sep = ""), "0;JEQ", 
             paste("(TRUE", lineNumber, ")", sep = ""), "@SP", "A=M-1", "M=-1", 
             paste("(FALSE", lineNumber, ")", sep = "")), output)
}

## function lt to stack
ltVM <- function(lineNumber, output){
  writeLines(c("@SP", "M=M-1", "A=M", "D=M", "A=A-1", "A=M", "D=A-D", 
             paste("@TRUE", lineNumber, sep = ""), "D;JLT", "@SP", "A=M-1", "M=0", 
             paste("@FALSE", lineNumber, sep = ""), "0;JEQ", 
             paste("(TRUE", lineNumber, ")", sep = ""), "@SP", "A=M-1", "M=-1", 
             paste("(FALSE", lineNumber, ")", sep = "")), output)
}

## function and to stack
andVM <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "D=M", "A=A-1", "M=M&D", "@SP", "M=M-1"), output)
}

## function or to stack
orVM <- function(output){
  writeLines(c("@SP", "A=M", "A=A-1", "D=M", "A=A-1", "M=M|D", "@SP", "M=M-1"), output)
}

################################################# ARITHMETIC / BOOLEAN COMMANDS #################################################
#endregion

################################################# FUNCTIONS #################################################
#endregion

#region main functions

chooseFunction <- function(currentLine, currentOutputFile, currentFileName){
    wordsInLine <- strsplit(currentLine, " ")[[1]] ## splits the word in the line by 1 white space
    
    switch(wordsInLine[1], ## switch with the first word, and goes to right function
           "function"={
             functionVM(wordsInLine[2], wordsInLine[3], currentOutputFile)
           },
           "call"={
             callVM(wordsInLine[2], wordsInLine[3], currentOutputFile, counter=callCounter)
             callCounter <<- callCounter + 1 ## callCounter++
           },
           "return"={
             returnVM(currentOutputFile)
           },
           "label"={
             labelVM(currentFileName,wordsInLine[2], currentOutputFile)
           },
           "goto"={
             gotoVM(currentFileName, wordsInLine[2] ,currentOutputFile)
           },
           "if-goto"={
             ifGotoVM(currentFileName, wordsInLine[2], currentOutputFile)
           },
           "push"={
             pushVM(wordsInLine[2], wordsInLine[3], currentOutputFile)
           },
           "pop"={
             popVM(wordsInLine[2], wordsInLine[3], currentOutputFile)
           },
           "add"={
             addVM(currentOutputFile)
           },
           "sub"={
             subVM(currentOutputFile)
           },
           "neg"={
             negVM(currentOutputFile)
           },
           "not"={
             notVM(currentOutputFile)
           },
           "eq"={
             eqVM(lineNumber=booleanCounter, currentOutputFile)
             booleanCounter <<- booleanCounter + 1 ## booleanCounter++
           },
           "gt"={
             gtVM(lineNumber=booleanCounter, currentOutputFile)
             booleanCounter <<- booleanCounter + 1 ## booleanCounter++
           },
           "lt"={
             ltVM(lineNumber=booleanCounter, currentOutputFile)
             booleanCounter <<- booleanCounter + 1 ## booleanCounter++
           },
           "and"={
             andVM(currentOutputFile)
           },
           "or"={
             orVM(currentOutputFile)
           },
           "//"={ ## optional case, may be removed if wanted
             writeLines(currentLine, currentOutputFile) ## write the comments on the .vm file
           },
           ## default
           {})
}

searchFiles <- function(pathToSearch){

  folders <- list.dirs(path = pathToSearch, full.names = TRUE, recursive = TRUE)
  #print(folders)
  for(folder in folders){
      ## find all files with .vm, from this directory
      files <- list.files(path=folder ,pattern="\\.vm$")

      if(length(files) == 0){ ## no files in folder
          next ## continue
      }

      folderName <- basename(folder)

      newFileName <- paste(folderName, ".asm", sep="")
      outputFile <- file.path(folder, newFileName)
      file.create(outputFile)
      currentOutputFile <- file(outputFile, "w")

      if(length(files) > 1){
        sysInit(currentOutputFile, TRUE) ## initialization function with call to sys.init
      }else{ ## only 1 file, no need to sys.init
        sysInit(currentOutputFile, FALSE) ## initialization function without call to sys.init
      }

      booleanCounter <<- 0
      callCounter <<- 0

      ## iterate throw every file, and write the correct machine translation
      for(file in files){
          fileName <- basename(file)
          currentFile <- file(file.path(folder, file), "r")
          lines <- readLines(currentFile)

          for(line in lines){
              if(!startsWith(line, "//")){ ## write every command as a comment
                writeLines(paste("//", toupper(line)), currentOutputFile)
              }
              chooseFunction(line, currentOutputFile, fileName)
          }
          close(currentFile)
      }
      close(currentOutputFile)
  }
}

main <- function(passedPath){

  filesPath <- gsub("\\\\", "/", passedPath[6])
  #filesPath <- file.path(vectorPath)

  if(length(filesPath) == 0){ ## if no arguments were passed
    filesPath <- getwd()      ## use the current directory
  }

  searchFiles(filesPath)
}

#endregion

#region main
################################################# MAIN #################################################

main(commandArgs())

################################################# MAIN #################################################
#endregion