## Aryeh Nigri

## Iterate throw xxx.jack files in a folder and create a file xxxT.xml
## for every token, write it's correct tag into the xml file
## The root tag is <tokens>

#region GLOBAL VARIABLES

keywords <- c("class", "constructor", "function", "method", "field", "static", "var",
                        "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if",
                        "else", "while", "return")

symbols <- c('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~')

idcharsRegex <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
                   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
                   '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

digitsRegex <- c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

#endregion

writeToXML <- function(tag, content, output){
    print(paste(tag, content, sep=" : "))
    writeLines(c(paste(paste("<", tag, ">", sep=""), content, paste("</", tag, ">", sep=""), sep=" ")), output)
}

writeSymbol <- function(character, index, output){
    index <- index + 1

    if(character == '<'){
        character = "&lt;"
    }
    if(character == '>'){
        character = "&gt;"
    }
    if(character == '&'){
        character = "&amp;"
    }
    writeToXML("symbol", character, output)

    return(index)
}

writeIdentifierOrKeyword <- function(characters, index, output){
    word <- characters[index]

    repeat{
        index <- index + 1
        character <- characters[index]
        if(!character %in% idcharsRegex) { break }
        word <- paste(word, character, sep="")
    }

    if(word %in% keywords){
        writeToXML("keyword", word, output)
    }
    else{
        writeToXML("identifier", word, output)
    }

    return(index)
}

writeIntegerConstant <- function(characters, index, output){
    number <- characters[index]

    repeat{
        index <- index + 1
        character <- characters[index]
        if(!character %in% digitsRegex) { break }
        number <- paste(number, character, sep="")
    }

    writeToXML("integerConstant", number, output)

    return(index)
}

writeStringConstant <- function(characters, index, output){
    stringContent <- ""

    repeat{
        index <- index + 1
        character <- characters[index]
        if(character == '"'){
            index <- index + 1   ## jump the closing "
            break
        }
        stringContent <- paste(stringContent, character, sep="")
    }

    writeToXML("stringConstant", stringContent, output)

    return(index)
}

handleSingleLineComment <- function(characters, index){
    
    repeat{
        index <- index + 1
        character <- characters[index]
        if(character == '\n'){
            index <- index + 1   ## jump the endline
            break
        }
    }

    return(index)
}

handleMultipleLinesComment <- function(characters, index){

    repeat{
        index <- index + 1
        character <- characters[index]
        if(character == '*'){
            nextCharacter <- characters[index + 1]
            if(nextCharacter == '/'){
                index <- index + 2   ## jump the closing */
                break
            }
        }
    }
    
    return(index)
}

searchFiles <- function(pathToSearch){

  folders <- list.dirs(path = pathToSearch, full.names = TRUE, recursive = TRUE)
  #print(folders)
  for(folder in folders){
      ## find all files with .jack, from this directory
      files <- list.files(path=folder ,pattern="\\.jack$")

      if(length(files) == 0){ ## no files in folder
          next ## continue
      }

      ## iterate throw every file, and write the correct tag token
      for(file in files){
          fileName <- basename(file)
          currentFile <- file(file.path(folder, file), "r")
          lines <- readLines(currentFile)

          newFileName <- gsub(".jack", "T.xml", fileName)
          outputFile <- file.path(folder, newFileName)
          file.create(outputFile)
          currentOutputFile <- file(outputFile, "w")
          
          allCharacters <- c()
          ## iterate throw every line, and write every letter in a vector, adding \n at the end
          for(line in lines){
              characters <- strsplit(line, "")[[1]]
              allCharacters <- c(allCharacters, characters, "\n")
          }
          
          writeLines(c("<tokens>"), currentOutputFile)

          index <- 1
          sizeOfFile <- length(allCharacters)
          
          while(index < sizeOfFile){
              character <- allCharacters[index]
              
              if(character == '/'){
                  nextCharacter <- allCharacters[index + 1]
                  if(nextCharacter == '/'){
                      index <- index + 1
                      index <- handleSingleLineComment(allCharacters, index)
                  }
                  else if (nextCharacter == '*') {
                      index <- index + 1
                      index <- handleMultipleLinesComment(allCharacters, index)
                  }
                  else {
                     index <- writeSymbol(character, index, currentOutputFile)
                  }
              }
              else if(character %in% symbols){
                  index <- writeSymbol(character, index, currentOutputFile)
              }
              else if(character %in% digitsRegex){
                  index <- writeIntegerConstant(allCharacters, index, currentOutputFile)
              }
              else if(character %in% idcharsRegex){
                  index <- writeIdentifierOrKeyword(allCharacters, index, currentOutputFile)
              }
              else if(character == '"'){
                  index <- writeStringConstant(allCharacters, index, currentOutputFile)
              }
              else{ ## ignore white spaces
                  index <- index + 1
              }
          }

          writeLines(c("</tokens>"), currentOutputFile)

          close(currentFile)
          close(currentOutputFile)
      }
  }
}


main <- function(passedPath){
  ##TODO rewrite this line to always get the argument correctly, now its only on vscode
  filesPath <- gsub("\\\\", "/", passedPath[6])
  #filesPath <- file.path(vectorPath)

  if(length(filesPath) == 0){ ## if no arguments were passed
    filesPath <- getwd()      ## use the current directory
  }

  searchFiles(filesPath)
}

main(commandArgs())