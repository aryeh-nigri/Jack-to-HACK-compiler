## Aryeh Nigri

## Iterate throw xxxT.xml files in a folder and create a file xxx.xml
## for every tag, write it's correct tree tag into the xml file

#region GLOBAL VARIABLES

keywords <- c("class", "constructor", "function", "method", "field", "static", "var",
                        "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if",
                        "else", "while", "return")

symbols <- c('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~')

op <- c('+', '-', '*', '/', '&', '|', '<', '>', '=')
unaryOp <- c('-', '~')
keywordConstant <- c("true", "false", "null", "this")

#endregion

#region WRITING FUNCTIONS

writeToXML <- function(content, levelXML, output) {
    spaces <- ""
    while(levelXML > 0){
            spaces <- paste("   ", spaces, sep = "")
            levelXML <- levelXML - 1
        }

    if(startsWith(content, "<")){
        writeLines(c(paste(spaces, content, sep="")), output)
    }
    else {
       writeLines(c(paste(spaces, "<", content, ">", sep="")), output)
    }
}

writeLinesToXML <- function(lines, index, numberOfLines, levelXML, output) {
    
    while(numberOfLines > 0){
            writeToXML(lines[index], levelXML, output)
            numberOfLines <- numberOfLines - 1
            index <- index + 1
        }

    return(index)

}

#endregion

#region EXPRESSIONS

writeUnaryOp <- function(lines, index, tagLevel, output){
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## symbol
    return(index)
}

writeOp <- function(lines, index, tagLevel, output){
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## symbol
    return(index)
}

writeExpressionList <- function(lines, index, tagLevel, output){
    writeToXML("expressionList", tagLevel, output)
    tagLevel <- tagLevel + 1

    words <- strsplit(lines[index], " ")[[1]]
    word <- words[2]  ## word between the tags

    if(word == ')'){   ## empty list
        return(index) ## ?
    }

    index <- writeExpression(lines, index, tagLevel, output) ## expression

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags
        if(word == ','){ ## (, expression)*
            index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ,
            index <- writeExpression(lines, index, tagLevel, output) ## expression
        }
        else{
            break
        }
    }

    tagLevel <- tagLevel - 1
    writeToXML("/expressionList", tagLevel, output)

    return(index)
}

writeSubroutineCall <- function(lines, index, tagLevel, output){
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## identifier

    words <- strsplit(lines[index], " ")[[1]]
    word <- words[2]  ## word between the tags
    if(word == '('){ ## ( expressionList )
        index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## (
        index <- writeExpressionList(lines, index, tagLevel, output) ## expressionList
        index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## )
    }
    else if(word == '.'){ ## . identifier ( expressionList )
        index <- writeLinesToXML(lines, index, 3, tagLevel, output) ## . identifier (
        index <- writeExpressionList(lines, index, tagLevel, output) ## expressionList
        index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## )
    }
    else{
        print("ERRADO")
    }

    return(index)
}

writeTerm <- function(lines, index, tagLevel, output){
    writeToXML("term", tagLevel, output)
    tagLevel <- tagLevel + 1

    words <- strsplit(lines[index], " ")[[1]]
    firstWord <- words[1]  ## the opening tag

    switch(firstWord, ## switch with the word
           "<integerConstant>"={
               index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## <integerConstant> integer </integerConstant>
           },
           "<stringConstant>"={
               index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## <stringConstant> string  </stringConstant>
           },
           "<keyword>"={
               secondWord <- words[2] ## word between tags

               if (secondWord %in% keywordConstant) {
                   index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## <keyword> keywordConstant </keyword>
               } else {
                  print("No keyword constant here...")
               }
           },
           "<identifier>"={ ## multiple options
               words <- strsplit(lines[index + 1], " ")[[1]] ## next line
               word <- words[2] ## word between tags

               if (word == '[') { ## varName [ expression ]
                    index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## varName [
                    index <- writeExpression(lines, index, tagLevel, output) ## expression
                    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ]
               } else if (word == '(' | word == '.') { ## subroutineCall
                    index <- writeSubroutineCall(lines, index, tagLevel, output) ## subroutineCall
               } else { ## varName
                    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## varName
               }
           },
           "<symbol>"={ ## ( or unaryOp
               secondWord <- words[2] ## word between tags

               if (secondWord %in% unaryOp) {
                    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## <symbol> unaryOp </symbol>
                    index <- writeTerm(lines, index, tagLevel, output) ## term
               } else if (secondWord == '(') {
                    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## (
                    index <- writeExpression(lines, index, tagLevel, output) ## expression
                    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## )
               } else {
                    print("No symbol here...")
               }
           },
           ## default
           {
               print("No term here...")
           })

    tagLevel <- tagLevel - 1
    writeToXML("/term", tagLevel, output)

    return(index)
}

writeExpression <- function(lines, index, tagLevel, output){
    writeToXML("expression", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeTerm(lines, index, tagLevel, output) ## term

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags
        if(word %in% op){ ## (op term)*
            index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## <symbol> op </symbol>
            index <- writeTerm(lines, index, tagLevel, output) ## term
        }
        else{
            break
        }
    }

    tagLevel <- tagLevel - 1
    writeToXML("/expression", tagLevel, output)

    return(index)
}

#endregion

#region STATEMENTS

writeLetStatement <- function(lines, index, tagLevel, output){
    writeToXML("letStatement", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## let varName

    words <- strsplit(lines[index], " ")[[1]]
    word <- words[2]  ## word between the tags
    if(word == '['){ ## ?
        index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## [
        index <- writeExpression(lines, index, tagLevel, output) ## expression
        index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ]
    }

    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## =
    index <- writeExpression(lines, index, tagLevel, output) ## expression
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ;

    tagLevel <- tagLevel - 1
    writeToXML("/letStatement", tagLevel, output)

    return(index)
}

writeIfStatement <- function(lines, index, tagLevel, output){
    writeToXML("ifStatement", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## if (
    index <- writeExpression(lines, index, tagLevel, output) ## expression
    index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## ) {
    index <- writeStatements(lines, index, tagLevel, output) ## statements
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## }

    words <- strsplit(lines[index], " ")[[1]]
    word <- words[2]  ## word between the tags
    if(word == "else"){ ## ?
        index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## else {
        index <- writeStatements(lines, index, tagLevel, output) ## statements
        index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## }
    }

    tagLevel <- tagLevel - 1
    writeToXML("/ifStatement", tagLevel, output)

    return(index)
}

writeWhileStatement <- function(lines, index, tagLevel, output){
    writeToXML("whileStatement", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## while (
    index <- writeExpression(lines, index, tagLevel, output) ## expression
    index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## ) {
    index <- writeStatements(lines, index, tagLevel, output) ## statements
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## }

    tagLevel <- tagLevel - 1
    writeToXML("/whileStatement", tagLevel, output)

    return(index)
}

writeDoStatement <- function(lines, index, tagLevel, output){
    writeToXML("doStatement", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## do
    index <- writeSubroutineCall(lines, index, tagLevel, output) ## subroutineCall
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ;

    tagLevel <- tagLevel - 1
    writeToXML("/doStatement", tagLevel, output)

    return(index)
}

writeReturnStatement <- function(lines, index, tagLevel, output){
    writeToXML("returnStatement", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## return

    words <- strsplit(lines[index], " ")[[1]]
    word <- words[2]  ## word between the tags
    if(word != ';'){
        index <- writeExpression(lines, index, tagLevel, output) ## expression?
    }

    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ;

    tagLevel <- tagLevel - 1
    writeToXML("/returnStatement", tagLevel, output)

    return(index)
}

writeStatement <- function(lines, index, tagLevel, output){
    words <- strsplit(lines[index], " ")[[1]]
    word <- words[2]  ## word between the tags

    switch(word, ## switch with the word, and goes to right function
           "let"={
               index <- writeLetStatement(lines, index, tagLevel, output) ## letStatement
           },
           "if"={
               index <- writeIfStatement(lines, index, tagLevel, output) ## ifStatement
           },
           "while"={
               index <- writeWhileStatement(lines, index, tagLevel, output) ##whileStatement
           },
           "do"={
               index <- writeDoStatement(lines, index, tagLevel, output) ## doStatement
           },
           "return"={
               index <- writeReturnStatement(lines, index, tagLevel, output) ##returnStatement
           },
           ## default
           {
               print("No statement here...")
           })

    return(index)
}

writeStatements <- function(lines, index, tagLevel, output){
    writeToXML("statements", tagLevel, output)
    tagLevel <- tagLevel + 1

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags
        if(word == "let" | word == "if" | word == "while" | word == "do" | word == "return"){
            index <- writeStatement(lines, index, tagLevel, output) ## statement*
        }
        else{
            break
        }
    }

    tagLevel <- tagLevel - 1
    writeToXML("/statements", tagLevel, output)

    return(index)
}

#endregion

#region PROGRAM STRUCTURE

writeClassVarDec <- function(lines, index, tagLevel, output){
    writeToXML("classVarDec", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 3, tagLevel, output) ## static|field type varName

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags
        if(word == ','){
            index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## (, varName)*
        }
        else{
            break
        }
    }

    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ;
    tagLevel <- tagLevel - 1
    writeToXML("/classVarDec", tagLevel, output)

    return(index)
}

writeParameterList <- function(lines, index, tagLevel, output){
    writeToXML("parameterList", tagLevel, output)
    tagLevel <- tagLevel + 1

    words <- strsplit(lines[index], " ")[[1]]
    word <- words[2]  ## word between the tags

    if(word == ')'){   ## empty list
        return(index) ## ?
    }

    index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## type varName

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags
        if(word == ','){
            index <- writeLinesToXML(lines, index, 3, tagLevel, output) ## (, type varName)*
        }
        else{
            break
        }
    }

    tagLevel <- tagLevel - 1
    writeToXML("/parameterList", tagLevel, output)

    return(index)
}

writeVarDec <- function(lines, index, tagLevel, output){
    writeToXML("varDec", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 3, tagLevel, output) ## var type varName

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags
        if(word == ','){
            index <- writeLinesToXML(lines, index, 2, tagLevel, output) ## (, varName)*
        }
        else{
            break
        }
    }

    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## ;
    tagLevel <- tagLevel - 1
    writeToXML("/varDec", tagLevel, output)

    return(index)
}

writeSubroutineBody <- function(lines, index, tagLevel, output){
    writeToXML("subroutineBody", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## {

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags
        if(word == "var"){
            index <- writeVarDec(lines, index, tagLevel, output) ## varDec*
        }
        else{
            break
        }
    }

    index <- writeStatements(lines, index, tagLevel, output) ## statements
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## }

    tagLevel <- tagLevel - 1
    writeToXML("/subroutineBody", tagLevel, output)

    return(index)
}

writeSubroutineDec <- function(lines, index, tagLevel, output){
    writeToXML("subroutineDec", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 4, tagLevel, output) ## write 4 starting lines
    index <- writeParameterList(lines, index, tagLevel, output) ## parameterList
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## )
    index <- writeSubroutineBody(lines, index, tagLevel, output) ## subroutineBody

    tagLevel <- tagLevel - 1
    writeToXML("/subroutineDec", tagLevel, output)

    return(index)
}

writeClass <- function(lines, index, output){
    tagLevel <- 0
    writeToXML("class", tagLevel, output)
    tagLevel <- tagLevel + 1

    index <- writeLinesToXML(lines, index, 3, tagLevel, output) ## class className {

    repeat{
        words <- strsplit(lines[index], " ")[[1]]
        word <- words[2]  ## word between the tags

        if(word == "static" | word == "field"){
            index <- writeClassVarDec(lines, index, tagLevel, output) ## classVarDec*
        }
        else if(word == "constructor" | word == "function" | word == "method"){
            index <- writeSubroutineDec(lines, index, tagLevel, output) ## subroutineDec*
        }
        else{
            break
        }
    }
    
    index <- writeLinesToXML(lines, index, 1, tagLevel, output) ## }
    tagLevel <- tagLevel - 1
    writeToXML("/class", tagLevel, output)
}

#endregion

#region MAIN FUNCTIONS

searchFiles <- function(pathToSearch){

  folders <- list.dirs(path = pathToSearch, full.names = TRUE, recursive = TRUE)
  #print(folders)
  for(folder in folders){
      ## find all files with .jack, from this directory
      files <- list.files(path=folder ,pattern="T\\.xml$")

      if(length(files) == 0){ ## no files in folder
          next ## continue
      }

      ## iterate throw every file, and write the correct tag token
      for(file in files){
          fileName <- basename(file)
          currentFile <- file(file.path(folder, file), "r")
          lines <- readLines(currentFile)

          newFileName <- gsub("T\\.xml", ".xml", fileName)
          outputFile <- file.path(folder, newFileName)
          file.create(outputFile)
          currentOutputFile <- file(outputFile, "w")
          
          index <- 2   ## starts from the second line
          writeClass(lines, index, currentOutputFile)

        #   ## iterate throw every line, and write every letter in a vector, adding \n at the end
        #   for(line in lines){
        #       words <- strsplit(line, " ")[[1]]
        #       word <- words[2]  ## palavra entre as tags

        #       switch(word, ## switch with the first word, and goes to right function
        #         "class"={
        #             functionVM(, , currentOutputFile)
        #         },
        #         "//"={ ## optional case, may be removed if wanted
        #             writeLines(currentLine, currentOutputFile) ## write the comments on the .vm file
        #         },
        #         ## default
        #         {})
              
        #   }
          
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

#endregion

main(commandArgs())