## Aryeh Nigri
## Moishe Fischbein

## Removes all comments and white space from the input stream and breaks it into Jack-
## language tokens, as specified by the Jack grammar.

library(R6)

keywords <- c("class", "constructor", "function", "method", "field", "static", "var",
                        "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if",
                        "else", "while", "return")

symbols <- c('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~')


JackTokenizer <- R6Class("JackTokenizer",
  public = list(

    currentToken = "character",
    currentTokenType = "character",
    pointer = "integer",
    tokens = NULL,     ##   List<String>

    ## Opens the input file/stream and gets ready to tokenize it.
    initialize = function(inputFile) {
      preprocessed <- ""
      line <- ""

      lines <- readLines(inputFile)
      allCharacters <- c()
      ## iterate throw every line, and write every letter in a vector, adding \n at the end
      for(line in lines){
          characters <- strsplit(line, "")[[1]]
          allCharacters <- c(allCharacters, characters, "\n")
      }
      
      index <- 1
      totalNumberOfCharacters <- length(allCharacters)
      self$tokens <- c()

      while(index < totalNumberOfCharacters){
          character <- allCharacters[index]
              
          if(character == '/'){
              nextCharacter <- allCharacters[index + 1]
              if(nextCharacter == '/'){   ##   // comments
                  index <- index + 1
                  repeat{
                      index <- index + 1
                      character <- allCharacters[index]
                      if(character == '\n'){
                          index <- index + 1   ##   jump the endline
                          break
                      }
                  }
              }
              else if (nextCharacter == '*') {   ##   /* comments */
                  index <- index + 1
                  repeat{
                      index <- index + 1
                      character <- allCharacters[index]
                      if(character == '*'){
                          nextCharacter <- allCharacters[index + 1]
                          if(nextCharacter == '/'){
                              index <- index + 2   ##   jump the closing */
                              break
                          }
                      }                      
                  }
              }
              else {   ##   just the / symbol
                 self$tokens <- c(self$tokens, character)
                 index <- index + 1
              }
          }
          else if(character %in% symbols){
              self$tokens <- c(self$tokens, character)
              index <- index + 1
          }
          else if(grepl("[0-9]", character)){   ##   a digit
              number <- character

              repeat{
                  index <- index + 1
                  character <- allCharacters[index]
                  if (grepl("[0-9]", character)) {
                     number <- paste(number, character, sep="")
                  } else {
                     break
                  }
              }

              self$tokens <- c(self$tokens, number)
          }## antes   [A-z0-9_]
          else if(grepl("[[:alnum:]_]", character)){   ##   an alphanumeric character(not number, if passed \d)
              word <- character

              repeat{
                  index <- index + 1
                  character <- allCharacters[index]
                  if (grepl("[[:alnum:]_]", character)) {
                     word <- paste(word, character, sep="")
                  } else {
                     break
                  }
              }

              self$tokens <- c(self$tokens, word)
          }
          else if(character == '"'){
            #   index <- writeStringConstant(allCharacters, index, currentOutputFile)
              stringContent <- character
            #   stringContent <- ""
              ## TODO make sure if string should be with or without ""

              repeat{
                  index <- index + 1
                  character <- allCharacters[index]
                  if (character == '"') {
                     stringContent <- paste(stringContent, character, sep="")
                     index <- index + 1   ##   jump the closing "
                     break
                  } else {
                     stringContent <- paste(stringContent, character, sep="")
                  }
              }

              self$tokens <- c(self$tokens, stringContent)
          }
          else{ ## ignore white spaces
              index <- index + 1
          }
      }
    #   print(self$tokens)

      self$pointer <- 1
      self$currentToken <- ""
      self$currentTokenType <- "NONE"

      close(inputFile)
    },

    ## Do we have more tokens in the input?
    hasMoreTokens = function() {
        return(self$pointer < (length(self$tokens) + 1))
    },

    ## Gets the next token from the input and makes it the current token.
    ## This method should only be called if hasMoreTokens() is true.
    ## Initially there is no current token.
    advance = function() {
        if (self$hasMoreTokens()) {
           self$currentToken <- self$tokens[self$pointer]
        #    print(paste("DEBUG :", self$currentToken))
           self$pointer <- self$pointer + 1
        } else {
           print("No more tokens.")
           quit()
        #    return
        }

        if (self$currentToken %in% keywords) {
           self$currentTokenType <- "KEYWORD"
        } else if (self$currentToken %in% symbols) {
           self$currentTokenType <- "SYMBOL"
        } else if (grepl("[0-9]+", self$currentToken)) {
           self$currentTokenType <- "INT_CONST"
        } else if (grepl("\"[^\"\n]*\"", self$currentToken)) {
            ## TODO need to verify if this works
           self$currentTokenType <- "STRING_CONST"
        } else if (grepl("[A-z_][A-z0-9_]*", self$currentToken)) {
           self$currentTokenType <- "IDENTIFIER"
        } else {
            print(paste("Unknown token:", self$currentToken))
        }

        # print(paste("DEBUG :", self$currentTokenType))
    },

    ## Returns the type of the current token.
    tokenType = function() {
        return(self$currentTokenType)
    },

    ## Returns the keyword which is the current token.
    ## Should be called only when tokeyType() is KEYWORD.
    keyWord = function() {
        if (self$currentTokenType == "KEYWORD") {
           return(toupper(self$currentToken))
        } else {
        #    print("Current token is not a keyword!")
           return("")
        }
    },

    ## Returns the character which is the current token.
    ## Should be called only when tokenType() is SYMBOL.
    symbol = function() {
        if (self$currentTokenType == "SYMBOL") {
        #    return(substr(self$currentToken, 1, 1))      ## currentToken[0]
           return(self$currentToken)
        } else {
        #    print("Current token is not a symbol!")
           return("")
        }
    },

    ## Return the identifier which is the current token.
    ## Should be called only when tokenType() is IDENTIFIER.
    identifier = function() {
        if (self$currentTokenType == "IDENTIFIER") {
           return(self$currentToken)
        } else {
        #    print("Current token is not an identifier!")
           return("")
        }
    },

    ## Returns the integer value of the current token.
    ## Should be called only when tokenType() is INT_CONST.
    intVal = function() {
        if (self$currentTokenType == "INT_CONST") {
           return(self$currentToken)
        } else {
        #    print("Current token is not an integer constant!")
           return("")
        }
    },

    ## Returns the string value of the current token without the double quotes.
    ## Should be called only when tokenType() is STRING_CONST.
    stringVal = function() {
        if (self$currentTokenType == "STRING_CONST") {
            size <- length(strsplit(self$currentToken, "")[[1]])
           return(substr(self$currentToken, 2, size - 1))
        } else {
        #    print("Current token is not a string constant!")
           return("")
        }
    },

    ## Moves pointer back.
    pointerBack = function() {
        if(self$pointer > 1){
            self$pointer <- self$pointer - 1
            self$currentToken <- self$tokens[self$pointer]
        }
    },

    ## Returns if current symbol is an op.
    isOp = function() {
        return(self$symbol() %in% c('+', '-', '*', '/', '&', '|', '<', '>', '='))
    }

  )
)