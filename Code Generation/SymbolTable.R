## Aryeh Nigri
## Moishe Fischbein

## Symbol table;

## This module provides services for creating and using a symbol table. Recall that each symbol has 
## a scope from which it is visible in the source code. The symbol table implements this abstraction 
## by giving each symbol a running number (index) within the scope. The index starts at 0, 
## increments by 1 each time an identifier is added to the table, and resets to 0 when starting a new 
## scope. The following kinds of identifiers may appear in the symbol table:

## Static:      Scope: class.
## Field:       Scope: class.
## Argument:    Scope: subroutine (method/function/constructor).
## Var:         Scope: subroutine (method/function/constructor).

## When compiling error-free Jack code, any identifier not found in the symbol table may be 
## assumed to be a subroutine name or a class name. Since the Jack language syntax rules suffice for 
## distinguishing between these two possibilities, and since no “linking” needs to be done by the 
## compiler, there is no need to keep these identifiers in the symbol table.

## SymbolTable: Provides a symbol table abstraction. The symbol table associates the identifier names found 
## in the program with identifier properties needed for compilation: type, kind, and running index. The symbol 
## table for Jack programs has two nested scopes (class/subroutine).

source("Symbol.R")

library(R6)
library(hash)


SymbolTable <- R6Class("SymbolTable",
  public = list(

    classSymbols = "hash",          ##   STATIC, FIELD
    subroutineSymbols = "hash",     ##   ARG, VAR
    indices = "hash",

    ## Creates a new empty symbol table and initiates all indices.
    initialize = function() {
        self$classSymbols <- hash()             ##   <String, Symbol>
        self$subroutineSymbols <- hash()        ##   <String, Symbol>
        self$indices <- hash()                  ##   <KIND, Integer>
        self$indices["ARG"] <- 0
        self$indices["FIELD"] <- 0
        self$indices["STATIC"] <- 0
        self$indices["VAR"] <- 0
    },

    ## Starts a new subroutine scope (i.e. resets the subroutine's symbol table).
    startSubroutine = function() {
        clear(self$subroutineSymbols)
        self$indices["ARG"] <- 0
        self$indices["VAR"] <- 0
    },

    ## Defines a new identifier of a given name, type and kind and assigns it a running index. 
    ## STATIC and FIELD identifiers have a class scope, while ARG and VAR identifiers have a 
    ## subroutine scope.
    define = function(name, type, kind) {
        if (kind == "ARG" | kind == "VAR") {
           index <- self$indices[[kind]]
           symbol <- Symbol$new(type, kind, index)
           self$indices[kind] <- index + 1              ##   indices.put(kind, index + 1)
           self$subroutineSymbols[name] <- symbol       ##   subroutineSymbols.put(name, symbol)
        } else if (kind == "STATIC" | kind == "FIELD") {
           index <- self$indices[[kind]]
           symbol <- Symbol$new(type, kind, index)
           self$indices[kind] <- index + 1              ##   indices.put(kind, index + 1)
           self$classSymbols[name] <- symbol            ##   classSymbols.put(name, symbol)
        }
    },

    ## Returns the number of variables of the given kind already defined in the current scope.
    varCount = function(kind) {
        return(self$indices[[kind]])
    },

    ## Returns the kind of the named identifier in the current scope.
    ## If the identifier is unknown in the current scope returns NONE.
    kindOf = function(name) {
        symbol <- self$lookUp(name)

        if (!is.null(symbol)) {
           return(symbol$getKind())
        }
        return("NONE")
    },

    ## Returns the type of the named identifier in the current scope.
    typeOf = function(name) {
        symbol <- self$lookUp(name)

        if (!is.null(symbol)) {
           return(symbol$getType())
        }
        return("")
    },
    
    ## Returns the index assigned to the named identifier.
    indexOf = function(name) {
        symbol <- self$lookUp(name)

        if (!is.null(symbol)) {
           return(symbol$getIndex())
        }
        return(-1)
    },

    ## Check if target symbol exists.
    lookUp = function(name) {

        if (has.key(name, self$classSymbols)) {
           return(self$classSymbols[[name]])
        } else if(has.key(name, self$subroutineSymbols)){
           return(self$subroutineSymbols[[name]])
        } else{
            # print(paste("SYMBOL NOT FOUND ON TABLES :", name))
            return(NULL)
        }
    }

  )
)