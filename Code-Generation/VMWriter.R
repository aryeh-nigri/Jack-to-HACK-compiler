## Aryeh Nigri
## Moishe Fischbein

## Output module for generating VM code.

## Emits VM commands into a file, encapsulating the VM command syntax.

library(R6)

VMWriter <- R6Class("VMWriter",
  public = list(

    outputFile = NULL,

    ## Creates a new file and prepares it for writing.
    initialize = function(outputFile = NA) {
      self$outputFile <- outputFile
    },

    ## Writes a VM push command.
    writePush = function(segment, index) {
        writeLines(c(paste("push", segment, index)), self$outputFile)
    },

    ## Writes a VM pop command.
    writePop = function(segment, index) {
        writeLines(c(paste("pop", segment, index)), self$outputFile)
    },

    ## Writes a VM arithmetic command.
    writeArithmetic = function(command) {
        writeLines(c(command), self$outputFile)
    },

    ## Writes a VM label command.
    writeLabel = function(label) {
        writeLines(c(paste("label", label)), self$outputFile)
    },

    ## Writes a VM goto command.
    writeGoto = function(label) {
        writeLines(c(paste("goto", label)), self$outputFile)
    },

    ## Writes a VM if-goto command.
    writeIf = function(label) {
        writeLines(c(paste("if-goto", label)), self$outputFile)
    },

    ## Writes a VM call command.
    writeCall = function(name, nArgs) {
        writeLines(c(paste("call", name, nArgs)), self$outputFile)
    },

    ## Writes a VM function command.
    writeFunction = function(name, nLocals) {
        writeLines(c(paste("function", name, nLocals)), self$outputFile)
    },

    ## Writes a VM return command.
    writeReturn = function() {
        writeLines(c("return"), self$outputFile)
    },

    ## Writes some command.
    writeCommand = function(command) {
        writeLines(c(command), self$outputFile)
    },

    ## Closes the output file.
    close = function() {
        close(self$outputFile)
    }
  )
)