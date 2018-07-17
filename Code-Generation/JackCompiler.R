## Aryeh Nigri
## Moishe Fischbein

## Top level driver that sets up and invokes the other modules.

## The compiler operates on a given source, where source is either a file name of the form 
## Xxx.jack or a directory name containing one or more such files. For each Xxx . jack input file, 
## the compiler creates a JackTokenizer and an output Xxx.vm file. Next, the compiler uses the 
## CompilationEngine, SymbolTable, and VMWriter modules to write the output file.

source("CompilationEngine.R")


library(R6)

JackCompiler <- R6Class("JackCompiler",
  public = list(

    ## Creates a new compilation engine with the given input and output.
    ## The next routine called must be compileClass()
    initialize = function(args) {
        self$main(args)
    },

    main = function(args) {
        filesPath <- gsub("\\\\", "/", args)
        fileName <- basename(filesPath)
        
        if (grepl("\\.", fileName)) { ## one file
            if (endsWith(fileName, ".jack")) { ## a jack file
                inputFile <- file(filesPath, "r")
                outputFileName <- gsub("\\.jack", ".vm", filesPath)
                file.create(outputFileName)
                outputFile <- file(outputFileName, "w")
                compilationEngine <- CompilationEngine$new(inputFile, outputFile)
            } else { ## wrong file
                print("Only .jack files are accepted!")
            }
        } else { ## a directory
            ## find all files with .jack in this directory
            jackFiles <- list.files(path=filesPath ,pattern="\\.jack$", full.names=TRUE)

            if(length(jackFiles) == 0){ ## no files in folder
                print("No .jack files in this directory!")
            } else {
                ## iterate throw every file, and compile it
                for(jackFile in jackFiles){
                    print(paste("FILE :", basename(jackFile)))
                    # print(jackFile)
                    inputFile <- file(jackFile, "r")
                    outputFileName <- gsub("\\.jack", ".vm", jackFile)
                    file.create(outputFileName)
                    outputFile <- file(outputFileName, "w")
                    compilationEngine <- CompilationEngine$new(inputFile, outputFile)
                    # CompilationEngine$new(inputFile, outputFile)
                }
            }

        }
    }
  )
)

jackCompiler <- JackCompiler$new(commandArgs(trailingOnly=TRUE))

















# main <- function(args) {
#     filesPath <- gsub("\\\\", "/", args)
#     fileName <- basename(filesPath)
    
#     if (grepl("\\.", fileName)) { ## one file
#         if (endsWith(fileName, ".jack")) { ## a jack file
#             inputFile <- file(filesPath, "r")
#             outputFileName <- gsub("\\.jack", ".vm", filesPath)
#             file.create(outputFileName)
#             outputFile <- file(outputFileName, "w")
#             compilationEngine <- CompilationEngine$new(inputFile, outputFile)
#         } else { ## wrong file
#             print("Only .jack files are accepted!")
#         }
#     } else { ## a directory
#         ## find all files with .jack in this directory
#         jackFiles <- list.files(path=filesPath ,pattern="\\.jack$", full.names=TRUE)

#         if(length(jackFiles) == 0){ ## no files in folder
#             print("No .jack files in this directory!")
#         }

#         ## iterate throw every file, and compile it
#         for(jackFile in jackFiles){
#             # print(jackFile)
#             inputFile <- file(jackFile, "r")
#             outputFileName <- gsub("\\.jack", ".vm", jackFile)
#             file.create(outputFileName)
#             outputFile <- file(outputFileName, "w")
#             # compilationEngine <- CompilationEngine$new(inputFile, outputFile)
#             CompilationEngine$new(inputFile, outputFile)
#         }
#     }
# }

# main(commandArgs(trailingOnly=TRUE))