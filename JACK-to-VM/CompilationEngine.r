## Aryeh Nigri

## Runs the Tokenizer.r script, and then the Analyzer.r script,
## using the same args as received.

main <- function(passedPath){
  source("Tokenizer.r")
  source("Analyzer.r")
}

main(commandArgs(trailingOnly=TRUE))