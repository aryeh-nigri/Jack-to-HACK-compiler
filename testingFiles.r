myPath <- "C:/Users/Arieh/Documents/project08"

folders <- list.dirs(path = myPath, full.names = TRUE, recursive = TRUE)
#print(folders)
for(folder in folders){
    ## find all files with .vm, from this directory
    files <- list.files(path=folder ,pattern="\\.vm$")

    if(length(files) == 0){ ## no files in folder
        next ## continue
    }

    folderName <- basename(folder)
    newFileName <- paste(folderName, ".asm", sep="")
    outputFile <- file.create(file.path(folder, newFileName))
    currentOutputFile <- file(outputFile, "w")

    ## iterate throw every file, and write the correct machine translation
    for(file in files){
        fileName <- basename(file)
        currentFile <- file(file, "r")
        lines <- readLines(currentFile)
        booleanCounter <- 0
        callCounter <- 0
        for(line in lines){
            if(!startsWith(line, "//")){ ## write every command as a comment
            writeLines(paste("//", toupper(line)), currentOutputFile)
            }
            chooseFunction(line, currentOutputFile, callCounter, fileName, booleanCounter)
        }
    }
}

chooseFunction <- function(currentLine, currentOutputFile, callCounter, currentFileName, booleanCounter){
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
             booleanCounter <- booleanCounter + 1 ## booleanCounter++
           },
           "gt"={
             gt(lineNumber, currentOutputFile)
             booleanCounter <- booleanCounter + 1 ## booleanCounter++
           },
           "lt"={
             lt(lineNumber, currentOutputFile)
             booleanCounter <- booleanCounter + 1 ## booleanCounter++
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
}