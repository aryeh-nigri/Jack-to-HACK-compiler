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

        for(line in lines){
            if(!startsWith(line, "//")){ ## write every command as a comment
            writeLines(paste("//", toupper(line)), currentOutputFile)
            }
    }
        }
    }

}