cat("*** Removing .aux files\n")
files <- list.files(path = ".", pattern = ".*\\.(aux|lol)$", recursive = T)
invisible(lapply(files, file.remove))