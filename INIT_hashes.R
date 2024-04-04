##Helper file to automatically create the comparative hashes

#copy of path names of eval.sub.R

script.name <- "evaluate_submission.R"  # name of this file itself
assets.path <- file.path(".github", "evaluation")  # path where additional files are stored
tests.path <- file.path(assets.path, "tests")  # test directory

lintr.file <- ".lintr"
rprofile.file <- ".Rprofile"
tests.dir <- file.path(assets.path, "tests")

assist.dir <- ".assist"

helpers.dir <- file.path(".github/helpers")
tables.dir <- file.path("tables")
tables.dir.tests <- file.path(".github/evaluation/tests/.tables")

#copy of readFile in eval.sub.R
readFile <- function(filename, description, len) {
  orig.options <- options("warn")
  on.exit(do.call(options, orig.options))
  rawstring <- withCallingHandlers(
    readBin(filename, "raw", if (missing(len)) file.info(filename)$size else len),
    error = function(e) stopfUnex(3, "Could not read %s %s from path %s",
                                  description, filename, working.path, error = e))
  if (all(rawstring < 128 & rawstring > 0)) {
    rawstring <- rawToChar(rawstring)
    gsub("\r\n?", "\n", rawstring)  # don't let windows or mac os break things for us
  } else {
    rawstring
  }
}

#functions that creates the hashfiles
saveHashFile <- function(filename, description, algo = "sha256") {
  
  if (file.exists(filename) && !dir.exists(filename)) {
    file.content <- readFile(filename, description)
  } else {
    files <- sort(list.files(filename, pattern = "\\.(r|csv|zip)$", ignore.case = TRUE, all.files = TRUE, full.names = TRUE, recursive = TRUE))
    file.content <- lapply(files, readFile, description = paste(description, "content"))
    file.content <- lapply(file.content, digest::digest, algo = "sha256", serialize = FALSE)
    file.content <- paste(c(file.content, ""), collapse = "\n")  # need to append final newline
  }
  file.hash <- digest::digest(file.content, algo = "sha256", serialize = FALSE)
  
  filename <- paste0(".github/evaluation/", basename(filename))
  
  write.table(file.hash, file = paste(filename, algo, sep = "."),
              col.names = FALSE, row.names = FALSE, quote = FALSE)
}


saveHashFile(script.name, "script-file")
saveHashFile(lintr.file, ".lintr-file")
saveHashFile(rprofile.file, ".Rprofile-file")
saveHashFile(tests.dir, "tests directory")
saveHashFile(helpers.dir, "helper-files")
saveHashFile(assist.dir, "assist-files")
saveHashFile(tables.dir, "tables folder")
saveHashFile(tables.dir.tests, ".tables tests folder")
