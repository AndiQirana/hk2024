# removes results from .R files in R/ when sourced

# derived from bschneidr's answer to 
# https://stackoverflow.com/questions/25548333/r-find-and-replace-multiple-scripts-at-once
# Define function to find-and-replace text in a single file
file_find_replace <- function(filepath, pattern, subpattern, replacement) {
  # if(!grepl(subpattern, pattern)) stop("'subpattern' needs to be found in 'pattern'")
  file_contents <- readLines(filepath)
  print(file_contents)
  updated_contents <- 
    lapply(file_contents, function(x) ifelse(grepl(pattern, x),
                                             gsub(subpattern, 
                                                  replacement,
                                                  x),
                                             x)
    )
  cat(unlist(updated_contents), file = filepath, sep = "\n")
}

# function to find and replace function body
# deletes function bodies in a given filepath
# function body is here defined as the lines between 'startLine'
# and 'endLine' with the additional requirement of the nuber of
# open curly brackets equaling 0 at the endline
# alternately, if the 'comStartLine' is found, it will delete
# all the lines from there until the next time an endline with a 
# net 0 of open curly brackets is found
file_func_find_replace <- function(filepath, startLine, endLine = "^\\}$", 
                                   replacement, comStartLine = "^# SOLUTION$") {
  file_contents <- readLines(filepath)
  
  for (lineCounter in seq_len(length(file_contents))) {
    # match the startLine and remember line number
    if(grepl(startLine, file_contents[lineCounter])
       || grepl(comStartLine, file_contents[lineCounter])){
      startNum <- lineCounter
      #comment starting line handling
      comStart <- FALSE
      if(grepl(comStartLine, file_contents[lineCounter])) comStart <- TRUE
      if(comStart){ 
        repeat {
          lineCounter <- lineCounter + 1
          if(grepl(startLine, file_contents[lineCounter])) break
        }
      }
      # curly bracket counter
      bc <- 1
      repeat {
        lineCounter <- lineCounter + 1
        # add one to counter per opening bracket and subtract per closing
        opb <- gregexpr("\\{", file_contents[lineCounter])[[1]]
        ifelse(-1 %in% opb,
               opbc <- 0, opbc <- length(opbc))
        clb <- gregexpr("\\}", file_contents[lineCounter])[[1]]
        ifelse(-1 %in% clb,
               clbc <- 0, clbc <- length(clbc))
        bc <- bc + opbc - clbc
        
        if(bc < 0) stop("There is an error in the function, '{' count doesnt match '}' count")
        if(bc == 0) ifelse(grepl(endLine, file_contents[lineCounter]), break, 
                           stop("'endLine' doesnt match curly brace count end of function.
                                Maybe the end '}' is a bit indented"))
      }
      # replace every function body line with 'NAL' and the first body line with replacement
      distance <- lineCounter - startNum
      if(distance > 1) {
        file_contents[startNum + 1] <- replacement
        if(distance > 2) {
          for (midLine in seq(startNum + 2, lineCounter - 1)) {
            file_contents[midLine] <- "NAL"
          }
        }
      }
      # delete completely if comStart
      if(comStart) file_contents[seq(startNum,lineCounter)] <- "NAL"
    }
  }
  # remove 'NAL' in contents
  updated_contents <- file_contents[file_contents != "NAL"]
  # write to file
  cat(updated_contents, file = filepath, sep = "\n")
}


# files in R/
r_scripts <- list.files(path = "R/", pattern = "(r|R)$")

# remove function arguments
# looks for lines like: 'ex01blabla...function(...) {'
# and removes the ... in function(...)
capture.output(lapply(r_scripts, function(x) file_find_replace(
                filepath = paste0("R/", x),
                pattern = "^ex[0-9]{2}.*function(.*) .*\\{$",
                subpattern = "function(.*) \\{$",
                replacement = "function() \\{"
                )
))

#remove solution in function
capture.output(lapply(r_scripts, function(x) file_func_find_replace(
                filepath = paste0("R/", x),
                startLine = "^ex[0-9]{2}.*function(.*) .*\\{$",
                replacement = "  # your code")
))


