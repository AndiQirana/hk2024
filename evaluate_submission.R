#!/usr/bin/env Rscript
library("checkmate")  # idiot renv doesn't load this *even though* it is in the lockfile otherwise.

stopfUnex <- function(errid, fmt, ..., error) {
  if (!missing(error)) {
    message <- sprintf("\n Message: \"%s\"", error$message)
  } else {
    message <- ""
  }
  fmt <- paste0(fmt, "%s
If this error was unexpected and you can not solve this problem, please contact a tutor of this course.
Quote this message and the Error ID: %s.
Traceback:
%s")
  args <- c(list(fmt = fmt), list(...),
            list(message, errid, paste0(capture.output(traceback(11, max.lines = 2)), collapse = "\n")))
  stop(do.call(sprintf, args), call. = FALSE)
}


setGlobals <- function(argv, uncompiled = FALSE) {
  #TODO check for 'argv[["dir"]]' not current wd
  
  if (uncompiled) {
    # check all necessary things present
    # assets.path or tests.path
    # CANONICAL_R_FILES
    CANONICAL_R_FILES <- list.files(file.path(dir, "R"))
    assets.path <- file.path(dir, "assets")  # path where additional files are stored
    tests.dir <- tests.dir <- file.path(dir, "tests")
    # optional tables dir
    tables.dir <- file.path(assets.path, "tables")
    # defaults from template
    lintr.file <- "template/.lintr"
    helpers.dir <- "template/.github/helpers"
    
  } else {
    
    #TODO append dir argument path to all locations; list variables in list and return
    CANONICAL_R_FILES <- list.files("R") # names of the files in /R
    
    script.name <- "evaluate_submission.R"  # name of this file itself
    assets.path <- file.path(".github", "evaluation")  # path where additional files are stored
    tests.path <- file.path(assets.path, "tests")  # test directory
    rprofile.file <- ".Rprofile"
    lintr.file <- ".lintr"
    tests.dir <- file.path(assets.path, "tests")
    helpers.dir <- ".github/helpers"
    tables.dir <- "tables"
    
  }
  
  #TODO if dir supplied check or maybe just supply function with current wd if called as main
  invocation.path <- getwd() 
  
  script.dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  if (is.null(script.dir)) {
    script.opts <- commandArgs(trailingOnly = FALSE)
    script.dir <- dirname(sub("^--file=", "", grep("^--file=", script.opts, value = TRUE)))
    if (length(script.dir) > 1) {
      stopfUnex(1,
                "Error while trying to determine script path: commandArgs() returned %s",
                paste0("\"", script.opts, "\"", collapse = ", "))
    }
  }
  
  if (!length(script.dir)) {
    message("Could not determine script path, assuming it is the current working directory.")
    script.dir = "."
  }
  
  setwd(script.dir)
  #print(script.dir)
  working.path <- getwd()
  
  argv[["CANONICAL_R_FILES"]] <- CANONICAL_R_FILES
  argv[["script.name"]] <- script.name
  argv[["assets.path"]] <- assets.path
  argv[["tests.path"]] <- tests.path
  argv[["lintr.file"]] <- lintr.file
  argv[["tests.dir"]] <- tests.dir
  argv[["helpers.dir"]] <- helpers.dir
  argv[["tables.dir"]] <- tables.dir
  argv[["rprofile.file"]] <- rprofile.file
  argv[["invocation.path"]] <- invocation.path
  argv[["script.dir"]] <- script.dir
  argv[["working.path"]] <- working.path
  
  #print(argv)
  #TODO
  return(argv)
}


# parse additional arguments such as which exercises to check and whether to skip style checks
parseAddArgs <- function(argv) {
  line_args <- commandArgs(trailingOnly = TRUE)
  SKIP_STYLE <- get0("SKIP_STYLE", ifnotfound = FALSE) || "--skip-style" %in% line_args
  EXERCISES <- get0("EXERCISES", ifnotfound = character(0))
  if (length(malformed.ex <- EXERCISES[!grepl("^[0-9]+(\\.[0-9]+)?$", EXERCISES, perl = TRUE)])) {
    stop(sprintf("Malformed EXERCISES value: %s", paste(sprintf("'%s'", malformed.ex), collapse = ", ")))
  }
  EXERCISES <- c(EXERCISES, sub("^--exercise=", "", grep("^--exercise=", line_args, value = TRUE)))
  if (length(malformed.ex <- EXERCISES[!grepl("^[0-9]+(\\.[0-9]+)?$", EXERCISES, perl = TRUE)])) {
    stop(sprintf("Malformed --exercise= argument(s): %s", paste(sprintf("'--exercise=%s'", malformed.ex), collapse = ", ")))
  }
  lint.exclusions <- NULL
  if (length(EXERCISES)) {
    included.exercises <- as.numeric(unique(EXERCISES, gsub("\\..*", "", EXERCISES)))
    if (length(included.exercises)) {
      excl <- file.path("R", argv[["CANONICAL_R_FILES"]][-included.exercises])
      lint.exclusions <- sapply(excl, function(x) Inf, simplify = FALSE)  # lintr exclusions have weird format
    }
  }
  
  argv[["SKIP_STYLE"]] <- SKIP_STYLE
  argv[["EXERCISES"]] <- EXERCISES
  argv[["lint.exclusions"]] <- lint.exclusions
  
  return(argv)
}


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

verifyHash <- function(filename, description, argv, assert = TRUE) {
  if (file.exists(filename) && !dir.exists(filename)) {
    file.content <- readFile(filename, description)
  } else {
    files <- sort(list.files(filename, pattern = "\\.(r|csv|zip)$", ignore.case = TRUE, all.files = TRUE, full.names = TRUE, recursive = TRUE))
    file.content <- lapply(files, readFile, description = paste(description, "content"))
    file.content <- lapply(file.content, digest::digest, algo = "sha256", serialize = FALSE)
    file.content <- paste(c(file.content, ""), collapse = "\n")  # need to append final newline
  }
  file.hash <- digest::digest(file.content, algo = "sha256", serialize = FALSE)
  #print(getwd())
  check.hash <- readFile(file.path(argv[["assets.path"]], paste0(basename(filename), ".sha256")),
                         sprintf("hash of %s", description), nchar(file.hash))
  if (file.hash != check.hash) {
    if (assert) stopfUnex(4, "Script file hash mismatch. You did not edit %s, did you?" , description)
    return(FALSE)
  }
  invisible(TRUE)
}

#function that lets you output colored text
printColor <- function(txt, color, highlight = FALSE) {
  colors <- c("green", "red", "yellow", "blue", "purple", "cyan")
  if(!(tolower(color) %in% colors)) stop(
    paste("must be one of: ", paste(colors, collapse = ", ")))
  if(class(highlight) != "logical") stop("highlight must be logical")
  
  val <- 0
  
  switch (color,
          "red" = val <- 31,
          "green" = val <- 32,
          "yellow" = val <- 33,
          "blue" = val <- 34,
          "purple" = val <- 35,
          "cyan" = val <- 36
  )
  
  if(highlight) val <- val + 10
  
  paste0("\033[0;", val, "m", txt,"\033[0m","\n")
}


# code style checking
checkStyle <- function(argv) { #TODO arguments
  SKIP_STYLE <- argv[["SKIP_STYLE"]]
  any.lints <- NULL
  lint.exclusions <- argv[["lint.exclusions"]]
  if (SKIP_STYLE) {
    cat(printColor("Skipping style test!\n", "yellow"))
  } else {
    cat("Checking Style...")
    any.lints <- lintr::lint_dir("R", exclusions = lint.exclusions)
    cat("DONE\n")
    if (length(any.lints)) {
      cat(printColor("Style issues found:\n", "red"))
      if (lintr:::in_github_actions()) {
        Sys.setenv(GITHUB_ACTIONS = "false")
        print(any.lints)
      } else {
        print(any.lints)
        options("lintr.rstudio_source_markers" = FALSE)
        print(any.lints)
      }
      
    } else {
      cat(printColor("No style issues found!\n", "green"))
    }
  }
  argv[["any.lints"]] <- any.lints
  return(argv)
}


exerciseTesting <- function(argv) {
  cat("-----\nStarting tests...")
  
  tests.dir <- argv[["tests.dir"]]
  testenv <- argv[["testenv"]]
  #print(sprintf("test dir here : %s", tests.dir))
  withCallingHandlers(
    invisible(capture.output(testsList <- testthat::test_dir(tests.dir, env = testenv,
                                                             reporter = testthat::ListReporter,
                                                             stop_on_failure = FALSE,
                                                             stop_on_warning = FALSE))),
    interrupt = function(cond) {
      message("\nInterrupted.")
      tb <- .traceback(3)
      callnames <- vapply(tb, function(x) paste(x, collapse = "\n"), character(1))
      ttindex <- grep("^test_that", callnames)
      if (length(ttindex)) {
        ttstatement <- callnames[[ttindex[[1]]]]
        ttstatement <- sub('^[^"]*"', "", ttstatement)
        ttstatement <- sub('".*', "", ttstatement)
        message(sprintf("Interrupt occurred most likely in test: %s", ttstatement))
        callnames <- callnames[seq_len(ttindex - 1)]
      }
      userfunctions <- paste(rex::escape(names(testenv)), collapse = "|")
      useful.lines <- grep(paste0("(^expect_|", userfunctions, ")"), callnames, perl = TRUE)
      message("Traceback (top level functions only):")
      for (ll in useful.lines) {
        message(sprintf("%s: %s", ll, callnames[[ll]]))
      }
    })
  cat(" DONE\nResults:\n\n")
  argv[["testsList"]] <- testsList
  return(argv)  
}


printTestResults <- function(argv) {
  EXERCISES <- argv[["EXERCISES"]]
  testsList <- argv[["testsList"]]
  any.lints <- argv[["any.lints"]]
  SKIP_STYLE <- argv[["SKIP_STYLE"]]
  any.failed <- FALSE
  skipped <- character(0)
  counter <- 0
  remaininglines <- 900
  for (testentry in testsList) {
    # check that tests are correctly named with "Exercise 01: ..."
    if(!grepl("^Exercise [0-9]{2}:", testentry$test)) stop(
      "Test should be named in the format: 'Exercise 01: ...'"
    )
    # create test-identities in the format of "1.1" for all tests
    testidentity <- sprintf("%s.%s",
                            as.numeric(regmatches(testentry$context, regexpr("[0-9]+", testentry$context))),
                            as.numeric(regmatches(testentry$test, regexpr("[0-9]+", testentry$test))))
    
    if (length(testentry$results) == 1 && "expectation_skip" %in% class(testentry$results[[1]])) {
      skipped <- c(skipped, paste(testentry$context, testentry$test))
      next
    }
    failed.result <- Filter(function(x) !"expectation_success" %in% class(x), testentry$results)
    
    #TODO whats the xounter for exactly?
    if (length(failed.result)) {
      if (testidentity %in% EXERCISES && counter < 15) {
        cat(sprintf("Test for %s %s:", testentry$context, testentry$test),
            printColor(sprintf("(%s) tests failed\n", length(failed.result)), "red"))
        faili <- 1
        for (fr in failed.result) {
          if (counter > 15) break else counter <- counter + 1
          msg <- sprintf("Failure %s:\n%s\n\n", faili,
                         gsub("\n", "\n** ", fixed = TRUE, fr$message)
          )
          msg <- c(strsplit(msg, "\n")[[1]], "")
          if (length(msg) > remaininglines) {
            msg <- c(head(msg, remaininglines), "...truncated. Solve some issues to see the rest!\n")
          }
          cat(paste(msg, collapse = "\n"))
          remaininglines <- remaininglines - length(msg)
          if (remaininglines < 0) break
          faili <- faili + 1
        }
      } else {
        counter <- counter + 1
        any.failed <- TRUE
        cat(sprintf("Test for %s %s:", testentry$context, testentry$test),
            printColor(sprintf("(%s) tests failed\n", length(failed.result)), "red"),
            sprintf("First failure:\n** %s\n\n",
                    gsub("\n", "\n** ", fixed = TRUE, failed.result[[1]]$message)))
      }
    } else {
      cat(sprintf("Test for %s -- %s:" , testentry$context, testentry$test),
          printColor("SUCCESS", "green"), "\n\n")
    }
  }
  if (length(skipped)) {
    cat(printColor("Skipped some tests:", "yellow"),
        sprintf("\n%s\n\n", paste(skipped, collapse = "\n")))
  }
  if (any.failed) {
    stop(printColor("Some tests failed.", "red"))
  }
  if (length(any.lints)) {
    stop("Please make sure you fix the style issues!")
  }
  if (length(skipped) || length(EXERCISES)) {
    message("Please note this did not check all tests.")
  } else if (SKIP_STYLE) {
    message(printColor("Please make sure you check the style as well!", "yellow"))
  } else {
    message("
  This looks fine. Make sure you push your results to GitHub and check
  that the test shows a green status there, then you're done!")
  }
}


hashCheck <- function(argv) {
  tests.dir <- argv[["tests.dir"]]
  helpers.dir <- argv[["helpers.dir"]]
  tables.dir <- argv[["tables.dir"]]
  script.name <- argv[["script.name"]]
  lintr.file <- argv[["lintr.file"]]
  rprolife.file <- argv[["rprofile.file"]]
  #TODO argv stuff
  verifyHash(script.name, "script-file", argv)
  verifyHash(lintr.file, ".lintr-file", argv)
  verifyHash(helpers.dir, "helper-files", argv)
  verifyHash(tables.dir, "'tables' folder", argv)
  verifyHash(rprolife.file, ".Rprofile-file", argv)
  # this Catch is used if the previous call of this script doesnt complete due to error
  # and the .assist folder is not removed from the tests directory
  if (!verifyHash(tests.dir, "tests directory", argv, FALSE)) {
    file.copy("./tables", "./.github/evaluation/tests", recursive = TRUE)
    verifyHash(tests.dir, "tests directory", argv)
  }
}


main <- function(inv.dir) {
  argv <- list("dir" = inv.dir)
  
  argv <- setGlobals(argv)
  argv <- parseAddArgs(argv)
  
  hashCheck(argv)
  
  ###TODO possibly
  script.name <- argv[["script.name"]]
  script.dir <- argv[["script.dir"]]
  #TODO need the working path?
  working.path <- argv[["working.path"]]
  invocation.path <- argv[["invocation.path"]]
  EXERCISES <- argv[["EXERCISES"]]
  
  if (!file.exists(script.name)) {
    stopfUnex(2, "Could not find evaluation file \"%s\" in directory \"%s\".
Script was called from path \"%s\" and then changed to \"%s\".
Maybe you did not launch the \"%s\"-script the intended way, using `RScript %s`?",
              script.name, working.path, invocation.path, script.dir, script.name, script.name)
  }
  ###
  
  testenv <- new.env(parent = .GlobalEnv)
  # .R files
  allfiles <- list.files("R", pattern="\\.[rR]$", recursive=TRUE, full.names=TRUE)
  # all relevant helper functions in the helper folder
  helper.files = list.files(c(".github/helpers"),
                            pattern="*.R$", full.names=TRUE,
                            ignore.case=TRUE)
  # source both into test environment
  invisible(lapply(helper.files, source, local = testenv,
         echo = FALSE, verbose = FALSE, prompt.echo = FALSE))

  #TODO this is pretty dumb. But i cant figure out how to get the location of errors when sourcing multiple files
  #TODO also tried to do this withCallingHandlers and a log at first, but scrapped for complexity
  # included to give better error msg when sourcing 'R' dir
  sourceWithHandlers <- function(file) {
    tryCatch(
      source(file, 
               local = testenv,
               echo = FALSE, verbose = FALSE, prompt.echo = FALSE),
        error = function(e) {
          stop(sprintf("Error sourcing the File %s\n >>> %s
Please remove code errors and erroneous function calls from the 'R' directory,
as testing cannot be executed without being able to source these files.",
                       file, e))
        },
        warning = function(w) {
          warning(sprintf("Warning when sourcing the File %s\n >>> %s
This may leed to problems down the line while testing.
Keep in mind and possibly resolve issues beforehand.",
                         file, w))
        }
      )
  }
  lapply(allfiles, sourceWithHandlers)
  
  
  testenv$EXERCISES <- EXERCISES
  testenv$SELF <- testenv
  testenv$WORKINGPATH <- working.path
  ###
  
  argv[["testenv"]] <- testenv
  
  #TODO next two lines necessary?
  #argv[["allfiles"]] <- allfiles
  #argv[["helper.files"]] <- helper.files
  
  argv <- checkStyle(argv)
  
  #print(paste("tests dir main", argv[["tests.dir"]]))
  printTestResults(exerciseTesting(argv))
  
  return(0)
}


# run main function when wanted (unless specified not to)
if (!exists("load.as.library")) main(inv.dir = getwd())

