# Exam 1

There are 4 tasks for you to solve.

The exam is structured like the homework repositories, with ‘.R’ files in the ‘R’ directory. You shall solve the tasks in this directory and you can test your solutions with ‘evaluate_submission.R’. Note that you will not always have access to all tests. 
Task 01 does not have tests.
Task 03 is tested with respect to statistics that the objects you are expected to create only. 
This means this tests are semi-blind.
The tasks are provided in the designated ".R" files in the "R" directory.


### Work on the Problems

1. Look at the `.R`-files in the `R/`-directory. They are sorted by increasing difficulty and subdivided into different topics they cover, giving you a hint about where in the material you should look to find guidance. In each file you will find empty functions, each with a heading in the way of *"Write a function that does X"*. Fill the body of the function (i.e. the part between the curly braces `{ }`) with code that does what is asked for. Example

     ```r
     # write a function that divides x by y
     ex01Divide <- function(x, y) {
       # your code
     }
     ```

    should become

    ```r
    # write a function that divides x by y
    ex01Divide <- function(x, y) {
      x / y
    }
    ```

    (remember that the function always returns the result of the last command executed in it.)

    In this Course, we are not making use of any libraries besides `checkmate`, `data.table`, `R6`, and `ggplot2`, and what comes with `R` by default. **The libraries are loaded automatically, so don't `library()` them in your code** (after `renv::restore()` you may have to restart your R-session to get access to them, see above). Please don't use `require(`, `library(`, or `::` / `:::` or anything else that loads other libraries, and don't install further packages for this task. Furthermore, this is an `R` course, so don't run any external software from your code.

    Remember that we are also checking **code style**.
	
	We are going to verify you are using `checkmate`. **E.g. when the task tells you to expect a single positive integer, you have to `assertInt(x, lower = 1)` or `assertCount(x, positive = TRUE)` your function argument.** (For `data.table` arguments, only `assertDataTable(<argument>)` is necessary).

    You don't need to write tests in `tests/` and the content is not going to be graded (but you are invited to do this for your own sake).

2. Work gradually towards a solution. I personally find it useful to work with a working script where I try out different functions on different inputs to see how they work in particular cases, and how they may work together. An example of what that may look like is the `working_notebook.Rmd` file that you can open in RStudio and look at. You may of course use any style you prefere.

### Check your Result Frequently

3. You can run the `evaluate_submission.R` script with

    ```sh
    Rscript evaluate_submission.R
    ```

    to check *all* results, but you can also do

    ```sh
    Rscript evaluate_submission.R --skip-style
    ```

    to skip the style-test. Use

    ```sh
    Rscript evaluate_submission.R --exercise=1
    ```

    to only evaluate a single exercise. You can also test sub-exercises `--exercise=1.1` and combine different `--exercise`-arguments as well as the `--skip-style` argument. For example,

    ```sh
    Rscript evaluate_submission.R --exercise=1 --exercise=2.1 --skip-style
    ```

    checks all of exercise 01, and task ex01 of exercise 02 and skips style checks. When `--exercise=` arguments are given and style checks are not skipped, then only the style of the `R`-files containing the requested exercise are checked. Notice, that selecting single exercises to evaluate often results in a more detailed error description!

    You can open `evaluation_notebook.Rmd` and run the evaluation from there. See the notebook on how to restrict checks to certain exercises.

    Reminder: to make sure that you run the same grading tool that we will use, **it is important that you don't change the `evaluate_submission.R` file, and that you don't change the content of the `.github` folder!** If you edit these files it is possible that your submission contains errors but that the script won't show them to you.

