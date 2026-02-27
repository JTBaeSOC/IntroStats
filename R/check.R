#' Check a practice exam answer
#'
#' Compares the student's calculator answer against the correct value
#' recomputed from whatever data is currently defined in the student's
#' environment. Both sides are rounded to 2 decimal places before comparison,
#' matching standard calculator precision.
#'
#' @param answer A numeric variable named q1 through q8 holding the student's
#'   answer. Assign your answer first, then pass the variable:
#'   \code{q1 <- 3.50; check(q1)}
#'
#' @return Prints \code{Correct!} or \code{Not quite} to the console.
#'   Returns \code{NULL} invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' treat1   <- c(10, 13, 5, 6, 8, 16)
#' control1 <- c(3, 12, 18, 5, 6, 8, 5)
#' q1 <- 1.52
#' check(q1)
#' }
check <- function(answer) {
  var_name <- deparse(substitute(answer))
  env      <- parent.frame()   # student's environment — picks up any edits to the data

  # Fetch a named object from the student's environment
  get_var <- function(name) {
    if (exists(name, envir = env, inherits = TRUE))
      get(name, envir = env, inherits = TRUE)
    else
      stop("Could not find '", name, "'. Make sure the data chunk has been run.",
           call. = FALSE)
  }

  valid_qs <- paste0("q", 1:8)
  if (!var_name %in% valid_qs) {
    message("Variable '", var_name, "' not recognised. ",
            "Make sure you name your answer q1 through q8.")
    return(invisible(NULL))
  }

  # Guard: answer not yet filled in
  if (is.na(answer)) {
    cat("\u270f\ufe0f Replace NA with your answer, then re-run check().\n")
    return(invisible(NULL))
  }

  # switch() is lazy — only fetches the variables needed for the question checked
  correct <- switch(var_name,
    q1 = {
      treat1   <- get_var("treat1")
      control1 <- get_var("control1")
      mean(treat1) - mean(control1)
    },
    q2 = {
      treat2   <- get_var("treat2")
      control2 <- get_var("control2")
      mean(treat2) - mean(control2)
    },
    q3 = { tiktok <- get_var("tiktok"); mean(tiktok) },
    q4 = { tiktok <- get_var("tiktok"); sd(tiktok) },
    q5 = {
      tiktok <- get_var("tiktok")
      (tiktok[1] - mean(tiktok)) / sd(tiktok)
    },
    q6 = {
      tbl <- get_var("wvs_tbl")
      tbl["Democrat", "Agree"] / sum(tbl["Democrat", ]) * 100
    },
    q7 = {
      tbl <- get_var("wvs_tbl")
      tbl["Republican", "Disagree"] / sum(tbl[, "Disagree"]) * 100
    },
    q8 = {
      tbl <- get_var("wvs_tbl")
      sum(tbl[, "Hard to say"]) / sum(tbl) * 100
    }
  )

  # Compare rounded to 2 d.p. — matches what a student does on a calculator
  if (round(as.numeric(answer), 2) == round(correct, 2)) {
    cat("\u2705 Correct! Well done.\n")
  } else {
    cat("\u274c Not quite \u2014 double-check your calculation and try again.\n")
  }

  invisible(NULL)
}
