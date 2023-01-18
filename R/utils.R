#' run full flow
#'
#' Runs a workflow defined in `_blog2newsletter.R`
#'
#' @param run_file which file to run through
#'
#' @export
#' @return invisible
b2n_run = function(run_file = "_blog2newsletter.R")
{
  if (fs::file_exists(run_file)) {
    message(cli::cli_alert_info("Running workflow at {.file {run_file}}!"))
    source(run_file, echo = FALSE)
    return(invisible())
  } else {
    stop(cli::format_error(c("Run file not found!",
                             "i" = "You provided {.file {run_file}}, and R can't find it!")))
  }
}
