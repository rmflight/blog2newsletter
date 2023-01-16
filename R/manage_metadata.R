#' create cache
#'
#' Creates the cache directory to be used by `blog2newsletter`.
#'
#' @param cache the subdirectory to use
#'
#' @export
#' @return invisible
b2n_init = function(r_script = "_blog2newsletter.R",
                    cache = "_blog2newsletter")
{
  if (!file.exists(r_script)) {
    file.create(r_script)
    cli::cli_alert_success("Created file {.file {r_script}}.")
  } else {
    cli::cli_alert_info("{.file {r_script}} already exists, doing nothing.")
  }

  if (!dir.exists(cache)) {
    dir.create(cache)
    cli::cli_alert_success("Created directory {.file {cache}}.")
  } else {
    cli::cli_alert_info("{.file {cache}} already exists, doing nothing.")
  }
  invisible()
}


b2n_check_for_cache = function(cache = "_blog2newsletter")
{

  if (dir.exists(cache)) {
    return(invisible())
  } else {
    stop(cli::format_error(c("Cache directory {.file {cache}} not found.",
                           "i" = "You can create the cache directory using {.fun b2n_init}.")))
  }
}

b2n_check_for_runfile = function(r_script = "_blog2newsletter.R")
{

  if (file.exists(r_script)) {
    return(invisible())
  } else {
  stop(cli::format_error(c("r_script {.file {r_script}} found.",
                             "i" = "You can create the run file using {.fun b2n_init}.")))
  }
}
