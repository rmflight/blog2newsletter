#' create cache
#'
#' Creates the cache directory to be used by `blog2newsletter`.
#'
#' @param cache the subdirectory to use
#'
#' @export
#' @return invisible
create_cache_directory = function(cache = "_blog2newsletter")
{
  root_dir = rprojroot::find_root(proj_or_pkg_root)
  cache_loc = file.path(root_dir, cache)
  dir_made = dir.create(cache_loc)
  if (dir_made) {
    cli::cli_alert_success("Created cache directory {.file {cache_loc}}.")
  } else {
    message(cli::format_message(c("x" = "Couldn't create cache directory {.file {cache_loc}}.",
                          "i" = "Do you have write permissions in this directory?")))
  }
  invisible()
}

proj_or_pkg_root = rprojroot::root_criterion(function(path){
  file.exists(file.path(path, "DESCRIPTION")) || file.exists(Sys.glob(file.path(path, "*.Rproj")))
},
  "has DESCRIPTION or .Rproj")

check_for_cache = function(cache = "_blog2newsletter")
{

  root_dir = rprojroot::find_root(proj_or_pkg_root)
  if (dir.exists(file.path(root_dir, cache)) || file.exists(file.path(root_dir, "DESCRIPTION"))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

message_missing_cache = function()
{
  has_cache = check_for_cache()

  if (!has_cache) {
    message(cli::format_message(c(
      "blog2newletter couldn't find the cache to store some information.",
      "i" = "You can create a cache using {.fun create_cache_directory}."
    )))
  }
  invisible()
}

rlang::on_load(message_missing_cache())
