create_cache_directory = function(cache = "_blog2newsletter")
{
  NULL
}

check_for_cache = function(cache = "_blog2newsletter")
{
  root_dir = rprojroot::find_root(rprojroot::is_rstudio_project)
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
