#' clean subscriber file
#'
#' Given fetched subscriber Google Sheet, cleans it and reformats it to be more useful.
#'
#' @param subscriber_info a data.frame from reading the Google Sheet
#'
#' @export
#' @return tibble
b2n_clean_subscribers = function(subscriber_info)
{
  email_col = which(grepl("email", names(subscriber_info), ignore.case = TRUE))
  categories_col = which(grepl("categories", names(subscriber_info), ignore.case = TRUE))

  split_categories = strsplit(subscriber_info[[categories_col]], ", ")
  nrep_categories = purrr::map_int(split_categories, length)

  tibble::tibble(email = rep(subscriber_info[[email_col]], times = nrep_categories),
                 category = unlist(split_categories))
}

#' fetch subscriber info
#'
#' Given a Google Sheet ID, fetches the subscriber data, cleans it, and caches
#' it so that it doesn't need to be fetched again.
#'
#' @param sheet_id either a Google Sheets URL or ID
#' @param cache where to cache the subscriber data
#' @param force_fetch override the cache, and force it to fetch from the web
#' @param cache_timeout how old should the cache be before definitely getting it from the web
#'
#' @return tibble
#' @export
b2n_fetch_subscribers = function(sheet_id = NULL,
                                 cache = "_blog2newsletter",
                                 force_fetch = FALSE,
                                 cache_timeout = 168)
{
  if (is.null(sheet_id)) {
    cli::cli_abort(c(
      "{.arg sheet_id} cannot be NULL!",
      "x" = "You called the function with {.arg sheet_id = NULL}."
    ))
  }

  subscriber_cache = file.path(cache, "subscribers")
  if (fs::file_exists(subscriber_cache)) {
    cache_mtime = file.mtime(subscriber_cache)
    if ((difftime(Sys.Date(), cache_mtime, units = "hours") > cache_timeout) || force_fetch) {
      subscriber_data = googlesheets4::read_sheet(sheet_id) |>
        b2n_clean_subscribers()
      save_subscriber_cache(subscriber_data, cache = subscriber_cache)
      message(cli::format_message(c(
        "Reading subscribers from the web, and caching them under {cache}.",
        "i" = "You can change the blog2newsletter cache location by modifying {.arg cache}."
      )))
    } else {
      subscriber_data = get_subscriber_cache(subscriber_cache)
      message(cli::format_message(c(
        "Reading subscribers from the cache file under {cache}.",
        "i" = "You can force fetching from the web using {.arg force_fetch = TRUE}."
      )))
    }
  } else {
    if (dir.exists(dirname(cache))) {
      subscriber_data = googlesheets4::read_sheet(sheet_id) |>
        b2n_clean_subscribers()
      save_subscriber_cache(subscriber_data, cache = subscriber_cache)
      message(cli::format_message(c(
        "Reading subscribers from the web, and caching them in {cache}.",
        "i" = "You can change the cache location by modifying {.arg cache}."
      )))
    } else {

      stop(cli::format_error(c("{.file {cache}} directory doesn't exist!",
                               "i" = "You can create the cache directory using {.fun create_cache_directory}.")))
    }

  }
  return(subscriber_data)
}

get_subscriber_cache = function(cache = "_blog2newsletter/subscribers")
{
  subscriber_data = readRDS(cache)
  subscriber_data
}

save_subscriber_cache = function(subscriber_data,
                                 cache = "_blog2newsletter/subscribers")
{
  saveRDS(subscriber_data, cache)
}
