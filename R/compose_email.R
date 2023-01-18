#' composes the email
#'
#' Given the blog directory, finds the RSS feed file, and grabs either the **latest**
#' or the post with a title matching the supplied title, and then finds the subscribers
#' that have the matching categories as in the post.
#'
#' @param blog_dir where the blog is (see details)
#' @param post whether to fetch the **latest** post or matching a title
#' @param subscriber_data the previously fetched subscriber data
#' @param from_email who is sending the email
#' @param extra_subject any extra text to add to the email subject
#'
#' @details While it is expected that **most** of the time that `blog_dir` will point
#'   to a local copy of a blog, and that the RSS file is at `_site/index.xml`,
#'   it doesn't have to. `blog_dir` can also be a fully qualified path the RSS file
#'   itself, or even the URL of an RSS file on the web, if you are using some other
#'   blogging platform and dont want to copy the RSS file to your local filesystem.
#'
#' @examples
#' \dontrun{
#' ## Using a local quarto blog,
#' ## where index.xml is under _site/index.xml
#' b2n_compose("path/to/local/blog",
#'              from_email = "me@place.com",
#'              extra_subject = "[cool newsletter]")
#'
#' ## using a local index.xml that is not in a quarto blog, or a non-standard location
#' b2n_compose("path/to/local/blog/_out/index.xml",
#'             ...)
#'
#' ## using a url
#' bn2_compose("https://myblog.com/index.xml",
#'             ...)
#' }
#'
#' @export
#' @return gmailr html message
b2n_compose = function(blog_dir,
                       post = "latest",
                       subscriber_data = NULL,
                       from_email = "",
                       extra_subject = "")
{
  if (is.null(subscriber_data)) {
    stop(cli::format_error(c("{.arg subscriber_data} is NULL!",
                             "x" = 'You left {.arg subscriber_data} as {.var NULL}.',
                             "i" = "Load {.arg subscriber_data} using {.var b2n_fetch_subscribers()} first.")))
  }
  post_data = b2n_find_post(blog_dir = blog_dir,
                            post = post)
  post_subscribers = b2n_match_subscribers_categories(unlist(post_data$item_category), subscriber_data)

  if (length(post_subscribers) == 0) {
    base::message(cli::format_message(c("x" = "No subscribers matching the post categories!",
                                        "i" = "No email to compose.")))
    return(invisible())
  }

  html_msg = gmailr::gm_mime() |>
    gmailr::gm_bcc(post_subscribers) |>
    gmailr::gm_from(from_email) |>
    gmailr::gm_subject(paste0(extra_subject, " ", post_data$item_title)) |>
    gmailr::gm_html_body(post_data$item_description)

  return(list(email = html_msg,
              post = dplyr::select(post_data, tidyselect::starts_with("item"))))
}

b2n_match_subscribers_categories = function(categories,
                                            subscriber_data)
{

  all_subscribers = subscriber_data |>
    dplyr::filter(category %in% "All") |>
    dplyr::pull(email)
  matched_subscribers = subscriber_data |>
    dplyr::filter(category %in% categories) |>
    dplyr::pull(email)
  c(all_subscribers, matched_subscribers)
}

#' find a post
#'
#' Return either the `latest` post, or one matching a provided title.
#'
#' @param blog_dir the location for the RSS
#' @param post which post to get
#'
#' @export
#' @return tibble
b2n_find_post = function(blog_dir,
                         post = "latest")
{
  if (fs::is_dir(blog_dir)) {
    rss_index = file.path(blog_dir, "_site", "index.xml")
    if (fs::file_exists(rss_index)) {
      rss_data = tidyRSS:::rss_parse(rss_index, list = FALSE, clean_tags = FALSE, parse_dates = TRUE) |>
        dplyr::arrange(dplyr::desc(item_pub_date))
    } else {
      stop(cli::format_error(c("No index.xml file found at {.file {rss_index}}:",
                               "x" = 'You provided {.arg blog_dir = "{blog_dir}"}',
                               "i" = "Please double check that the blog directory exists and has an RSS feed.")))
    }

  } else if (fs::is_file(blog_dir)) {
    if (fs::file_exists(blog_dir)) {
      rss_data = tidyRSS:::rss_parse(blog_dir, list = FALSE, clean_tags = FALSE, parse_dates = TRUE) |>
        dplyr::arrange(dplyr::desc(item_pub_date))
    } else {
      stop(cli::format_error(c("No file found at {.file {blog_dir}}:",
                               "x" = 'You provided {.arg blog_dir = "{blog_dir}"}',
                               "i" = "Please double check that the file exists.")))
    }
  } else if (grepl("^http", blog_dir)) {
    rss_data = tidyRSS::tidyfeed(blog_dir, list = FALSE, clean_tags = FALSE, parse_dates = TRUE) |>
      dplyr::arrange(dplyr::desc(item_pub_date))
  }

  if (post %in% "latest") {
    use_post = rss_data |>
      dplyr::slice_head(n = 1)
    cli::cli_alert_success("Using latest post titled: {use_post$item_title}.")
  } else {
    use_post = rss_data |>
      dplyr::filter(grepl(post, item_title, ignore.case = TRUE))
    if (nrow(use_post) == 0) {
      stop(cli::format_error(c("No posts found to send out:",
                               "x" = "You provided the title {.arg {post}}",
                               "i" = "Double check the spelling of the title provided in {.arg post}?")))
    } else if (nrow(use_post) > 1) {
      stop(cli::format_error(c("Multiple posts found to send out:",
                               "x" = "You provided the title {.arg {post}}",
                               "i" = "Double check the spelling of the title provided in {.arg post}?")))
    } else {
      cli::cli_alert_success("Matched supplied post title {post}: {use_post$item_title}.")
    }
  }
  return(use_post)
}

#' send it
#'
#' Send the drafted newsletter!
#'
#' @param draft_email email created by `b2n_compose`
#' @param cache the blog2newsletter cache location
#' @param force send it even if it was previously sent
#'
#' @export
#' @seealso b2n_compose
b2n_send_newsletter = function(draft_email,
                               cache = "_blog2newsletter",
                               force = FALSE)
{
  email_cache = file.path(cache, "emails")
  if (fs::dir_exists(cache)) {
    if (fs::file_exists(email_cache)) {
      old_emails = readRDS(email_cache)
    } else {
      old_emails = draft_email$post[-1, ]
      old_emails$send_date = as.character(NA)
    }
  }
  new_post = draft_email$post
  match_loc = which(old_emails$item_title %in% new_post$item_title)
  if (length(match_loc) == 0) {
    message(cli::cli_alert_info("Sending newsletter!"))
    gmailr::send_message(draft_email$email)

    new_post$send_date = as.character(Sys.time())
    old_emails = dplyr::bind_rows(old_emails, new_post)
    saveRDS(old_emails, file = email_cache)
    return(invisible())
  } else {

    if (length(match_loc) == 1) {
      match_links = old_emails[match_loc, ]
      if (force) {
        message(cli::cli_alert_info("Forcing sending newsletter!"))
        gmailr::send_message(draft_email$email)
        old_emails$send_date[match_loc] = as.character(Sys.time())
        saveRDS(old_emails, file = email_cache)
        return(invisible())
      } else {
        stop(cli::format_message(c('Found match to previously sent newsletter!',
                                   "i" = 'You previously sent "{match_links$item_title}" on {match_links$send_date}.',
                                   "i" = 'You can force resending it using {.arg force = TRUE}.')))
      }
    } else {
      stop(cli::format_message(c("Found multiple matches to previously sent newsletters.",
                                 "i" = "This shouldn't have happened, please examine the cache directly.",
                                 "i" = 'You can do this with {.var old_emails = readRDS("{cache}")}')))
    }
  }
}


#' creates draft in gmail
#'
#' Given the composed email, sends the email to GMail so it can be viewed in the *drafts* folder.
#'
#' @param draft_post the post, created with `b2n_compose`
#'
#' @export
#' @return NULL
b2n_draft_email = function(draft_post)
{
  gmailr::gm_create_draft(draft_post$email)
}
