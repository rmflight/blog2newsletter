#' composes the email
#'
#' Given the blog directory, finds the RSS feed file, and grabs either the **latest**
#' or the post with a title matching the supplied title, and then finds the subscribers
#' that have the matching categories as in the post.
#'
#' @param blog_dir where the blog is on your local machine
#' @param post whether to fetch the **latest** post or matching a title
#' @param subscriber_data the previously fetched subscriber data
#'
#' @export
#' @return gmailr html message
b2n_compose = function(blog_dir,
                       post = "latest",
                       subscriber_data = NULL,
                       from_email = NULL,
                       extra_subject = NULL)
{
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
    gmailr::gm_subject(paste0(extra_subject, post_data$item_title)) |>
    gmailr::gm_html_body(post_data$item_description)

  return(html_msg)
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

b2n_find_post = function(blog_dir,
                         post = "latest")
{
  rss_index = file.path(blog_dir, "_site", "index.xml")
  if (file.exists(rss_index)) {
    rss_data = tidyRSS:::rss_parse(rss_index, list = FALSE, clean_tags = FALSE, parse_dates = TRUE) |>
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
    } else if (nrow(use_post > 1)) {
      stop(cli::format_error(c("Multiple posts found to send out:",
                               "x" = "You provided the title {.arg {post}}",
                               "i" = "Double check the spelling of the title provided in {.arg post}?")))
    } else {
      cli::cli_alert_success("Matched supplied post title {post}: {use_post$item_title}.")
    }
  }
  return(use_post)
}

b2n_send_newsletter
