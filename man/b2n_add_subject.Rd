% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compose_email.R
\name{b2n_add_subject}
\alias{b2n_add_subject}
\title{add subject}
\usage{
b2n_add_subject(post_email, presubject = "")
}
\arguments{
\item{post_email}{the post email from \code{b2n_post_to_email}}

\item{presubject}{extra text to prepend to the subject line}
}
\value{
list
}
\description{
Adds the post title as a subject, and optionally adds extra text in front of it.
}
