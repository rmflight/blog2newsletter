% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compose_email.R
\name{b2n_from}
\alias{b2n_from}
\title{add from address}
\usage{
b2n_from(post_email, from_email = NULL)
}
\arguments{
\item{post_email}{generated by \code{b2n_post_to_email}}

\item{from_email}{the email address to use}
}
\value{
list
}
\description{
Adds a \code{from} email address to the \code{post_email}.
}
