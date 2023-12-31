% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google-storage.R
\name{download_cloud_file}
\alias{download_cloud_file}
\title{Download an object from a cloud storage bucket to a local file}
\usage{
download_cloud_file(name, provider, options, file = name)
}
\arguments{
\item{name}{the name of the object in the storage bucket.}

\item{provider}{cloud provider to use, either "gcs" or "aws"}

\item{options}{named list with cloud provider options, see details}

\item{file}{a file-path (character) where the object will be saved. Default
is the object name.}
}
\value{
the file path
}
\description{
Download object from the cloud storage to a local file
}
\examples{

# Google Cloud Services
\dontrun{
authentication_details <- readLines("location_of_json_file.json")
download_cloud_file(
  name = "timor-landings-v2_metadata__20210326084600_54617b3__.json",
  provider = "gcs",
  options = list(
    service_account_key = authentication_details,
    bucket = "my-bucket"
  )
)
}
}
