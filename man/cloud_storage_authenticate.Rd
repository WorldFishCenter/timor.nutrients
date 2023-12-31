% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google-storage.R
\name{cloud_storage_authenticate}
\alias{cloud_storage_authenticate}
\title{Authenticate to a storage cloud provider}
\usage{
cloud_storage_authenticate(provider, options)
}
\arguments{
\item{provider}{cloud provider to use, either "gcs" or "aws"}

\item{options}{named list with cloud provider options, see details}
}
\description{
Usually used internally by other functions
}
\details{
### Google Cloud Services

For Google Cloud Services ("gcs") options must be a list with the field
`service_account_key` with the contents of the authentication json file you
have downloaded from your Google Project.

This function uses [googleCloudStorageR::gcs_auth] under the hood to
authenticate.
}
\examples{

# Google Cloud Services
\dontrun{
authentication_details <- readLines("location_of_json_file.json")
cloud_storage_authenticate(
  provider = "gcs",
  options = list(
    service_account_key = authentication_details,
    bucket = "my-bucket"
  )
)
}
}
