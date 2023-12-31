% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processing.R
\name{run_permanova_clusters}
\alias{run_permanova_clusters}
\title{Run PERMANOVA on Clustered Data}
\usage{
run_permanova_clusters(x, permutations = NULL, parallel = NULL)
}
\arguments{
\item{x}{A dataframe containing the variables of interest and a 'clusters' group column.}

\item{permutations}{The number of permutations to be used in the PERMANOVA analysis.}

\item{parallel}{Optional; a parallel backend to be used with the PERMANOVA analysis. If NULL, parallel processing is not used.}
}
\value{
A PERMANOVA result in tidy object from `broom::tidy`.
}
\description{
This function performs a Permutational Multivariate Analysis of Variance (PERMANOVA)
on a given dataset. It first selects a range of variables  (from zinc to vitaminA)
and clusters, then calculates the Euclidean distance matrix,
and finally runs PERMANOVA using the `vegan::adonis2` function.
}
\examples{
\dontrun{
# Example usage:
# run_permanova_clusters(your_data, 999, parallel = 2)
}
}
