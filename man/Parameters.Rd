\name{Parameters}
\alias{Parameters}
\title{
  Parameters of the Experimental Designs
}
\description{
  This package includes a function for generating parameters for the input designs, as well as incidence matrices. This is a general function that can be used to investigate the characterization properties of any block design.
}
\usage{
  Parameters(input_design)
}
\arguments{
  \item{input_design}{
    A design which should be in a matrix format
  }
}
\value{
  Experimental designs in matrix form considering rows as blocks and fill the empty cells with zeros
}
\examples{
  library(Design.parameters)
  design=matrix(1:9, nrow = 3, ncol = 3)
  Parameters(design)
}
