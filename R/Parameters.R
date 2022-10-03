#' Parameters of the Experimental Designs
#'
#' @param input_design A design which should be in a matrix format
#'@description This package includes a function for generating parameters for the input designs, as well as incidence matrices. This is a general function that can be used to investigate the characterization properties of any block design.
#'
#' @return  Experimental designs in matrix form considering rows as blocks and fill the empty cells with zeros
#'
#' @examples
#' library(Design.parameters)
#' input_design=matrix(1:9, nrow = 3, ncol = 3)
#' Parameters(input_design)
#' @export

Parameters=function(input_design){
  v = max(input_design)
  b = nrow(input_design)
  k = ncol(input_design)
  N = matrix(0, v, b)
  N_matrix = function(input_design) {
    for (i in 1:b) {
      for (j in 1:k) {
        N[input_design[i, j], i] = N[input_design[i, j], i] + 1
      }
    }
    N
  }
  a=N_matrix(input_design)
  npn=a%*%t(a)
  r1=npn[row(npn)==col(npn)]
  r=unique(r1)
  l1=npn[row(npn)!=col(npn)]
  l=unique(l1)
  ######################################
  results=list(  "treatments"=v,
                 "blocks"=b,
                 "replications"=r,
                 "block size"=k,
                 "lambda's"=l,
                 "N Incidence matrix"=a,
                 "N.N'"=npn)
  print(results)
}

