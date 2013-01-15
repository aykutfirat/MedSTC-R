\name{medSTC-package}
\Rdversion{1.0}
\alias{medSTC-package}
\docType{package}
\title{
  Max-margin supervised Sparse Topical Coding model (Med-STC) for multi-class classification
}
\description{
  This package employs sparse topical coding models for multi-class classification.
}
\details{
\tabular{ll}{
Package: \tab medSTC\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2013-1-15\cr
License: \tab GPL \cr
LazyLoad: \tab yes\cr
}
}
\author{
  Jun Zhu (\email{junzhu@cs.cmu.edu}),
  Aykut Firat (\email{aykut@crimsonhexagon.com})
}
\references{
  \cite{Jun Zhu, and Eric P. Xing. 
  		Sparse Topical Coding, 
  		In Proc. of 27th Conference on Uncertainty in 
  		Artificial Intelligence (UAI), Barcelona, Spain, 2011.}
}
\keyword{ package }
\seealso{
Functions to fit models:
\code{\link{medSTC}}

Functions for prediction:
\code{\link{predict.medSTC}}

Included data sets:
\code{\link{newsgroups}}
}
\examples{
## See the demo for the newsgroup example:

\dontrun{demo(medSTC)}

}
