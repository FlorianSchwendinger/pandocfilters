% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inline_elements.R
\name{Image}
\alias{Image}
\title{Image.}
\usage{
Image(target, text, caption = "", attr = Attr())
}
\arguments{
\item{target}{a character string giving the target (hyper reference)}

\item{text}{a inline object or a list of inline objects giving the visible part}

\item{caption}{a character string describing the picture}

\item{attr}{an optional object of type \code{"Attr"}}
}
\description{
A constructor of an inline object of type \code{"Image"}.
}
\details{
Further Usage examples can be found in the README.
}
\examples{
Image("https:://Rlogo.jpg", "some_text", "fig:some_caption")

}
\seealso{
Other Inline element constructors: \code{\link{Cite}},
  \code{\link{Code}}, \code{\link{Emph}},
  \code{\link{LineBreak}}, \code{\link{Link}},
  \code{\link{Math}}, \code{\link{Note}},
  \code{\link{Quoted}}, \code{\link{RawInline}},
  \code{\link{SmallCaps}}, \code{\link{SoftBreak}},
  \code{\link{Space}}, \code{\link{Span}},
  \code{\link{Strikeout}}, \code{\link{Strong}},
  \code{\link{Str}}, \code{\link{Subscript}},
  \code{\link{Superscript}}
}
