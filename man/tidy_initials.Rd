\name{tidy_initials}
\alias{tidy_initials}
\title{Consistent author names in dot-bib files}
\description{
In a dot-bib file, the same author can appear with slightly different names in different papers: "A. Psmith", "Alan Psmith", "Alan B. Psmith", "A. Bertram Psmith", and so on. If you are not careful, your citations can come out funny as a result. For example, you might see "Psmith et al. (1999)" but "A.B. Psmith (2004)" even though Alan Bertram is the only Psmith you are citing. With Biblatex and PDFs, you can suppress such nonsense via "uniquename=false" and "uniquelist=false". But with CSL and MSWord etc, it seems to be harder--- well, so does pretty much everything, actually.

To circumvent the problem, you can call \code{tidy_initials} on your bib file beforehand (which \code{\link{lyxzip2word}} does automatically by default, on a temporary copy of the dot-bib, unless you ask it not to). This will ensure all plausibly-identical authors have exactly the same bib entries (initials only, and the longest possible set), and should eliminate silly citations.

The merging rules, which depend slightly on the \code{gungho} argument, are as follows:

\itemize{
\item "Alan Psmith" and "A. Psmith" are always assumed to be the same (and "Alan Psmith" will be remapped to "A. Psmith", since only initials are retained when there's a discrepancy).
\item "Alan Psmith" and "Alicia Psmith" are never merged. That is: if there's a full name rather than initial, it's taken seriously.
\item Mismatched initials are never merged, eg "A.B. Psmith" and "A.C. Psmith".
\item If \code{gungho=TRUE}, then missing initials are ignored, so that "A. Psmith" gets merged with "A.B. Psmith". If there's also an "A.C. Psmith", then it's the luck-of-the draw as which one "A. Psmith" will be merged with.
\item If \code{gungho=FALSE}, then merging only occurs when two authors also have the same number of initials; so "Alan Bertram Psmith" gets merged with "A.B. Psmith" but not with "A. Bertram Psmith".
}

In order to make this surprisingly tricky programming task a bit easier, \code{tidy_initials} calls \code{\link{tex2utf8}}, which you might find useful in its own right.
}
\usage{tidy_initials( bib, file=NULL, outfile=NULL, gungho=TRUE)
}
\arguments{
\item{ bib}{character vector containing the bibliography contents. Use one of \code{bib} or \code{file}.}
\item{ file}{if supplied, this is used in place of \code{tex} (which is then ignored), to read the lines in from.}
\item{ outfile}{if supplied, the result will be written here.}
\item{ gungho}{TRUE if you prefer to assume that middle initials are easy-come-easy-go. Saves pain, but in rare cases could merge genuinely different people.}
}
\value{The modified contents, as a character vector. If \code{outfile} is supplied, that file will be created.
}
\seealso{\code{\link{tex2utf8}}, \code{\link{lyxzip2word}}
}
\examples{
# Not compulsory to have an EXAMPLES -- you can put examples into other sections.
# Here's how to make a "don't run" example:
\dontrun{
reformat.my.hard.drive()
}
}
\keyword{misc}
