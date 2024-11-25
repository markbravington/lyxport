\name{requote_lyx}
\alias{requote_lyx}
\title{Clean up LyX quotation marks}
\description{
This function \emph{tries} to sensibly turn assorted types of quotation marks in a LyX document into LyX "dynamic quotes". The latter make it easy to render the document with any of a number of defined "nationalesque" quotation schemes, just by tweaking a single item in Document->Settings->Language. For example, you can get outer double/inner single quotes, outer single/inner double, guillemets, and so on; see "Quotation marks" in the LyX UserGuide (currently section 3.9.4.2). Without this functionality, a large LyX document can end up having multiple types of quotation marks (especially if it is multi-authored or includes excerpts of other documents), which can't easily be changed or searched for or made coherent.

Not all quotation marks should be changed: for example, straight quotes within listings or ERT should be left alone. \code{requote_lyx} tries to get that right. However, many funny-looking things come out OK when exported to Latex (which is a necessary step in producing eg nice MSWord documents, using the other functions in this package).

\code{requote_lyx} mainly aims at double-quotes (since IME these are the commonest defaults for normal quotation), but does some single-quote stuff too:

\itemize{
\item Apostrophes are left alone (deliberately; they are tricky!)
\item Any explicit single quotes are made dynamic, but their singleness is kept; it's assumed to be deliberate.
\item Hard-wired directional single-quote characters are turned into dynamic \emph{double} quotes, just like hard-wired directional double-quotes. Coz that was \emph{probably} the intention of an author who just prefers single-quotes for outer.
}

\code{requote_lyx} isn't aiming at perfection, and may well not be foolproof; there might be situations where it doesn't work properly, because LyX used some structuring that I hadn't anticipated. Sorry.
}
\usage{requote_lyx( filename = NULL, lyx = NULL, outfile = NULL)
}
\arguments{
\item{ filename}{optional name of file to read from}
\item{ lyx}{ or you can pass the actual text in directly, as a character vector}
\item{ outfile}{ optional filename to write the output to.}
}
\value{The modified LyX text will be returned, invisibly. Also, if \code{outfile} is not NULL, the modified LyX text will be written to \code{outfile}.
}
\keyword{misc}
