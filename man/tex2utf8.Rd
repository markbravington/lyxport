\name{tex2utf8}
\alias{tex2utf8}
\title{Transform Biblatex bibliography to native UTF8 characters}
\description{
Biblatex bibliography (dot-bib) files may contain legacy Latex/Bibtex representations of characters, such as "\\c\{G\}" for "ģ". These are normally fine--- though some of them are technically incorrect, but may still work--- but not always, e.g. when looking for consistent names, as in \code{\link{tidy_initials}} (qv). So you can \emph{try} \code{tex2utf8} to translate such representations into "native UTF8 codepoints". It might help you.

This might even work on more general Latex (ie not on a bib file) but you are on your own there...
}
\usage{tex2utf8(tex, file = NULL, outfile = NULL, debrace = FALSE)
}
\arguments{
\item{ tex}{character vector containing the bibliography contents. Provide just one of \code{tex} or \code{file}.}
\item{ file}{if supplied, this is used in place of \code{tex} (which is ignored), to read the lines in from.}
\item{ outfile}{if supplied, the result will be written here.}
\item{ debrace}{whether to remove superfluous braces around single UTF8 characters. IME these are mostly legacy effects of Latex representation, rather than deliberate statements about upper/lower case (the only legit use I can think of). Extra braces are usually harmless if you are using \code{tex2utf8} as a standalone, but they do muck up \code{tidy_initials} (qv), so \code{\link{lyxzip2word}} sets this option TRUE when tidying up the biblio (unless I have given an option to change that, which I haven't yet).}
}
\value{The modified contents.
}
\details{This is harder than it sounds. The key was to find a couple of tables on WWW; see source code for details. \code{tex2utf8} tries to fix up common Bibtex representation errors (i.e. semi-problems in my own master biblio file, mostly from WWW sources), but probably won't catch everything. And there may be "native UTF8 codepoints" for some characters that aren't in the (original?) Latex list, and won't be transformed. They can of course be produced in Latex by a composite (eg "\\k\{n\}"; I have no idea whether that is a real character in some alphabet). They will stay composite in the output.
}
\seealso{\code{lyzip2word}, \code{\link{tidy_initials}}
}
\examples{
# Not compulsory to have an EXAMPLES -- you can put examples into other sections.
# Here's how to make a "don't run" example:
\dontrun{
reformat.my.hard.drive()
}
}
\keyword{misc}
