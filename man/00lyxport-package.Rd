\name{lyxport-package}
\alias{lyxport}
\alias{lyxport-package}
\alias{lyxport}
\docType{package}
\title{How to use the lyxport package}
\description{
The \code{lyxport} R{} package is for exporting LyX documents to MSWord--- which I sometimes have to do, under duress--- and perhaps other formats. Unlike LyX's built-in "MS Word Open Office XML" export, \code{lyxport} does proper cross-referencing including tables, figures (correctly sized), lists, equations, appendices, and bibliography. Most of the heavy lifting is still done by Pandoc, as in LyX's built-in export option; but Pandoc--- wonderful though it is!--- doesn't get everything right even with well-known filters (as you have probably discovered yourself by now, else you mightn't be reading this). So the package contains a lot of behind-the-scenes fiddly code in order to save you lots of manual post-tinkering.

Once you've installed the package in R{}, you need to set up a few things in LyX itself, so that you can export directly from LyX. The easiest way is to run \code{\link{lyxprefhack}} in R{}; you need to read its documentation \emph{first} to see what the LyX system requirements are, and to find out what to do if \code{\link{lyxprefhack}} doesn't succeed. You can instead do (most of) the setup manually in LyX. After that, you don't need to run R{} or load the \pkg{lyxport} package in order to export to MSWord from LyX; though you might want to use \code{lyxport} from R{} for its other minor features, described below.

When setup is successful, you should see in LyX an "MSWord (lyxport)" option in "File->Export", and a "lyxport" item at the end of "Help->Specific manuals", which contains \bold{full package documentation} (which you may wish to consider at least opening...). To just see the features without, like, having to, like, actually read \emph{documentation}, do "File->Open example" and filter for "lyx", to open "lyxport-demo.lyx". You can try exporting with LyX's built-in MSWord option, and with \code{lyxport}. I haven't tested every LyX feature; it's mostly just stuff I need. More things might get added.

Under the hood, the conversion work is done by \code{\link{lyxzip2word}}, which is normally called by LyX on your behalf. However, if you want to use \code{lyxport} to do improved exports to \emph{other} formats besides MSWord, then you might need to experiment with running \code{\link{lyxzip2word}} directly from R{}.

There are also some helper utilities which you can run from R{}:

\itemize{
\item \code{\link{requote_lyx}} (qv) to help sort out quotation marks, e.g. in case your document includes imports from other formats such as plain-text.
\item \code{\link{tidy_initials}} (qv) and \code{\link{tex2utf8}} to tweak bibliography files to be MSWord-ready. These are called automatically by \code{\link{lyxzip2word}}, unless you tell it not to, but you might also find them useful in their own right.
}
}
\keyword{misc}
