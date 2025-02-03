\name{lyxport-package}
\alias{lyxport}
\alias{lyxport-package}
\alias{lyxport}
\docType{package}
\title{How to use the lyxport package}
\description{
The \code{lyxport} R{} package is for exporting LyX documents to MSWord--- which I sometimes have to do, under duress--- and perhaps other formats. Unlike LyX's built-in "MS Word Open Office XML" export, \code{lyxport} does proper cross-referencing including tables, figures (correctly sized), lists, equations, appendices, and bibliography. Most of the heavy lifting is still done by Pandoc, as in LyX's built-in export option; but Pandoc--- wonderful though it is!--- doesn't get everything right even with well-known filters (as you have probably discovered yourself by now, else you mightn't be reading this). So the package contains a lot of behind-the-scenes fiddly code in order to save you lots of manual post-tinkering.

Once you've installed the package, run \code{\link{lyxprefhack}} to set things up for direct use from LyX. Then you should see an "MSWord (lyxport)" option in "File->Export", and a "lyxport" item at the end of "Help->Specific manuals", where the \bold{full package documentation} lives (you may wish to consider at least opening it...). To just see the features, do "File->Open example" and filter for "lyx", to open "lyxport-demo.lyx". You can try exporting with LyX's built-in MSWord option, and with \code{lyxport}. I haven't tested every LyX feature; it's mostly just stuff I need. More things might get added.

The package currently has one main user-visible function, \code{\link{lyxprefhack}}, which you need to call (once) for setup. The conversion work is done by \code{\link{lyxzip2word}}, but you don't normally need to call it directly; \code{\link{lyxzip2word}} is normally called by LyX on your behalf, when you export to "MSWord (lyxport)" or perhaps some other format. There are also some helper utilities:

\itemize{
\item \code{\link{requote_lyx}} (qv) to help sort out quotation marks, e.g. in case your document includes imports from other formats such as plain-text.
\item \code{\link{tidy_initials}} (qv) and \code{\link{tex2utf8}} to tweak bibliography files to be MSWord-ready. These are called automatically by \code{\link{lyxzip2word}}, unless you tell it not to, but you might also find them useful in their own right.
}
}
\section{Just in case}{\code{\link{lyxprefhack}} makes a number of assumptions about your LyX config files (for good reasons). It works for me, but that could be dumb luck; if it goes wrong for you, be aware that it makes backups of your "preferences" and "ui/stdmenus.inc" files--- so you can manually restore them. If you can't see "lyxport" as a help option, try typing "help-open lyxport-docu" in the minibuffer. And you can also see a PDF version of that documentation in R{}, via \code{RShowDoc("lyxport-docu",package="lyxport")}.
}
\keyword{misc}
