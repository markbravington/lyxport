\name{lyxport-package}
\alias{lyxport}
\alias{lyxport-package}
\alias{lyxport}
\docType{package}
\title{How to use the lyxport package}
\description{
The \code{lyxport} R{} package is for exporting LyX documents to MSWord--- which I sometimes have to do, under duress--- and perhaps other formats. Unlike LyX's built-in “MS Word Open Office XML” export, \code{lyxport} does proper cross-referencing including tables, figures (correctly sized), lists, equations, appendices, and bibliography. Most of the heavy lifting is still done by Pandoc, as in LyX's built-in export option; but Pandoc--- wonderful though it is!--- doesn't get everything right even with well-known filters (as you have probably discovered yourself by now, else you mightn't be reading this). So the package contains a lot of behind-the-scenes fiddly code in order to save you lots of manual post-tinkering.

Once you've installed the package, run \code{\link{lyxprefhack}} to set things up for direct use from LyX. Then you should see an "MSWord (lyxport)" option in "File->Export", and a "lyxport" item at the end of "Help->Specific manuals", where the full package documentation lives. To see the features, do "File->Open example" and filter for "lyx", to open "lyxport-demo.lyx". I haven't tested every LyX feature; it's mostly just stuff I need. More things might get added.

The package currently has 3 user-visible functions: \code{\link{lyxprefhack}} for setup, \code{\link{lyxzip2word}} for the main conversion, and \code{\link{requote_lyx}} to help sort out quotation marks, mainly in case your document includes imports from other formats such as plain-text. From R{}, you are only likely to call the 1st (once) and perhaps the 3rd; \code{\link{lyxzip2word}} is normally called by LyX on your behalf, when you export to "MSWord (lyxport)".
}
\section{Just in case}{\code{\link{lyxprefhack}} makes a number of assumptions about your LyX config files (for good reasons). It works for me, but that could be dumb luck; if it goes wrong for you, be aware that it makes backups of your "preferences" and "ui/stdmenus.inc" files--- so you can manually restore them. If you can't see "lyxport" as a help option, try typing "help-open lyxport-docu" in the minibuffer. And you can also see a PDF version of that documentation in R{}, via \code{RShowDoc("lyxport-docu",package="lyxport")}.
}
\keyword{misc}
