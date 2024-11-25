\name{lyxprefhack}
\alias{lyxprefhack}
\title{Prepare LyX for better export to MSWord}
\description{This function should be called just after you install the \pkg{lyxport} package, and you probably won't need to call it again. It modifies your LyX "preferences" file to add better MSWord export, with shortcut "W" in File->Export; see \bold{Details}. It also creates a LyX help file and an example that you can access straight from LyX--- so you may never need to use this package again from R{}, since almost everything you need is accessible from LyX itself. (One exception is if you want to use \code{\link{requote_lyx}} (qv) to sort out quotation-mark problems. Many people will never need it.)
}
\usage{lyxprefhack( userdir)
}
\arguments{\item{ userdir}{Where your config files live; see section 2 of LyX's "Customization" manual. R{} will prompt you for it if you don't set the parameter, so you can copy it from "Help->About" in LyX; single backslashes are OK.}
}
\value{Overwrites the "preferences" file (unless there's no change required), after backing up the old one to "old_preferences<N>" (guaranteed not to overwrite any existing backup). Copies one file from the R{} installation to LyX's "<userdir>/doc" ("lyxport-docu.lyx") and one to LyX's "<userdir>/examples" ("eqntest1.lyx").
}
\details{
To enable automatic export from LyX, you need to add two Preference settings inside LyX, either manually or by editing the "preferences" file in your LyX Userdir. The function \code{\link{lyxprefhack}} will attempt to do the latter, at your own risk.

The two settings can be accessed from the "Tools->Preferences->File Handling" menu. First, define a new File Format, which should a copy of LyX's built-in "MS Word Open Office XML" but with a different name. The only fields you need are:

\itemize{
\item Format name: MSWord (lyxport)
\item Tick the boxes for "Document Format", "Show in Export menu", and "Vector graphics format"
\item Short name: wordx
\item Extensions: docx
\item Shortcut: you don't \emph{need} this, but I use "W" so I can export via "Alt-F E W"
}

Second, you need to define a Converter, as follows:

\itemize{
\item From: Lyx Archive (zip) - \emph{not} from straight LyX!
\item To: MSWord (lyxport) - ie the name of the new Format
\item Converter: Rscript --no-save --no-restore --verbose -e lyxport::lyxzip2word(FROM_LYX=TRUE) $$i $$r $$p 1 > docxconv.log 2>&1
}

When the converter runs, it will write a logfile into LyX's temporary folder, which unfortunately is a bit hard to find if things go wrong (if they don't, you don't need to find it). Weirdly, if you try to put the logfile into the "main" folder (ie where source and export live), by using "$$r/<something" after the redirect, then LyX says it can't execute the command...
}
\seealso{\code{\link{lyxzip2word}}, \code{\link{lyxport}}
}
\examples{
\dontrun{
lyxprefhack()
}
misc
}
\keyword{misc}
