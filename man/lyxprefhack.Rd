\name{lyxprefhack}
\alias{lyxprefhack}
\title{Prepare LyX for better export to MSWord}
\description{
This function should be called just after you install the \pkg{lyxport} package, and you probably won't need to call it again. It modifies your LyX "preferences" file to add better MSWord export, with shortcut "W" in File->Export; see \bold{Details}. It also creates a LyX help file and an example that you can access straight from LyX--- so you may never need to use this package again from R{}, since almost everything you need is accessible from LyX itself. (One exception is if you want to use \code{\link{requote_lyx}} (qv) to sort out quotation-mark problems. Many people will never need it.)

BTW: make sure LyX is on your PATH first (it surely will be in Linux/Macs, though I can't test it; it probably will be on Windows, but not necessarily. If not, you might have to do something externally to R{}, then restart R{}. I'm not your mother, so you will have to figure that out yourself...
}
\usage{lyxprefhack( userdir)
}
\arguments{\item{ userdir}{Where your config files live; see section 2 of LyX's "Customization" manual. R{} will prompt you for it if you don't set the parameter (and if it also can't be auto-deduced; see below), so you can copy it from "Help->About" in LyX; single backslashes are OK.}
}
\section{Userdir}{I reckon the best way to run LyX (not just for this), is to set the userdir via an envar of the form "LYX_USERDIR_24x" (or "...23x" for LyX 2.3, etc), as documented in LyX's "Customization" manual (currently in the section "...multiple configurations", though I only use one config). That means file associations etc all use exactly with the same LyX setup as if you launch LyX directly.
}
\value{Overwrites the "preferences" file (unless there's no change required), after backing up the old one to "old_preferences<N>" (guaranteed not to overwrite any existing backup). Adds a "lyxport" option to "Help->Specific manuals", in the file "ui/stdmenus.inc", again making a backup of the latter if any change was needed. If "stdmenus.inc" did not exist in "userdir/ui", then it's copied there from (an attempt to find...) LyX's system dir. Also, copies one file from the R{} installation to LyX's "<userdir>/doc" ("lyxport-docu.lyx") and one set of files to LyX's "<userdir>/examples/lyxport" ("lyxport-demo.lyx" and associated files).
}
\details{
To enable automatic export from LyX, you need to add two Preference settings inside LyX, either manually or by editing the "preferences" file in your LyX Userdir. The function \code{\link{lyxprefhack}} will attempt to do the latter, at your own risk.

The two settings can instead be set manually inside LyX, from the "Tools->Preferences->File Handling" menu. First, define a new File Format, which should be a copy of LyX's built-in "MS Word Open Office XML" but with a different name. The only fields you absolutely need are:

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
\item Converter: \code{Rscript --no-save --no-restore --verbose -e lyxport::lyxzip2word(FROM_LYX=TRUE) $$i $$r $$p 1 > docxconv.log  2>&1}
}

When the converter runs, it will write a logfile into LyX's temporary folder, which unfortunately is a bit hard to find if things go wrong (if they don't, you don't need to find it). Weirdly, if you try to put the logfile into the "main" folder (ie where source and export live), by using "$$r/<something" after the redirect, then LyX says it can't execute the command...
}
\seealso{\code{\link{lyxzip2word}}, \code{\link{lyxport}}
}
\examples{
\dontrun{
lyxprefhack()
}
}
\keyword{misc}
