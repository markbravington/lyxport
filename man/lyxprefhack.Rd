\name{lyxprefhack}
\alias{lyxprefhack}
\title{Prepare LyX for better export to MSWord}
\description{
This function should be called just after you (re)install the \pkg{lyxport} package (and/or LyX itself), after which you probably won't need to call it again. You should first \bold{close} any running LyX instances (it usually won't matter, but the changes won't be picked up until you re-start LyX anyway). It modifies your LyX "preferences" and "ui" files to add better MSWord export, with shortcut "W" in File->Export; see \bold{Details}. It also creates a LyX help file and an example that you can access straight from LyX--- so you may never need to use this package again from R{}, since almost everything you need is accessible from LyX itself. (One exception is if you want to use \code{\link{requote_lyx}} (qv) to sort out quotation-mark problems. Many people will never need it.)

BTW: make sure LyX is on your PATH first (it surely will be in Linux/Macs, though I can't test it; it probably will be on Windows, but not necessarily. If not, you might have to do something externally to R{}, then restart R{}. I'm not your mother, so you will have to figure that out yourself...

Note that \code{lyxprefhack} should include a self-test routine at the end. I haven't done so (yet): partly cozza Priorities, darling, Priorities; and partly coz there might be some hassle about ensuring that a new LyX instance gets started. My recommendation is to try exporting from LyX yourself, via "File-Open_example" and the "lyxport-demo.lyx" file. In principle, and obvs untested: a check from R{} might be \emph{something} like this:

\preformatted{
  system2( 'lyx', sprintf( '-n -v -batch -e wordx \%s',
      system.file( 'examples/lyxport-demo.lyx', package='lyxport')))
}
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
To enable automatic export from LyX, you need to add two or three Preference settings inside LyX, either manually or by editing the "preferences" file in your LyX Userdir. The function \code{\link{lyxprefhack}} will attempt to do the latter, at your own risk.

The settings can instead be set manually inside LyX, from the "Tools" menu (on Macs, apparently the menu is called "LyX" instead). First, go to to the "Preferences->File Handling" submenu. Then define a new File Format, which should be a copy of LyX's built-in "MS Word Open Office XML" but with a different name. The only fields you absolutely need are:

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

Third, you need to make sure that LyX can launch R{}. Often this will already be set up (e.g. if you can already run \code{knitr}, or because R{} is on your "system path" anyway and can be launched from any other program). But if not, you need to add it to the "Tools->Preferences->PATH_prefix" list, using forward-slashes; I suggest putting it after all the entries containing "$LyXDir", but it probably doesn't matter where.
\subsection{Version proofing }{There is a kilogeek-level subtlety with PATH prefix and R{} versions, which I will mention here "for completeness"--- partly because I find it generally useful, not just for \code{\link{lyxport}}. R{} often needs updating; I don't want to have to remember each time which obscure LyX features I need to also update--- nor indeed in lots of other bits of software which might occasionally want to call R{}. It would be nice if I could set an envar(s) that holds the current R{} path (well, I can and I do do that), and ask LyX to use the envar instead of some absolute path, but that second part doesn't currently work. Instead, I have set up a symlink folder for R{} whose nominal path is always fixed, and I use that as the PATH_prefix in LyX (and other places, eg the start-menu item for R). Then, when R{} is updated, I just run a little command-shell script that updates the symlink. I use a similar trick fro LyX itself, obvs not to tell LyX about where it lives, but rather to be able to refer to the current LyX consistently from other pieces of software. I did say "kilogeek"--- you were warned!
}
}
\seealso{\code{\link{lyxzip2word}}, \code{\link{lyxport}}
}
\examples{
\dontrun{
lyxprefhack()
}
}
\keyword{misc}
