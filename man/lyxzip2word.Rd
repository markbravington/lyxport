\name{lyxzip2word}
\alias{lyxzip2word}
\title{Convert LyX to MSWord or other format}
\description{
\code{lyxzip2word} starts from a "LyX archive" (zip export from LyX) and converts to MS Word (or potentially other formats) using various tools, mostly Pandoc. See \code{RShowDoc("lyxport-docu",package="lyxport")} for more information about normal use from LyX, and any requirements of your LyX source file (eg specifying the bibliography format).

You don't normally need to know anything about this function, since it is called automatically from LyX. However, I should document it for maintenance-type reasons. Also, if you want to experiment with exporting to other formats, you might want to use it direct from R{}, setting the \code{outext} and \code{panoutopts} arguments.
}
\usage{
lyxzip2word(
  zipfile,
  outext= 'docx',
  panoutopts= outext,
  origdir= dirname( zipfile),
  tempdir= base::tempdir(),
  copy= FALSE,
  FROM_LYX= !interactive(),
  refdir= NULL,
  lyxdir= NULL,
  lyx_userdir= NULL,
  natnum_pandoc= FALSE, # devil or deep-blue-sea?
  crossref_pandoc= TRUE,
  verbose= FALSE,
  dbglyx= ''
)
}
\arguments{
\item{  zipfile}{Name of input file, normally a LyX-zip archive with a path. Extension is optional, but ".zip" is assumed. If (for experimentation only) the extension is ".lyx", then all the other necessary files had better be in \code{tempdir}, which had better be the same folder as the in-fact-not-zippy \code{zipfile}.}
\item{  outext}{File extension of output.}
\item{  panoutopts}{For pandoc's writer, to tell it what kind of output to produce, ie pandoc's "-t" argument. Normally the default of \code{outext} is fine, but there might be other useful options for specific writers, eg "markdown+myfilter-theirfilter".}
\item{  origdir}{where \code{zipfile} lives, and where the output should go; deduced from zipfile if missing.}
\item{  tempdir}{where to unpack the zipfile and create temporary files etc.}
\item{  copy}{whether to copy \code{zipfile} to \code{tempdir} (leaving the original in place), or \emph{move} it (in which case the original is "lost"). Normally it's fine to just move it, because the zipfile is only a temporary step in the creation of your magnificent MSWord-flavoured document...}
\item{  FROM_LYX}{set TRUE iff called from inside LyX by a Converter, in which case \code{zipfile}, \code{origdir}, \code{tempdir}, and possibly \code{dbglyx} will be set according to \code{\link{commandArgs}}.}
\item{  refdir}{ Top of the folder-tree for bibliography-finding. It should have a a folder \code{./bibtex/bib/} underneath it, containing dot-bib files. It is normally found by automatic magic.}
\item{  lyxdir}{ Where Lyx executable lives. However, you should probably make sure Lyx is on the search path anyway, otherwise things may not work; if it is, then you can leave this NULL.}
\item{  lyx_userdir}{what you'd pass in the "-u <userdir>" option when starting LyX. However, as of Lyx 2.4.2.1, there's a bug which stops that working (in the particular context of \code{lyxzip2word}). What this means is, you can't use your own private layouts/modules in your own userdir tree; you'd have to copy them to Lyx's system layouts folder.}
\item{  natnum_pandoc}{whether to turn on pandoc's own numbering scheme (when reading and when writing). More trouble than it's worth so far, hence the default is FALSE.}
\item{  crossref_pandoc}{whether to use the "pandoc-crossref" filter when reading the Latex source. The default is TRUE, but \code{lyxzip2word} actually has to spend quite a bit of time fixing incorrect xrefs, and I suspect I could get FALSE to work with just minor mods, leading to one less "dependency". But everything's fine at the moment, so leave well enough alone.}
\item{  verbose}{if TRUE}
\item{  dbglyx}{Only for debugging, obvs. Should be blank, or a positive integer as per "lyx -dbg". Any number will also cause R{} to print out various things, such as paths.}
}
\value{Should produce a file "<zipfilename>.<outext>" in folder \code{origdir}. There will also be various files \code{tempdir} (which will be LyX's session tempdir, if this was invoked from LyX itself), including a logfile "docxconv.log" which should/might contain useful error messages if there are any. Look carefully thru LyX's "View->Messages" window to see where that LyX tempdir is (it changes from one LyX session to the next). The formal R{} return-value of \code{lyxzip2word} is TRUE or FALSE according as whether it thinks everything worked.
}
\details{
The steps in the conversion process are (actually there's more than this, this list is out-of-date...):

\itemize{
\item Move & extract zip file
\item Generate Tex, by running "lyx --export"
\item Merge any input/include files
\item Add eqn labels: eqn_labels_for_word()
\item Check and prepare bibliography
\item Twiddle any appendices, so as to not confuse pandoc
\item Export Tex -> pandoc-native: pandoc
\item Fix labelled-eqn column widths: eqalignfix()
\item Perhaps move the bibliography to before appendices
\item Export pandoc-native -> docx: pandoc
}
}
\seealso{\code{\link{lyxport}}
}
\examples{
\dontrun{
# In LyX, open the "lyxport-demo.lyx" example, then File->Export->Lyxzip. Then try exporting
# to a non-MSWord format, via eg...
lyxzip2word( 'lyxport-demo.zip', outext='html')
}
misc
}
\keyword{misc}
