\name{lyxprefhack}
\alias{lyxprefhack}
\title{Prepare LyX for better export to MSWord}
\description{
This function should be called just after you (re)install the \pkg{lyxport} package (and/or LyX itself), after which you probably won't need to call it again until you update LyX. It tries to modify the "preferences" and "ui/stdmenus.inc" files in your LyX Userdir to add better MSWord export, with shortcut "W" in "File->Export"; see DETAILS. It also creates a LyX help file and an example that you can access straight from LyX.

\bold{Before} running \code{lyxprefhack}, you need to check that the LyX requirements are met, as explained next. Then you should probably close all running LyX instances before running \code{lyxprefhack}.

If \code{lyxprefhack} works, everything is easy. If it fails, don't panic; you can make the necessary changes to LyX preferences etc yourself, as explained below. It's not possible for to me guess all the quirks of how people have set up LyX on their different OSes, so failure is a possibility.

Before changing your "preferences" and "ui/stdmenus.inc" files, \code{lyxprefhack} makes backups, with obvious names. If things get severely broken, you can restore those files yourself manually.
\subsection{Note}{Everything here has been tested on Windows only, though David Miller has provided some helpful and colorful feedback about Linux and Macs. On Macs at least, the precise locations/names of LyX menu items and/or clicky buttons might be a bit different. Seek and ye shall find, maybe.
}
\subsection{Requirements}{
#1 From LyX, you need to check that the following built-in LyX exports work:

\itemize{
\item Export to "Lyx Archive" (dot-zip on Windows, dot-tar-dot-gz on Linux/Macs)
\item Export to "MS Word Office Open XML" (or very similar name)
\item Export to PDF (if this fails, you're in big trouble!)
}

If you need a file to practise on, "File->Open_example" and filter to "Welcome.lyx". It's best to immediately \emph{save} it somewhere else, before you try to export.

(This is largely a check on other programs needed by LyX export; are they installed correctly, from LyX's PoV? Usually yes, but not always...)

#2 Make sure that LyX has saved a "preferences" file in your Userdir (see "Help->About_LyX" for whereabouts; you can open the folder to check). Experienced LyX users will already have a "preferences", but it \emph{isn't} automatically created in a new LyX setup. If not, you'll have to get LyX to make one. I suggest "Tools->Preferences->Identity", change your name, and click "Apply". You should see a "preferences" file in your Userdir. You can now change your Identity back if you want, and "Apply" again.

(Note: I have tried to automate this step, but so far failed; I suspect LyX bugs. It's not hard to do it yourself, though.)

#3 You also need to make sure that LyX can run R{}, and vice versa. If the folders with the R{} and LyX executables are already in your PATH--- and they probably are--- then you're probably fine. But you still have to check. (If you don't know what "your PATH" means, then it's a Google moment.) Notwithstanding claims that Linux & Mac users never have any problems here, I am a bit skeptical...

From R{}, you can check with \code{Sys.which( "lyx")} (or possibly "LyX" on Linux/Macs, IDNK); if that comes back non-empty, it's a jolly good sign. If not, you need to add the LyX executable folder to your PATH--- either "globally", or automatically when R{} starts, eg via ".Rprofile" and \code{\link{Sys.setenv}}--- the deetails are up to you.

From LyX, I'm not sure how to check directly, but \code{lyxprefhack} tries to check for you. If it reports a problem, then in LyX go to "Tools->Preferences->Paths->PATH_prefix" and add the R{} executable folder to the end; use forward-slashes, even on Windows. See Appendix C3 of LyX's "User Guide" for mor info. Remember to "Apply" or "OK". And if you know in advance that R{} \emph{won't} be in your PATH automatically when LyX starts (and there can be sensible reasons for that), then do the same thing.

#4 Not \emph{absolutely} essential, but ImageMagick is needed to make sure graphics come out the right size (as best as I can). On Windows, this is installed automatically along with LyX, so it should be there. IDNK whether the same courtesy is extended on Linux or Macs. If not, best to install it; make sure it's on your PATH, or add its path to "Tools->Preferences->Paths->PATH_prefix" as per #3.

#6 Make sure that you have an editor/viewer for MSWord documents in your PATH, or in "...PATH_prefix" as above. I use LibreOffice, but presumably you could use MSWord itself; details are up to you.

#7 Work out where your LyX Userdir is (eg "Help->About_LyX"; there are other ways for experienced users). It is needed as an argument to \code{lyxprefhack}. (However, for advanced users: there is no need to do this if you normally set the userdir via an envar (environment variable), eg "LYX_USERDIR_24x" or "LYX_USERDIR_23x" as per LyX's "Customization" manual, because \code{lyxprefhack} will look for that envar automatically.)

This list isn't exhaustive. There might be other LyX features that aren't properly set up on your system (eg bibliographic stuff, exotic Latex packages), but AFAIK if you can do a PDF export for your document-of-choice, then \code{\link{lyxport}} should work too.

.DID.IT.WORK?

\code{lyxprefhack} will error-stop if various checks fail. If not, it will do a self-test at the end, to see if it can export the example "lyxport-demo.lyx". That might also fail. Or not! You might see some warnings about "'py' is not recognized as an internal or external command, operable program or batch file."; you can safely ignore them,AFAIK.

If \code{lyxprefhack} gave you an error message, then no it didn't work, and hopefully the message is informative enough to help. Check the Requirements above, and if they're OK, try making the (few) changes manually in LyX, as per the next section.

If you get a happy message saying that export worked, then you are probably fine! But you should still check for the mannual in LyX, as in the list below.

If things didn't work but you were able to set up LyX manually, there are three things to check:

\itemize{
\item Does "Help->Specific_manuals" give you "lyxport" option? Can it open the file? (It's not essential; the same file can be viewed from R{}, as per \code{?lyxport}. But it's very handy, and you should check this even if \code{lyxprefhack} reports success.)
\item Does "File->Open_example" (then filter for "lyxport") open up the "lyxport-demo" example?
\item Is there an "MSWord (lyport)" option in "File->Export_as"?
\item Does it export the "lyxport-demo" successfully?
}

If you're sure that the Requirements are OK, and you've tried the above including manual setup but still have problems, please email me to let me know what happened (details are good!). I'm not going to say "bug report" because it may not be my fault :) It's not a zero-sum game BTW ; it might not be your fault, either!
}
\subsection{Manual setup in lyx}{
There isn't much, actually, but it is fiddly--- hence \code{lyxprefhack}! If that fails and you want to know more, read this...

Reading LyX's "Customization" manual, in p'tic sections 3.1 "Formats" and 3.3 "Converters" should help if you get stuck with the descriptions below.

#1 In "Tools->Preferences->File_Handling", you need to add one "File format" and one "Converter". For the former, select the "File Formats" dialog and click "New". You will get a bunch of empty items. Complete them as follows:

\describe{
\item{Format}{MSWord (lyxport)}
}

 (Tick the boxes for "Document format", "Show in export menu", and "Vector graphics format", in that order.)

\describe{
\item{Short name}{wordx}
\item{Extensiosn}{docx}
\item{MIME}{application/vnd.openxmlformats-officedocument.wordprocessingml.document}
\item{Shortcut}{W}
\item{Editor}{Custom, swriter}
\item{Viewer}{Custom, swriter}
\item{Copier}{<blank>}
}

 Default Output Formats:

\describe{
\item{With TeX fonts}{PDF (pdflatex)}
\item{Japanese}{PDF (dvipdfm)}
\item{With non-TeX fonts}{PDF (XeTex)}
}

You might be able to use a different Editor and Viewer, eg MSWord itself--- but I don't have that installed on my system, so I use LibreOffice ("swriter") instead. It's good, but not perfect (math is not right). You could look at LyX's built-in format for "MS Word Office Open XML", or whatever it's called, for guidance.

Then "Apply" or "OK".

#2 For the Converter (which you can only create after you've defined the new File Format), you have to have an existing converter selected (doesn't matter which one) and then starting changing things before you get the option to Add--- you can't just go from an empty definition. This can seem a bit nerve-wracking because it feels like you might break the existing one if you're not careful. And you might. So don't.

\describe{
\item{From format}{LyX Archive (zip)}
\item{To format}{MSWord (lyxport)}
\item{Converter}{Rscript --no-save --no-restore --verbose -e lyxport::lyxzip2word(FROM_LYX=TRUE) $$i $$r $$p 1 > docxconv.log 2>&1}
\item{Extra flag}{<blank>}
\item{Converter File Cache}{<I prefer to leave this unchecked, but you might not>}
}

 Security:

\describe{
\item{Forbid used of needauth converters}{<unchecked>}
\item{Use needauth option}{<check>}
}

Then Add, and OK or Apply.

Now "MSWord (lyxport)" should be available from "File->Export_as" menu. It might even work!

#3 To make visible the LyX helpfile for \code{\link{lyxport}}, you need to it to LyX's menu system. First, create a folder "doc/" in Userdir. Then copy into it the file "lyxport-docu.lyx" from the "examples/" folder of this R{} package. Now you need to tell LyX that it's available, via editing a file that lives in the "ui/" folder of Userdir: (on Windows...) the file is "stdmenus.inc". Somewhere in it, there should be a "HELP MENU" with two sub-menus. Add the following line to the end of the "Examples" sub-menu (you can also include a line "Separator" immediately before it):

\preformatted{
  Item "lyxport" "help-open lyxport-docu"
}

#4 "lyxport-demo" example comes as 4 files in the "examples" folder of the R{} package. You can copy them to a folder "examples/lyxport" in your Userdir. I think they should be immediately visible to LyX via "File->Open_example".

\preformatted{
}
}
}
\usage{lyxprefhack( userdir=NULL, self_test=TRUE)
}
\arguments{
\item{ userdir}{Where your config files live; see section 2 of LyX's "Customization" manual. R{} will prompt you for it if you don't supply it (and if it also can't be auto-deduced; see below), so you can copy it from "Help->About" in LyX; single backslashes or forward-slashes are OK on Windows.}
\item{ self_test}{set FALSE to omit the self-test, which does take a few seconds (e.g. if you know it will work, or you know it will fail...).}
I reckon the best way to run LyX (not just for this), is to set the userdir via an envar of the form "LYX_USERDIR_24x" (or "...23x" for LyX 2.3, etc), as documented in LyX's "Customization" manual (currently in the section "...multiple configurations", though I only use one config). That means file associations etc all use exactly with the same LyX setup as if you launch LyX directly. If you have set this up already, then leave \code{userdir} blank.
}
\value{
As described in DID.IT.WORK and MANUAL.SETUP.IN.LYX:
\item{ - Overwrites the file "<userdir>/preferences" ((unless theres no change required). The old "preferences" will be backed up to "old_preferences<N>" (guaranteed not to overwrite any existing backup).}{}
\item{ - Adds a "lyxport" option to "Help->Specific manuals", in the file "<userdir>/ui/stdmenus.inc", again making a backup of the latter if any change was needed. If "stdmenus.inc" did not exist in "userdir/ui", then its copied there from (an attempt to find...) LyXs system dir.}{}
\item{ - Copies one file from the R installation to LyXs "<userdir>/doc" ("lyxport-docu.lyx") and one set of files to LyXs "<userdir>/examples/lyxport" ("lyxport-demo.lyx" and associated files).}{}
The actual R{} return-value is probably a string such as "LyX archive ...zip created successfully" (which is good, you can ignore it) or some error output if the self-test failed. If \code{self_test=FALSE} you should get 0, which signifies nothing :)
\subsection{Version proofing}{
There is a petageek-level subtlety with PATH prefix and R{} versions, which I will mention here "for completeness"--- partly because I find it generally useful, not just for \code{\link{lyxport}}.

R{} often needs updating; I don't want to have to remember each time which obscure LyX features I need to also update--- nor indeed in lots of other bits of software which might occasionally want to call R{}. It would be nice if I could set an envar(s) that holds the current R{} path (well, I can and I do do that), and ask LyX to use the envar instead of some absolute path, but that second part doesn't currently work. Instead, I have set up (in Windows) a symlink folder for R{} whose nominal path is always fixed, and I use that as the PATH_prefix in LyX (and other places, eg the start-menu item for R). Then, when R{} is updated, I just run a little command-shell script that updates the symlink. I use a similar trick fro LyX itself, obvs not to tell LyX about where it lives, but rather to be able to refer to the current LyX consistently from other pieces of software. I did say "petageek"--- you were warned!
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
