# This is package lyxport 

".onLoad" <-
function( libname, pkgname){
## lyxport .onLoad ##

  # Create wrapper
  run_Cloaders_lyxport()

}


"captured" <-
function( s, m, idx=m@capture.names %except% '') {
  if( is.character( m) && length( s)) { # then it's a regex in waiting
    m <- gregexpr( m, s)[[1]]
  }
  
  # NB m may already have been set to zero-length
  if( length( s) && length( m) && m[1]>0) {
    start <- m@capture.start[,idx]
    res <- matrix( substring( s, start, start + m@capture.length[,idx]-1), ncol=length( idx))
  } else {
    res <- matrix( '', nrow=0, ncol=length( idx))
  }
  # Bloody R bug stops this working in 1-row case:
  # dimnames( res)[[2]] <- idx
  res <- as.data.frame( res, stringsAsFactors=FALSE)
  if( is.character( idx))
    names( res) <- idx
return( res)
}


"check_lyx_pandoc" <-
function( 
  stop_or_warn= stop,
  lyxdir= NULL
){
  if(Sys.info()["sysname"]=="Windows"){
    # "C:/ProgramData/Lyx/lyx_current/resources"
    lyxdir <- lyxdir %||% dirname( dirname( Sys.which( 'LyX')))
    if( !dir.exists( lyxdir)){
      lyxdir <- file.path( Sys.getenv( 'LYX'))
    }
    # Check for existence of _runnable_ Lyx & pandoc
    # Should be platform-agnostic, ie not require or repudiate dot-exe
    ##   ^^^^^ lol, it isn't
    # Don't use normalizePath() coz it unpacks symlinks
    lyxdir <- gsub( '\\', '/', lyxdir, fixed=TRUE)
    lyxec <- file.path( lyxdir, 'bin', 'lyx')
  }else{
    # once again, having correct paths by default makes your life easier ;)
    lyxec <- Sys.which( 'lyx')
  }

  if( !nzchar( Sys.which( lyxec))){ # sneaky old me!
    stop_or_warn( "No LyX executable in path :(")
  }

  if( !nzchar( Sys.which( 'pandoc'))){ # sneaky old me!
stop_or_warn( "No pandoc executable in path :(")
  }

return( lyxec)
}


"eqalignfix" <-
function( native_file){
  native <- readLines( native_file)

  # All lines seem to start with punctuation  
  # Indents matter!
  # Here's a snippet of what we expect:

  r"--{
, Table
    ( "" , [] , [] )
    (Caption Nothing [])
    [ ( AlignLeft , ColWidthDefault )
    , ( AlignLeft , ColWidthDefault )
    ]
    (TableHead...
  }--"

  
  indent <- nchar( sub( '^( *).*', '\\1', native))
  tabstart <- grep( '^ *[[,] +Table$', native)
  tabind <- indent[ tabstart]
  tabend <- match_after( tabind, indent, tabstart+1, 0)
stopifnot( all( tabend > 0)) # table must end properly

  # May as well restrict now to tables that look like eqn-holders...
  defcol <- '^ +[[,] *[(] *AlignLeft *, *ColWidthDefault *[)]'
  L2_ok <- grepl( 'Caption Nothing', native[ tabstart+2])
  L3_ok <- grepl( defcol, native[ tabstart+3])
  L4_ok <- grepl( defcol, native[ tabstart+4])
  L5_ok <- grepl( '^ +\\]$', native[ tabstart+5])
  ok <- L2_ok & L3_ok & L4_ok & L5_ok
  tabstart <- tabstart[ ok]
  tabend <- tabend[ ok]
  
  # Find eqn labels
  FIXME <- '#/fixLABELalignEQ/###@' # inserted by eqn_labels_for_word()
  eqlab <- grep( FIXME, native, fixed=TRUE)
  eqtab <- findInterval( eqlab, tabstart)
stopifnot( all( tabend[ eqtab] > eqlab)) # not inside table

  # Fix labels themselves
  native[ eqlab] <- sub( FIXME %&% '([^"]*)"', '(\\1)"', native[ eqlab])
  
  native[ tabstart[ eqtab] + 3] <- sub( 'AlignLeft', 'AlignDefault',
      native[ tabstart[ eqtab] + 3])
    native[ tabstart[ eqtab] + 3] <- sub( 'ColWidthDefault', 'ColWidth 0.8',
      native[ tabstart[ eqtab] + 3])

  native[ tabstart[ eqtab] + 4] <- sub( 'AlignLeft', 'AlignRight',
      native[ tabstart[ eqtab] + 4])
    native[ tabstart[ eqtab] + 4] <- sub( 'ColWidthDefault', 'ColWidth 0.1',
      native[ tabstart[ eqtab] + 4])

  if( FALSE){ # I've decided not to do this. Up to user to get it right in LyX.
    # Also, fix up double-parens in refs to equations:
    # things like this:      , Str "((A1))"
    # but we also get this :/ : Str "eqn\160((A1
    # and you could get valid Str "(5))." at end of parenthesised sentence!
    # Sigh....

    # native <- xgsub( native, 'Str "([^(]*)[(]+([A-Z]?[0-9]+)[)]+"', # did have a * there...
    #    'Str "\\1(\\2)"')

    # Two _opening_ parens are deffo bad tho
    xsparl <- grep( 'Str +".*[(][(]', native)
    xspar <- native[ xsparl] |> 
        xsub( '^[^"]*"[^(]*', '') |>
        xsub( '[^(].*', '')
    xspar <- nchar( xspar)
    for( ixs in unique( xspar)){
      ibad <- which( xspar==ixs)
      # Trim-to-one this number of opening parens...
      # ... and only the same number of closing parens
      native[ xsparl[ ibad]] <- native[ xsparl[ ibad]] |>
          xsub( strrep( '(', ixs), '(', fixed=TRUE) |>
          xsub( strrep( ')', ixs), ')', fixed=TRUE)
    }
  }

  writeLines( native, native_file)
return( NULL)
}


"figurefix" <-
function( panfile, natnum){
## Remove superfluous "Figure N" (only if natnum=TRUE)
## And tweak figure numbers in Appendices

  pan <- readLines( panfile)
  
  r"--{
    Here is pandoc getting it wrong by inserting a hard-wired "Figure N" (and it also gets the N wrong!
    
    But it does not _always_ do this.
    
    , Figure
        ( "fig:mss" , [] , [] )
        (Caption
           Nothing
           [ Plain
               [ Str "Figure"
               , Space
               , Str "4:"
               , Space
               , Str "Spatial"
               , Space
               , Str "distribution"
               , Space
   }--"

  # Usual indent shenanigans  
  Fig <- grep( '^ *(, *)?Figure', pan)
  has_cap <- grepl( '^ *[(] *Caption', pan[ Fig+2])
  Fig <- Fig[ has_cap]
  has_spurious <- natnum && grepl( '"Figure"', pan[ Fig+5])
  
  if( any( has_spurious)){
    Fig_spur <- Fig[ has_spurious]
    # thankfully, no one-liners AFAICS. So we can delete 4 lines and 
    # replace the comma at next SOL by sqbrak

    pan[ Fig_spur+9] <- sub( ',', '[', pan[ Fig_spur+9])
    zappo <- c( outer( 5:8, Fig_spur, '+'))
    pan <- pan[ -zappo]
  }
  
  # Renumber Apx figs. Same machinery as tablefix()

  r"--{
    , Figure
        ( "fig:cod" , [] , [] )
        (Caption
           Nothing
           [ Plain
               [ Str "Figure"
               , Space
               , Str "5:"
               , Space
               , Str "figgo"
        
  }--"
  
  figlabz <- sub( '^[^"]*"([^"]+)".*', '\\1', pan[ Fig+1])
  extract.named( renumber_apx_floats( 
      'fig', Fig, figlabz, pan)) # creates pan & numz
      
  # Links done; now do actual numbers in captions
  possfignumls <- grep( ', Str "[0-9]+:"', pan)
  fignuml <- possfignumls[ findInterval( Fig, possfignumls)+1]

  # Some figures might be unlabelled (mm==NA): grep ignores
  for( ifig in grep( '^[A-Z]', numz)){
    # IE we are in Apx
    pan[ fignuml[ ifig]] <- sub( '"[0-9]+:"', sprintf( 
        '"%s:"', numz[ ifig]), pan[ fignuml[ ifig]])
  }

  writeLines( pan, panfile)
invisible( pan)
}


"find_cites" <-
function( tex){
  # Allow for superfluous (?) "*" after \cite
  citelines <- grep( r"(\\cite[a-zA-Z]*[*]?[{].*[}])", tex)
  if( !length( citelines)){
return( character())
  }
  
  tex <- tex[ citelines]
  m <- gregexpr( r"(\\cite[a-zA-Z]*[*]?[{]([^}]*)[}])", tex)
  cites <- regmatches(  tex, m) |> unlist() |> 
      sub( '.*[{]', '', x=_) |> sub( '[}].*', '', x=_)
  
  cites <- unlist( strsplit( cites, ',', fixed=TRUE))
return( unique( cites))
}


"fix_subfloats" <-
function( tex){
  r"--{
  pandoc doesn't handle subfloat. Need to break that up into separate things...
  This (unsubfloated) figure works:
  
  \begin{figure}[H]
  \caption{Platforms and observers on the \emph{Shonan Maru}\label{fig:shonan}}

  \includegraphics[scale=0.6]{SM-platforms-edited}
  \end{figure}

  This doesn't (andbut NB fig nums are skrood in PDF too; first of the 3 is Fig 3 and the other two are Fig 4!) :
  
  \begin{figure}[H]
  \caption{Spatial distributions of AMWs during SOWER.\label{fig:spatabund}}
  
  \subfloat[1986–1994]{\resizebox{0.95\columnwidth}{!}{\includegraphics{figs/agg-dens-1}}
  
  }
  \end{figure}
  
  \begin{figure}[H]
  \caption{Spatial distributions of AMW by year (cont.)}
  
  \ContinuedFloat
  
  \subfloat[1995–2002]{\resizebox{0.95\columnwidth}{!}{\includegraphics{figs/agg-dens-2}}
  
  }
  \end{figure}
  
  \begin{figure}[H]
  \caption{Spatial distributions of AMW by year (cont.)}
  
  \ContinuedFloat
  
  \subfloat[2003–2004]{\resizebox{0.95\columnwidth}{!}{\includegraphics{figs/agg-dens-3}}
  
  }
  \end{figure}
  }--"
  
  # Try removing \ContinuedFloat and the subfloat{} wrapping, and 
  # repeating the label...
  
  # Actually, it could be the resizebox{} coz the MSS fig doesn't appear either

}


"fixup_bib_title_case" <-
function( 
  bibs,             # bib files
  cites,            # names, eg from find_cites()
  prefolder= NULL  # folder prefix
){
  newbib <- character()
  
  if( length( prefolder)){
    is_local <- file.exists( bibs) # precedence
    # Only one bib location allowed (apart from local)
    bibs[ !is_local] <- file.path( prefolder, bibs[ !is_local])
  }
  
  for( bibf in bibs){
    bibl <- readLines( bibf)
    reflines <- grep( '^ *@[a-zA-Z0-9]+ *[{]', bibl)
    
    refs <- bibl[ reflines] |> 
        xsub( '^[^{]*[{] *', '') |>
        xsub( ' *, *$', '')
        
    used <- refs %in% cites
    if( any( used)){
      endref <- c( reflines[-1]-1, length( bibl))
      keepy <- unlist( mapply( ':', reflines[ used], endref[ used]))
      newbib <- c( newbib, bibl[ keepy])
    }
  }
  
  # Now the titles...
  titlines <- grep( '^ *[Tt]itle *= *["{]', newbib)
  titles <- newbib[ titlines]
  # Quote or curly?
  startch <- sub( '^[^=]*= *([^ ]).*', '\\1', titles)
  titles <- sub( '.[^"{]*["{] *', '', titles)
  
  # Remove stuff at the end--- usually a comma, but not if final line
  # in the entry
  titles <- sub( ' *, *$', '', titles)
  # ... and the string closifier. Trailing spaces already gone.
  titles[ startch=='"'] <- sub( ' *"$', '', titles[ startch=='"'])
  titles[ startch=='{'] <- sub( ' *[}]$', '', titles[ startch=='{'])
  
  # Assumed definite lowercase (rare) is already marked
  
  # In each title, are desired capitals
  # already curlified? If any are, presumably all are
  # Complication of eg \emph{Lynx lynx}...
  # This isn't perfect, becoz laziness
  # NB this takes the input caps _seriously_ ! so you gotta sentence-case
  # yer bibs except where you Really Want Capitals
  
  # If first char is lowercase, presumably deliberate (eg software)
  low1st <- substring( titles, 1, 1) %in% letters
  need_caseprot <- grepl( "(([\\]emph *[{])|[ /('a-z0-9-])[A-Z]", titles)
  titles[ need_caseprot] <- titles[ need_caseprot] |> 
    xgsub( "(([\\]emph *[{])|[ /('a-z0-9-])([A-Z][A-Z0-9]*)", '\\1{\\3}') |>
    xsub( ' -', '-', fixed=TRUE) |> 
    xsub( '- ', '-', fixed=TRUE)
  titles[ low1st] <- sprintf( '{%s}%s', 
      tolower( substring( titles[ low1st], 1, 1)),
      substring( titles[ low1st], 2))
  newbib[ titlines[ need_caseprot]] <- sprintf( 
      '  title={%s},', titles[ need_caseprot])
  
return( newbib)
}


"fixup_fuxup" <-
function( panfile){
  pan <- readLines( panfile)
  
r"--{
  Latex citations are well fucked up by pandoc!
  
  - citep (parens round all): OK with single or xtuple, except missing commas before date
  - citet (parens round date):  OK with SINGLE refs, but not with xtuple eg   Nonk & Ponk (2013; Palsbøll et al., 1997)
    - citealp: adds spurious wrapping parens around everything, loses commas before date
    - citealt:
      -- does not drop commas like it should
      -- adds spurious params around the date only
      -- for >1 citation, spurious parens around whole thing
      -- eg bad (Allen, 2003; Nonk & Ponk; 2013) or Nonk & Ponk (2013).    
      
  NB you _can_ get all-on-one-liners :(
  eg [ Str "(Branch," , Space , Str "2006)" ]

    Also, biblatex options such as uniquename=false and uniquelist=false don't seem to be respected. I find those two so useful that there's an option to auto-enforce them...
}--"

  # Find all blocks that might contain pandoc's attempt at citation-text
  # As ever, indentation level is key...
  nind <- nchar( sub( '[^ ].*', '', pan))
  start_sqstr <- grep( '^ *\\[ Str "', pan) 
   # %such.that% !endsWith( pan[.], ']')
  end_sqstr <- start_sqstr # default: one-liner  

  not1liner <- !endsWith( pan[start_sqstr], ']')
  endsq <- grep( '^ *\\]', pan)
  first_plaus_end <- findInterval( start_sqstr[ not1liner], endsq)+1
  end_sqstr[ not1liner] <- endsq[ match_after( 
      nind[ start_sqstr[ not1liner]], nind[ endsq], first_plaus_end)]

  preadd_commas <- function( frag){
      # frag is some lines from pan
      # Append comma to last string before each date
       # Ignore any risk of all-one-liners...
       if( length( frag)==1){ # one-liner
         frag <- gsub( '(" +, +Space +, +"[0-9]+)', ',\\1', frag)
       } else {
         strlines  <- grep( ' Str "', frag)
         datelines <- grep( '"[0-9]+', frag)
         strlines <- strlines %except% datelines
         prev_strlines <- strlines[ findInterval( datelines, strlines)]
         frag[ prev_strlines] <- sub( '" *$', ',"', frag[ prev_strlines])
       }
       frag <- sub( ',,+', ',', frag)
     return( frag)
     }

  # citep
  for( i in grep( ' Str ".*\\*\\*FIXCITEP\\*\\*"', pan)){
     iblok <- findInterval( i, start_sqstr)+1
     strblock <- start_sqstr[ iblok] : end_sqstr[ iblok]
     pan[ strblock] <- preadd_commas( pan[ strblock])
  }

  # citet
  for( i in grep( ' Str ".*\\*\\*FIXCITET\\*\\*"', pan)){
     iblok <- findInterval( i, start_sqstr)+1
     strblock <- start_sqstr[ iblok] : end_sqstr[ iblok]
     
     # citet should have no commas, 
     # and have parens around numbers (only)
     # I'm gonna keep semicolons, which should only occur 
     # between xtuple citations.
     
     pan[ strblock] <- pan[ strblock] |>
         xgsub( '( +),( +)', '\\1#\\2') |>    # keep structural commas         xgsub( ',', '') |>                  # zap other commas
         xgsub( '[(]*([0-9]+[a-z]*)[)]*', '(\\1)') |> # xtuple parens -> single
         xgsub( '[(]([A-Za-z])', '\\1') |>    # no parens before words
         xgsub( '( +)#( +)', '\\1,\\2') |>    # replace structural commas
         xgsub( '([(][0-9]+[a-z])[)],', '\\1,') # hackfix 2003a,b in same citation...
  }

  # citealp
  # could in fact do this without loop, but...
  for( i in grep( ' Str ".*\\*\\*FIXCITEALP\\*\\*"', pan)){
     iblok <- findInterval( i, start_sqstr)+1
     s <- start_sqstr[ iblok]
     e <- e0 <- end_sqstr[ iblok]
     if( e==s) e <- e+1 # cozza e-1 below
     
     # citealp: removing wrapping parens
     pan[ s] <- sub( '(', '', pan[ s], fixed=TRUE)
     # Replace _final_ paren only, tho shouldn't be others
     pan[ e-1] <- sub( '(.*)[)]', '\\1', pan[ e-1])
     
     # Commas done last, coz parens need to be sorted out first
     pan[ s:e0] <- preadd_commas( pan[ s:e0]) # keep one-liners on one line
  }

  # citealt
  for( i in grep( ' Str ".*\\*\\*FIXCITEALT\\*\\*"', pan)){
     iblok <- findInterval( i, start_sqstr)+1
     strblock <- start_sqstr[ iblok] : end_sqstr[ iblok]     
     
     # No commas or parens (though commas perhaps permissible if >2 names)
     # and commas must be kept at start (modify & put back)...
     # No semicolons after letters
     pan[ strblock] <- pan[ strblock] |>
         xgsub( '^( *),', '\\1#') |>    # keep structrual commas at SOL
         xgsub( '[(),]', '') |>         # zap parens and other commmas
         xgsub( '((?<![0-9])[a-zA-Z.]);', '\\1') |>   # zap semis after words (keep after numbers)
         xgsub( '^( *)#', '\\1,')       # replace structural commmas   
    }

  # Elim the warnings!
  pan <- gsub( '*\\*\\*FIXCITE(P|T|ALP|ALT)\\*\\*', '', pan)
  # Trim empty lines --- be careful of eg "(**FIXCITE ..."
  pan <- pan[ !grepl( '^ *(, +)?Str ""$', pan)]

  writeLines( pan, panfile)
invisible( pan)
}


"get_refdoc" <-
function( tex, outext, refdir){
  # Search for lines with %% Pandoc reference-doc/template 
  # Similar to last part of handle_bib()
  
  refdoc <- grep( '^ *%% *[Pp]andoc +(reference-doc|template) +', tex, 
      value=TRUE) %such.that% endsWith( ., '.' %&% outext)
  if( !length( refdoc)){
return( '')
  }
  
  # Use last if several!
  refdoc <- sub( '.*[Pp]andoc +', '', trimws( tail( refdoc, 1))) 
  
  # Sometimes ya need template, sometimes it's reference-doc
  tempordoc <- sub( '^([^ ]+) .*', '\\1', refdoc)
  
  # What is the actual file, and where does it live?
  refdoc <- sub( '.* +', '', refdoc)
  refdoc <- if( startsWith( refdoc, './')) substring( refdoc, 3) else 
        file.path( refdir, refdoc)
  refdoc <- sprintf( '--%s=%s', tempordoc, refdoc)
return( refdoc)
}


"handle_appendices_tex" <-
function( tex){
  r"--{ 
  Pandoc doesn't understand Latex appendices, nor where the biblio should go, so fudge things by:
  
    - after \appendix, changing section{blah} to section*{Appendix <N>: blah} before Pandocking
   
    - moving the pandoc-native Div("refs") block to before the first appendix (different function)
    
    Assumes that general xrefs to Apxes are dun rite manually
  }--"
  
  has_appendix <- match( '\\appendix', tex, 0)
  if( has_appendix){
    numapx <- which( startsWith( tex, '\\section{')) %such.that% (. > has_appendix)
    nonumapx <- which( startsWith( tex, '\\section*{')) %such.that% (. > has_appendix)
    
    for( iapx in seq_along( numapx)){
      # Unnumbered section, with explicit Appendix lable
      tex[ numapx[ iapx]] <- sub( 'n{', sprintf( 'n*{Appendix %s: ', LETTERS[ iapx]), 
          tex[ numapx[ iapx]], fixed=TRUE)
    }
    
    tex[ nonumapx] <- sub( 'n*{', 'n*{Appendix: ', tex[ nonumapx], fixed=TRUE)
  }
  
return( tex)
}


"handle_bib" <-
function( texlines, refdir, uniqnaml, fixinit){
  ## Bibliography (if required): 
  # First, gotta find 'em
  
  # Get ready for bibhack, if specified
  bibhack_fun <- identity
  start_bibhack <- rev( grep( '(?i)^ *%% +Bibhack', texlines))[1] # use last
  if( !is.na( start_bibhack)){
    end_bibhack <- grep( '(?i)^ *%% End bibhack', texlines)
    end_bibhack <- (end_bibhack %such.that% (. > start_bibhack))[1]

    if( !is.na( end_bibhack)){
      bibhack_fun <- try( eval( parse( text=
          texlines[ (start_bibhack+1) %upto% (end_bibhack-1)])[[1]]))
    }

stopifnot( 
        bibhack_fun %is.a% 'function',
        length( args( bibhack_fun)) == 1
    )
  }
        
  biblines <- which( startsWith( texlines, '\\addbibresource{'))
  printbib <- which( texlines == '\\printbibliography')

  if( !length( biblines) || !length( printbib)){
    resourcio <- '' # clearer code...
  } else { # we have biblio. Need to process it. Need to find it, first!
    # if unmodded bib was used, then...
    # resourcio <- sprintf( '--citeproc --resource-path %s/bibtex/bib',
    #    refdir)

    # but Pandoc "Citations" section says that titles will be sentence-cased
    # unless uppers are protected. So, protect 'em... 
    # First, extract used refs to a new file...
      
    bibfiles <- texlines[ biblines] |> 
        sub( '^[^{]*[{] *', '', x=_) |>
        sub( ' *[}] *$', '', x=_)
        
    cites <- find_cites( texlines)
    bibble <- fixup_bib_title_case( bibfiles, cites, 
        prefolder=file.path( refdir, 'bibtex', 'bib'))  
    if( uniqnaml && (fixinit %not.in% c( 'false', 'F'))){
      bibble <- tex2utf8( bibble) # not needed otherwise
      bibble <- tidy_initials( bibble, gungho= (fixinit=='gungho') )
    }
    bibble <- bibhack_fun( bibble)
    
    writeLines( bibble, 'bibble.bib')
    texlines[ biblines[ 1]] <- '\\addbibresource{bibble.bib}'
    if( length( biblines)>1){ # avoid replacing ALL LINES !!!
      texlines[ biblines[ -1]] <- ''
    }
      
    # ... and prepare to tell pandoc to use that file
    resourcio <- sprintf( '--citeproc --resource-path .')
    
    # Citation format: use last one, if several
    CSL <- rev( grep( '^ *%% *CSL +', texlines))[1]
    if( !is.na( CSL)){
      CSL <- trimws( sub( '^ *%% *CSL +', '', texlines[ CSL]))
      CSL <- if( startsWith( CSL, './')) substring( CSL, 3) else 
          file.path( refdir, CSL)
      resourcio <- sprintf( '--csl=%s %s', CSL, resourcio)
    }
    
    # And add an unnumbered section beforehand
    texlines <- multinsert( texlines, printbib, '\\section*{\\bibname}')
  }
return( list( texlines=texlines, resourcio=resourcio))
}


"housekeeping" <-
function( nlocal=sys.parent()) mlocal({
## Executes "in-line" within lyxzip2word
## Sets paths, moves files around, ...

  if( FROM_LYX){
    args <- commandArgs(TRUE)
    zipfile <- sub( '[.]lyx$', '.zip', args[1])
    origdir <- args[ 2]
    tempdir <- args[3]
    if( length( args)>3){
      dbglyx <- args[ 4]
    }
  }    
  
  lyxec <- check_lyx_pandoc( lyxdir=lyxdir) # or crash
  if( nzchar( dbglyx)){ print_debugs( 1) }
  
  # For image resizing (almost always necessary, I suspect)
  magick <- Sys.which( 'magick') # ImageMagick
  if( !nzchar( magick)){
    magick <- file.path( dirname( lyxec), 'imagemagick', 'magick')
    ok <- system2( magick, '-version')
    if( !ok){
warning( 'No "magick" in PATH or in LyX--- won\'t be able to scale images')
      magick <- NA # won't try
    } # but carry on... (?wise?)
  }
 
  r"--{
  Try to find LyX version. Works from R directly, but somehow not when R is launched from inside LyX, which is when we actually need it. Sigh. 4/2/2025: add "-n" in desperation.
  }--"
  
  lyxver <- system2( lyxec, '-n -version', stdout=TRUE) 
  if( nzchar( dbglyx)){ cat( 'lyxver: '); print( lyxver)}
  
  if( !length( lyxver) || is.character( lyxver) || any( is.na( lyxver))){
    # NFI why this doesn't effing work in 2.4.2.1 from Lyx
    # it works FINE from R, regardless of LYX_USERDIR_24x
    scatn( "Can't work out LyX version: guessing 2.4.2.1")
    lyxver <- '24'
    lyxver_full <- numeric_version( '2.4.2.1')
  } else {
    lyxver_full <- lyxver[1] |>
      xsub( '[^0-9]*', '') |> 
      xsub( ' *[(].*', '')
    lyxver <- lyxver_full |>
      xsub( '([.][0-9]+)[.].*', '\\1') |>
      xsub( '[.]', '')
    lyxver_full <- numeric_version( lyxver_full)
  }
  if( nzchar( dbglyx)){ print( lyxver_full)}
  
  if( lyxver_full <= '2.4.2.1'){
    Sys.unsetenv( sprintf( 'LYX_USERDIR_%sx', lyxver))
    # otherwise we get SIGSEGV later :(
    # Usually OK, but local modules/layouts are unavailable
  }
  
  force( origdir) # before modding zipfile
  origdir <- sub( '/*$', '', origdir) # strip trailers

  # on the Microsoft Windows "operating" "system" LyX generates zip files
  if(Sys.info()["sysname"] == "Windows"){
    zipfile <- sub( '[.][a-zA-Z0-9]*$', '', basename( zipfile)) %&% '.zip'
  }else{
    # on a real operating system we get a .tar.gz
    zipfile <- sub( '[.]*[a-zA-Z0-9]*[.][a-zA-Z0-9]*$', '',
                   basename( zipfile)) %&% '.tar.gz'
  }
  # tools::file_path_sans_ext( basename( zipfile)) %&% '.zip'
  
## Move & extract (if it's a zip)
stopifnot( file.exists( file.path( origdir, zipfile)))

  # zipfile is in the original folder, presumably cozza export... 
  # move it to tempdir. Normally we don't need it
  tempdir <- sub( '/*$', '', tempdir) # strip trailers
  if( tempdir != origdir){
    from <- file.path( origdir, zipfile)
    to <- file.path( tempdir, zipfile)
    if ( !copy){
      file.rename( from, to) 
    } else {
      file.copy( from, to, overwrite=TRUE)
    }
  }

  if( nzchar( dbglyx)){ print_debugs( 2) }

  odir <- normalizePath( origdir) # for copying later

## LyX version and userdir (former in case bug gets fixed)  
  # Try to set lyx_userdir  
  if( lyxver_full > '2.4.2.1'){
    # Assume this bug will be fixed in later releases
    if( is.null( lyx_userdir)){
      # Try to find the LyX userdir. Not so easy...  
      lyx_userdir <- Sys.getenv( sprintf( 'LYX_USERDIR_%sx', lyxver))

      # This should be hardwired to work on my machines
      if( !dir.exists( lyx_userdir)){
        # Don't use normalizePath() coz it unpacks symlinks
        lyx_userdir <- file.path( dirname( lyxec), '..', '..', 
            'lyx_current_userdir')
      }
    }

    # To tell standalone Lyx about userdir, if any:
    if( !dir.exists( lyx_userdir)){
      warning( sprintf( 
          "Can't work out userdir--- try setting envar LYX_USERDIR_%sx",
          lyxver
        ))
      lyx_userdir_info <- '' 
    } else {
      lyx_userdir_info <- '-userdir ' %&% lyx_userdir
    }
  } else {
    lyx_userdir_info <- '' # gotta disallow for now, AFAICS; LyX bug
  }

  if( nzchar( dbglyx)){ print_debugs(3)}
})


"lyxprefhack" <-
function( userdir=NULL){
## A few sanity checks on file availability first...
  lyxec <- Sys.which( 'lyx')
  if( !nzchar( lyxec)){
stop( r"--{
    First, make sure the LyX executable is in your PATH--- right now it isn't, according to 'Sys.which('lyx')'}--" |> trimws())
  }

  while( is.null( userdir)){ # allow 'break'
    r"--{
    The best way for a user to control this, is via envar "LYX_USERDIR_**x" where "*" is replaced by the major+minor version, eg "23" or (currently) "24". Many users *won't* have done that, but if they have, we should honour it...
    }--"
    lyxver <- system2( lyxec, '-n -version', stdout=TRUE) |>
        grep( '^LyX ', x=_, value=TRUE) |> 
        _[1] |> 
        xsub( '^[^ ]* +([0-9])[.]([0-9]+).*', '\\1\\2')
    if( !is.na( lyxver)){
      userdir <- Sys.getenv( sprintf( 'LYX_USERDIR_%sx', lyxver))
      if( nzchar( userdir)){
  break
      }
    }
  
    cat( 'LyX user dir (where your local "preferences" file is; get from "Help->About LyX"; single backslashes are OK): ')
    userdir <- readline()
  }
    
stopifnot(
  file.exists( defile <- file.path( userdir, 'lyxrc.defaults')),
  file.exists( uprefile <- file.path( userdir, 'preferences'))
)

  # Check that userdir has menu file (might not have been copied yet)
  uidir <- file.path( userdir, 'ui')
  if( !dir.exists( uidir)){
    dir.create( uidir)
  }
  menfile <- file.path( uidir, 'stdmenus.inc')
  if( !file.exists( menfile)){
    r"--{
    Get systemdir programmatically. Might be possible by launching Lyx and asking it to Do Something, and parsing the output. But I'm not sure; rabbit hole alert!
    }--"
    sysdir <- file.path( lyxec, '../../Resources')
    if( !file.copy( file.path( sysdir, 'ui/stdmenus.inc'), menfile)){
stop( sprintf( 'Could not find/copy "stdmenus.inc" into "%s/ui" folder', userdir))
    }
  }

  def <- readLines( defile) %that.match% '^\\\\Format +[^ ]+ +docx '
  
  # New file format (alias for MSWord)
  if( !length( def)){
    warning( '"lyxrc.defaults" looks iffy: cannot find "docx" format. Making it up instead...')
    newdef <- trimws( r"--{
      \format "wordx" "docx" "MS Word XML" "W" "swriter" "swriter" "document,vector,menu=export" "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
    }--")
  } else {
    # Split into tokens. I am being fancy here...
    # Issue is quoted strings with (perhaps) spaces, & unquoted strings.
    # First, replace all spaces within quoted strings by @
    # Simplest to split into chars
    
    newdef <- trimws( def[1]) |>
      xgsub( '\t', ' ') # tabzzzzz...
    r <- gregexpr( ' "[^"]+"', newdef)[[1]]
    quoteds <- unlist( mapply( 
        function( from, len) seq( from=from+2, length.out=len-3), 
        r, attr( r, 'match.length')))
        
    chr <- charToRaw( newdef)
    spaces <- which( chr==charToRaw( ' '))
    chr[ spaces %that.are.in% quoteds] <- charToRaw( '@')
    newdef <- rawToChar( chr) |>    # a string again
        xsub( 'F', 'f') |>          # decapitalize Format
        strsplit( ' +') |>          # tokenize
        unlist() |>                 # there's only one line
        xgsub( '@', ' ')            # put spaces back
    # Replace and wrap in quotes:
    newdef[ 2:5] <- sprintf( '"%s"', 
        c( 'wordx', 'docx', 'MSWord (lyxport)', 'W'))
    newdef <- paste( newdef, collapse=' ')
  }

  upref <- oupref <- readLines( uprefile)
  
  ## Make sure R is in path...
  # This is *only* an issue on Windows where you are actively discouraged
  # from doing anything useful and reproducible on the computer.
  # Any Mac/Linux installation will automatically make R accessible in the
  # system path because that's a thing that makes your life easier.
  # so you don't need any of this awkward chutney
  thisR <- commandArgs()[1]
  if(Sys.info()["sysname"] == "Windows"){
    prepathl <- startsWith( upref, '\\path_prefix ')
    prepath <- upref[ prepathl] |> 
        xsub( '^[^"]+"', '') |> 
        xsub( '"$', '') |>
        strsplit( ';', fixed=TRUE) |>
        _[[1]]
    nonbuiltin <- !grepl( '$LyXDir', prepath) # don't look here for R
    
    if( any( nonbuiltin)){
      # What filext are we looking for..?
      mebbe_ext <- sub( '.*([.][^.]+)$', '\\1', basename( thisR))
      # Look for R or R.exe (or R.godknowswhat on Macs and beyond...)
      gotR <- file.exists( file.path( prepath[ nonbuiltin], 'R' %&% mebbe_ext))
      if( any( gotR)){
        nonbuiltin[] <- FALSE # signal that we need to add
      }
    }
    
    if( any( nonbuiltin)){
      prepath <- c( prepath,  dirname( thisR))
      # Want fwd-slash, and I think spaces are OK
      # Avoiding normalizePath() here, to preserve symlinks
      upref[ prepathl] <- sprintf( '\\path_prefix "%s"',
        paste( gsub( '\\', '/', prepath, fixed=TRUE), collapse=';'))
    }
  } # end of awkward Windows chutney
  
  format_start <- grep( '^#+ +FORMATS SECTION', upref)[1]
  conv_start <- grep( '^#+ +CONVERTERS SECTION', upref)[1]
  if( is.na( format_start+conv_start)){
stop( "Can't find FORMAT and/or CONVERTERS section: preferences file looks malformed")  
  }

  newconvo <- trimws( r"--{
      \converter "lyxzip" "wordx" "Rscript --no-save --no-restore --verbose -e lyxport::lyxzip2word(FROM_LYX=TRUE) $$i $$r $$p 1 > docxconv.log  2>&1" ""
    }--")

  mm <- match( c( newdef, newconvo), upref, 0)
  if( any( mm == 0)){ # something to change
    # Obsolete definitions?
    obso <- grep( '^(\\\\format +"wordx")|(\\\\converter +"lyxzip" +"wordx")', upref) %except% mm  
    if( length( obso)){
      warning( 'Commenting-out old preferences WRTO "wordx" format')
      upref[ obso] <- '# ' %&% upref[ obso]
    }

    last_form <- max( format_start+1, grep( '^\\\\format ', upref))
    last_conv <- max( format_start+1, grep( '^\\\\converter ', upref))

    # Only insert new ones if not obso there!
    upref <- multinsert( upref, at=c( last_form, last_conv)[ mm==0], 
        c( newdef, newconvo)[ mm==0])
  }
  
  if( !identical( upref, oupref)){
    oprefiles <- dir( userdir, pattern='^old_preferences[0-9]+$')
    oprefile <- if( !length( oprefiles)){
        'old_preferences1'
      } else {
        sprintf( 'old_preferences%i', 1+max( as.integer( xsub( '[^0-9]', '', oprefiles))))
      }
    file.copy( uprefile, file.path( userdir, oprefile))
    writeLines( upref, uprefile)
    scatn( sprintf( 'preferences file backed-up to "%s", and updated with new goodies', oprefile))
  } # if changes
  
  mkdir( file.path( userdir, 'examples/lyxport'))  # less fussy than dir.create
  egfiles <- dir( system.file( 'examples', package='lyxport'),
      full.names=TRUE, no..=TRUE)
  file.copy( egfiles, file.path( userdir, 'examples/lyxport'),
      overwrite=TRUE)
  
  # Also, might need to tweak the \origin line in lxyport-demo.lyx (or any other dot-lyx examples).
  # IDNK whether this matters, but the point is to avoid LyX hard-coding the _original_ path of minibib.bib
  # etc, when it exports to Latex.
  for( iex in grep( '[.]lyx$', basename( egfiles))){
    slurp <- readLines( egfiles[ iex])
    orig <- startsWith( slurp, '\\origin ')
    if( any( orig)){
      slurp[ orig] <- '\\origin /userdir/examples/lyxport/'
      writeLines( slurp, file.path( userdir, 'examples/lyxport', 
          basename( egfiles[ iex])))
    }
  }
  
  mkdir( file.path( userdir, 'doc')) # less fussy than dir.create
  file.copy( system.file( 'doc/lyxport-docu.lyx', package='lyxport'), 
      file.path( userdir, 'doc'), 
      overwrite=TRUE)

  # Add item to Help submenu
  men <- readLines( menfile)
  
  lyxport_item <- 'Item "lyxport" "help-open lyxport-docu"'
  already <- any( grepl( '^[ \t]*' %&% lyxport_item, men))
  if( !already){
    menstart <- grep( '^[ \t]*Menu "', men)
    menend <- grep( '^[ \t]*End *$', men)
    submenl <- grep( '^[ \t]*Submenu "', men)

    helpmenstart <- menstart[ grep( '"help"', men[ menstart])[1]]
    subhelpref <- min( submenl %such.that% (.>helpmenstart))
    subhelpname <- sub( '.*("[^"]+") *$', '\\1', men[ subhelpref])

    subhelpstart <- menstart[ grep( subhelpname, men[ menstart], fixed=TRUE)[1]]
    # add just before the corresponding "End"
    subhelpend <- min( menend %such.that% (.>subhelpstart))
    indent <- sub( '[^ \t].*', '', men[ subhelpend-1])
    men <- multinsert( men, subhelpend-1, sprintf( '%s%s', indent,
        c( 'Separator', lyxport_item)))
    file.copy( menfile, file.path( userdir, 'ui/old_stdmenus.inc'), overwrite=TRUE)
    writeLines( men, menfile)
    scatn( 'Updated "ui/stdmenus.inc", previous version in "ui/old_stdmenus.inc"')
  }

return( as.cat( c( newdef, newconvo)))
}


"lyxzip2word" <-
function( 
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
){
  housekeeping() # set paths, move files, ...
  
  owd <- setwd( tempdir) # might move deeper in a moment
  on.exit( setwd( owd), add=TRUE)

  is_lyx <- grepl( '(?i)[.]lyx$', zipfile)
  if( !is_lyx){
    # once again I am asking windows to come with utilities that are useful
    if(Sys.info()["sysname"] == "Windows"){
      contents <- unzip( zipfile, overwrite=TRUE) # overwrites by default
      # lyx_file <- sub( '(?i)zip$', 'lyx', zipfile)
      # ... might not work with foldery stuff
      toplyx <- grep( sub( '.zip$', '[.]lyx$', basename( zipfile)), contents, 
          value=TRUE)[1]
    }else{
      # this is obnoxious
      contents <- untar( zipfile, list=TRUE)
      untar( zipfile)
      toplyx <- grep( sub( '.tar.gz$', '[.]lyx$', basename( zipfile)), contents, 
          value=TRUE)[1]
    }
    setwd( dirname( toplyx))
    lyx_file <- basename( toplyx) 
  }

## Export to Tex, using Lyx itself
  # Seems to work despite warning about "'py' not recognized"

  scatn( 'About to texify from %s', lyx_file)
  tex_file <- sub( '(?i)lyx$', 'tex', lyx_file)
  unlink( tex_file) # thus force lyx to re-export...

  ok <- system2( lyxec,
      sprintf( '-batch %s %s -f all --export latex %s', 
        lyx_file,
        lyx_userdir_info,
        if( nzchar( dbglyx)) sprintf( '-dbg %s', dbglyx) else ''
      ))
stopifnot( ok==0, file.exists( tex_file))  

## Merge input/include (perhaps nested...}
  texlines <- readLines( tex_file)
  repeat{ 
    includel <- grep( r"(^ *\\in(put|clude)[{])", texlines)
    if( !length( includel)){
  break
    }
    incfiles <- sub( '^[^{]*[{]([^}]+)[}].*', '\\1', texlines[ includel])
    includees <- lapply( incfiles, readLines)
    texlines <- massrep( texlines, includel, includees)
  }

## Tex processing...

  # Do we want section numbering? Take an inspired guess, *before* Apx tweaks
  nonum <- sum( startsWith( texlines, '\\section*{'))
  yesnum <- sum( startsWith( texlines, '\\section{'))

  if( is.null( refdir)){
    # We may not need to actually know refdir (ie if no CSS and no template)
    # But I think the stuff below always *should* work as long as Latex
    # is installed, so Just Do It.
    # According to the internet...
    refdir <- system2( 'kpsewhich', '-var-value TEXMFHOME', stdout=TRUE)
    if( !nzchar( refdir)){
stop( "Can't find TEXMFHOME from kpsewhich")      
    }
  }

  # Biblatex options that I like:
  uniqnaml <- any( grepl( 'unique(name|list)=false', texlines))
  fixinit <- rev( grep( '^%% +tidy_initials +(gungho|timid|false|F)( |$)', texlines, value=TRUE))[1]
  fixinit <- if( is.na( fixinit)) 'gungho' else named( cq( gungho, timid, false, 'F'))[ 
      sub( '^%% +tidy_initials +([^ ]+) *.*', '\\1', fixinit)]

  # mvbutils::mlocal() and mvbutils::extract.named() are simpler than
  # the manual extraction next, but for the sake of "clarity"...
  l <- handle_bib( texlines, refdir, uniqnaml, fixinit) 
  texlines <- l$texlines
  resourcio <- l$resourcio
  
  # Any Pandoc output refdoc/template?
  refdoc <- get_refdoc( texlines, outext, refdir) 
  texlines <- handle_appendices_tex( texlines)
  texlines <- presortout_cites( texlines)
  texlines <- xref_numlists( texlines)

## Eqn labels (into tables, plus resolve xrefs to them)
  # Modify the tex file
  texlines <- prepare_eqn_labels( texlines, eqalignfix=TRUE)
  
## Scaling of graphics, by actually adjusting figure files  
  if( !is.na( magick)){
    rescale_images( texlines, magick) # does not change tex
    
    # pandoc doesn't honour scale for now, but stop it from ever
    # trying in future!
    texlines <- gsub( '[[]scale=0.[0-9]\\]', '', texlines)    
  }

  texlines <- zap_subfloats( texlines)

  writeLines( texlines, tex_file)

## Export to Pandoc native
  # XREFS via -F pandoc-crossref ... must come early in arglist
  # If pandoc-crossref is installed, that is.
  pandoc_xref <- file.path( dirname( Sys.which( 'pandoc.exe')), 
      'pandoc-crossref.exe')
  pandoc_xref <- if( crossref_pandoc && file.exists( pandoc_xref)){
      '-F ' %&% pandoc_xref 
    } else {
      ''
    }
  pandoc_file <- sub( 'tex$', 'pandoc', tex_file)

  # Need "-s" (standalone), else title/author/abstract disappear...
  pandoc_native_args <- sprintf( 
      '-s %s %s %s %s -f latex -t native -o %s',
      tex_file,
      pandoc_xref,
      if( yesnum > nonum) '--number-sections' else '', 
      resourcio,
      pandoc_file    
    )
  
  scatn( 'About to convert to native pandoc')
  test1 <- system2( 'pandoc', pandoc_native_args)
  if( test1 != 0){
stop( "Initial conversion to pandoc failed--- check the log")
  }

## pandoc-crossref does not work reliably :(
# Also, since I'm avoiding number-sections at the docx stage (coz it fux Apx)
# ... I gotta manually add numbers to section titles here
  manually_xref( texlines, pandoc_file)

## pandoc screws up most Latex citation styles, with extra/wrong parens...
  fixup_fuxup( pandoc_file) # uniqnaml

## Fix equation-table colwidths
  eqalignfix( pandoc_file)
  # ... NB for more general table-col-width case, would need to 
  # process the tex file too (to read the widths).
  # A substantial task, though do-able. NFN.

## Without native-numnbering, Tables are not labelled as such :(
## With nat-num, figure legends double-up the "Figure N" (:
## Latter is lesser of two evils, probably...
## Anyway, also gotta renumber tabs & figs in Apxes

  tablefix( pandoc_file)
  figurefix( pandoc_file, natnum_pandoc)
  
## Put the goddamn biblio BEFORE the goddamn appendices, FFS
  # (or wherever the Latex source says to bloody well put it)
  if( nzchar( resourcio)){
    maybe_move_bib( pandoc_file, texlines)
  }


## Convert native to docx
  docx_file <- sub( 'tex$', outext, tex_file)    
  pandoc_docx_args <- sprintf( '%s %s -f native %s %s -t %s -o %s',
      if( verbose) '--log=pandoc.log --verbose' else '',
      if( natnum_pandoc && (yesnum > nonum)) '--number-sections' else '', 
      pandoc_file,
      refdoc,
      panoutopts %&% if( natnum_pandoc && (yesnum > nonum)) '+native_numbering',
      docx_file
    )
  # ... used to have "+native_numbering" but that doubles-up Figure labels
  # ... pandoc-crossref seems to work well.

  scatn( sprintf( 'About to convert to %s', outext))
  test2 <- system2( 'pandoc', pandoc_docx_args)
  
  if( test2==0){
    new_docx_file <- file.path( odir, docx_file)
    unlink( new_docx_file) # if it existed...
    file.rename( docx_file, new_docx_file)
  }
  
return( test2==0)
}


"manually_xref" <-
function( tex, panfile, texfile=NULL){
## Sigh... pandoc-crossref DOESN'T WORK consistently 
## (some docus OK others not) at least not on sections.
## So I will have to bloody well do it myself :(

  # No xrefs to named sections--- what's the point? how to do subsecs? etc
  
  # Start by working out the numberings from the Tex file
  if( !is.null( texfile)){
    tex <- readLines( texfile)
  }

  apx_start <- which( tex=='\\appendix')[1] # or NA
  if( is.na( apx_start)){
    apx_start <- length( tex)+1 # all secs before "appendix"
  }

  # ... plus all un-numbered pure sections after apx-start
  # which are top-level apxes. I think specific Appendices should be "section*" but 
  # this should also catch "part*". Make them all "numbered" (starless).
  apxlines <- grep( '^[\\](section|part)[*][{]Appendix', tex) %such.that%
      (.>apx_start)
  tex[ apxlines] <- sub( '(section|part)[*]', 'section', tex[ apxlines])
      
  # All numbered (sub)sections...
  iseclines <- grep( '^\\\\(sub)*(section|paragraph)[*]?[{]', tex)
  seclines <- tex[ iseclines]
  nsecs <- length( seclines)
  is_num <- !grepl( '^[^*{]*[*]', seclines)
  
  # Just those with label, eg
  # \subsection{Challenges for AMW{subsec:Challenges}}
  has_seclab <- grepl( '[{](sub)*(sec|par):[^{}]*[}]', seclines)
  seclab <- sub( '.*[{]([^}]+)[}]+ *$', '\\1', seclines[ has_seclab])
  iseclab <- which( has_seclab)

  # Find max depth of subsection--- the next depth below is paragraph
  # Don't care about un-numbered ones
  allsubsecs <- unique( grep( '^[\\](sub)+section[{]', seclines, value=TRUE) |>
      xgsub( 'sec.*', '') |> # lose all bar subs at first
      xsub( '^.', '')) # lose backslash at start
  max_subsec_depth <- if( length( allsubsecs)){ max( nchar( allsubsecs)) %/% 3} else 0

  # In actual labeled secheads:
  subs <- sub( '^.((sub)*).*', '\\1', seclines)
  subdepth <- nchar( subs) %/% 3
  # What does "paragraph" mean? One deeper than deepest sub^n section. 
  # ditto sub^paragraph
  is_par <- grepl( '^[\\](sub)*paragraph', seclines)
  subdepth[ is_par] <- subdepth[ is_par] + max_subsec_depth + 1
  
  # Appendix resets top-level counter to 'A' rather than '<n>'
  # Subsequent \section increases the letter
  # \subsection works normally
  # Fix "A.1" etc at the end, if desired
  
  # Generate secnumbers de novo
  # Should have EMPTY secnumbers for unnumbered, eg bibliography
  # to greatly simplify logic of adding secnumbers (if any) to pandoc format
  
  # Hacky stuff if this is child doco exported on its own--- put some 0's in...
  # ... not sure if that "solves" everything...
  ctr <- character( nsecs)
  preves <- integer(0) # only relevant if directly processing a child
  for( idepth in 0 %upto% max( subdepth)){
    these <- which( is_num & (subdepth==idepth))

    if( idepth>0){ # subsection etc
      parent <- findInterval( iseclines[ these], iseclines[ preves])
      partab <- table( parent)
      
      # Count sibsecs with same parent: 1, 2, 3, ...
      sib_since_parent <- unlist( lapply( partab, seq_len))
      ctr[ these] <- sprintf( '%s.%i', 
          if( length( preves)) ctr[ preves[ parent]] else '0',
        sib_since_parent)
    } else { # section (top-level)
      ctr[ these] <- as.character( seq_along( these)) # 1 upwards
      is_apx <- iseclines[ these] > apx_start
      ctr[ these[ is_apx]] <- # A upwards
          LETTERS[ seq_len( sum( is_apx))]
    }
    preves <- these
  } # for idepth
  
  # ctr <- sub( '([A-Z])[.]', '\\1', ctr) # fix A.1 etc. ACtually leave--- like PDF

#  r"--{
#  Now let's resolve borked links in the pandoc file
#  
#  Example of unresolved link (ie need to fix):
#      , Link
#          ( ""
#          , []
#          , [ ( "reference-type" , "ref" )
#            , ( "reference" , "subsec:Challenges" )
#            ]
#          )
#          [ Str "[subsec:Challenges]" ]
#          ( "#subsec:Challenges" , "" )
#      , Str "."
#
#  vs one that WAS resolved by pandoc-crossref (ie this is what the pandoc should look like):
#
#      , Link
#          ( ""
#          , []
#          , [ ( "reference-type" , "ref" )
#            , ( "reference" , "subsec:testy-subsec" )
#            ]
#          )
#          [ Str "1.1" ]
#          ( "#subsec:testy-subsec" , "" )
#
#
#  The diff is that the line before the hash-label also contains the hash-label, rather than a numeric-type strin.
#  }--"


  pan <- readLines( panfile)
  
  # Look only in Links. Usual indent shenanigans:
  nind <- nchar( sub( '[^ ].*', '', pan))
  start_Link <- grep( '^ *(, *)?Link', pan)
  end_Link <- match_after( nind[ start_Link], nind, start_Link+1)-1
  for( l in seq_along( start_Link)){
    # Various ways we _could_ look this up; this one will do, I hope
    linklines <- start_Link[l]:end_Link[l]
    link <- pan[ linklines]
    ilinklab <- grep( '^ *[(] *"#', link)[1]
    if( is.na( ilinklab)){
  next # mebbe a doi or hyperlink. Anyway, can't fix!
    }
    linklab <- sub( '.*#([^"]+)".*', '\\1', link[ ilinklab])
    linkref <- sprintf( '[%s]', linklab)
    unres <- grep( linkref, link, fixed=TRUE)
    if( !length( unres)){
  next # already resolved!
    }
    
    # Find the right ctr:
    correcto <- match( linklab, seclab, 0)
    if( !correcto){
  next # not a section. (For now, not resolving eqn/fig/tab links here.)
    }
    
    pan[ linklines] <- sub( 
        linkref, 
        ctr[ iseclab[ correcto]], 
        link, 
        fixed=TRUE
      )
  }

## Add explicit section numbers (or A... for apx) to the 
## goddamn section sodding titles too
## otherwise Apx gets borked by number_sections

  r"--{
  Example:
  
    , Header
        2
        ( "a-recap-of-sower-cp2-and-cp3-operations" , [] , [] )
        [ Str "A"
        , Space
        , Str "recap"
        , Space
        , Str "operations"
        , Span
            ( "subsec:recap-SOWER"
            , []
            , [ ( "label" , "subsec:recap-SOWER" ) ]
            )
            []
        ]
        
  That Str "A" needs to be prepended by section number  ie ctr[] .
    
  You also get one-line Headers, which end in a rt-sqbrak. EG:
    
    , Header 3 ( "clustering" , [] , [] ) [ Str "Clustering" ]
    
  Occasionally you also get a 2-liner :/
  
  And the very first section after front-matter starts "  [ Header"
  }--"  

  meta_start <- grep( '^ *Meta$', pan)[1]
  main_start <- match_after( nind[ meta_start], nind, meta_start+1)
  
  headerl <- grep( '^ *([[,] *)Header', pan) %such.that% 
      (. >= main_start)

stopifnot( length( headerl)==length( seclines))  

  shift_headerl <- headerl
  oneliner <- endsWith( pan[ headerl], ']')
  twoliner <- !oneliner & (nind[ headerl+2]==nind[ headerl])
  shift_headerl[ twoliner] <- shift_headerl[ twoliner] + 1
  
  # otherz: gotta find where to add 
  otherz <- !oneliner & !twoliner
  sqstart <- grep( '^ +[[]', pan)
  shift_headerl[ otherz] <- sqstart[ 
      findInterval( headerl[ otherz], sqstart)+1]
  
  # Copy everything prior to the _final_ space-lsq-space (ie greedy) 
  for( i in seq_along( headerl)){
    pan[ shift_headerl[ i]] <- sub( '(.*) [[] ', sprintf(
        '\\1 [ Str "%s" , Space , ', ctr[ i]),
        pan[ shift_headerl[ i]])
  }
  
  # Top-level appendices now have superfluous letter at the start:
  # eg [ Str "A" , Space , Str "Appendix"
  pan[ shift_headerl] <- sub( '[[] Str "[A-Z]" , Space , Str "Appendix"', '[ Str "Appendix"', pan[ shift_headerl])
    
  writeLines( pan, panfile)
invisible( pan)
}


"match_after" <-
function( x, table, start=rep(1,length( x)), nomatch=NA){
  if( (lx <- length( x)) < (ls <- length( start)))
    x <- rep( x, (ls+lx-1) %/% lx)[ 1:ls]
  else if( lx > ls)
    start <- rep( start, (lx+ls-1) %/% ls)[ 1:ls]

  if( !is.integer( x)) {
    uall <- unique( c( x, table))
    x <- match( x, uall)
    table <- match( table, uall)
  } else
    table <- as.integer( table) # safety check for next

  start <- as.integer( start)
stopifnot( all( start %in.range% c( 1, length( table))))

  ans <- as.integer( start)+0L # force a copy
  do_match_after( x, table, ans) # Cwrapper
  ans[ ans==0] <- nomatch
return( ans)

# match_after( rep( 1, 3), (1:9)%%3, c( 1, 3, 5)) # c(1,4,7)
}


"match_before" <-
function( x, table, start=rep(1,length( x)), nomatch=NA) {
  lt <- length( table)
  lt+1 - match_after( x, rev( table), lt+1-start, nomatch=nomatch)
}


"maybe_move_bib" <-
function( panf, tex){
## Rewrite pandoc-native file 'panf' iff reqd

  if( FALSE){
    # This from manually_xref()
    # I'm concerned that Apx sections might not all start with the word "Appendix"...
    apx_start <- which( tex=='\\appendix')[1] # or NA
    if( is.na( apx_start)){
      apx_start <- length( tex)+1 # all secs before "appendix"
    }
  }

  tapx <- grep( r"--{^\\(section|part)[*][{]Appendix '}--", tex)
  tbib <- which( tex=='\\printbibliography')
  if( length( tbib)>1){
warning( "Only one biblio allowed, sorreeee...")
    tbib <- tbib[ 1]
  }
  
  if( tbib > max( c( 0, tapx))){ # 0 to stop warning
return()
  } # no move required
  
  pan <- readLines( panf)
  papx <- which( endsWith( pan, ' Header')) %such.that% grepl( '^ *[(] "appendix', pan[ .+2]) 
  pbib <- which( endsWith( pan, '( "refs"')) %such.that% endsWith( pan[ .-1], ' Div')[1]-1
  
  nind <- nchar( sub( '[^ ].*', '', pan))
  refend <- match_after( nind[ pbib], nind, pbib+1)-1
  
  refs <- pan[ pbib:refend]
  
  pan <- massrep( pan, list( papx[1], pbib:refend), list( c( refs, pan[ papx[1]]), character()))
writeLines( pan, panf)
}


"partequiv" <-
function( D){
  # D is a matrix of T or F, saying whether i & j are deffo diff or _maybe_ same
  n <- nrow( D)
  class <- rep( 0L, n)

  D[ is.na( D)] <- 0L
  
  
  # Split into deffo diffs (some may not be classified)
  ec <- list()
  l <- list( 1:n)
  while( length( l)){
    top <- l[[1]]
    Dll <- D[top,top,drop=FALSE]
    if( sum( Dll)==0){
      # These are all equiv
      ec <- c( ec, l[1])
      l <- l[-1]
    } else {
      # Split this class
      cDl <- colSums( Dll)
      first <- match( FALSE, cDl==0) # gotta be one
      l <- c( list( top[ which( Dll[,first]==0)], top[ which( Dll[,first] > 0)]), l[-1])
    }
  }
  
  # While unclassified remain: loop over deffo diff classes, each time incorping any 
  # unclassified that don't clash with this class
    
  
  eclass <- rep( 0, n)
  eclass[ unlist( ec)] <- rep( seq_along( ec), times=lengths( ec))

  # Any unclassed ones? Move to first compatible eclass
  for( u in which( eclass==0)){
    cDu <- colSums( D[,u,drop=FALSE])
    cDu[u] <- 99 # yourself doesn't count!
    ok1 <- match( 0L, cDu)
    eclass[ u] <- eclass[ ok1]
  }
  

return( eclass)
}


"perl.style.regex" <-
function( envir=parent.frame()) {
  # All regex funcs should use Perl syntax, unless fixed=TRUE, and assume byte-only
  for( rfun in cq( sub, gsub, grep, grepl, regexpr, gregexpr)) {
    f <- get( rfun, baseenv())
    formals(f)$useBytes <- TRUE
    formals(f)$perl <- quote( !fixed)
    assign( rfun, f, envir)
  }
  
  tm <- `%that.match%` # from mvbutils
  environment( tm) <- envir # so it uses perl-style
  assign( '%that.match%', tm, envir=envir)
}


"prefhack" <-
function( userdir){
stop( 'NYI!')

  prefile <- file.path( userdir, 'preferences')
stopifnot( file.exists( prefile))

  pref <- readLines( prefile)
  

}


"prepare_eqn_labels" <-
function( 
  slurp, # contents of tex file
  eqalignfix= FALSE
){
## 'texfile' produced from lyx by export->Latex(plain)
## actually as part of lyxzip archive, so in separate folder
r"--{
  This modifies texfile so eqn numbers (and labels, if used) will appear in output, by putting (numbered) eqns into 2-column tables with number in RH column. Multiline "gather" etc eqns have a separate row for each  line. All refs to eqn labels are resolved explicitly. Writes output to a file "<path>/new-<texfile>" unless overwrite=TRUE. If the latter and preserve_original==TRUE, then the original is first copied to "<path>/orig-<texfile>".
  
  pandoc 3.5 does not preserve Latex column width or alignment specs--- all columns seem to get mapped to "AlignDefault, ColWidthDefault". To fix this, you can set 'eqalignfix=TRUE'. Then export (via pandoc) to pandoc-native format, then tweak that to using the eqalignfix() function, then continue processing from native to docx. (Gotta check that this sequence works OK with xrefs, citations, ...).  Or I _could_ write a more official JSON-based pandoc filter instead of tweaking the native format, but that's harder; it would require me to parse JSON, which adds overhead. I can just grep() my way thru the native-format version. When 'eqalignfix=TRUE', the equation number labels have a crazy string prepended to them.
  
  Gotta pay attention to whether eqn env is starred or not. Unstarred ones number all lines by default, unless they end with \nonumber.
  
  Labelling an equation (or just one line of it) in Lyx, makes the environment unstarred. Otherwise, it's starred.
  
  \[..]\ is a synonym for \equation* (unnumbered) so we can ignore thos (and indeed use them inside the fake tables)
  
}--"
  
  # Don't tackle eqns inside existing tables...
  in_table <- function( l){
      start <- which( startsWith( slurp, '\\begin{tabular}'))
      end <- which( startsWith( slurp, '\\end{tabular}'))
      fi1 <- findInterval( l, start)
      fi2 <- findInterval( l, end)
    return( fi1 != fi2)
    }
  
  # Happily, we only need to look for *unstarred* equations. 
  # Main type is "\gather". Displayed-equation is "\equation"
  # Also do "\align" here even tho it won't work right
  # Other types ignored. NB "\[...]\" is unstarred.

  
  fake_count <- 0
  for( eqn_type in cq( gather, eqnarray, align, 
      alignat, flalign, flalignat, equation, align)){
    # I'm going to replace each eqn in turn, which means modifying slurp. 
    # Hence I have to re-look for inside-tables every time
    # This leads to awkward loopery: gotta find first eqn of this type
    # that is NOT inside a table
    
    repeat{ 
      start <- which( startsWith( slurp, sprintf( '\\begin{%s}', eqn_type)))
      end <- which( startsWith( slurp, sprintf( '\\end{%s}', eqn_type)))

      ok <- if( length( start)) which( !in_table( start)) else integer()
      if( !length( ok)){
    break
      }
      i <- ok[ 1]
      
      # Prepare to break into separate lines, and add fake labels if reqd
      eqlines <- slurp[ (start[ i]+1) : (end[ i]-1)]
      
      r"--{
      Double-slash line breaks can also occur *within* <xyz>matrix/array blocks. Those aren't separate lines. They can be nested! So here I for once resort to actually counting the begins & ends, coz there aren't many lines.
      Assumes they are correctly paired, which LyX will ensure unless the user is ERTing their way around
      }--"
      r"--{
      \begin{gather*}
      \left(\begin{array}{c}
      \begin{array}{cc}
      a & b\end{array}\\
      y
      \end{array}\right)=0\\
      \begin{cases}
      wotif & sumthin\end{cases}
      \end{gather*}
      }--"
      
      # find character counts of begin/end arrays etc, effectively amalgamating lines
      n <- length( eqlines)
      matrix_depth_EOL <- rep( 0, n)
      begmat <- gregexpr( 
          '(?<![\\\\])\\\\begin[{](array|.matrix|cases|aligned|alignedat|split|gathered)[}]', 
          eqlines, perl=TRUE)
      if( any( unlist( begmat) > 0)){
        charb4 <- cumsum( c( 0, nchar( eqlines))) # 0 in line 1; nchar(l1) in line 2; etc
        
        nope <- do.on( begmat, .[1]<0)
        begmat[ nope] <- vector( 'list', sum( nope))
        begmat <- mapply( '+', charb4[-n], begmat, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        begmat <- unlist( begmat)

        endmat <- gregexpr( '(?<![\\\\])\\\\end[{](array|.matrix)[}]', eqlines, perl=TRUE)
        nope <- do.on( endmat, .[1]<0)
        endmat[ nope] <- vector( 'list', sum( nope))
        endmat <- mapply( '+', charb4[-n], endmat, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        endmat <- unlist( endmat)
  
        begendcount <- rep( 0, charb4[n+1])    
        begendcount[ begmat] <- +1
        begendcount[ endmat] <- -1
        begendcount <- cumsum( begendcount)
        
        matrix_depth_EOL <- begendcount[ charb4[ -1]]
      }      
      
      lbreaks <- unique( c( which( endsWith( eqlines, '\\\\') & (matrix_depth_EOL==0)), length( eqlines))) # must be at least 1
      eqlines[ lbreaks] <- sub( r"--{ *\\\\ *$}--", '', eqlines[ lbreaks])
      nonum <- grepl( r"--{\\nonumber *$}--", eqlines[ lbreaks])
      if( all( nonum)){ # I don't think LyX will let this happen, but...
    next
      }

      has_label <- grepl( r"--{\\label[{][^}]+[}] *$}--", eqlines[ lbreaks])
      # Lines that aren't nonum and don't have a label, need a fake label
      make_fake <- !nonum & !has_label
      for( ifake in which( make_fake)){
        fake_count <- fake_count + 1
        eqlines[ lbreaks[ ifake]] <- eqlines[ lbreaks[ ifake]] %&% 
            sprintf( '\\label{eq:FAKEWEEBLE%i}', ifake)
      } # for lines needing fake label
      
      # Lyx-inserted mpersands for alignment now won't work, and
      # will make pandoc barf (fair enuf, they are invalid
      # Bona fide Ampersands get preceded by backslash
      eqlines <- gsub( ' & ', ' ', eqlines, fixed=TRUE)
            
      # Make a table...
      # I would LIKE to have the RH column narrow and the LH column very wide
      # and ideally to right-justify the RH column, and centre-justify the LH column.
      # But these Latex settings do not seem to be honoured by Pandoc 
      # :(
      #table <- '\\begin{tabular}{cr}'
      # table <- r"--{\begin{tabular}{>{\centering}m{0.9\textwidth}>{\raggedright\arraybackslash}}m{0.1\textwidth}}}--"
      # Never mind, the pandoc gets fixed later by eqalignfix()
      
      table <- r"--{\begin{tabular}{p{0.9\textwidth}p{0.1\textwidth}}}--"
      
      prevend <- 0
      for( iline in seq_along( lbreaks)){
        last_line <- eqlines[ lbreaks[ iline]]
        if( nonum[ iline]){
          label <- ''
          last_line <- sub( r"--{ *\\nonumber *}--", '', last_line)
        } else {
          label <- sub( r"--{.*(\\label[{][^}]*[}]).*}--", '\\1', last_line)
          last_line <- sub( r"--{ *\\label[{][^}]*[}] *}--", '', last_line)
        }

        table <- c( table, 
            r"--{\noindent\begin{minipage}[t]{1\columnwidth}%}--",
            r"--{\[}--",
            eqlines[ (prevend+1) %upto% (lbreaks[ iline]-1)],
            last_line,
            r"--{\]}--",
            '%',
            sprintf( r"--{\end{minipage} & %s \tabularnewline}--", label)
          )
        prevend <- lbreaks[ iline]
      } # for each line
      table <- c( table, '\\end{tabular}', '')

      # Replace original lines
      slurp <- c( slurp[ 1:(start[i]-1)], table, slurp[ -(1:end[i])])
    } # for eqns of this type
  } # for types of eqn

  # Manually resolve all eqn label-numbers and their xrefs
  eqlabprefix <- if( eqalignfix) '#/fixLABELalignEQ/###@' else '' 
  eqlabel_lines <- grep( '\\label{eq:', slurp, fixed=TRUE)
  if( length( eqlabel_lines)){ # avoid corner case...
    eqlabels <- sub( '.*[{](eq:[^}]*)[}].*', '\\1', slurp[ eqlabel_lines])

    # After Appendices, there's deffo a secprefix
    apxes <- grep( '^[\\](section|part)[*][{]Appendix ', slurp)
    
    # Pre-Apxes: turn on sectionized numbering (2.4) etc only after numberwithin{...}, if any
    secprefix <- rep( '', length( eqlabels))
    section_numbered_equations <- match( '\\numberwithin{equation}{section}', slurp, 0)
    if( section_numbered_equations>0){
      numsecs <- which( startsWith( slurp, '\\section{')) %except% apxes
      numinsec <- eqlabel_lines > section_numbered_equations
      secprefix[ numinsec] <- findInterval( eqlabel_lines[ numinsec], numsecs) %&% '.' # wrong for apxes, fixed next
    }
    fi <- findInterval( eqlabel_lines, apxes)
    secprefix[ fi>0] <- LETTERS[ fi[ fi>0]] # %&% '.' # better sans dot
      
    prev_secprefix <- c( 'NOB', head( secprefix, -1)) # so first will be a change
    
    for( i in seq_along( eqlabels)){
      ectr <- if( secprefix[ i] == prev_secprefix[ i]) {ectr+1} else 1
      newlabi <- sprintf( '%s%i', secprefix[ i], ectr) # used to wrap this in parens, but double trouble; now up to user
      slurp[ eqlabel_lines[ i]] <- sub( sprintf( '\\\\label[{]%s[}]', eqlabels[ i]), 
          eqlabprefix %&% newlabi, slurp[ eqlabel_lines[ i]])
      slurp <- slurp |> 
          xgsub( sprintf( '\\\\ref[{]%s[}]', eqlabels[ i]), newlabi) |>
          xgsub( sprintf( '\\\\eqref[{]%s[}]', eqlabels[ i]), sprintf( '(%s)', newlabi))
    }
  }

return( slurp)

#  file.rename( texfile, file.path( dirname( texfile), 
#      'orig-' %&% basename( texfile)))
#  writeLines( slurp, texfile)
  
  # pandoc -s -F pandoc-crossref.exe --resource-path d:/docs/refs/bibtex/bib --citeproc --number-sections -f latex -o $$o -t docx $$i
}


"presortout_cites" <-
function( tex){
  # pandoc skrooz up citations horribly 
  # citet bad, citep & citealp missing commas
  # citealp and citealt particularly egregious
  # Mark them in Latex, so they can be recognized post pandoc
  
  tex <- tex |> 
      xgsub( '(\\\\citep[{])', '**FIXCITEP**\\1') |>
      xgsub( '(\\\\citet[{])', '**FIXCITET**\\1') |>
      xgsub( '(\\\\citealp[{])', '**FIXCITEALP**\\1') |>
      xgsub( '(\\\\citealt[{])', '**FIXCITEALT**\\1')
  
return( tex)
}


"print_debugs" <-
function( whicho, nlocal=sys.parent()) mlocal({
  switch( whicho, {
      scatn( 'Running lyxzip2word()')   
      print( Sys.time())  
      scatn( 'Path is:')
      print( Sys.getenv( 'PATH'))
      scatn( 'lyxec is: %s', lyxec)
      scatn( 'Sys.which() gives: %s', Sys.which( 'LyX'))
      scatn( 'LU %s', Sys.getenv( 'LYX_USERDIR_24x'))
    },{
      scatn( 'Version (full & boiled): %s %s', lyxver_full, lyxver)
      scatn( 'origdir: %s', origdir)
      scatn( 'tempdir: %s', tempdir)
      scatn( 'is it there? %s', file.exists( file.path( tempdir, zipfile)))
    },{
      scatn( 'lyx_userdir_info: %s', lyx_userdir_info)
    }
  )


})


"regmac" <-
function( macname, line) {
  pos <- regexpr( "(?U)(^|[^\\\\])(\\\\\\\\)*\\\\" %&% macname %&% "\001{0,1}" %&% 
      "((?<!\\\\)\\{(\\\\\\{|\\\\\\}|[^\\{\\}]|(?<!\\\\)\\{.*(?<!\\\\)\\})*(?<!\\\\)\\})",
      line, perl=TRUE)
  end.of.arg <- pos + attr( pos, 'match.length') - 1
  macname.start <- regexpr( to.regexpr( macname) %&% '\001{0,1}', substring( line, pos)) - 1
  pos <- c( pos) + macname.start - 1
  
return( c( pos, pos + attr( macname.start, 'match.length') + 1, end.of.arg))
}


"renumber_apx_floats" <-
function(
  tag_type, # 'tab' or 'fig' (or...)
  litemz,   # line of figure/table
  labz,     # label 
  pan       # source
){
## returns list( pan, numz)

  llinx <- grep( sprintf( '^ *[(] +"#%s:', tag_type), pan)
  if( !length( llinx)){
return( list( pan=pan, numz=seq_along( labz)))
  }
  
  refs <- sub( '.*#([^"]+)".*', '\\1', pan[ llinx])
  numz <- match( refs, labz, 0) # DIY order! Don't trust pandoc

  mm <- match( labz, refs, 0)
  litemz <- litemz[ mm>0] # can't fix others
  labz <- labz[ mm>0]
  
  # Check if the Float is in Apx <N>. If so, adjust 
  # the numz (and all the links)
  # Do it NOW, so that numz are copied correctly into the table captions
  # What a massive PITFA. And prolly I will have to do this for Figures too
  
  apx_starts <- grep( '"appendix-[a-z]-', pan) # it will do
  apxcount <- findInterval( litemz, apx_starts)
  for( apxi in unique( apxcount) %except% 0){
    tabinaxpi <- which( apxcount==apxi)
    
    # I try to avoid nested loops in R, but sometimes...
    for( j in seq_along( tabinaxpi)){
      linx2this <- which( refs==labz[ tabinaxpi][j])
      numz[ linx2this] <- sprintf( '%s%i', LETTERS[ apxi], j)
    }
  }
  
  # Change _all_ numbers (potentially) cozza pandoc no wurk rite
  for( i in which( numz>0)){
    pan[ llinx[i]-1] <- sub( 
        '([^"]*")([^"]*)(".*)', 
        sprintf( '\\1%s\\3', numz[ i]), 
        pan[ llinx[i]-1]
      )
  }

  mm[ mm==0] <- NA
return( returnList( pan, numz=numz[ mm]))
}


"requote_lyx" <-
function( filename= NULL,
  lyx= NULL,
  outfile= NULL
){
r"--{ 
  Basically fix stuff inside plain layout or other text thingy. The real aim is to get something which exports cleanly to plain Latex, so that single-quoted things are `things' and double-quoted ones are ``like this''. Direct straight "quotes" don't exist, except as args to functions or inside special things like Listings. LyX uses "quote insets" to handle most quotes, and is generally clever enough to ignore them when exported.
  
  Must avoid doing replacements inside Lyx commands, eg an unindented line like this:
caps "false"
  
  The ones in the preamble start with \, but later they don't. EG a citation looks like this:
  \begin_inset CommandInset ref
  LatexCommand ref
  reference "sec:Technical"
  plural "false"
  caps "false"
  noprefix "false"
  nolink "false"
  \end_inset

  Indentation is important here. AFAIK, the *first* line in each plain-layout bit is not indented, but the rest are. Although there can be exceptions, eg when a bit of text is within \emph on ... \emph default or \begin_inset...\end_inset . However, the unindented lines that follow those (perhaps after some empty lines) contain only punctuation. (But could that punctuation be double-quote?)
  
  So one can look in:

  (i) indented lines  
  (ii) unindented lines that are preceded by begin_layout
  (iii) unind starting with double-quote (?but is this opeingin or closing?)
  
  AVOID anything inside "listings" (eg flip the case on those lines) or ERT, or equations. Ver

  Other than inside exceptions, bona fide double quotes should be replaced by dynamic quotes, ie Quotes xld...Quotes xrd. Before that, all existing LyX quotes, such as \begin_inset Quotes grd<newline>\end_inset, should first be replaced by double-quotes (or single ones), to avoid woe. Challenge is to do that while not mucking up indentation...
>>  
A word with non-dynamic but normal Lyx 
\begin_inset Quotes eld
\end_inset

quotes
\begin_inset Quotes erd
\end_inset

 around it...
<<

or

>>
\begin_layout Standard
\begin_inset Quotes xld
\end_inset

Starting
\begin_inset Quotes xrd
\end_inset

 a new layout with a quoted word...
\end_layout

\begin_layout Standard
and at the 
\begin_inset Quotes xld
\end_inset

end
\begin_inset Quotes xrd
\end_inset


\end_layout
<<
    
  
}--"

  if( !missing( filename)){
    lyx <- readLines( filename)
  }

  # Single quotes are generally too hard, cozza legit apostrophes; but at least we can make any explicit single-quotes into dynamic ones.
  lyx <- sub( '^(\\\\begin_inset Quotes ).(.)s$', '\\1x\\2s', lyx)

  # First, _remove_ all existing Lyx-style double quotes
  # NB earlier version also zapped Lyx-style single quotes, but 
  # not now; hence code looks a bit odd
  qlyx1 <- grepl( '^\\\\begin_inset Quotes ..d$', lyx)
  qlyx2 <- lyx == '\\end_inset'
  qlyx1 <- which( c( head( qlyx1, -1) & qlyx2[ -1], FALSE))
  qch <- substring( lyx[ qlyx1], nchar( lyx[ qlyx1]), nchar( lyx[ qlyx1])+1)
  weird <- which( (qch != 'd') & (qch != 's'))
  if( length( weird)){
stop( 'Weird native-Lyx quote on line(s) ' %&% paste( qlyx1[ weird], collapse=','))
  }
  qch <- ifelse( qch=='d', '"', "'")
  
  if( length( qlyx1)){
    r"--{
      Tricky! If we replace the entire block of 3 lines just with a quote-mark at the end of the _preceding_ line, then it will mess up a Lyx command (eg \begin_layout Standard). If we put the quote at the start of the _following_ line, then it can muck up indenting etc. And I don't think we can trust whether a right-quote is _really_ at the end; it's possible to trick Lyx into _starting_ a quote with a dynamic right-quote, by inserting and then deleting...
      
      Check if preceding line was indeed Lyx command, and if not then tweak that one. else tweak follower.
      No: always tweak follower!
    }--"
  
    comstart <- startsWith( lyx[ qlyx1-1], '\\')
    comstart[] <- TRUE
    lyx[ qlyx1[ !comstart]-1] <- lyx[ qlyx1[ !comstart]-1] %&% qch[ !comstart]
    lyx[ qlyx1[ comstart] + 3] <- qch[ comstart] %&% lyx[ qlyx1[ comstart] + 3]
    
    lyx <- lyx[ -sort( c( qlyx1, qlyx1+1, qlyx1+2))]
  }

  # Occasionally ” (and possibly its LH opening counterpart) appears
  # as direct character. Also true for single-quote version. Turn 
  # them ALL into plain double-quotes
  lyx <- gsub( '[”“‘’]', '"', lyx)

  nl <- length( lyx)
  
  # Ignore stuff inside equation-esque pure Latexy \begin{x}...\end{x} pairs
  
  # AFAICS Lyx constructs all start begin_<blah>, except branch and index
  # At least within my one (long) test document
  is_begin <- grepl( '^\\\\(begin|branch |index )[^{]', lyx)
  begins <- which( is_begin)
  is_end <- grepl( '^\\\\end[^{]', lyx)
  ends <- which( is_end)
  
  # end_types <- unique( lyx[ ends]) # FYI
  
  cumbeg <- cumsum( is_begin)
  cumend <- cumsum( is_end)
  
  becount <- cumbeg - cumend
  
stopifnot(
    all( becount >= 0),
    tail( becount, 1) == 0
  )
  
  start_listings <- which( (lyx == '\\begin_inset listings') | 
      (lyx == '\\begin_inset ERT'))

  if( length( start_listings)){
    # Find first end after listing-start with same becount (as the line before)
    end_listings <- match_after( 
        becount[ start_listings]-1, becount, start_listings)

stopifnot( all( lyx[ end_listings] == '\\end_inset')) # nec but not suff

    ignori <- unlist( mapply( ':', start_listings, end_listings))
  } else {
    ignori <- integer()
  }
  
  check <- rep( TRUE, nl)
  check[ ignori] <- FALSE

  # Double quotes! At last!
  
  # Dynamic quote sequence *for use in sub* is:
  lq <- r"--{
\\begin_inset Quotes xld
\\end_inset

}--"
  rq <- sub( 'l', 'r', lq, fixed=TRUE)
  
  # Fake repeat{} to allow break-to-EOF
  repeat{ 
    iqd <- grep( '"', lyx, fixed=TRUE)
  if( !length( iqd)) break
    iqd <- iqd[ check[ iqd]]
    iqd <- iqd[ substring( lyx[ iqd], 1, 1) != '\\']
  if( !length( iqd)) break  
    
    # Text, or Lyx argument etc?
    prevbeg <- match_before( becount[ iqd]-1, becount, iqd)+1
    is_layout <- startsWith( lyx[ prevbeg], '\\begin_layout ')
    iqd <- iqd[ is_layout]
  if( !length( iqd)) break
  
    # lqd <- lyx[ iqd]

    # Check direction of each double-quote:
    # Left or right? (Open or close?)
    
    # Look at character immediately before double-quote, or immed after
    # Only tricky if double-quote is by itself on line!
    # Could be closer, if next line is \end_layout
    # or opener, if next is \begin_layout
    # I don't _think_ there will be anything except \begin or \end
    # but let's assume that ONLY \end means close

    nchqd <- nchar( lyx[ iqd])
    if( any( nchqd==1)){
      nonemps <- which( nzchar( lyx))
      next_nonemps <- nonemps[ findInterval( iqd[ nchqd==1], nonemps)+1]

      rq1 <- gsub( r"((?m)[\\](?=\\))", '', rq, perl=TRUE) |> substring( 2)
      lq1 <- sub( 'r', 'l', rq1)
      is_closers <- substring( lyx[ next_nonemps], 1, 5) == '\\end_'
      lyx[ iqd[ nchqd==1]] <- ifelse( is_closers, rq1, lq1)
    }

    # Lines with more than just a double-quote:
    # inspect the char *before* dubquote, if there is one
    # or 2nd char, if dubquote is first char
    
    preopeners <- unlist( strsplit( ' +-/*', '')) # non-nulls before opener
    preclosers <- unlist( strsplit( ' .?,)-:;', '')) # non-nulls after closer
    
    for( i in iqd[ nchqd>1]){
      # Allow for multiple quotes in the same line
      line <- lyx[ i]
      repeat{
        reg1 <- regexpr( '"', line, fixed=TRUE)
      if( reg1<0) break # on to next line

        if( reg1>1){ # check char before
          is_closer <- substring( line, reg1-1, reg1-1) %not.in% preopeners
        } else { # check char after
          is_closer <- substring( line, 2, 2) %in% preclosers 
        }
        
        line <- sub( '"', if( is_closer) rq else lq, line)
      }
      lyx[ i] <- line
    } # for iqd
  break
  } # fake repeat

  if( !is.null( outfile)){
    writeLines( lyx, outfile)
  }
  
return( invisible( lyx))
}


"rescale_eps" <-
function( tex){
## pandoc does not honour [scale=x] in includegraphics
  r"--{
  This was my first attempt, manually changing BoundingBox settings in EPS. But, the EPS files aren't used by pandoc anyway (or not if PNG is available, and presumably that's true for JPG too).
  
  So, this function seems to be obsolete, but I'm keeping it in case the code is useful in future.
  
  Instead, use rescale_images()
  }--"
  
  r"--{
  Can have multiple includegraphics in a single line...

    \includegraphics[scale=0.2]{nice-gs-Rplot037} & \includegraphics[scale=0.2]{nice-gs-Rplot039}\tabularnewline
  
  Use wonderful utils::strcapture & a monster regex to glub this
  }--"
  
  extract.named( strcapture( 
      '[\\]includegraphics[[]scale=([0-9.]+)\\][{]([^}]*)',
      scimages, 
      data.frame( scale=numeric(), file=character())
    )) # scale & file
  
  eps_file <- sub( '([^.])$', '\\1.eps',  file)
  for( i in which( file.exists( eps_file))){
    eps <- readLines( eps_file[ i])
    bbl <- grep( '^%%[A-Za-z]*BoundingBox:', eps)
    bb <- sub( '.*: *', '', eps[ bbl])
    is_int <- !grepl( '.', bb, fixed=TRUE)
    is_neg0 <- grepl( '-0(?![.])', bb, perl=TRUE) # just in case "-0" vs "0" matters in EPS    

    nums <- strsplit( bb, ' +') |> lapply( as.numeric) # NB it's a list
    nums[ !is_int] <- FOR( nums[ !is_int], paste( sprintf( '%8.3f', . * scale[ i]), collapse= ' '))
    nums[ is_int] <- FOR( nums[ is_int], paste( sprintf( '%6i', as.integer( . * scale[ i])), collapse= ' '))
    nums[ is_neg0] <- gsub( ' 0(?![.])', ' -0', nums[ is_neg0], perl=TRUE)
    for( j in seq_along( bbl)){
      eps[ bbl[ j]] <- sub( ':.*', ': ' %&% nums[[ j]], eps[ bbl[ j]])
    }
    writeLines( eps, eps_file[ i])
  }
  
invisible( tex)
}


"rescale_images" <-
function( tex, magick){
r"--{
  pandoc does not honour [scale=x] in includegraphics
  Must ignore the EPS produced by LyX->Latex, in favour of the original file
  (if there is a non-EPS one) coz the latter is what pandoc will use/find.
  
  IDNK how Latex treats scale. However, things seem to work (at least for PNG) if I overwrite the original image with a new one where density and units have been set explicitly--- instead of 72DPI, it's (72*scale)DPI.
  
  Thx2 brilliance of ImageMagick!
  
  Fingers Xed!
}--"
  
  lscim <- grep( '\\includegraphics[scale=', tex, fixed=TRUE)
  scimages <- tex[ lscim]
  # ... but not if it's in a comment (unlikely?)
  scimages <- sub( '%%.*', '', scimages)
      
  r"--{
  Can have multiple includegraphics in a single line...

    \includegraphics[scale=0.2]{nice-gs-Rplot037} & \includegraphics[scale=0.2]{nice-gs-Rplot039}\tabularnewline
  
  Use wonderful utils::strcapture & a monster regex to glub this
  }--"
  
  extract.named( strcapture( 
      '[\\]includegraphics[[]scale=([0-9.]+)\\][{]([^}]*)',
      scimages, 
      data.frame( scales=numeric(), files=character())
    )) # scale & file
  
  # There should *always* be EPS file cozza LyX->Latex
  # But, preference any other (ie the original)
  
  for( i in seq_along( files)){
    file <- dir( dirname( files[ i]), pattern=basename( files[ i]), 
        full.names=TRUE)
    if( length( file)>1){
      file <- (file %that.dont.match% '[.]eps$')[1]
    }

    if( FALSE){ # but this could be useful, so I'm keeping it
      size <- suppressWarnings( # cozza incomplete final line
          system2( magick, 
            'identify -format "%[fx:w/72] by %[fx:h/72] inches" ' %&%
            file,
            stdout=TRUE
          ))
    }
    check <- system2( magick,
        sprintf( '%s -density %i -units PixelsPerInch %s',
        file, round( 72 * scales[ i]), file))
    if( check != 0){
      warning( 'Problem scaling "%s" for pandoc', file)
    }
  }
  
invisible( tex)
}


"singlify" <-
function( fullpath) {
  dir <- dirname( fullpath)
  lines <- readLines( fullpath)
  plines <- paste( lines, collapse='\n')
  
  # captured() and perl.style.regex() used to be in ADR package
  # Now copied here
  # captured <- ADR:::captured
  perl.style.regex() # avoid perl=T throughout
  

##################
  # \include, \input, \externaldocument
  # Assume these occur at start of line (after whitespace)
  # Don't try all-at-once; plodding way copes with nested
  repeat{
    # Skip commented-out versions
    # Lookbehind assertion so any substitution starts in the right place
    next_inc <- regexpr( '(?s)(?<=^|\\n)\\s*\\\\(?<inctype>include|input|externaldocument)' %&%
        '\\s*[{](?<incfile>[^}]+)[}]', plines)
    if( next_inc <= 0)
  break
   
    caps <- next_inc@capture.start[1,]
    lens <- next_inc@capture.length[1,]
    ends <- caps + lens - 1
   
    inctype <- substring( plines, caps[ 'inctype'], ends[ 'inctype'])
    is_extdoc <- inctype == 'externaldocument'
    is_include <-  inctype == 'include'
   
    # input seems to want dot-tex suffix; include doesn't :/
    ifile <- file_path_sans_ext( substring( plines, caps[ 'incfile'], ends[ 'incfile']))
    inclines <- readLines( file.path( dir, ifile %&% '.tex'))

    if( is_extdoc) {
      # make dot-aux file
      # this should be more careful with tempfiles but fucken latex is to blame
      check <- system2( 'latex', stdout=NULL, c( 
          sprintf( '-aux-directory=%s', dir), 
          file.path( dir, ifile %&% '.tex')))
      if( check != 0) {
stop( sprintf( "latex failed on %s; try dot-log file in same folder for details", file.path( dir, ifile %&% '.tex')))
      }
      file.copy( ifile %&% '.aux', file.path( dir, ifile %&% '.aux'), overwrite=TRUE)
      unlink( ifile %&% c( '.aux', '.dvi', '.log'))
      
      inclines <- readLines( file.path( dir, ifile %&% '.aux'))
      
      inclines <- c( 
          sprintf( '\\begin{filecontents}{temp-%s.aux}', basename( ifile)), 
          inclines, 
          '\\end{filecontents}',
          sprintf( '\\EXTERNALBL00DYDOCUMENT{temp-%s}', ifile)
          )
    } else if( is_include) {
      inclines <- c( '\\newpage', inclines, '\\pagebreak') 
    }
    
    plines <- substring( plines, 1, next_inc-1) %&% 
        paste( inclines, collapse='\n') %&%
        substring( plines, next_inc + next_inc@match.length)
    
  } # repeat
  
  plines <- gsub( 'EXTERNALBL00DYDOCUMENT', 'externaldocument', plines, fixed=TRUE)
  

############################
  # References
  # For some reason [\\] is not working, hence four backslashes
  cites <- captured( plines, '(?s)\\\\cite\\w*\\s*[{](?<cites>[^}]+)[}]')$cites
  cites <- paste( cites, collapse=',')
  cites <- gsub( '\\s+', ',', cites) 
  cites <- unique( strsplit( cites, ',')[[1]] %except% '')

  plines <- sub( '(?s)(\\\\addbibresource\\s*[{])', 
      '\\\\usepackage{filecontents}\n\\1', plines)
  # Find bib resources--- assume command is first on line--- skip if commented out (or otherwise not first)
  biblio <- captured( plines, '(?s)(?<=^|\\n)\\s*\\\\addbibresource\\s*[{](?<biblio>[^}]+)[}]')
  biblios <- biblio$biblio
  
  for( abib in biblios) {
    bibentries <- readLines( file.path( dir, abib))
    bibentries <- paste( bibentries, collapse='\n')
    bibentries <- strsplit( bibentries, '[}]\\s*@')[[1]]

    # Fix first and last, where split-sequence is not removed
    bibentries[ -1] <- '@' %&% bibentries[ -1]
    bibentries[ -length( bibentries)] <- bibentries[ -length( bibentries)] %&% '}'

    shortnames <- sub( '(?s)@[^{]*[{]\\s*([^ ,]+)[ ,].*', '\\1', bibentries)
    names( bibentries) <- shortnames
    cited <- shortnames %in% cites
    if( !any( cited)) { # drop this  file--- empty bib files cause woe
      plines <- sub( '(?s)(?<=^|\\n)\\s*(\\\\addbibresource)\\s*[{]\\s*' %&% 
          sub( '.', '[.]', abib, fixed=TRUE) %&% '\\s*[}]', '\n', plines)
    } else {
      bibentries <- bibentries[ cited]
      
      # cat( bibentries, sep='\n', file=file.path( dirname, 'temp-' %&% abib)) 

      # Mod the \addbibresource ref to include file beforehand, and use temp name
      fixbib <- sub( '.', '[.]', abib, fixed=TRUE)
      reg <- '(?s)(?<=^|\\n)\\s*(\\\\addbibresource)\\s*[{]\\s*' %&% fixbib %&% '\\s*[}]' 
      preg <- regexpr( reg, plines)
      plines <- paste( c( 
          substring( plines, 1, preg-1),
          '\\begin{filecontents*}{temp-' %&% abib %&% '}',
          paste( bibentries, collapse='\n'),
          '\\end{filecontents*}',
          '',         
          '\\addbibresource{temp-' %&% abib %&% '}',
          '',
          substring( plines, preg+preg@match.length)),
          collapse='\n') 
    } # if cited
  } # for abib

##############################
  # Single left quotes--- after space or parenthesis or BoL
  # Could do with character class, but fixed=TRUE  is clear...
  plines <- gsub( " '", " \`", plines, fixed=TRUE)
  plines <- gsub( "('", "(\`", plines, fixed=TRUE)
  plines <- gsub( "\n'", "\n\`", plines, fixed=TRUE)
  
  cat( plines, sep='\n', file=file.path( dir, 'mod-' %&% basename( fullpath)))
}


"tablefix" <-
function( panfile){
r"--{
  This is what pandoc emits (without native-numbering, which causes other WORSE problems) :/. It does contain the table label, but the caption line should be "Table N. Glossary" not just "Glossary"

  , Div
      ( "tab:Glossary" , [] , [] )
      [ Table
          ( "" , [] , [] )
          (Caption Nothing [ Plain [ Str "Glossary" ] ])
          [ ( AlignCenter , ColWidthDefault ) ]
          (TableHead ( "" , [] , [] ) [])
          [ TableBody ( "" , [] , [] ) (RowHeadColumns 0) [] [] ]

   or (longer caption)
  
  , Div
        ( "tab:ssx-overest" , [] , [] )
        [ Table
            ( "" , [] , [] )
            (Caption
               Nothing
               [ Plain
                   [ Str "Cases"
                   , Space
                   , Str "in"


   Elsewhere in the file, we can find a link to the Table, with the number correctly inserted. So, assuming all tables do get a mention in the text (so that a link exists), we can just look for the link. That appears to be the only place that a number is found.
   
   What a massive PITA though.
   
      , Link
          ( ""
          , []
          , [ ( "reference-type" , "ref" )
            , ( "reference" , "tab:Glossary" )
            ]
          )
          [ Str "1" ]
          ( "#tab:Glossary" , "" )

          
}--"
  
  pan <- readLines( panfile)
  
  tablez <- grep( '^ *[[] +Table', pan)
  tablez <- tablez %such.that% (
      endsWith( pan[ .-2], ', Div') &
      grepl( '(Caption', pan[.+2], fixed=TRUE)
    )
  tablabz <- sub( '^[^"]*"([^"]+)".*', '\\1', pan[ tablez-1])
  
  # For inscrutable reasons, pandoc sometimes repeats a label...
  # ... eg with that 2-part table of g0 & ESW
  dupz <- duplicated( tablabz)
  # Duplicated labels imply a repeated Caption :( 
  # which should be replaced by a Space (if the Caption is entirely removed,
  # the tables get run together)
  if( any( dupz)){
    r"--{
      ( "tab:g0" , [] , [] )
      [ Table
          ( "" , [] , [] )
          (Caption
             Nothing
             [ Plain
                 [ Str "Repeat"
                 ]
             ])

  
    }--"
    
    capstarts <- grep( '^ *[(]Caption', pan)
    sqstarts <- grep( '^ *[[]', pan)
    
    dupcapz <- capstarts[ findInterval( tablez[ dupz], capstarts)+1]
    # Find eg "[ Plain" above (might not always be Plain I guess)
    dccontz <- sqstarts[ findInterval( dupcapz, sqstarts)+1]
    
    # Find end of dup cop contents, via indents
    nind <- nchar( sub( '[^ ].*', '', pan))
    end_dcc <- match_after( nind[ dccontz], nind, dccontz+1)
    
    # Get ready to replace everything between 
    # each "[ Plain" and its closinger, by an indented  "Space"
    lrepz <- mapply( ':', dccontz+1,  end_dcc-1, 
        SIMPLIFY=FALSE, USE.NAMES=FALSE)
    newlz <- FOR( dccontz, 
        strrep( ' ', nind[ .]+4) %&% '[ Space ]'
      )
    # ready for massrep later
  }
  
  # Only deal with the *first* occurrence of the label
  # (others, ie duplicates, are extra tables inside the same Table-float) 
  tablabz <- tablabz[ !dupz]
  tablez <- tablez[ !dupz]
  
  extract.named( renumber_apx_floats( 
    'tab', tablez, tablabz, pan)) # creates pan & numz & mm
  
  r"--{
  Now insert "Table N." before start of actual caption. Might or might not be a one-liner.
  }--"
  
  # NB *four* backslashes required to insert *one*...
  dquotel <- grep( '"', pan, fixed=TRUE)
  capl <- dquotel[ findInterval( tablez+1.5, dquotel)+1] 
  # ... line with first word of caption
  # Caption can either start with Str, or with Math inline

  for( i in which( !is.na( numz))){ # ignore unlabelled ones  (??)
    pan[ capl[ i]] <- sub( '(Math|Str) ', sprintf( 
        'Str "Table", Space, Str "%s.", Space, \\1 ', 
        numz[ i]), pan[ capl[ i]])
  }
  
  # Those duplicated captions...
  if( any( dupz)){
    pan <- massrep( pan, atlist=lrepz, replist=newlz)
  }
  
  writeLines( pan, panfile)
invisible( pan)
}


"tex2utf8" <-
function( tex, file=NULL, outfile=NULL, debrace=FALSE){
## tex is eg the lines in a dot-bib file

# This table comes from https://arxiv.org/edit-user/tex-accents.php

  if( !is.null( file)){
    tex <- readLines( file)
  }
  
  converto <- r"--{
      Ä   \"A ä   \"a Á   \'A á   \'a Ȧ   \.A ȧ   \.a Ā   \=A
      ā   \=a Â   \^A â   \^a À   \`A à   \`a Ą   \k{A} ą   \k{a}
      Å   \r{A} å   \r{a} Ă   \u{A} ă   \u{a} Ǎ   \v{A} ǎ   \v{a} Ã   \~A
      ã   \~a Ć   \'C ć   \'c Ċ   \.C ċ   \.c Ĉ   \^C ĉ   \^c
      Ç   \c{C} ç   \c{c} Č   \v{C} č   \v{c} Ď   \v{D} ď   \v{d} Ë   \"E
      ë   \"e É   \'E é   \'e Ė   \.E ė   \.e Ē   \=E ē   \=e
      Ê   \^E ê   \^e È   \`E è   \`e Ȩ   \c{E} ȩ   \c{e} Ę   \k{E}
      ę   \k{e} Ĕ   \u{E} ĕ   \u{e} Ě   \v{E} ě   \v{e} Ġ   \.G ġ   \.g
      Ĝ   \^G ĝ   \^g Ģ   \c{G} ģ   \c{g} Ğ   \u{G} ğ   \u{g} Ǧ   \v{G}
      ǧ   \v{g} Ĥ   \^H ĥ   \^h Ȟ   \v{H} ȟ   \v{h} Ï   \"I ï   \"i
      Í   \'I í   \'i İ   \.I Ī   \=I ī   \=i Î   \^I î   \^i
      Ì   \`I ì   \`i Į   \k{I} į   \k{i} Ĭ   \u{I} ĭ   \u{i} Ǐ   \v{I}
      ǐ   \v{i} Ĩ   \~I ĩ   \~i Ĵ   \^J ĵ   \^j Ķ   \c{K} ķ   \c{k}
      Ǩ   \v{K} ǩ   \v{k} Ĺ   \'L ĺ   \'l Ļ   \c{L} ļ   \c{l} Ľ   \v{L}
      ľ   \v{l} Ń   \'N ń   \'n Ņ   \c{N} ņ   \c{n} Ň   \v{N} ň   \v{n}
      Ñ   \~N ñ   \~n Ö   \"O ö   \"o Ó   \'O ó   \'o Ȯ   \.O
      ȯ   \.o Ō   \=O ō   \=o Ô   \^O ô   \^o Ò   \`O ò   \`o
      Ő   \H{O} ő   \H{o} Ǫ   \k{O} ǫ   \k{o} Ŏ   \u{O} ŏ   \u{o} Ǒ   \v{O}
      ǒ   \v{o} Õ   \~O õ   \~o Ŕ   \'R ŕ   \'r Ŗ   \c{R} ŗ   \c{r}
      Ř   \v{R} ř   \v{r} Ś   \'S ś   \'s Ŝ   \^S ŝ   \^s Ş   \c{S}
      ş   \c{s} Š   \v{S} š   \v{s} Ţ   \c{T} ţ   \c{t} Ť   \v{T} ť   \v{t}
      Ü   \"U ü   \"u Ú   \'U ú   \'u Ū   \=U ū   \=u Û   \^U
      û   \^u Ù   \`U ù   \`u Ű   \H{U} ű   \H{u} Ų   \k{U} ų   \k{u}
      Ů   \r{U} ů   \r{u} Ŭ   \u{U} ŭ   \u{u} Ǔ   \v{U} ǔ   \v{u} Ũ   \~U
      ũ   \~u Ŵ   \^W ŵ   \^w Ÿ   \"Y ÿ   \"y Ý   \'Y ý   \'y
      Ȳ   \=Y ȳ   \=y Ŷ   \^Y ŷ   \^y Ź   \'Z ź   \'z Ż   \.Z
      ż   \.z Ž   \v{Z} ž   \v{z}
      å   {\aa} Å   {\AA} æ   {\ae} Æ   {\AE} Ð   {\DH} ð   {\dh} đ   {\dj}
      Đ   {\DJ} ð   {\eth}  Ð   {\ETH}  ı   {\i}  ł   {\l}  Ł   {\L}  ŋ   {\ng}
      Ŋ   {\NG} Ø   {\O}  ø   {\o}  œ   {\oe} Œ   {\OE} ß   {\ss} þ   {\th}
      Þ   {\TH}
    }--"
    
  charz <- converto |> trimws() |> strsplit( '[ \n]+') |> unlist() |> matrix( ncol=2, byrow=TRUE)

  # Generic accentors that accept any Latin character in {}
  # If these 
  # From https://tex.stackexchange.com/tags/accents/info
  accentors <- r"--{
      \` (grave accent): à
      \' (acute accent): á
      \^ (circumflex or “hat”): â
      \" (umlaut or dieresis): ä
      \~ (tilde or “squiggle”): ã
      \= (macron or “bar”): ā
      \. (dot accent): ȧ
      \u (breve accent): ă
      \v (háček or “check”): ǎ
      \H (long Hungarian umlaut): ő
      \t (tie-after accent): a͡
      \c (cedilla): ş
      \d (dot-under accent): ạ
      \b (bar-under accent): ο̩
      \k (ogonek): ą
    }--"
  accentors <- accentors |> trimws() |> strsplit( '\n') |> unlist() |> trimws() |> xsub( ' .*', '')
  accentors <- accentors |> trimws() |> strsplit( '\n') |> unlist() |>  trimws() |> xsub( ' .*', '')
  all_accent_markers <- unique( substring( accentors, 2, 2)) # should be unique anyway
  all_accent_markers <- c( all_accent_markers %except% '^', '^') # gotta come last in grep
  all_nonch <- all_accent_markers %except% c( letters, LETTERS)

  all_accents <- c( outer( accentors, c( letters, LETTERS), function( x, y) sprintf( '%s{%s}', x, y)))
  which_utf8 <- match( sub( '[{](.)[}]', '\\1', all_accents), charz[,2], 0)
  
  # For those that aren't in the main table above, IDNK the UTF8 equiv, so don't include them
  # Otherwise, eg \~{n} is an alias for \~n that Latex knows about
  all_known_accents <- all_accents[ which_utf8>0]
  which_utf8 <- which_utf8[ which_utf8>0]
  
  accent_alias_charz <- cbind( charz[ which_utf8], all_known_accents)
  charz <- rbind( charz, accent_alias_charz)
  
  utf8 <- charz[,1]
  latex <- charz[,2]
  
  # Try to speed things up. Most lines don't have such things
  slash <- grep( '\\', tex, fixed=TRUE)
  poss <- slash[ grep( sprintf( '[\\][%s]', paste( unique( substring( latex, 2, 2)), collapse='')), 
      tex[ slash])]
  
  texposs <- as.cat( tex[ poss]) # for ease of display while debugging...
  # Fix illegal stuff like {\'}e...
  texposs <- gsub( sprintf( '[{]([\\][%s])[}]([a-zA-Z])', paste( all_accent_markers, collapse='')), 
      '\\1{\\2}', texposs)
      
  # and special-case the REMARKABLY TEDIOUS \'\i for \'ı etc. This is valid (?) for all over-the-char accents
  # which I think includes all the non-char ones. Below-the-char, one should distinguish between i and ı ...
  texposs <- gsub( sprintf( '[\\]([%s])[\\]([ijIJ])', paste( all_nonch, collapse='')), r"{\\\1\2}", texposs)
  
  for( i in seq_along( utf8)){
    texposs <- gsub( latex[ i], utf8[ i], texposs, fixed=TRUE)
  }

  if( debrace){
    # Remove superfluous braces around single (UTF8) letters. I think braces no longer matter, except at start of word?
    # and they can confuse tidy_initials(). Superfluous braces around pure-Latin characters are probably OK, coz they
    # are unlikely to be in author names. 
    texposs <- gsub( sprintf( '(?<=[^. -])[{]([%s])[}]', paste( unique( utf8), collapse='')), '\\1', texposs, perl=TRUE)
  }
  
  tex[ poss] <- texposs
  
  if( !is.null( outfile)){
    writeLines( tex, outfile)
  }
return( tex)
}


"tidy_initials" <-
function( bib, file=NULL, outfile=NULL, gungho=TRUE){
## What this really needs, is a converter from Latex specials direct to UTF8
## Otherwise, the upper-lower case spotting won't work perfectly
  if( !is.null( file)){
    bib <- readLines( file)
  }

  authl <- grep( '^ *[Aa]uthor *=', bib)
  
  # Assume that authors with the same surname and first initial are the same
  # unless they have different first NAMES or different 2nd-plus initials
  # (missing does not count)
  
  # Strip cladding
  auths <- bib[ authl] |>
      trimws() |> 
      xsub( '^[^"{]*["{]', '') |>
      xsub( '["}] *(,?) *$', '')

  auths <- strsplit( auths, ' +and +')
  nauthper <- lengths( auths) # per line
  auths <- trimws( unlist( auths))
  surnames <- trimws( xsub( auths, ',.*', '')) # always exists
  givens <- trimws( xsub( auths, '.*,', '')) # might not...
  
  weirdy <- startsWith( surnames, '{') & endsWith( surnames, '}') # {IWC}
  givens[ weirdy] <- ''

  # J. Paul Getty III...
  roman_numerals <- as.roman( 1:20) # gotta stop somewhere...
  surnames[ !weirdy] <- sub( sprintf( ' +(%s)$', 
      paste( roman_numerals, collapse='|')), '_\\1', surnames[ !weirdy])
  spacy <- grepl( ' ', surnames) & !weirdy # Mark Brav
  givens[ spacy] <- sub( ' +[^ ]+$', '', givens[ spacy]) # all but last
  surnames[ spacy] <- sub( '.* +', '', surnames[ spacy]) # last only
  givens[ surnames==auths] <- ''

  # \p{Lu} is uppercase letter (Unicode-ready) in PCRE regex; \p{Ll} lowercase; \p{L} either.  
  allCAPS <- grepl( '^\\p{Lu}+$', givens, perl=TRUE)
  lgivens <- rep( list(), length( auths))
  lgivens[ allCAPS] <- strsplit( givens[ allCAPS], '')
  lgivens[ !allCAPS] <- strsplit( givens[ !allCAPS], '([.]| ) *')
  names( lgivens) <- surnames
  ngiv <- lengths( lgivens)
  
  # Hiroshi==H==<nothing>, Hiroshi != Helen
  gungho_defdif <- function( x, y) !( x == y | is.na( x == y)) # missing compatible with anything
  timid_defdif <- function( x, y) xor( is.na( x), is.na( y)) | (!is.na( x==y) & x!=y) # missing only with missing
  
  defdif <- if( gungho) gungho_defdif else timid_defdif
  
  extendo <- function( x) c( x, rep( '', maxlen-length( x)))
  
  multisurtab <- table( surnames[ !weirdy]) %such.that% (.>1)
  for( m in names( multisurtab)){
    # Equivalence classes...
    # Ambiguous if there's A.B.Nothing and A.Nothing and A.C.Nothing...
    # ... I will go with A.Nothing == A.B.Nothing, and leave user to fix!
    # But make sure it's not otherwise order-dependent (ie A/AB/AC; both AB and AC match A, 
    # but they dont match each other)
    
    ism <- which( surnames==m)
    nm <- length( ism)
    maxlen <- max( ngiv[ ism])
    if( !maxlen){
  next
    } # mononomic
    
    givtab <- sapply( lgivens[ ism], extendo)
    dim( givtab) <- c( maxlen, nm) # in case sapply does not return a matrix; 
    # ... only needed when maxlen==1, but just FORCE it anyway
    
    # Identical ones don't need to be checked
    pasty <- apply( givtab, 2, paste, collapse='')
    check_class <- match( pasty, pasty)
    check <- which( check_class == 1:nm)
    ncheck <- length( check)
    
    if( ncheck>1){
      # Now it's quite complicated...    
      givtab <- givtab[, check, drop=FALSE]
      init <- substring( givtab, 1, 1)
      init[ !nzchar( init)] <- NA
      rest <- substring( givtab, 2)
      rest[ !nzchar( rest)] <- NA
  
      # Form equiv classes; 
      # Always match eg S. and Simon, regardless of gungho; but 
      # whether to match S.N.Wood and S.Wood depends on gungho.
      # gungho version is: if not provably different, same class
      # (with ambiguity re A.Nothing as above)
      
      # Which pairwise comps show definite differences?
      # To make this interesting, minimize loop nesting by vectorization
      # Basically doing outer() by hand, coz I can't figure out how not to
      # This is potentially inefficient O(n^2) for prodigiously large 
      # numbers of authors with same surname... :)
      
      ij <- as.matrix( expand.grid( j=1:maxlen, i1=1:ncheck, i2=1:ncheck))
      dist <- defdif( init[ ij[,c(1,2)]], init[ ij[,c(1,3)]]) + 
          gungho_defdif( rest[ ij[,c(1,2)]], rest[ ij[,c(1,3)]])

      dist <- dist > 0
      dim( dist) <- c( maxlen, ncheck*ncheck)
      dist <- colSums( dist)
      dim( dist) <- c( ncheck, ncheck)
      
      eclass <- partequiv( dist)

      # Form initials-only (but all known initials) for each eclass
      ues <- unique( eclass)
      new_givens <- character( length( ues))
      for( ue in ues){
        ui <- init[,ue==ues,drop=FALSE]
        ui[ is.na( ui)] <- ''
        mnc <- max.col( nchar( ui), ties.method='first')
        new_ui <- ui[ cbind( 1:maxlen, mnc)]
        new_givens[ ue] <- paste( new_ui[ nzchar( new_ui)], collapse='.') %&% '.'
      }
      
      # Gotta replace identical ones, too
      full_eclass <- eclass[ match( check_class, check_class[ check])]
      givens[ ism] <- new_givens[ full_eclass]
    } # if check
    
  } # for surname

  # It is simpler to recompose all author lists, than just ones which have changed
  givens <- gsub( '(\\p{Lu} *)(?![.\\p{Ll}])', '\\1. ', givens, perl=TRUE) # break up strings of unfullstopped initials
  givens <- gsub( '(\\p{Lu}[.]) +', '\\1', givens, perl=TRUE) # zap spaces between initials
  surnames[ !weirdy] <- gsub( '_', ' ', surnames[ !weirdy]) # see earlier
  nicey_names <- sprintf( '%s%s%s', surnames, ifelse( nchar( givens), ', ', ''), givens)
  per_ref <- split( nicey_names, rep( seq_along( nauthper), nauthper))
  new_auths <- lapply( per_ref, paste, collapse= ' and ')
  
  # replace old author lists, keeping format of line (mindful of terminal commas etc)
  bib[ authl] <- sprintf( '%s%s%s',
      sub( '^([^"{]*["{]).*', '\\1', bib[ authl]),
      new_auths,
      sub( '.*(["}] *(,?)) *$', '\\1',  bib[ authl])
    )
  
  if( !is.null( outfile)){
    writeLines( bib, outfile)
  }
  
return( bib)  
}


"xref_numlists" <-
function( tex){
  # I am just gonna assume we _never_ get this in text
  lenu <- grep( '(?<![\\\\])\\\\label[{]enu:', tex, perl=TRUE)
  if( !length( lenu)){
return( tex)
  }
  
  
  lstart <- which( startsWith( tex, '\\begin{enumerate}'))
  lend <- which( startsWith( tex, '\\end{enumerate}'))
  
  # Lists can be nested... gotta count 'em :(
  n <- length( tex)
  ldepth <- rep( 0, n)
  ldepth[ lstart] <- +1
  ldepth[ lend] <- -1
  ldepth <- cumsum( ldepth)
  dmax <- max( ldepth)
  
  litems <- which( startsWith( tex, '\\item '))
  nit <- length( litems)
  
  # Matrix of list items: 
  # row of (1,0,0,0) -> 1; 
  # row of (2,3,0,0) -> 2.3; etc

  itcount <- matrix( 0, nit, dmax)
  for( d in 1:dmax){
    # How many items in this depth within parent list/sublist?
    at_d <- which( ldepth[ litems]==d)
    itsper <- table( findInterval( 
        litems[ at_d], 
        lstart %such.that% (ldepth[.]==d)))
    # (Sub)list seq num
    itcount[ at_d, d] <- unlist( lapply( itsper, seq_len))
    if( d>1){
      this_prev_d <- at_prev_d[ findInterval( 
          litems[ at_d], litems[ at_prev_d])]
      itcount[ at_d, 1 %upto% (d-1)] <- itcount[ this_prev_d, 1 %upto% (d-1)]
    }
    at_prev_d <- at_d
  }
  
  # Only need rows (items) with labels
  itcount <- itcount[ findInterval( lenu, litems),]
  enulab <- sub( '.*(?<![\\\\])\\\\label[{](enu:[^}]*)[}].*', 
      '\\1', tex[ lenu], perl=TRUE)
  max_count <- apply( itcount, 2, max)
  
  # How to label the different levels. I have named these 
  # as per Latex (see Customized Lists in LyX UserGuide)
  # in case I ever support user-specified labels (unlikely...)
  dctr_fun <- list(
      arabic= identity, 
      alph= function( x) letters[x], 
      roman= function(x) sprintf( '(%s)', as.roman( x)),
      Alph= function( x) LETTERS[x]
    )
  if( dmax > 4){ # really???
    dctr_fun <- c( dctr_fun, 
        rep( list( function( x) '-' %&% x),
        dmax - 4
      ))
  }
  for( d in 1:dmax){
    nz <- itcount[,d] > 0
    itcount[ nz, d] <- dctr_fun[[d]]( itcount[ nz, d])
  }
      
  enunum <- apply( itcount, 1, 
      function( x) paste( x[ x>0], collapse=''))  
  
  lenuref <- grep( '(?<![\\\\])\\\\ref[{]enu:', tex, perl=TRUE)
  # Use assertions to match only the label itself
  greg <- gregexpr( '(?<![\\\\])\\\\ref[{]enu:[^}]*[}]',
      tex[ lenuref], perl=TRUE)
  reflab <- regmatches( tex[ lenuref], greg)
  reflab <- FOR( reflab, sub( '.*[^{][{](enu:[^}]*)[}].*', '\\1', .))
  refrep <- FOR( reflab, enunum[ match( ., enulab)]) 
  regmatches( tex[ lenuref], greg) <- refrep

return( tex)  
}


"zap_subfloats" <-
function( tex){ 
## Also fixes the caption on \ContinuedFloat
  subl <- which( startsWith( tex, '\\subfloat'))
  subl <- subl[ grepl( '^\\\\subfloat[{[]', tex[ subl])]
  if( !length( subl)){
return( tex)
  }
  
  r"--{
  Subfloat captions won't look nice in MSWord anyway, so omit them. Better to incorporate into the main float caption, in LyX itself.
  
  \subfloat[1986–1994]{\includegraphics[scale=0.2]{figs/agg-dens-1}
  closing brace is 2 lines further
  }--"
  
  tex[ subl+1] <- sub( '^[^{]*[{]', '', tex[ subl]) # the include
  tex[ subl] <- xsub( tex[ subl], 
      '^\\\\subfloat[^{]*[{].*', '', # zap subcap  
      # '^\\\\subfloat([^{]*)[{].*', '\\1', # keep subcap
      tex[ subl]) # any "sub-caption"
  tex[ subl+2] <- ''  

## \ContinuedFloat: insert ref to label of previous fig
  r"--{
    Someone _might_ include "\label{fig" in actual text, but then the backslash would be doubled. Should be able to use negative look-behind assertion to avoid that, but I'm ****ed if I can get it to work :/
  }--"
  
  contifigs <- which( startsWith( tex, '\\ContinuedFloat\\caption'))
  if( length( contifigs)){
    figlabs <- grep( '\\label{fig:', tex, fixed=TRUE)
    figlabs <- figlabs %that.dont.match% '[\\][\\]label[{]fig:'
    lastfiglab <- findInterval( contifigs, figlabs)
    ok <- lastfiglab>0 # would be user error, but...
    contifigs <- contifigs[ ok] 
    lastfiglab <- lastfiglab[ ok]
    # get LAST label from that line...
    figref <- sub( '.*[\\]label([{]fig:[^}]+[}]).*', 
        'Figure \\\\ref\\1 ',
        tex[ figlabs[ lastfiglab]])
    
    # Splice the refs in, immediately after "caption{"
    tc <- tex[ contifigs]
    tex[ contifigs] <- sprintf( '%s%s%s',
        sub( '[{].*', '{', tc),
        figref,
        sub( '^[^{]*[{]', '', tc))
  }

return( tex)  
}

