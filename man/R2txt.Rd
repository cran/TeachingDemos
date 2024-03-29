\name{txtStart}
\alias{txtStart}
\alias{txtStop}
\alias{txtComment}
\alias{txtSkip}
\alias{etxtStart}
\alias{etxtStop}
\alias{etxtComment}
\alias{etxtSkip}
\alias{etxtPlot}
\alias{wdtxtStart}
\alias{wdtxtStop}
\alias{wdtxtComment}
\alias{wdtxtSkip}
\alias{wdtxtPlot}
\alias{mdtxtStart}
\alias{mdtxtStop}
\alias{mdtxtComment}
\alias{mdtxtSkip}
\alias{mdtxtPlot}
\title{Save a transcript of commands and/or output to a text file. }
\description{
  These functions save a transcript of your commands and their output to
  a script file, possibly for later processing with the "enscript" or
  "pandoc"
  program.

  They work as a combinations of \code{sink} and \code{history} with a
  couple extra bells and whistles.
}
\usage{
txtStart(file, commands=TRUE, results=TRUE, append=FALSE, cmdfile,
          visible.only=TRUE)

txtStop()

txtComment(txt,cmdtxt)

txtSkip(expr)

etxtStart(dir = tempfile("etxt"), file = "transcript.txt",
           commands = TRUE, results = TRUE, append = FALSE,
           cmdbg = "white", cmdcol = "red", resbg = "white",
           rescol = "navy", combg = "cyan", comcol = "black",
           cmdfile, visible.only = TRUE)

etxtStop()

etxtComment(txt, cmdtxt)

etxtSkip(expr)

etxtPlot(file=paste(tempfile('plot',R2txt.vars$dir),'.eps',sep=''),
         width=4, height=4)

wdtxtStart(commands=TRUE, results=TRUE, fontsize=9, cmdfile,
          visible.only=TRUE)

wdtxtStop()

wdtxtComment(txt,cmdtxt)

wdtxtSkip(expr)

wdtxtPlot(height=5, width=5, pointsize=10)

mdtxtStart(dir=tempfile('mdtxt'), file='transcript.md',
           commands=TRUE, results=TRUE, append=FALSE,
           cmdfile, visible.only=TRUE)

mdtxtStop()

mdtxtComment(txt,cmdtxt)

mdtxtSkip(expr)

mdtxtPlot(file=tempfile('plot',R2txt.vars$dir,'.png'),
                     width=4, height=4)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{ Directory to store transcript file and any graphics file in }
  \item{file}{ Text file to save transcript in }
  \item{commands}{ Logical, should the commands be echoed to the
         transcript file }
  \item{results}{ Logical, should the results be saved in the transcript
         file }
  \item{append}{ Logical, should we append to \code{file} or replace it }
  \item{cmdbg}{ Background color for command lines in \code{file} }
  \item{cmdcol}{ Color of text for command lines in \code{file} }
  \item{resbg}{ Background color for results sections in \code{file} }
  \item{rescol}{ Text color of results sections in \code{file} }
  \item{combg}{ Background color for comments in \code{file} }
  \item{comcol}{ Text color of comments in \code{file} }
  \item{cmdfile}{ A filename to store commands such that it can be
         \code{source}d or copied and pasted from }
  \item{visible.only}{ Should non-printed output be included, not
         currently implemented.}
  \item{txt}{Text of a comment to be inserted into \code{file} }
  \item{cmdtxt}{Text of a comment to be inserted into \code{cmdfile} }
  \item{expr}{An expression to be executed without being included in
         \code{file} or \code{cmdfile} }
  \item{width}{Width of plot, passed to \code{dev.copy2eps},
    \code{wdPlot}, or \code{dev.copy}}
  \item{height}{Height of plot, passed to \code{dev.copy2eps},
    \code{wdPlot}, or \code{dev.copy}}
  \item{fontsize}{Size of font to use in MSWord}
  \item{pointsize}{ passed to \code{wdPlot} }
}
\details{
These functions are used to create transcript/command files of your R
         session.  There are 4 sets of functions, those starting with
         "txt",those starting with "etxt", and those starting with
	 "wdtxt" and those starting with "mdtxt".  The "txt" functions
         create a plain text transcript while the "etxt" functions
         create a text file with extra escapes and commands so that it
         can be post processed with enscript (an external program) to
         create a postscript file and can include graphics as well.  The
         postscript file can be converted to pdf or other format file.

	 The "wdtxt" functions will insert the commands and results into
	 a Microsoft Word document.

	 The "mdtxt" functions create a text file but with MarkDown
	 escapes so that it can be post processed with "pandoc" (an
	 external program) to create other formats such as html, pdf, MS
	 Word documents, etc.  If the command starts with the string
	 "pander" or "pandoc" (after optional whitespace) then the
	 results will be inserted directly, without escapes, into the
	 transcript file.  This assumes that you are using code from the
	 "pander" package which generates markdown formatted output.
	 This will create nicer looking tables and other output.

	 If \code{results} is TRUE and \code{commands} is FALSE then the result
	 is similar to the results of \code{sink}.  If \code{commands}
	 is true as well then the transcript file will show both the commands
	 and results similar to the output on the screen.  If both
	 \code{commands} and \code{results} are FALSE then pretty much
	 the only thing these functions will accomplish is to waste some
	 computing time.

	 If \code{cmdfile} is
	 specified then an additional file is created with the commands
	 used (similar to the \code{history} command), this file can be
	 used with \code{source} or copied and
	 pasted to the terminal.

	 The Start functions specify the file/directory to create and
	 start the transcript, \code{wdtxtStart} will open Word if it is
	 not already open or create a connection to an open word window.
	 The prompts are changed to remind you
	 that the commands/results are being copied to the transcript.
	 The Stop functions stop the recording and reset the prompts.

	 The R parser strips comments and does some reformatting so the
	 transcript file may not match exactly with the terminal
	 output.  Use the \code{txtComment}, \code{etxtComment},
	 \code{wdtxtComment}, or \code{mdtxtComment}
	 functions to add a comment.  This will show up as a line offset
	 by whitespace in the transcript file, highlighted in the
	 etxt version, and the default font in Word.
	 If \code{cmdtxt} is specified then that line
	 will be inserted into \code{cmdfile} preceded by a # so it
	 will be skipped if sourced or copied.

	 The \code{txtSkip}, \code{etxtSkip}, \code{wdtxtSkip}, and
	 \code{mdtxtSkip} functions will run the code in \code{expr} but
	 will not include the commands or results in the transcript file
	 (this can be used for side computations, or requests for help,
	 etc.).

	 The \code{etxtPlot} function calls \code{dev.copy2eps} to
	 create a copy of the current plot and inserts the proper command
	 into the transcript file so that the eps file will be included
	 in the final postscript file after processing.

	 The \code{wdtxtPlot} function calls \code{wdPlot} to send a
	 copy of the current graph to MS Word.

	 The \code{mdtxtPlot} function calls \code{dev.copy} to create a
	 copy of the current plot as a .png file and inserts the proper
	 command into the transcript file so that the .png file will be
	 included when processing with pandoc.
}
\value{
  Most of these commands do not return anything of use.  The exceptions
  are:

  \code{etxtStop} returns the name of the transcript file (including the
  directory path).

  \code{txtSkip}, \code{etxtSkip}, \code{wdtxtSkip}, and
  \code{mdtxtSkip} return the value of \code{expr}.

}
%\references{ ~put references to the literature/web site here ~ }
\author{ Greg Snow, \email{538280@gmail.com} }
\note{
These commands do not do any fancy formatting of output, just what you
see in the regular terminal window.  If you want more formatted output
then you should look into \code{Sweave}, \code{knitr}, or the R2HTML package.

The MS word functions will insert into the current word document at the
location of the cursor.  This means that if you look at the document and
move the current location to somewhere in the middle (or have another
word document open with the location in the middle), when you go back to
R, the new transcript will be inserted into the middle of the document.
So be careful to position the cursor at the end of the correct document
before going back to R.  Note that the "wdtxt" functions depend on the
"R2wd" package which in turn depends on tools that are not free.

Do not use these functions in combination with R2HTML or \code{sink}.
Only one of these sets of functions will work at a time.
}
\seealso{\code{\link{sink}}, \code{\link{history}},
  \code{\link{Sweave}}, the odfWeave package, the R2HTML package, the
  R2wd package, the pander package }
\examples{
\dontrun{
etxtStart()
etxtComment('This is todays transcript')
date()
x <- rnorm(25)
summary(x)
stem(x)
etxtSkip(?hist)
hist(x)
etxtPlot()
Sys.Date()
Sys.time()
my.file <- etxtStop()

# assumes enscript and ps2pdf are on your path
system(paste('enscript -e -B -p transcript.ps ', my.file) )
system('ps2pdf transcript.ps')

# if the above commands used mdtxt instead of etxt and the pandoc
# program is installed and on your path (and dependent programs) then use:
system(paste('pandoc -o transcript.docx ', my.file))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ character }
\keyword{ IO }% __ONLY ONE__ keyword per line
\keyword{ utilities }