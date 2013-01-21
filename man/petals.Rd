\name{petals}
\alias{petals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Play the Petals Around the Rose game
}
\description{
This plays the lateral thinking game Petals Around the Rose.  This is a
game where 5 regular dice are rolled and the players then try to figure
out how many petals are around the rose.
}
\usage{
petals(plot = TRUE, txt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot}{ Should the dice be plotted to the current/default
    graphics device.
}
  \item{txt}{ Should the dice be shown in the console window using text.
%%     ~~Describe \code{txt} here~~
}
}
\details{
  At least one of the arguments \code{plot} and \code{txt} needs to be
  true, otherwise you will be guessing blind (or testing your psychic
  abilities).

  The game is usually played with 5 physical dice, one person who knows
  the rules (the potentate of the rose, here the computer), and one or
  more players trying to learn the puzzle.  The potentate can only give
  the players the following 3 rules:

  \enumerate{
  \item The name of the game is "Petals Around the Rose" and the name is
  significant.

  \item The answer is always 0 or an even number.

  \item The potentate can tell the answer for any roll after any guesses
  are made.
}

The potentate (or other player) then rolls the 5 dice and any players
are then allowed to guess.  The potentate either confirms a correct
guess or tells the correct answer, then the game continues with another
roll.  Players are not to discuss their reasoning so that each can solve
it themselves.  When a player thinks they have worked out the reasoning
they demonstrate it by getting correct guesses, but not by discussing it
with anyone.  Generally 6 correct guesses in a row is considered
evidence that they have figured out the rules and they are then
considered a potentate of the rose.

For this implementation the computer will simulate the role of 5 dice
and display the results and ask for a guess of how many petals are
around the rose.  The player then enters their guess and the computer
then either confirms that it is correct or gives the correct answer.

Pressing enter without making a guess ends the game.

}
\value{

  This function only returns NULL, it is run for its side effects.

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  \url{http://www.borrett.id.au/computing/petals-bg.htm}
%% ~put references to the literature/web site here ~
}
\author{Greg Snow, \email{greg.snow@imail.org}
%%  ~~who you are~~
}
\note{
  Casual viewing of the function source code is unlikely to reveal the
  secret (and therefore this could be used as an example of one way to
  disguise portions of code from casual examination).  More on
  disguising source code is at \url{https://stat.ethz.ch/pipermail/r-devel/2011-October/062236.html}.

  Some basic debugging can reveal the secret, but that would be cheating
  and an admission that such a simple game has defeated you, so don't do
  it, just keep playing until you figure it out.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{dice}
}
\examples{
if(interactive()){
  petals()
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }

