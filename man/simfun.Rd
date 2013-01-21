\name{simfun}
\alias{simfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Create a function to simulate data
}
\description{
This function is used to create a new function that will simulate data.  This could be used by a teacher to create homework or test conditions that the students would then simulate data from (each student could have their own unique data set) or this function could be used in simulations for power or other values of interest.
}
\usage{
simfun(expr, drop, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{
  This is an expression, usually just one or more statements, that will generate the simulated data.
}
  \item{drop}{
A character vector of names of objects/columns that will be dropped from the return value.  These are usually intermediate objects or parameter values that you don't want carried into the final returned object.
}
  \item{\dots}{
Additional named items that will be in the environment when \code{expr} is evaluated.
}
}
\details{
  This function creates another function to simulate data.  You supply the general ideas of the simulation to this function and the resulting function can then be used to create simulated datasets.  The resulting function can then be given to students for them to simulate datasets, or used localy as part of larger simulations\
  
  The environment where the expression is evaluated will have all the columns or elements of the \code{data} argument available as well as the \code{data} argument itself.  Any variables/parameters passed through \code{...} in the original function will also be available.  You then supply the code based on those variables to create the simulated data.  The names of any columns or parameters submitted as part of \code{data} will need to match the code exactly (provide specific directions to the users on what columns need to be named).  Rember that indexing using factors indexes based on the underlying integers not the character representation.  See the examples for details.
  
  The resulting function can be saved and loaded/attached in different R sessions (it is important to use \code{save} rather than something like \code{dput} so that the environment of the function is preserved).
  
  The function includes an optional seed that will be used with the \code{\link{char2seed}} function (if the seed is a character) so that each student could use a unique but identifiable seed (such as their name or something based on their name) so that each student will use a different dataset, but the instructor will be able to generate the exact same dataset to check answers.
  
  The "True" parameters are hidden in the environment of the function so the student will not see the "true" values by simply printing the function.  However an intermediate level R programmer/user would be able to extract the simulation parameters (but the correct homework or test answer will not be the simulation parameters).
}
\value{
The return value is a function that will generate simulated datasets.  The function will have 2 arguments, \code{data} and \code{seed}.  The \code{data} argument can be either a data frame of the predictor variables (study design) or a list of simulation parameters.  The \code{seed} argument will be passed on to \code{\link{set.seed}} if it is numeric and \code{\link{char2seed}} if it is a character.

The return value of this function is a dataframe with the simulated data and any explanitory variables passed to the function.

See the examples for how to use the result function.
}
\author{Greg Snow, \email{greg.snow@imail.org}}
\note{
  This function was not designed for speed, if you are doing long simulations then hand crafting the simulation function will probably run quicker than one created using this function.
  
  Like the prediction functions the data frame passed in as the data argument will need to have exact names of the columns to match with the code (including capitolization).
  
  This function is different from the \code{\link{simulate}} functions in that it allows for different sample sizes, user specified parameters, and different predictor variables.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{set.seed}}, \code{\link{char2seed}}, \code{\link{within}}, \code{\link{simulate}}, \code{\link{save}}, \code{\link{load}}, \code{\link{attach}}
}
\examples{
# Create a function to simulate heights for a given dataset

simheight <- simfun( {h <- c(64,69); height<-h[sex]+ rnorm(10,0,3)}, drop='h' )

my.df <- data.frame(sex=rep(c('Male','Female'),each=5))
simdat <- simheight(my.df)
t.test(height~sex, data=simdat)

# a more general version, and have the expression predefined
# (note that this assumes that the levels are Female, Male in that order)

myexpr <- quote({
  n <- length(sex)
  h <- c(64,69)
  height <- h[sex] + rnorm(n,0,3)
})

simheight <- simfun(eval(myexpr), drop=c('n','h'))
my.df <- data.frame(sex=sample(rep(c('Male','Female'),c(5,10))))
(simdat <- simheight(my.df))

# similar to above, but use named parameter vector and index by names

myexpr <- quote({
  n <- length(sex)
  height <- h[ as.character(sex)] + rnorm(n,0,sig)
})

simheight <- simfun(eval(myexpr), drop=c('n','h','sig'), 
  h=c(Male=69,Female=64), sig=3)
my.df <- data.frame( sex=sample(c('Male','Female'),100, replace=TRUE))
(simdat <- simheight(my.df, seed='example'))

# Create a function to simulate Sex and Height for a given sample size 
# (actually it will generate n males and n females for a total of 2*n samples)
# then use it in a set of simulations
simheight <- simfun( {sex <- factor(rep(c('Male','Female'),each=n))
                      height <- h[sex] + rnorm(2*n,0,s)
                      }, drop=c('h','n'), h=c(64,69), s=3)
(simdat <- simheight(list(n=10)))

out5  <- replicate(1000, t.test(height~sex, data=simheight(list(n= 5)))$p.value)
out15 <- replicate(1000, t.test(height~sex, data=simheight(list(n=15)))$p.value)

mean(out5  <= 0.05)
mean(out15 <= 0.05)

# use a fixed population

simstate <- simfun({
  tmp <- state.df[as.character(State),]
  Population <- tmp[['Population']]
  Income <- tmp[['Income']]
  Illiteracy <- tmp[['Illiteracy']]
}, state.df=as.data.frame(state.x77), drop=c('tmp','state.df'))
simstate(data.frame(State=sample(state.name,10)))

# Use simulation, but override setting the seed

simheight <- simfun({
  set.seed(1234)
  h <- c(64,69)
  sex <- factor(rep(c('Female','Male'),each=50))
  height <- round(rnorm(100, rep(h,each=50),3),1)
  sex <- sex[ID]
  height <- height[ID]
}, drop='h')
(newdat <- simheight(list(ID=c(1:5,51:55))))
(newdat2<- simheight(list(ID=1:10)))

# Using a fitted object

fit <- lm(Fertility ~ . , data=swiss)
simfert <- simfun({
  Fertility <- predict(fit, newdata=data)
  Fertility <- Fertility + rnorm(length(Fertility),0,summary(fit)$sigma)
}, drop=c('fit'), fit=fit)

tmpdat <- as.data.frame(lapply(swiss[,-1], 
            function(x) round(runif(100, min(x), max(x)))))
names(tmpdat) <- names(swiss)[-1]
fertdat <- simfert(tmpdat)
head(fertdat)
rbind(coef(fit), coef(lm(Fertility~., data=fertdat)))

# simulate a nested mixed effects model
simheight <- simfun({
  n.city <- length(unique(city))
  n.state <- length(unique(state))
  n <- length(city)
  height <- h[sex] + rnorm(n.state,0,sig.state)[state] + 
    rnorm(n.city,0,sig.city)[city] + rnorm(n,0,sig.e)
}, sig.state=1, sig.city=0.5, sig.e=3, h=c(64,69),
  drop=c('sig.state','sig.city','sig.e','h','n.city','n.state','n'))

tmpdat <- data.frame(state=gl(5,20), city=gl(10,10), 
  sex=gl(2,5,length=100, labels=c('F','M')))
heightdat <- simheight(tmpdat)

# similar to above, but include cost information, this assumes that 
# each new state costs $100, each new city is $10, and each subject is $1
# this shows 2 possible methods

simheight <- simfun({
  n.city <- length(unique(city))
  n.state <- length(unique(state))
  n <- length(city)
  height <- h[sex] + rnorm(n.state,0,sig.state)[state] + 
    rnorm(n.city,0,sig.city)[city] + rnorm(n,0,sig.e)
  cost <- 100 * (!duplicated(state)) + 10*(!duplicated(city)) + 1
  cat('The total cost for this design is $', 100*n.state+10*n.city+1*n, 
  '\n', sep='')
}, sig.state=1, sig.city=0.5, sig.e=3, h=c(64,69),
  drop=c('sig.state','sig.city','sig.e','h','n.city','n.state','n'))

tmpdat <- data.frame(state=gl(5,20), city=gl(10,10), 
  sex=gl(2,5,length=100, labels=c('F','M')))
heightdat <- simheight(tmpdat)
sum(heightdat$cost)


# another mixed model method

simheight <- simfun({
  state <- gl(n.state, n/n.state)
  city <- gl(n.city*n.state, n/n.city/n.state)
  sex <- gl(2, n.city, length=n, labels=c('F','M') )
  height <- h[sex] + rnorm(n.state,0,sig.state)[state] + 
    rnorm(n.city*n.state,0,sig.city)[city] + rnorm(n,0,sig.e)
}, drop=c('n.state','n.city','n','sig.city','sig.state','sig.e','h'))

heightdat <- simheight( list(
  n.state=5, n.city=2, n=100, sig.state=10, sig.city=3, sig.e=1, h=c(64,69)
))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ datagen }
\keyword{ design }
