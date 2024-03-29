% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pikachur.R
\name{pokedexr}
\alias{pokedexr}
\title{Pokedex: Who's that Pokemon?!}
\usage{
pokedexr(color, type)
}
\arguments{
\item{color}{Color of the Pokemon.}

\item{type}{Type of the Pokemon (e.g. water, electric, fire, grass, fighting, psychic, flying, ghost, steel).}
}
\value{
data frame
}
\description{
Ever walk through a random street and get caught off guard by a wild Pokemon? You exclaim, "Oh! a wild <insert Pokemon name> appeared!" But the problem is...you don't know its name!
Fear not, young Pokemon master-in-training! This function allows the user to narrow their search on what type of Pokemon they are facing and provides basic information on each Pokemon.
That's right, it's your very own Pokedex in function form!
}
\details{
The function asks the user for the color (e.g. yellow, brown, red) and type (e.g. water, fire, grass) of the Pokemon.
The output shows a dataframe of various Pokemon and basic stats related to each Pokemon. This function can query information from all 807 Pokemon.
}
\examples{
pokedexr("red", "fire")
# give the color and the type of the Pokemon
}
\author{
Montsie Guerrero <\url{http://github.com/acg2176}>
}
\keyword{color}
\keyword{pokedex,}
\keyword{pokemon,}
\keyword{type,}
