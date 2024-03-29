% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pikachur.R
\name{pokemaster}
\alias{pokemaster}
\title{Strength or Weakness? Figure out your Pokemon's next Move!}
\usage{
pokemaster(type1, type2)
}
\arguments{
\item{type1}{Type of Pokemon. (e.g. fire, steel, ice, psychic, ghost, electric).}

\item{type2}{Type of Pokemon. (e.g. fire, steel, ice, psychic, ghost, electric).}
}
\value{
data frame
}
\description{
Oh no! Your in the middle of your first Pokemon battle and you didn't just forget to call mom about it,
you forgot which moves your Pokemon can do! Which type of move is strong against X and weak against Y? To figure out the best strategy,
this function allows the Pokemon trainer to find out a dataset of moves they can use to make the most "It's super effective!" move.
}
\details{
This function takes in 2 arguments of types and determines whether the relationship of these types is either
a strength or a weakness (e.g. water is a strength while fire is a weakness). If there is no relationship, the function will let the user/trainer know. There are 18 Pokemon types to choose from: normal, fighting, flying, poison, ground, rock, bug, steel,
fire, water, grass, electric, psychic, ice, dragon, dark, fairy, shadow. The output is a dataset of 15 Pokemon moves (based on type) that can be used by the
Pokemon player for his/her next optimal move. Go forth and get that Gym Badge, young Pokemaster!
}
\examples{
pokemaster("grass","ground")
# add two types of Pokemon moves
}
\author{
Montsie Guerrero <\url{http://github.com/acg2176}>
}
