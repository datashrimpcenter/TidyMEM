\name{LC_scalog}
\alias{LC_scalog}
\title{Calculer un scalogramme représentant les valeurs propres d'une RDA}
\usage{
LC_scalog(res, np = 999, alpha = c(0.05, 0.01, 0.001), cex = 2)
}
\description{
Cette fonction calcule un scalogramme (Legendre et Legendre, 2012, p. 864) représentant les valeurs propres d'une RDA avec une série de fonctions propres spatiales (par exemple, dbMEM) comme variables explicatives. Les fonctions propres doivent être classées par ordre décroissant et être orthogonales entre elles. Dans le cas de la RDA, le \( R^2 \) représente la variance, tandis que dans le cas de la CCA, il représente l'inertie.
}
\arguments{
\item{res}{Un objet de classe "rda" ou "cca" résultant d'une analyse de la fonction rda() ou cca() du package vegan.}
\item{np}{Un entier spécifiant le nombre de permutations à utiliser pour le test ANOVA. Par défaut : 999.}
\item{alpha}{Un vecteur de niveaux de signification pour le test ANOVA. Par défaut : c(0.05, 0.01, 0.001).}
\item{cex}{Un coefficient de taille pour les symboles dans le graphique. Par défaut : 2.}
}
\value{
L'objet de test ANOVA correspondant.
}
\author{
Léo Chassy, inspiré par Legendre et Legendre (2012).
}
\examples{
library(vegan)
library(tidyverse)

data("mite.xy")
fs.h <- decostand(mite, method = "hellinger")

data("mite")
data("mite.env")
data("mite.pcnm")

f.dbmem <- as.data.frame(dbmem(mite.xy, silent = FALSE))
f.dbmem.rda <- rda(fs.h ~ ., f.dbmem)
anova(f.dbmem.rda, permutations = 9999)

# Calculer et tracer le scalogramme
LC_scalog(f.dbmem.rda)
}
