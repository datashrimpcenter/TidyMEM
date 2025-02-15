\name{ggtriplotRDA}
\alias{ggtriplotRDA}
\title{Tracer un triplot à partir d'un objet de classe "rda"}
\usage{
ggtriplotRDA(res.rda, ax1 = 1, ax2 = 2, site.sc = "lc", scaling = 2,
           plot.sites = TRUE, plot.spe = TRUE, plot.env = TRUE,
           plot.centr = TRUE, arrows.only = FALSE, label.sites = FALSE,
           label.spe = FALSE, label.env = FALSE, label.centr = FALSE,
           mult.spe = 1, mult.arrow = 1, select.spe = NULL,
           mar.percent = 0, optimum = FALSE, move.origin = c(0, 0),
           silent = TRUE)
}
\description{
Cette fonction permet de tracer un triplot (scaling 1 ou 2) à partir d'un objet de classe "rda" contenant les résultats RDA de la fonction rda() du package vegan. Cette nouvelle version peut gérer des résultats RDA avec un seul axe canonique ou avec toute combinaison significative d'axes canoniques et/ou résiduels, comme dans la fonction vegan::plot.cca(). Les packages requis sont : vegan, tidyverse, ggrepel.
}
\arguments{
##### Paramètres généraux
\item{res.rda}{Un objet rda{vegan}.}
\item{ax1, ax2}{Axes à tracer en abscisse et ordonnée. Valeurs par défaut : 1 et 2.}
\item{site.sc}{Peut être défini sur "lc" (contraintes linéaires ou scores de modèle, par défaut)
ou "wa" (moyennes pondérées, par défaut dans vegan).}
\item{scaling}{Type de mise à l'échelle : seuls 1 ou 2 sont pris en charge. Par défaut : 2.}

##### Éléments à tracer
\item{plot.sites}{Si TRUE, les sites seront tracés sous forme de petits cercles.}
\item{plot.spe}{Si TRUE, les espèces (ou d'autres variables de réponse) seront tracées.}
\item{plot.env}{Si TRUE, des flèches pour les variables explicatives seront tracées.}
\item{plot.centr}{Si TRUE, des symboles seront tracés aux centroids des niveaux de facteurs.}
\item{arrows.only}{Si TRUE, tracer des flèches pour les variables explicatives quantitatives
et les classes de facteurs.}
\item{label.sites}{Si TRUE, des étiquettes sont ajoutées aux symboles des sites.}
\item{label.spe}{Si TRUE, des étiquettes sont ajoutées aux flèches des espèces.}
\item{label.env}{Si TRUE, des étiquettes sont ajoutées aux flèches des variables environnementales.}
\item{label.centr}{Si TRUE, des étiquettes sont ajoutées aux centroids des niveaux de facteurs.}

##### Multiplicateurs, sélection des espèces à tracer
\item{mult.spe}{Multiplicateur pour la longueur des flèches des espèces. Par défaut : 1.}
\item{mult.arrow}{Multiplicateur pour la longueur des flèches environnementales. Par défaut : 1.}
\item{select.spe}{Vecteur contenant une sélection des numéros d'espèces à tracer
dans le biplot, par ex. c(1,2,5,8,12). Tracer toutes les espèces si select.spe=NULL
(valeur par défaut). Les espèces bien représentées dans le graphique RDA
peuvent être identifiées à l'aide de goodness(RDA.output.object, display="species").}

##### Position du graphique dans le cadre, marges
\item{mar.percent}{Facteur pour agrandir la taille du graphique afin d'accommoder
tous les éléments et étiquettes. Les valeurs positives augmentent les marges autour
du graphique, les valeurs négatives les réduisent.}
\item{optimum}{Si TRUE, les plus longues flèches des espèces et environnementales
sont étirées à une longueur égale à la distance à l'origine du site le plus éloigné
de l'origine dans le graphique des (ax1, ax2). Cela représente une combinaison
optimale des trois éléments. Les longueurs des flèches des espèces et environnementales
peuvent être modifiées à l'aide des arguments mult.spe et mult.arrow.}
\item{move.origin}{Déplacer l'origine du graphique de gauche à droite et de haut en bas.
Par défaut : move.origin=c(0,0). Ex. move.origin=c(-1,0.5) déplace l'origine de 1 unité
à gauche et de 0.5 unité vers le haut.}

##### Variantes
\item{silent}{Si FALSE, les étapes intermédiaires de calcul sont imprimées. Par défaut : TRUE.}
}
\examples{
# Exemple 1 - Tableau 11.3 de Legendre & Legendre (2012, p. 644),
# uniquement les 6 premières espèces
library(vegan)
library(tidyverse)
library(ggrepel)

Y.mat <-
  matrix(c(1, 0, 0, 11, 11, 9, 9, 7, 7, 5, 0, 0, 1, 4, 5, 6, 7, 8, 9, 10, 0,
           0, 0, 0, 17, 0, 13, 0, 10, 0, 0, 0, 0, 0, 7, 0, 10, 0, 13, 0, 0,
           0, 0, 8, 0, 6, 0, 4, 0, 2, 0, 0, 0, 1, 0, 2, 0, 3, 0, 4), 10, 6)
rownames(Y.mat) <- 1:10
colnames(Y.mat) <- paste0("sp", 1:6)
spe <- as.data.frame(Y.mat)
Depth <- 1:10
Sub. <- factor(c(rep(1, 3), 3, 2, 3, 2, 3, 2, 3),
               labels = c("Sable", "Corail", "Autre"))
env <- cbind(data.frame(Depth), data.frame(Sub.))
summary(env)

# Deux variables explicatives
rda.out <- rda(spe ~ ., env)

ggtriplotRDA(rda.out)                              # scaling 2, scores lc
ggtriplotRDA(rda.out, site.sc = "wa")              # valeurs par défaut de vegan
ggtriplotRDA(rda.out, scaling = 1)                 # scaling 1, scores lc
ggtriplotRDA(rda.out, site.sc = "wa", scaling = 1) # scaling 1, scores wa

# Un seul axe canonique
rda.out <- rda(spe ~ Depth, env)

ggtriplotRDA(rda.out)                              # scaling 2, scores lc
ggtriplotRDA(rda.out, site.sc = "wa")              # valeurs par défaut de vegan
ggtriplotRDA(rda.out, scaling = 1)                 # scaling 1, scores lc
ggtriplotRDA(rda.out, site.sc = "wa", scaling = 1) # scaling 1, scores wa

# Exemple 2 - Données de dunes
data(dune)
data(dune.env)
rda.dune <- rda(dune ~ A1 + Management, dune.env)
tmp <- goodness(rda.dune)
( sp.sel <- which(tmp[, 2] >= 0.4) )

ggtriplotRDA(rda.dune)
ggtriplotRDA(rda.dune, scaling = 1, select.spe = sp.sel)
}
\license{
GPL-2
}
\authors{
Francois Gillet, Daniel Borcard & Pierre Legendre, 2021
}
