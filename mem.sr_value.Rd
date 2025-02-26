\name{sr_value}
\alias{sr_value}
\title{Représentation d'une valeur dans un graphique}
\usage{
sr_value(dfxy, z, xax = "x", yax = "y", method = c("bubble", "greylevel"),
                 zmax = NULL, csize = 1, cpoint = 0, pch = 20,
                 clegend = 0.75, neig = NULL, cneig = 1,
                 sub = "", possub = "topleft")
}
\description{
Cette fonction réalise un diagramme de dispersion avec la représentation d'une valeur pour une variable. Elle reprend la fonction sr.value qui elle même reprend la fonction s.value comprise dans le package ade4.
}
\arguments{
\item{dfxy}{Un cadre de données contenant deux coordonnées.}
\item{z}{Un vecteur des valeurs correspondant aux lignes de dfxy.}
\item{xax}{Colonne pour l'axe des x. Par défaut : "x".}
\item{yax}{Colonne pour l'axe des y. Par défaut : "y".}
\item{method}{Une chaîne de caractères spécifiant la méthode d'affichage :
    \itemize{
        \item{"bubble"} : utilise des bulles dont la taille est proportionnelle à la valeur.
        \item{"greylevel"} : utilise des carrés de taille égale avec un niveau de gris proportionnel à la valeur.
        Par défaut, la première option est choisie.
    }}
\item{zmax}{Une valeur numérique, par défaut max(abs(z)), permettant d'imposer une échelle commune pour plusieurs dessins dans le même dispositif.}
\item{csize}{Un coefficient de taille pour les symboles. Par défaut : 1.}
\item{cpoint}{Une taille de caractère pour le traçage des points, utilisée avec par("cex")*cpoint. Si zéro, aucun point n'est tracé.}
\item{pch}{Si cpoint > 0, un entier spécifiant le symbole ou le caractère unique à utiliser pour tracer les points. Par défaut : 20.}
\item{clegend}{Une taille de caractère pour la légende, utilisée par par("cex")*clegend. Par défaut : 0.75.}
\item{neig}{Un graphique voisin.}
\item{cneig}{Une taille pour les lignes du graphique voisin, utilisée avec par("lwd")*cneig. Par défaut : 1.}
\item{sub}{Une chaîne de caractères à insérer comme légende.}
\item{possub}{Une chaîne de caractères indiquant la position du sous-titre ("topleft", "topright", "bottomleft", "bottomright"). Par défaut : "topleft".}
}
\value{
L'appel correspondant.
}
\author{
Léo Chassy, inspiré de Daniel Chessel
}
\examples{
# Exemple d'utilisation

    library(vegan)
    library(tidyverse)
    library(ggsci)
    library(ggrepel)
    library(adespatial)
    library(latex2exp)
    library(performance)
    library(gridExtra)

data("mite.xy")
fs.h <- decostand(mite, method = "hellinger")

data("mite")
data("mite.env")
data("mite.pcnm")

f.dbmem <- as.data.frame(dbmem(mite.xy, silent = FALSE))
f.dbmem.rda <- rda(fs.h ~ ., f.dbmem)
anova(f.dbmem.rda, permutations = 9999)

# Sélection des variables dbMEM
f.R2a <- RsquareAdj(f.dbmem.rda)$adj.r.squared
f.dbmem.fwd <- forward.sel(fs.h, as.matrix(f.dbmem), adjR2thresh = f.R2a)
nb.sig.dbmem <- nrow(f.dbmem.fwd)
dbmem.sign <- sort(f.dbmem.fwd[, 2])
dbmem.red <- f.dbmem[, c(dbmem.sign)]
f.dbmem.rda2 <- rda(fs.h ~ ., data = dbmem.red)

# Tracer les axes canoniques significatifs
plots <- list()
for (i in 1:nb.ax) {
  plots[[i]] <- sr_value(mite.xy, f.rda2.axes[, i],
                                sub = paste("RDA", i), method = "bubble")
}

# Affichage avec grid.arrange
grid.arrange(grobs = plots, nrow = 1)
}
