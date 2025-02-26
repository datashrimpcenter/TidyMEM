\name{osc.mem}
\alias{osc.mem}
\title{Visualiser les oscillations des MEM selon l'axe spatial des X}
\usage{
osc.mem(dfxy = W, z = dbmem, fwd = dbmem.fwd, mem_selection = dbmem.signif, color = "seagreen4", ncol = 2)
}
\description{
Cette fonction sert à visualiser les variations spatiales le long de l'axe X (longitudes).
}
\arguments{
\item{dfxy}{W, Structure spatiale et/ou temporelle (data.frame de 2 colonnes)}
\item{z}{Le data.frame avec les dbmem venant de la fonction dbmem dans le package adespatial}
\item{fwd}{modèle de rda significatif venant de la fonction forward.sel du package adespatial}
\item{mem_selection}{Les mem significatifs selectionnées}
\item{color}{La couleur de la courbe t'entends ?}
\item{ncol}{le nombre de colonnes affichées}
}
\value{
Graphique présentant toutes les oscillations significatives.
}
\author{
Léo Chassy, Claude 3.5 Haiku (20241022).
}
\examples{
library(vegan)
library(adespatial)
library(tidyverse)

data("mite.xy")



data("mite")
fs.h <- decostand(mite, method = "hellinger")
data("mite.env")
data("mite.pcnm")

f.dbmem <- as.data.frame(
  dbmem(mite.xy, silent = FALSE))

f.dbmem.rda <- rda(fs.h ~., f.dbmem)
# anova(f.dbmem.rda, permutations = 9999)

## Step 3. Since the R-square is significant, compute the adjusted
## R2 and run a forward selection of the dbmem variables

f.R2a <- RsquareAdj(f.dbmem.rda)$adj.r.squared

f.dbmem.fwd <- forward.sel(fs.h, as.matrix(f.dbmem), adjR2thresh = f.R2a)

nb.sig.dbmem <- nrow(f.dbmem.fwd) # Number of signif. dbMEM
# Identity of the significant dbMEM in increasing order

dbmem.sign <- sort(f.dbmem.fwd[ ,2])


osc.mem(dfxy = mite.xy, z = f.dbmem, fwd = f.dbmem.fwd, mem_selection = dbmem.sign)
}
