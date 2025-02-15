\name{quickMEM}
\alias{quickMEM}
\title{Analyse rapide des cartes d'eigenvecteurs basées sur la distance (dbMEM)}
\usage{
quickMEM(Y, XY, stand = FALSE, thresh = NULL, method = "fwd",
          myspat = NULL, alpha = 0.05, rangexy = FALSE,
          detrend = TRUE, perm.max = 999)
}
\description{
Cette fonction effectue une analyse rapide des dbMEM (Distance-based Moran's Eigenvector Maps) comme décrit par Dray et al. (2006) et Legendre et Legendre (2012). Les eigenvecteurs sont utilisés pour identifier les structures spatiales positives. Si seules des coordonnées géographiques sont fournies, la fonction calcule automatiquement les variables dbMEM. Les coordonnées géographiques sont obligatoires même si des variables spatiales complexes sont fournies.
}
\arguments{
\item{Y}{Un vecteur numérique, une matrice ou un cadre de données contenant les données de réponse prêtes pour l'analyse.}
\item{XY}{Une matrice contenant les coordonnées géographiques (x ou x-y) des sites.}
\item{stand}{Indique si les données de réponse doivent être standardisées à une moyenne de 0 et une variance unitaire. Par défaut : FALSE.}
\item{thresh}{Distance de troncature fournie par l'utilisateur (utilisée si la valeur automatiquement calculée n'est pas appropriée).}
\item{method}{Méthode de sélection des variables dbMEM : "none" pour aucune sélection, "fwd" pour la sélection en avant basée sur la régression. Par défaut : "fwd".}
\item{myspat}{Matrice optionnelle contenant des variables spatiales pré-calculées comme MEM, AEM ou polynômes.}
\item{alpha}{Niveau de signification des tests (par défaut : 0.05).}
\item{rangexy}{Si TRUE, redimensionne les coordonnées spatiales pour un minimum de 0 sur les deux axes et un maximum de 10 sur l'axe ayant la plus grande portée.}
\item{detrend}{Si TRUE (par défaut), teste si une tendance linéaire est présente et significative avant l'analyse dbMEM, et si oui, effectue un détrend des données de réponse.}
\item{perm.max}{Limite du nombre maximum de permutations dans les tests RDA. Par défaut : 999.}
}
\value{
Une liste contenant les variables dbMEM, les résultats de RDA, un test global de la RDA, et, si pertinent, les tests des axes canoniques.
}
\author{
Daniel Borcard, Juillet 2016.
}
\examples{
data(mite)
data(mite.xy)
mite.h <- decostand(mite, "hellinger")

# Application usuelle avec les paramètres par défaut
exampleA <- quickMEM(mite.h, mite.xy)

# Application avec des polynômes spatiaux pré-calculés de troisème ordre
xy.poly3 <- poly(as.matrix(mite.xy), degree = 3)
exampleB <- quickMEM(mite.h, mite.xy, myspat = xy.poly3, alpha = 0.01, rangexy = TRUE)

# Ajustement d'un plan sur les données de premier ordre
exampleC <- quickMEM(mite.h, mite.xy, myspat = mite.xy, method = "none", detrend = FALSE)
}
