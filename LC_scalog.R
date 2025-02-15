# FONCTION ####
#
# A function to compute a scalogram (Legendre and Legendre 2012,
# p. 864) representing the eigenvalues of an RDA with a series of
# spatial eigenfunctions (e.g. dbMEM) as explanatory variables.
# The eigenfunctions must be placed in decreasing order and they
# must be orthogonal to one another.
# In case of RDA the R^2 is variance, in case of CCA it is inertia.
#
#
# Reprend la fonction de Legendre et Legendre (2012)
#
library(vegan)
library(tidyverse)
#
LC_scalog <- function(res, np = 999, alpha = c(0.05, 0.01, 0.001), cex = 2) {
  # Test ANOVA
  test <- anova(res, by = "terms", permutations = how(nperm = np))
  inert <- test[, 2]
  variance <- inert[-length(inert)] / sum(inert)
  signi <- test$"Pr(>F)"[-length(test$"Pr(>F)")]
  n <- length(variance)
  # Définir le label de l'axe y
  ylabel <- if (class(res)[1] == "rda") {
    expression(italic(R)^{2})
  } else {
    "Inertia"
  }
#
  # Préparer les données pour ggplot
  df <- data.frame(
    Eigenfunction = 1:n,
    Variance = variance,
    Significance = signi
  )
#
  # Créer le scalogramme avec ggplot
  p <- ggplot(df, aes(x = Eigenfunction, y = Variance)) +
    geom_line(linewidth = 0.5, color = "gray20", linetype  = "dashed") +
    geom_point(aes(fill = cut(Significance, breaks = c(-Inf, alpha, Inf), labels = c(1, sort(alpha, decreasing = TRUE)))), size = 4, color = 'black', shape = 21) +
    scale_fill_viridis_d(option = "C", name = "p-value", labels =c(paste("p <= ", alpha[3]),
                                                                   paste("p <= ", alpha[2]),
                                                                   paste("p <= ", alpha[1]),
                                                                   paste("p > 0.05")))+
    labs(title = "Scalogram", x = "Eigenfunction number", y = ylabel) +
    theme_bw()  +
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')
    )
#
  print(p)

  invisible(test)
}
#
#
# EXEMPLE ####
#
# library(vegan)
# library(tidyverse)
#
# data("mite.xy")
# fs.h <- decostand(mite, method = "hellinger")
#
# data("mite")
# data("mite.env")
# data("mite.pcnm")
#
# f.dbmem <- as.data.frame(
#   dbmem(mite.xy, silent = FALSE))
#
# f.dbmem.rda <- rda(fs.h ~., f.dbmem)
# anova(f.dbmem.rda, permutations = 9999)
#
#
#
# LC_scalog(f.dbmem.rda)
