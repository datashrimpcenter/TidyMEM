# FONCTION ####
#
#
# Cette fonction reprend la fonction faite par François Gillet (25 Aôut 2012) nommée sr.value.R, qui est également implémentée dans ce package.
# performs the scatter diagram with the representation of a value for a variable
#
library(vegan)
library(tidyverse)
library(ggsci)
library(ggrepel)
library(adespatial)
library(latex2exp)
library(performance)
library(gridExtra)
#
#
    sr_value <- function(dfxy, z, xax = dfxy[1], yax = dfxy[2], method = c("bubble", "greylevel"),
                                zmax = NULL, csize = 1, cpoint = 0, pch = 20,
                                clegend = 0.75, neig = NULL, cneig = 1,
                                sub = "", possub = "topleft",
                                viridis_option = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                viridis_direction = 1,
                                viridis_begin = 0, viridis_end = 1
                            ) {
#
      # Vérification de la longueur de z
      if (length(z) != nrow(dfxy)) {
        stop(paste("Non equal row numbers", nrow(dfxy), length(z)))
      }
#
#
      # Création du graphique de base
      p <- ggplot(dfxy, aes_string(x = xax, y = yax)) +
        theme_bw()  +
        theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA),
          legend.background = element_rect(fill='transparent'),
          legend.box.background = element_rect(fill='transparent'),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        ggtitle(sub)
#
      # Méthode d'affichage
      if (method == "greylevel") {
        breaks <- pretty(z, 6)
        numclass <- cut(z, breaks, include.lowest = TRUE, labels = FALSE)
#
        p <- p + geom_point(aes(size = z, fill = factor(numclass)), shape = 21, color = "black", alpha = 0.7) +
          scale_fill_grey() +
          guides(size = FALSE, fill = guide_legend(title = "Values"))+
          theme(legend.position = "bottom")
#
      } else if (method == "bubble") {
        if (is.null(zmax)) zmax <- max(abs(z))
        dfxy$size <- csize * sqrt(abs(z) / zmax) * 10  # Ajustement de la taille

        p <- p + geom_point(aes(size = z^2, fill = z), shape = 21, color = "black", alpha = 0.7) +
          scale_fill_viridis_c(option = viridis_option,
                               direction = viridis_direction,
                               begin = viridis_begin, end = viridis_end) +
          guides(fill = guide_legend(title = "Spatial variation"), size = "none")+
          theme(legend.position = "bottom")
      }
#
      # Ajout de la légende
      if (cpoint > 0) {
        p <- p + geom_point(aes(size = csize), shape = pch, alpha = 0.5)
      }
#
#
      print(p)
    }
#
#
    # EXEMPLE ####
#

# library(vegan)
# library(adespatial)
# library(tidyverse)
#
# data("mite.xy")
#
#
#
# data("mite")
# fs.h <- decostand(mite, method = "hellinger")
# data("mite.env")
# data("mite.pcnm")
#
# f.dbmem <- as.data.frame(
#   dbmem(mite.xy, silent = FALSE))
#
# f.dbmem.rda <- rda(fs.h ~., f.dbmem)
# anova(f.dbmem.rda, permutations = 9999)
#
# ## Step 3. Since the R-square is significant, compute the adjusted
# ## R2 and run a forward selection of the dbmem variables
#
# f.R2a <- RsquareAdj(f.dbmem.rda)$adj.r.squared
#
# f.dbmem.fwd <- forward.sel(fs.h, as.matrix(f.dbmem), adjR2thresh = f.R2a)
#
# nb.sig.dbmem <- nrow(f.dbmem.fwd) # Number of signif. dbMEM
# # Identity of the significant dbMEM in increasing order
#
# dbmem.sign <- sort(f.dbmem.fwd[ ,2])
#
# # Write the significant dbMEM to a new object
# dbmem.red <- f.dbmem[ ,c(dbmem.sign)]
# ## Step 4. New dbMEM analysis with 8 significant dbMEM variables
# ## Adjusted R-square after forward selection: R2adj = 0.2418
#
# f.dbmem.rda2 <- rda(fs.h~., data = dbmem.red)
#
#
#
# f.fwd.R2a <- RsquareAdj(f.dbmem.rda2)$adj.r.squared
# anova(f.dbmem.rda2, permutations = 9999)
# axes.test <- anova(f.dbmem.rda2, by = "axis", permutations = 9999)
# # Number of significant axes
# nb.ax <- length(which(axes.test[ , ncol(axes.test)] <= 0.05))
#
# # Step 5. Plot the significant canonical axes
# #
# f.rda2.axes <- scores(f.dbmem.rda2, choices = c(1:nb.ax), display = "lc", scaling = 1)
#
# plots <- list()
#
# # Boucle pour générer les graphiques
# for (i in 1:nb.ax) {
#   plots[[i]] <- sr_value(mite.xy, f.rda2.axes[, i],
#                                 sub = paste("RDA", i), method = "bubble",
#                                 viridis_option = "C",
#                                 viridis_direction = -1,
#                                 viridis_begin = 0.1, viridis_end = 0.9)
# }
#
# # Affichage avec grid.arrange
# grid.arrange(grobs = plots, nrow = 1)
