# TEST
#
# La fonction que j'ai écrite ne donne pas le bon résultat alors qu'il prend les mêmes données (encore une histoire de grammaire, je suis vraiment pas loin)

# Via Québec Ocean, j'ai découvert l'outil ComparIA, qui permet de comparer les IA en te donnant une réponse à l'aveugle entre deux modèles et tu choisis le meilleur.

# Ma fonction ####
# Elle fonctionne mais est erronée
#
# osc.mem <- function(dfxy,# Coordonnées spatiales
#                     z,# dbMEM
#                     fwd,# forward selection
#                     mem_selection,# dbmem significatives
#                     color = "forestgreen",
#                     fill = "aliceblue",
#                     ncol = 2){
#
#
#   require(tidyverse)
#   require(adespatial)
#   require(vegan)
#   require(cowplot)
#
#   plots <- vector("list", length(fwd$order))
#
#   for (i in 1:length(mem_selection)) {
#
#     plots[[i]] <- ggplot(data = data.frame(x = (1:length(dfxy[, 1])),
#                                            y = z[, sort(fwd$order[i])]),
#                          aes(x = x, y = y)) +
#       geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
#       geom_line(linewidth = 1, color = color) +
#       # stat_smooth(method = "loess", se = TRUE, color = color, span = 0.2)+
#       labs(y = paste("dbMEM", sort(fwd$order[i])), x = "Coordonnée X") +
#       theme_bw() +
#       theme(
#         panel.background = element_rect(fill = 'transparent'),
#         plot.background = element_rect(fill = 'transparent', color = NA),
#         legend.background = element_rect(fill = 'transparent'),
#         legend.box.background = element_rect(fill = 'transparent')
#       )
#   }
#
#   plot_grid(plotlist = plots, ncol = ncol)
# }






# EXEMPLE sans fonction ####
#
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
# # anova(f.dbmem.rda, permutations = 9999)
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
#
# osc.mem(dfxy = mite.xy, z = f.dbmem, fwd = f.dbmem.fwd, mem_selection = dbmem.sign, color = "forestgreen")
#
#
#
#
#
# # Sans la fonction
# plots <- vector("list", length(dbmem.sign))
#
# for (i in 1:length(dbmem.sign)) {
#   plots[[i]] <- ggplot(data = data.frame(x = (1:length(mite.xy[, 1])),
#                                          y = f.dbmem[, sort(f.dbmem.fwd$order)[i]])) +
#     geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
#     geom_line(mapping = aes(x = x, y = y), linewidth = 1, color = "forestgreen") +
#     labs(y = paste("dbMEM", sort(f.dbmem.fwd$order)[i]), x = "Coordonnée X") +
#     theme_bw() +
#     theme(
#       panel.background = element_rect(fill = 'transparent'),
#       plot.background = element_rect(fill = 'transparent', color = NA),
#       legend.background = element_rect(fill = 'transparent'),
#       legend.box.background = element_rect(fill = 'transparent')
#     )
# }
#
# cowplot::plot_grid(plotlist = plots, ncol = 2)



# CHATGPT-4o ####
# Fonctionne mais est nulle, non opti et aggaçante (je l'ai reconnu à l'aveugle, donc maybe c'est biaisé)
#
# osc.mem <- function(dfxy, # Coordonnées spatiales
#                     z, # dbMEM
#                     fwd, # forward selection
#                     mem_selection, # dbmem significatives
#                     color = "forestgreen",
#                     fill = "aliceblue",
#                     ncol = 2){
#
#   require(tidyverse)
#   require(adespatial)
#   require(vegan)
#   require(cowplot)
#
#   plots <- vector("list", length(mem_selection))
#
#   for (i in 1:length(mem_selection)) {
#     mem_index <- mem_selection[i] # Utiliser mem_selection pour l'index
#
#     plots[[i]] <- ggplot(data = data.frame(x = (1:length(dfxy[, 1])),
#                                            y = z[, mem_index]), # Utiliser mem_index pour accéder à z
#                          aes(x = x, y = y)) +
#       geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
#       geom_line(linewidth = 1, color = color) +
#       labs(y = paste("dbMEM", mem_index), x = "Coordonnée X") +
#       theme_bw() +
#       theme(
#         panel.background = element_rect(fill = 'transparent'),
#         plot.background = element_rect(fill = 'transparent', color = NA),
#         legend.background = element_rect(fill = 'transparent'),
#         legend.box.background = element_rect(fill = 'transparent')
#       )
#   }
#
#   plot_grid(plotlist = plots, ncol = ncol)
# }





# Claude 3.5 Haiku (20241022) ####
# Carré, Allemand
#
osc.mem <- function(dfxy, z, fwd, mem_selection, color = "seagreen4", ncol = 2) {

  require(tidyverse)
  require(cowplot)

  plots <- vector("list", length(mem_selection))

  for (i in 1:length(mem_selection)) {
    plots[[i]] <- ggplot(data = data.frame(x = (1:length(dfxy[, 1])),
                                           y = z[, sort(fwd$order)[i]]),
                         aes(x = x, y = y)) +
      geom_abline(intercept = 0, slope = 0, linetype = "dashed", color = "aquamarine4") +
      geom_line(linewidth = 1, color = color) +
      labs(y = paste("dbMEM", sort(fwd$order)[i]), x = "Coordonnée X") +
      theme_bw() +
      theme(
        panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent')
      )
  }

  plot_grid(plotlist = plots, ncol = ncol)
}
