# Figures du poster capricorne (M1, 2023) : reseaux d'haplotypes COI et carte des foyers.
# Lancer depuis outils/rapports/capricorne : Rscript figures.R
# Donnees : alignements COI du TP (donnees/alb.fas, donnees/clb.fas, non versionnes).
suppressPackageStartupMessages({ library(ape); library(pegas); library(sf); library(ggplot2) })

region_alb <- function(l) {
  r <- rep("Indéterminé", length(l))
  r[grepl("Chine|Cixi|Hefei|Yuncheng|Qingdao|Lingan|Wuhan|Qingtongxia|Xian|Bameng|Langfang|Bazhou|Jingyuan|Huining|Mongolia", l)] <- "Chine"
  r[grepl("Korea|Incheon", l)] <- "Corée"
  r[grepl("^American|^NYC|JerseyCity", l)] <- "Amérique du Nord"
  r[grepl("Gallspach|Ebersberg|Neubiberg|Feldkirchen|Rapagnano|Cornuda|Zurich|Winterthur|Rickenbach|Berikon|MonteCarlo", l)] <- "Europe hors France"
  r[grepl("Furiani|Corse", l)] <- "France : Corse"
  r[grepl("Strasbourg", l)] <- "France : Strasbourg"
  r[grepl("Gien", l)] <- "France : Gien"
  r[grepl("StAnne", l)] <- "France : Ste-Anne-sur-Brivet"
  r
}
region_clb <- function(l) {
  r <- rep("Indéterminé", length(l))
  r[grepl("China", l)] <- "Chine"; r[grepl("Japan", l)] <- "Japon"
  r[grepl("Korea", l)] <- "Corée du Sud"; r[grepl("Italy", l)] <- "Italie"
  r[grepl("France", l)] <- "France : Royan"
  r
}
coul <- c("Chine" = "#B5562B", "Corée" = "#C9A227", "Corée du Sud" = "#C9A227", "Japon" = "#8C5A9E",
          "Amérique du Nord" = "#2F6E9E", "Europe hors France" = "#9DB8A6", "Italie" = "#9DB8A6",
          "France : Corse" = "#6B8E3D", "France : Strasbourg" = "#3E4A42",
          "France : Gien" = "#1A6B3C", "France : Ste-Anne-sur-Brivet" = "#7FC79A", "France : Royan" = "#1A6B3C", "Indéterminé" = "#D9D6CC")

reseau <- function(fichier, exclure, region, sortie, titre) {
  x <- read.dna(file.path("donnees", fichier), "fasta")
  x <- x[!grepl(exclure, rownames(x)), ]
  h <- suppressWarnings(haplotype(x))
  net <- suppressWarnings(haploNet(h))
  reg <- region(rownames(x))
  tab <- t(sapply(attr(h, "index"), function(i) table(factor(reg[i], levels = unique(names(coul))))))
  tab <- tab[, colSums(tab) > 0, drop = FALSE]
  pdf(file.path("figures", sortie), width = 7.2, height = 4.3)
  layout(matrix(1:2, 1), widths = c(5, 2.6))
  par(mar = c(0, 0, 0, 0), family = "sans")
  set.seed(3)
  plot(net, size = 0.9 * sqrt(attr(net, "freq")), pie = tab, bg = coul[colnames(tab)],
       labels = FALSE, show.mutation = 2, threshold = 0, scale.ratio = 1.4, cex = 0.7)
  par(mar = c(0, 0, 0, 0)); plot.new()
  legend("left", legend = colnames(tab), fill = coul[colnames(tab)], border = NA, bty = "n", cex = 0.95, y.intersp = 1.3)
  dev.off()
  cat(sortie, ":", nrow(x), "séquences,", nrow(tab), "haplotypes\n")
  invisible(list(h = h, reg = reg, x = x))
}

alb <- reseau("alb.fas", "^CLB", region_alb, "reseau_alb.pdf", "A. glabripennis (COI, 360 pb)")
clb <- reseau("clb.fas", "OUTGROUP", region_clb, "reseau_clb.pdf", "A. chinensis (COI, 395 pb)")

# controles : haplotype de Gien/Ste-Anne propre a ces deux foyers ; Royan partage un haplotype chinois
g <- which(sapply(attr(alb$h, "index"), function(i) any(grepl("Gien", rownames(alb$x)[i]))))
stopifnot(all(alb$reg[attr(alb$h, "index")[[g]]] %in% c("France : Gien", "France : Ste-Anne-sur-Brivet")))
r <- sapply(attr(clb$h, "index"), function(i) any(grepl("Royan", rownames(clb$x)[i])))
stopifnot(any(sapply(attr(clb$h, "index")[r], function(i) "Chine" %in% clb$reg[i])))

# --- Carte des foyers detectes en France (d'apres DRAAF Centre-Val de Loire, 2019)
dep <- st_read("../../../carte/departements.geojson", quiet = TRUE)
foyers <- data.frame(
  nom = c("Gien (2003)", "Sainte-Anne-sur-Brivet (2004)", "Strasbourg (2008)", "Saint-Louis (2011)",
          "Furiani (2013)", "Divonne-les-Bains (2016)", "Royan (2018)"),
  lon = c(2.63, -2.00, 7.75, 7.56, 9.42, 6.14, -1.03), lat = c(47.69, 47.46, 48.58, 47.59, 42.66, 46.36, 45.62),
  espece = c(rep("A. glabripennis", 6), "A. chinensis"),
  statut = c("actif", "éradiqué en 2010", "éradiqué en 2019", "éradiqué en 2019", "actif", "actif", "actif"))
pts <- st_as_sf(foyers, coords = c("lon", "lat"), crs = 4326)
g <- ggplot() + geom_sf(data = dep, fill = "#F3F1EA", colour = "white", linewidth = 0.2) +
  geom_sf(data = pts, aes(colour = espece, shape = statut == "actif"), size = 3.2, stroke = 1.2) +
  ggrepel::geom_text_repel(data = foyers, aes(lon, lat, label = nom), size = 2.8, colour = "#1D2420",
                           min.segment.length = 0.3, box.padding = 0.5, seed = 2) +
  scale_colour_manual(values = c("A. glabripennis" = "#1A6B3C", "A. chinensis" = "#B5562B"), name = NULL) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), labels = c(`TRUE` = "foyer actif en 2019", `FALSE` = "foyer éradiqué"), name = NULL) +
  coord_sf(xlim = c(-5, 10), ylim = c(41.3, 51.2), expand = FALSE) +
  theme_void(base_size = 10) + theme(legend.position = "bottom", legend.box = "vertical",
                                     legend.text = element_text(face = "italic"), legend.margin = margin())
ggsave("figures/carte_foyers.pdf", g, width = 4.6, height = 5, device = "pdf")
