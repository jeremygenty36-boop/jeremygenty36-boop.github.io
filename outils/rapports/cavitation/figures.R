# Courbes de vulnerabilite a la cavitation (TP M1, 2023) : P50 par genotype x traitement
# et figures du rapport. Lancer depuis outils/rapports/cavitation : Rscript figures.R
# Donnees du TP (donnees/, non versionnees) ; modele = fitplc (Weibull, effet aleatoire individu).
suppressPackageStartupMessages({ library(fitplc); library(ggplot2) })

lire <- function(f) {
  d <- read.table(f, sep = "\t", header = TRUE)
  d$WP <- as.numeric(d$WP); d$PLC <- as.numeric(d$PLC)
  d[!is.na(d$WP) & !is.na(d$PLC), ]
}
serre <- lire("donnees/greenhouse.txt")
champ <- lire("donnees/field.txt")
noms <- c(skado = "Skado", koster = "Koster", dellinois = "Dellinois", jp = "Jean Pourpet")

ajuste <- function(d, g, t) {
  s <- d[d$genotype == g & d$treatment == t, ]
  f <- fitplc(s, varnames = c(PLC = "PLC", WP = "WP"), random = individual,
              x = 50, coverage = 0.95, bootci = FALSE)
  cf <- coef(f)
  wp <- seq(min(s$WP), max(s$WP), length.out = 100)
  # fweibull renvoie la conductivite relative ; tensions positives
  plc <- 100 * (1 - fweibull(-wp, cf["SX", "Estimate"], cf["PX", "Estimate"]))
  list(p50 = data.frame(genotype = g, traitement = t, P50 = -cf["PX", "Estimate"],
                        bas = -cf["PX", 3], haut = -cf["PX", 2]),
       courbe = data.frame(genotype = g, traitement = t, WP = wp, PLC = plc),
       points = transform(s, traitement = t))
}

figure <- function(d, gens, trts, couleurs, fichier) {
  r <- lapply(seq_len(length(gens) * length(trts)), function(i)
    ajuste(d, gens[(i - 1) %/% length(trts) + 1], trts[(i - 1) %% length(trts) + 1]))
  p50 <- do.call(rbind, lapply(r, `[[`, "p50"))
  cb  <- do.call(rbind, lapply(r, `[[`, "courbe"))
  pts <- do.call(rbind, lapply(r, `[[`, "points"))
  pts$traitement <- pts$treatment
  for (x in c("p50", "cb", "pts")) {
    v <- get(x); v$genotype <- factor(noms[as.character(v$genotype)], levels = noms[gens])
    v$traitement <- factor(v$traitement, levels = trts); assign(x, v)
  }
  g <- ggplot(pts, aes(WP, PLC, colour = traitement)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 50, ymax = 50, colour = "grey70", linewidth = 0.3) +
    geom_rect(data = p50, aes(xmin = bas, xmax = haut, ymin = -Inf, ymax = Inf, fill = traitement),
              inherit.aes = FALSE, alpha = 0.15) +
    geom_vline(data = p50, aes(xintercept = P50, colour = traitement), linetype = 2, linewidth = 0.4) +
    geom_point(size = 1.2, alpha = 0.7) +
    geom_line(data = cb, linewidth = 0.8) +
    facet_wrap(~genotype, ncol = 2) +
    scale_colour_manual(values = couleurs, name = NULL) +
    scale_fill_manual(values = couleurs, guide = "none") +
    labs(x = "Potentiel hydrique du xylème (MPa)", y = "Perte de conductivité (%)") +
    coord_cartesian(ylim = c(0, 100)) +
    theme_bw(base_size = 10) +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#E8F2EC", colour = NA),
          strip.text = element_text(face = "bold"))
  h <- 2.4 * ceiling(length(gens) / 2) + 0.6
  ggsave(fichier, g, width = 6.3, height = h, device = "pdf")
  p50
}

couleurs <- c(lowN = "#5B655E", highN = "#B5562B", WNF = "#2F6E9E", WF = "#B5562B", DNF = "#C9A227")
res <- rbind(
  figure(serre, c("skado", "koster"), c("lowN", "highN"), couleurs, "figures/fig_serre.pdf"),
  figure(champ, c("dellinois", "koster", "jp", "skado"), c("WNF", "WF"), couleurs, "figures/fig_fertilisation.pdf"),
  figure(champ, c("dellinois", "koster", "jp", "skado"), c("WNF", "DNF"), couleurs, "figures/fig_irrigation.pdf"))
res[, 3:5] <- round(res[, 3:5], 2)
print(res, row.names = FALSE)
write.csv(res, "figures/p50.csv", row.names = FALSE)

# auto-controle : chaque P50 est dans son IC et dans la plage physiologique mesuree
stopifnot(all(res$bas <= res$P50 & res$P50 <= res$haut), all(res$P50 < -0.5 & res$P50 > -4))
