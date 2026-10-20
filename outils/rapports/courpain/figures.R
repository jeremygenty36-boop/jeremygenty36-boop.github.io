# Figures du compte rendu Courpain (L3, 2021). Lancer depuis outils/rapports/courpain : Rscript figures.R
# Donnees : tableau CAH reorganise du classeur du TP (donnees/releves.csv, non versionne), 45 releves 2018-2021,
# coefficients d'abondance-dominance de Braun-Blanquet ; groupes = ceux retenus en 2021 (19T3b isole).
suppressPackageStartupMessages({ library(vegan); library(ggplot2) })
brut <- read.csv("donnees/releves.csv", check.names = FALSE, colClasses = "character")
grp <- unlist(brut[1, -(1:4)]); tab <- brut[-1, ]
ab <- as.matrix(tab[, -(1:4)]); rownames(ab) <- tab$taxon
# recouvrement median de chaque classe (+, r, i = 0,1 %) ; presence = coefficient non vide
ri <- c("+" = 0.1, "r" = 0.1, "i" = 0.1, "1" = 2.5, "2" = 15, "3" = 37.5, "4" = 62.5, "5" = 87.5)
stopifnot(all(ab[ab != ""] %in% names(ri)))
cov <- matrix(ifelse(ab == "", 0, ri[ab]), nrow(ab), dimnames = dimnames(ab))
pa <- (cov > 0) * 1
milieux <- c(Humide = "Milieu humide", Megaphorbiaie = "Mégaphorbiaie", Sec = "Milieu sec", Isole = "19T3b (isolé)")
coul <- c("Milieu humide" = "#2F6E9E", "Mégaphorbiaie" = "#6B8E3D", "Milieu sec" = "#C9A227", "19T3b (isolé)" = "#B5562B")
theme_c <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        strip.background = element_rect(fill = "#E3EEDC", colour = NA), strip.text = element_text(face = "bold"))
sauve <- function(g, f, h) ggsave(file.path("figures", f), g, width = 6.3, height = h, device = "pdf")

# --- AFC (analyse des correspondances) sur presence-absence, releves x especes
ca <- cca(t(pa))
vp <- round(100 * ca$CA$eig / sum(ca$CA$eig), 2)
sr <- as.data.frame(scores(ca, display = "sites", choices = 1:2, scaling = 1))
sr$milieu <- factor(milieux[grp], levels = milieux); sr$nom <- rownames(sr)
sp <- as.data.frame(scores(ca, display = "species", choices = 1:2, scaling = 1))
g <- ggplot(sr, aes(CA1, CA2)) +
  geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(data = sp, colour = "grey70", size = 0.8) +
  geom_point(aes(colour = milieu), size = 2.4) +
  geom_text(aes(label = nom, colour = milieu), size = 2.2, vjust = -0.9, show.legend = FALSE) +
  scale_colour_manual(values = coul, name = NULL) +
  labs(x = sprintf("Axe 1 (%.1f %%)", vp[1]), y = sprintf("Axe 2 (%.1f %%)", vp[2])) + theme_c
sauve(g, "fig_afc.pdf", 4.6)

# --- CAH des releves : distance de Jaccard, lien moyen (UPGMA)
hc <- hclust(vegdist(t(pa), "jaccard"), "average")
pdf("figures/fig_cah.pdf", width = 6.3, height = 3.6)
par(mar = c(4, 4, 1, 1), family = "sans", cex = 0.8)
dd <- as.dendrogram(hc)
dd <- dendrapply(dd, function(n) {
  if (is.leaf(n)) attr(n, "nodePar") <- list(pch = 19, cex = 0.7, col = coul[milieux[grp[attr(n, "label")]]],
                                             lab.col = coul[milieux[grp[attr(n, "label")]]], lab.cex = 0.75)
  n })
plot(dd, ylab = "Dissimilarité de Jaccard", ylim = c(0, 1))
legend("topright", legend = milieux, col = coul[milieux], pch = 19, bty = "n", cex = 0.85)
dev.off()

# --- Frequences relatives et classes de presence par milieu
gr3 <- c("Humide", "Megaphorbiaie", "Sec")
fi <- sapply(gr3, function(m) rowMeans(pa[, grp == m, drop = FALSE]))
classe <- function(f) cut(f, c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("I", "II", "III", "IV", "V"), include.lowest = FALSE)
hist <- do.call(rbind, lapply(gr3, function(m) {
  f <- fi[fi[, m] > 0, m]; data.frame(milieu = milieux[m], classe = classe(f), row.names = NULL)
}))
hist$milieu <- factor(hist$milieu, levels = milieux[gr3])
g <- ggplot(hist, aes(classe, fill = milieu)) + geom_bar(width = 0.7) + facet_wrap(~milieu, scales = "free_y") +
  scale_fill_manual(values = coul, guide = "none") + scale_x_discrete(drop = FALSE) +
  labs(x = "Classe de présence", y = "Nombre d'espèces") + theme_c
sauve(g, "fig_histogrammes.pdf", 2.6)

# --- Spectres biologiques : presence (DQ, somme des fi) et recouvrement (DRi, somme des Ri moyens)
types <- c(TH = "Thérophytes", HC = "Hémicryptophytes", GE = "Géophytes", NP = "Nanophanérophytes", P = "Phanérophytes")
tb <- tab$type_bio
spec <- do.call(rbind, lapply(gr3, function(m) {
  dq <- tapply(fi[, m], tb, sum); dr <- tapply(rowMeans(cov[, grp == m, drop = FALSE]), tb, sum)
  rbind(data.frame(milieu = milieux[m], mesure = "Présence (DQ)", type = names(dq), pct = 100 * dq / sum(dq), row.names = NULL),
        data.frame(milieu = milieux[m], mesure = "Recouvrement (DRi)", type = names(dr), pct = 100 * dr / sum(dr), row.names = NULL))
}))
spec <- spec[spec$type %in% names(types) & spec$pct > 0, ]
spec$type <- factor(types[spec$type], levels = types)
spec$milieu <- factor(spec$milieu, levels = rev(milieux[gr3]))
g <- ggplot(spec, aes(pct, milieu, fill = type)) + geom_col(width = 0.65) +
  geom_text(aes(label = ifelse(pct >= 6, sprintf("%.0f", pct), "")), position = position_stack(vjust = 0.5),
            colour = "white", size = 2.8) +
  facet_wrap(~mesure) +
  scale_fill_manual(values = c("#C9A227", "#6B8E3D", "#8C5A9E", "#B5562B", "#3E4A42"), name = NULL) +
  labs(x = "Part (%)", y = NULL) + theme_c + guides(fill = guide_legend(nrow = 2))
sauve(g, "fig_spectres.pdf", 2.9)

out <- reshape(spec[, c("milieu", "mesure", "type", "pct")], idvar = c("milieu", "type"), timevar = "mesure", direction = "wide")
out[, 3:4] <- round(out[, 3:4], 1); print(out, row.names = FALSE)
cat("Valeurs propres AFC (%) :", vp[1:3], "\n")
cat("Especes par milieu :", colSums(fi > 0), " ; total :", nrow(pa), "\n")
write.csv(out, "figures/spectres.csv", row.names = FALSE)
