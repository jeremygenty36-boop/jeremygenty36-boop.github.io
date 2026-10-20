# Figures du rapport Sandillon (M1, 2024). Lancer depuis outils/rapports/restauration : Rscript figures.R
# Flore : valeurs reprises du rapport de 2024 (spectres, Ellenberg). Carabes : donnees brutes 2017
# (donnees/carabes.txt, non versionnees ; lignes 1-9 = site 2006, 10-14 = site 2016, 15-19 = temoin).
suppressPackageStartupMessages({ library(vegan); library(ggplot2) })
sites <- c("Témoin", "Friche 2006", "Friche 2016")
coul <- c("Témoin" = "#5B655E", "Friche 2006" = "#B07A2A", "Friche 2016" = "#2F6E9E")
theme_r <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "top",
        strip.background = element_rect(fill = "#F3EADB", colour = NA), strip.text = element_text(face = "bold"))
sauve <- function(g, f, h) ggsave(file.path("figures", f), g, width = 6.3, height = h, device = "pdf")

# --- Flore : spectres biologiques (part des vivaces, %)
spec <- data.frame(site = rep(sites, 2), mesure = rep(c("Présence", "Recouvrement"), each = 3),
                   vivace = c(67.7, 62.5, 60.9, 66.1, 64.1, 82.5))
spec <- rbind(transform(spec, type = "Vivaces", pct = vivace), transform(spec, type = "Annuelles", pct = 100 - vivace))
spec$site <- factor(spec$site, levels = rev(sites))
g <- ggplot(spec, aes(pct, site, fill = type)) + geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.0f %%", pct)), position = position_stack(vjust = 0.5), colour = "white", size = 3) +
  facet_wrap(~mesure) + scale_fill_manual(values = c(Vivaces = "#6B8E3D", Annuelles = "#C9A227"), name = NULL) +
  labs(x = "Part des espèces (%)", y = NULL) + theme_r
sauve(g, "fig_spectres.pdf", 2.6)

# --- Flore : ecogrammes (indices d'Ellenberg moyens)
ell <- data.frame(site = sites, F = c(5, 4, 5), R = c(5, 6, 6), N = c(5, 5, 5))
eco <- rbind(data.frame(site = ell$site, x = ell$F, y = ell$N, axe = "Trophie (N)"),
             data.frame(site = ell$site, x = ell$F, y = ell$R, axe = "pH (R)"))
g <- ggplot(eco, aes(x, y, colour = site)) +
  geom_point(size = 3, position = position_jitter(width = 0.12, height = 0.12, seed = 1)) +
  facet_wrap(~axe) + scale_colour_manual(values = coul, name = NULL) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9) + scale_y_continuous(limits = c(1, 9), breaks = 1:9) +
  labs(x = "Humidité édaphique (F)", y = "Valeur d'Ellenberg") + theme_r
sauve(g, "fig_ecogrammes.pdf", 3)

# --- Carabes
d <- read.table("donnees/carabes.txt", sep = "\t", header = TRUE)
grp <- factor(rep(c("Friche 2006", "Friche 2016", "Témoin"), c(9, 5, 5)), levels = sites)
tot <- rowsum(d, grp)

# rang-abondance
ra <- do.call(rbind, lapply(sites, function(s) {
  a <- sort(unlist(tot[s, ]), decreasing = TRUE); a <- a[a > 0]
  data.frame(site = s, rang = seq_along(a), n = a)
}))
ra$site <- factor(ra$site, levels = sites)
g <- ggplot(ra, aes(rang, n, fill = site)) + geom_col(width = 0.8) + facet_wrap(~site, scales = "free_x") +
  scale_y_log10() + scale_fill_manual(values = coul, guide = "none") +
  labs(x = "Rang de l'espèce", y = "Effectif (échelle log)") + theme_r
sauve(g, "fig_rang.pdf", 2.6)

# accumulation (methode exacte) par site
acc <- do.call(rbind, lapply(sites, function(s) {
  a <- specaccum(d[grp == s, ], method = "exact")
  data.frame(site = s, n = a$sites, S = a$richness, sd = a$sd)
}))
acc$site <- factor(acc$site, levels = sites)
g <- ggplot(acc, aes(n, S, colour = site, fill = site)) +
  geom_ribbon(aes(ymin = S - 1.96 * sd, ymax = S + 1.96 * sd), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
  scale_colour_manual(values = coul, name = NULL) + scale_fill_manual(values = coul, guide = "none") +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Nombre de stations (étoiles de pièges)", y = "Richesse cumulée") + theme_r
sauve(g, "fig_accumulation.pdf", 3.2)

# rarefaction sur les effectifs cumules par site
rar <- do.call(rbind, lapply(sites, function(s) {
  x <- unlist(tot[s, ]); n <- unique(round(seq(1, sum(x), length.out = 80)))
  data.frame(site = s, n = n, S = as.numeric(rarefy(x, n)))
}))
rar$site <- factor(rar$site, levels = sites)
nmin <- min(rowSums(tot))
g <- ggplot(rar, aes(n, S, colour = site)) + geom_line(linewidth = 0.9) +
  geom_vline(xintercept = nmin, linetype = 2, colour = "grey50") +
  scale_colour_manual(values = coul, name = NULL) +
  labs(x = "Nombre d'individus tirés", y = "Richesse attendue") + theme_r
sauve(g, "fig_rarefaction.pdf", 3.2)

# indices : moyenne par station (comme en 2024) et sur les effectifs cumules par site
H <- diversity(d); S <- specnumber(d)
ind <- data.frame(site = sites,
  N = rowSums(tot)[sites], S = specnumber(tot)[sites],
  S_rar = round(as.numeric(rarefy(tot, nmin))[match(sites, rownames(tot))], 1),
  H_station = round(tapply(H, grp, mean)[sites], 2),
  Simpson_station = round(tapply(diversity(d, "simpson"), grp, mean)[sites], 2),
  Pielou_station = round(tapply(H / log(S), grp, mean)[sites], 2),
  H_site = round(diversity(tot)[sites], 2),
  Simpson_site = round(diversity(tot, "simpson")[sites], 2),
  Pielou_site = round((diversity(tot) / log(specnumber(tot)))[sites], 2))
print(ind, row.names = FALSE)
write.csv(ind, "figures/indices_carabes.csv", row.names = FALSE)
stopifnot(identical(as.numeric(ind$S), c(22, 18, 27)))  # richesses du rapport de 2024
