// Carte interactive des extremes climatiques (E-OBS, France hors Corse).
// Donnees : donnees.json (R/02_exporter_site.R) ; contours : departements.geojson.
"use strict";
const $ = id => document.getElementById(id);
const NS = "http://www.w3.org/2000/svg";
const DEMI = 0.05;                       // demi-cote d'une cellule (grille 0,1 deg)
const EN = document.documentElement.lang === "en";      // version anglaise : en.html (lang="en")
const tr = (fr, en) => EN ? en : fr;
const LOC = EN ? "en-GB" : "fr-FR";
// noms, definitions et unites des indices en anglais (donnees.json est en francais)
const IND_EN = {
  TXx: { nom: "TXx: heat extreme", def: "Highest daily maximum temperature from March to November." },
  HWN: { nom: "HWN: heatwaves", unite: "waves", def: "Number of heatwaves from May to September: at least 3 consecutive days above the 90th percentile of daily Tmax (1979-1989 reference)." },
  TNn: { nom: "TNn: cold extreme", def: "Lowest daily minimum temperature from September (previous year) to May." },
  CWN: { nom: "CWN: cold spells", unite: "waves", def: "Number of cold spells from September (previous year) to May: at least 3 consecutive days below the 10th percentile of daily Tmin (1979-1989 reference)." },
  SPEI6: { nom: "SPEI6: drought", def: "6-month SPEI drought index (rainfall minus evapotranspiration), lowest value from March to August. Negative = drier than the 1981-2010 normal." },
  WG10P: { nom: "WG10P: very dry days", def: "Share of days from March to August when the daily rainfall-minus-evapotranspiration balance falls below its monthly 10th percentile (1979-1989 reference). About 10% over the reference period." }
};

// ---------- projection Lambert-93 (EPSG:2154) ----------
const L93 = (() => {
  const a = 6378137, e = 0.0818191910428158, r = Math.PI / 180;
  const m = p => Math.cos(p) / Math.sqrt(1 - e * e * Math.sin(p) ** 2);
  const t = p => Math.tan(Math.PI / 4 - p / 2) / Math.pow((1 - e * Math.sin(p)) / (1 + e * Math.sin(p)), e / 2);
  const p1 = 44 * r, p2 = 49 * r, p0 = 46.5 * r, l0 = 3 * r;
  const n = (Math.log(m(p1)) - Math.log(m(p2))) / (Math.log(t(p1)) - Math.log(t(p2)));
  const F = m(p1) / (n * Math.pow(t(p1), n)), r0 = a * F * Math.pow(t(p0), n);
  return (lon, lat) => {
    const rho = a * F * Math.pow(t(lat * r), n), th = n * (lon * r - l0);
    return [700000 + rho * Math.sin(th), 6600000 + r0 - rho * Math.cos(th)];
  };
})();

// ---------- palettes ----------
const PAL = {
  chaud: ["#fff5d6", "#fed976", "#fd8d3c", "#e31a1c", "#800026"],
  froid: ["#08306b", "#2171b5", "#6baed6", "#c6dbef", "#f7fbff"],
  froid_n: ["#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"],
  sec:   ["#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"],
  sec_n: ["#fbf6e9", "#f6e8c3", "#dfc27d", "#bf812d", "#8c510a", "#543005"],
  tendance_t: ["#2166ac", "#92c5de", "#f7f7f7", "#f4a582", "#b2182b"],
  mort: ["#fff7ec", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000"],
  tendance_sec: ["#8c510a", "#d8b365", "#f7f7f7", "#5ab4ac", "#01665e"]
};
const hex = h => [1, 3, 5].map(i => parseInt(h.slice(i, i + 2), 16));
function couleur(pal, x, estompe = 0) {   // x dans [0,1] ; estompe = part de gris clair melangee
  x = Math.min(1, Math.max(0, x)); const k = x * (pal.length - 1), i = Math.min(pal.length - 2, Math.floor(k)), f = k - i;
  const a = hex(pal[i]), b = hex(pal[i + 1]), g = [232, 232, 228];
  return `rgb(${a.map((v, j) => Math.round((v + (b[j] - v) * f) * (1 - estompe) + g[j] * estompe)).join(",")})`;
}
const fmt = (v, d = 1) => v == null || Number.isNaN(v) ? "n.d." : v.toLocaleString(LOC, { minimumFractionDigits: d, maximumFractionDigits: d });

// ---------- etat ----------
const E = { couche: "climat", esp: "TOUT", depSel: null, courbeEsp: null,
  indice: "TXx", mode: "annee", annee: 2024, periode: 0, sel: null, D: null, deps: null };
const ESP_NOM = EN ? { TOUT: "All species", QUPE: "Sessile oak", QURO: "Pedunculate oak", FASY: "Beech", PISY: "Scots pine",
  PINI: "Black pine", ABAL: "Silver fir", PIAB: "Norway spruce", BEPE: "Silver birch" } :
  { TOUT: "Toutes essences", QUPE: "Chêne sessile", QURO: "Chêne pédonculé", FASY: "Hêtre", PISY: "Pin sylvestre",
  PINI: "Pin noir", ABAL: "Sapin pectiné", PIAB: "Épicéa commun", BEPE: "Bouleau verruqueux" };
const COL_BASE = { "SAF-8": "#D95F02", "EOB-10": "#1B9E77", "CHE-1": "#7570B3", "SAF-DS": "#FC8D62", "EOB-DS": "#66C2A5" };
const PERIODES = [[1979, 1988], [1989, 1998], [1999, 2008], [2009, 2018], [2015, 2024]];
// palettes de valeur et de tendance par indice (tendance : couleur selon le sens physique)
const STYLE = {
  TXx:   { pal: "chaud",   tend: "tendance_t",   coul: "#d7301f", record: tr("record de chaleur", "record heat") },
  HWN:   { pal: "chaud",   tend: "tendance_t",   coul: "#d7301f", record: tr("année record", "record year") },
  TNn:   { pal: "froid",   tend: "tendance_t",   coul: "#2171b5", record: tr("record de froid", "record cold") },
  CWN:   { pal: "froid_n", tend: "tendance_t_inv", coul: "#2171b5", record: tr("année record", "record year") },
  SPEI6: { pal: "sec",     tend: "tendance_sec", coul: "#8c510a", record: tr("sécheresse record", "record drought") },
  WG10P: { pal: "sec_n",   tend: "tendance_sec_inv", coul: "#8c510a", record: "année record" }
};
PAL.tendance_t_inv = PAL.tendance_t.slice().reverse(); PAL.tendance_sec_inv = PAL.tendance_sec.slice().reverse();
const AIDE_PERIODE = { max: tr("valeur la plus forte de la période", "highest value of the period"), min: tr("valeur la plus basse de la période", "lowest value of the period"),
  sum: tr("total sur la période", "total over the period"), mean: tr("moyenne sur la période", "mean over the period") };

// valeur d'une cellule selon le mode courant
function valeur(ind, c) {
  const I = E.D.indices[ind], an = E.D.annees;
  if (E.mode === "annee") { const v = I.valeurs[an.indexOf(E.annee)][c]; return v == null ? null : v / 100; }
  if (E.mode === "periode") {
    const [a, b] = PERIODES[E.periode], w = [];
    for (let j = an.indexOf(a); j <= an.indexOf(b); j++) { const v = I.valeurs[j][c]; if (v == null) return null; w.push(v / 100); }  // NA reste NA
    return agreger(I.agg, w);
  }
  const v = I.tendance[c]; return v == null ? null : v / 100;
}
function agreger(agg, w) {               // regle du stage : max, min, somme ou moyenne selon l'indice
  if (agg === "max") return Math.max(...w); if (agg === "min") return Math.min(...w);
  const s = w.reduce((a, b) => a + b, 0); return agg === "sum" ? s : s / w.length;
}
function echelle(ind) {
  if (E.couche === "mortalite") {
    const v = Object.values(E.O.par_dep[E.esp]).map(x => x.pct).filter(x => x != null).sort((a, b) => a - b);
    return { lo: 0, hi: Math.max(5, Math.ceil(v[Math.floor(v.length * .95)] / 5) * 5), pal: PAL.mort, unite: tr("% de placettes avec au moins un arbre mort", "% of plots with at least one dead tree") };
  }
  const I = E.D.indices[ind];
  if (E.mode === "tendance") {
    const t = I.tendance.filter(v => v != null).map(v => Math.abs(v / 100)).sort((a, b) => a - b);
    const m = t[Math.floor(t.length * .98)] || 1;
    return { lo: -m, hi: m, pal: PAL[STYLE[ind].tend], unite: (I.unite || "SPEI") + tr(" par décennie", " per decade") };
  }
  if (ind === "SPEI6") { const m = Math.max(Math.abs(I.echelle[0]), Math.abs(I.echelle[1])); return { lo: -m, hi: m, pal: PAL.sec, unite: tr("SPEI (sans unité)", "SPEI (no unit)") }; }
  let { echelle: [lo, hi] } = I;
  if (E.mode === "periode" && (I.agg === "sum" || I.agg === "mean")) [lo, hi] = echellePeriodes(ind);  // totaux ou moyennes decennaux
  return { lo, hi, pal: PAL[STYLE[ind].pal], unite: I.unite + (E.mode === "periode" && I.agg === "sum" ? tr(" sur 10 ans", " over 10 years") : "") };
}
const _ech = {};
function echellePeriodes(ind) {           // quantiles 2-98 % des valeurs de periode, toutes periodes confondues (fixe)
  if (_ech[ind]) return _ech[ind];
  const mode = E.mode, per = E.periode, v = [];
  E.mode = "periode";
  PERIODES.forEach((_, k) => { E.periode = k; E.D.cellules.forEach((_, c) => { const x = valeur(ind, c); if (x != null) v.push(x); }); });
  E.mode = mode; E.periode = per; v.sort((a, b) => a - b);
  return (_ech[ind] = [v[Math.floor(v.length * .02)], v[Math.floor(v.length * .98)]]);
}

// ---------- carte ----------
const cv = $("carte"), ctx = cv.getContext("2d");
let proj, polys = [];
function preparer() {
  const pts = E.D.cellules.flatMap(([lon, lat]) => [L93(lon - DEMI, lat - DEMI), L93(lon + DEMI, lat + DEMI), L93(lon - DEMI, lat + DEMI), L93(lon + DEMI, lat - DEMI)]);
  const xs = pts.map(p => p[0]), ys = pts.map(p => p[1]);
  const x0 = Math.min(...xs), x1 = Math.max(...xs), y0 = Math.min(...ys), y1 = Math.max(...ys);
  const marge = 24, W = cv.width - 2 * marge, H = cv.height - 2 * marge, k = Math.min(W / (x1 - x0), H / (y1 - y0));
  const ox = marge + (W - k * (x1 - x0)) / 2, oy = marge + (H - k * (y1 - y0)) / 2;
  proj = (lon, lat) => { const [x, y] = L93(lon, lat); return [ox + (x - x0) * k, oy + (y1 - y) * k]; };
  proj.k = k;                              // pixels par metre
  E.depPaths = E.deps.features.map(f => {
    const g = f.geometry, P = g.type === "Polygon" ? [g.coordinates] : g.coordinates, path = new Path2D();
    P.forEach(poly => poly.forEach(ring => ring.forEach(([lon, lat], i) => { const [x, y] = proj(lon, lat); i ? path.lineTo(x, y) : path.moveTo(x, y); })));
    return { code: f.properties.code, nom: f.properties.nom, path };
  });
  polys = E.D.cellules.map(([lon, lat]) => [[lon - DEMI, lat - DEMI], [lon + DEMI, lat - DEMI], [lon + DEMI, lat + DEMI], [lon - DEMI, lat + DEMI]].map(p => proj(...p)));
}
function dessiner() {
  const sc = echelle(E.indice), I = E.D.indices[E.indice];
  ctx.clearRect(0, 0, cv.width, cv.height);
  if (E.couche === "mortalite") {
    const O = E.O.par_dep[E.esp];
    E.depPaths.forEach(d => {
      const v = O[d.code] && O[d.code].pct;
      ctx.fillStyle = v == null ? "#d9d9d6" : couleur(sc.pal, (v - sc.lo) / (sc.hi - sc.lo)); ctx.fill(d.path);
      ctx.strokeStyle = "rgba(40,40,40,.5)"; ctx.lineWidth = 0.7; ctx.stroke(d.path);
    });
    const s = E.depPaths.find(d => d.code === E.depSel);
    if (s) { ctx.strokeStyle = "#000"; ctx.lineWidth = 2.4; ctx.stroke(s.path); }
    nord(); echelleKm(); legende(sc); return;
  }
  polys.forEach((p, c) => {
    const v = valeur(E.indice, c);
    ctx.beginPath(); p.forEach(([x, y], i) => i ? ctx.lineTo(x, y) : ctx.moveTo(x, y)); ctx.closePath();
    const est = E.mode === "tendance" && !(I.p[c] < 0.05) ? 0.65 : 0;
    ctx.fillStyle = v == null ? "#bbb" : couleur(sc.pal, (v - sc.lo) / (sc.hi - sc.lo), est);
    ctx.fill(); ctx.strokeStyle = ctx.fillStyle; ctx.lineWidth = 0.6; ctx.stroke();
  });
  // contours des departements
  ctx.strokeStyle = "rgba(40,40,40,.45)"; ctx.lineWidth = 0.6;
  E.deps.features.forEach(f => {
    const g = f.geometry, P = g.type === "Polygon" ? [g.coordinates] : g.coordinates;
    P.forEach(poly => poly.forEach(ring => { ctx.beginPath(); ring.forEach(([lon, lat], i) => { const [x, y] = proj(lon, lat); i ? ctx.lineTo(x, y) : ctx.moveTo(x, y); }); ctx.stroke(); }));
  });
  // cellule selectionnee
  if (E.sel != null) { const p = polys[E.sel]; ctx.strokeStyle = "#000"; ctx.lineWidth = 2.2; ctx.beginPath(); p.forEach(([x, y], i) => i ? ctx.lineTo(x, y) : ctx.moveTo(x, y)); ctx.closePath(); ctx.stroke(); }
  nord(); echelleKm(); legende(sc);
}
function nord() {                          // fleche du nord en haut a droite
  const x = cv.width - 34, y = 30;
  ctx.fillStyle = getComputedStyle(document.body).color; ctx.beginPath(); ctx.moveTo(x, y - 18); ctx.lineTo(x + 9, y + 10); ctx.lineTo(x, y + 4); ctx.lineTo(x - 9, y + 10); ctx.closePath(); ctx.fill();
  ctx.font = "bold 13px sans-serif"; ctx.textAlign = "center"; ctx.fillText("N", x, y + 26);
}
function echelleKm() {                     // echelle en bas a droite (100 km)
  const L = 100000 * proj.k, x1 = cv.width - 24, x0 = x1 - L, y = cv.height - 22;
  ctx.fillStyle = ctx.strokeStyle = getComputedStyle(document.body).color; ctx.lineWidth = 2;
  ctx.beginPath(); ctx.moveTo(x0, y - 5); ctx.lineTo(x0, y); ctx.lineTo(x1, y); ctx.lineTo(x1, y - 5); ctx.stroke();
  ctx.font = "12px sans-serif"; ctx.textAlign = "center"; ctx.fillText("100 km", (x0 + x1) / 2, y - 8);
}
function legende(sc) {
  const n = 60, stops = Array.from({ length: n }, (_, i) => couleur(sc.pal, i / (n - 1))).join(",");
  const dec = E.couche === "mortalite" ? 0 : E.mode === "tendance" || E.indice === "SPEI6" ? 2 : 1;
  const ticks = [0, .25, .5, .75, 1].map(f => `<span style="position:absolute;left:${f * 100}%;transform:translateX(${f === 0 ? "0" : f === 1 ? "-100%" : "-50%"})">${fmt(sc.lo + f * (sc.hi - sc.lo), dec)}</span>`).join("");
  const sec = E.couche === "climat" && E.indice === "SPEI6" && E.mode !== "tendance" ? `<span>${tr("← plus sec · plus humide →", "← drier · wetter →")}</span>` : "";
  $("legende").innerHTML = `<div style="height:12px;border-radius:6px;background:linear-gradient(90deg,${stops})"></div>
    <div style="position:relative;height:18px;font-size:12px">${ticks}</div>
    <div style="display:flex;justify-content:space-between;font-size:12px;color:var(--mut)"><span>${sc.unite}${E.couche === "climat" && E.mode === "tendance" ? tr(" · estompé = non significatif", " · faded = not significant") : ""}</span>${sec}</div>`;
}

// survol / clic : cellule sous le pointeur (recherche du centre le plus proche)
function cellule(ev) {
  const r = cv.getBoundingClientRect(), x = (ev.clientX - r.left) * cv.width / r.width, y = (ev.clientY - r.top) * cv.height / r.height;
  let best = null, d = Infinity;
  polys.forEach((p, c) => { const cx = (p[0][0] + p[2][0]) / 2, cy = (p[0][1] + p[2][1]) / 2, dd = (cx - x) ** 2 + (cy - y) ** 2; if (dd < d) { d = dd; best = c; } });
  const pas = Math.abs(polys[best][2][1] - polys[best][0][1]);
  return d < pas * pas ? best : null;
}
const nomDep = code => (E.deps.features.find(f => f.properties.code === code) || { properties: { nom: code } }).properties.nom;
function departement(ev) {
  const r = cv.getBoundingClientRect(), x = (ev.clientX - r.left) * cv.width / r.width, y = (ev.clientY - r.top) * cv.height / r.height;
  return E.depPaths.find(d => ctx.isPointInPath(d.path, x, y));
}
cv.addEventListener("mousemove", ev => {
  if (E.couche === "mortalite") {
    const d = departement(ev), tip = $("infobulle");
    if (!d) { tip.hidden = true; return; }
    const o = E.O.par_dep[E.esp][d.code];
    tip.hidden = false; tip.style.left = ev.offsetX + "px"; tip.style.top = ev.offsetY + "px";
    tip.textContent = `${d.nom} (${d.code}) : ` + (!o ? tr("aucune placette", "no plots") : o.pct == null ? tr(`effectif trop faible (${o.n} placettes)`, `too few plots (${o.n})`) : tr(`${fmt(o.pct, 1)} % (${o.morts} sur ${o.n} placettes)`, `${fmt(o.pct, 1)}% (${o.morts} of ${o.n} plots)`));
    return;
  }
  const c = cellule(ev), tip = $("infobulle");
  if (c == null) { tip.hidden = true; return; }
  const [lon, lat, dep] = E.D.cellules[c], v = valeur(E.indice, c), sc = echelle(E.indice);
  tip.hidden = false; tip.style.left = ev.offsetX + "px"; tip.style.top = ev.offsetY + "px";
  tip.textContent = `${nomDep(dep)} (${dep}) · ${fmt(lat, 2)}° N, ${fmt(lon, 2)}° E : ${fmt(v, 2)} ${sc.unite}`;
});
cv.addEventListener("mouseleave", () => $("infobulle").hidden = true);
cv.addEventListener("click", ev => {
  if (E.couche === "mortalite") { const d = departement(ev); E.depSel = d ? d.code : null; dessiner(); grapheMort(); return; }
  const c = cellule(ev); E.sel = c; dessiner(); graphe();
});

// ---------- graphique : moyenne France + cellule choisie ----------
function graphe() {
  const svg = $("graphe"), I = E.D.indices[E.indice], an = E.D.annees;
  const W = 520, H = 300, m = { g: 44, d: 12, h: 14, b: 30 };
  const fr = I.france, cel = E.sel == null ? null : an.map((_, j) => I.valeurs[j][E.sel] == null ? null : I.valeurs[j][E.sel] / 100);
  const tout = fr.concat(cel || []).filter(v => v != null);
  let lo = Math.min(...tout), hi = Math.max(...tout); const pad = (hi - lo) * .08 || 1, positif = lo >= 0; lo -= pad; hi += pad;
  if (positif) lo = Math.max(lo, 0);       // comptes et pourcentages : pas d'axe sous zero
  const X = a => m.g + (a - an[0]) / (an[an.length - 1] - an[0]) * (W - m.g - m.d), Y = v => H - m.b - (v - lo) / (hi - lo) * (H - m.h - m.b);
  svg.replaceChildren();
  const el = (t, a) => { const e = document.createElementNS(NS, t); Object.entries(a).forEach(([k, v]) => e.setAttribute(k, v)); svg.append(e); return e; };
  for (let i = 0; i <= 4; i++) { const v = lo + i * (hi - lo) / 4; el("line", { x1: m.g, x2: W - m.d, y1: Y(v), y2: Y(v), stroke: "currentColor", "stroke-opacity": .12 }); el("text", { x: m.g - 6, y: Y(v) + 4, "text-anchor": "end" }).textContent = fmt(v, 1); }
  [1980, 1990, 2000, 2010, 2020].forEach(a => el("text", { x: X(a), y: H - 10, "text-anchor": "middle" }).textContent = a);
  if (E.mode === "annee") el("line", { x1: X(E.annee), x2: X(E.annee), y1: m.h, y2: H - m.b, stroke: "currentColor", "stroke-opacity": .35, "stroke-dasharray": "3 3" });
  if (E.mode === "periode") { const [a, b] = PERIODES[E.periode]; el("rect", { x: X(a), y: m.h, width: X(b) - X(a), height: H - m.h - m.b, fill: "currentColor", "fill-opacity": .06 }); }
  const ligne = (s, coul, w) => { let d = ""; s.forEach((v, j) => { if (v == null) return; d += (d && s[j - 1] != null ? "L" : "M") + X(an[j]).toFixed(1) + "," + Y(v).toFixed(1); }); el("path", { d, fill: "none", stroke: coul, "stroke-width": w }); };
  const reg = s => { const P = s.map((v, j) => [an[j], v]).filter(p => p[1] != null), n = P.length, mx = P.reduce((a, p) => a + p[0], 0) / n, my = P.reduce((a, p) => a + p[1], 0) / n;
    const b = P.reduce((a, p) => a + (p[0] - mx) * (p[1] - my), 0) / P.reduce((a, p) => a + (p[0] - mx) ** 2, 0); return { b, a: my - b * mx }; };
  const coul = STYLE[E.indice].coul;
  ligne(fr, cel ? "#9aa29c" : coul, cel ? 1.5 : 2.2);
  const rf = reg(fr); el("line", { x1: X(an[0]), x2: X(an[an.length - 1]), y1: Y(rf.a + rf.b * an[0]), y2: Y(rf.a + rf.b * an[an.length - 1]), stroke: cel ? "#9aa29c" : coul, "stroke-dasharray": "5 4", "stroke-width": 1.2 });
  if (cel) { ligne(cel, coul, 2.2); const rc = reg(cel); el("line", { x1: X(an[0]), x2: X(an[an.length - 1]), y1: Y(rc.a + rc.b * an[0]), y2: Y(rc.a + rc.b * an[an.length - 1]), stroke: coul, "stroke-dasharray": "5 4", "stroke-width": 1.2 }); }
  const [lon, lat, dep] = E.sel == null ? [] : E.D.cellules[E.sel];
  $("titre-graphe").textContent = cel ? `${I.nom}${tr(" :", ":")} ${nomDep(dep)} (${fmt(lat, 2)}° N, ${fmt(lon, 2)}° E)` : tr(`${I.nom} : moyenne France`, `${I.nom}: France average`);
  $("aide-graphe").textContent = cel ? tr("En couleur : le lieu choisi ; en gris : la moyenne France. Pointillés : tendance linéaire.", "In colour: the selected location; in grey: the France average. Dashed: linear trend.") : tr("Cliquez sur la carte pour afficher l'évolution d'un lieu. Pointillés : tendance linéaire.", "Click the map to show a location's time series. Dashed: linear trend.");
  const s = cel || fr, r = reg(s), u = I.unite || "", vals = s.map((v, j) => [v, an[j]]).filter(p => p[0] != null);
  const ext = vals.reduce((a, p) => (I.sens === "haut" ? p[0] > a[0] : p[0] < a[0]) ? p : a);
  const nonsig = cel && !(I.p[E.sel] < 0.05) ? tr(" (non significative)", " (not significant)") : "";
  const RECORD = STYLE[E.indice].record;
  let c3;
  if (E.mode === "annee") c3 = [`${fmt(s[an.indexOf(E.annee)], 1)} ${u}`, tr(`en ${E.annee}`, `in ${E.annee}`)];
  else if (E.mode === "periode") {
    const [a, b] = PERIODES[E.periode], w = s.slice(an.indexOf(a), an.indexOf(b) + 1);
    const v = w.some(x => x == null) ? null : agreger(I.agg, w);
    c3 = [`${fmt(v, 1)} ${u}`, `${{ max: "maximum", min: "minimum", sum: "total", mean: tr("moyenne", "mean") }[I.agg]} ${a}-${b}`];
  } else if (cel) c3 = [`p = ${fmt(I.p[E.sel], 3)}`, I.p[E.sel] < 0.05 ? tr("tendance significative", "significant trend") : tr("tendance non significative", "non-significant trend")];
  else { const ok = I.p.filter(x => x != null), sig = ok.filter(x => x < 0.05).length; c3 = [`${Math.round(100 * sig / ok.length)}${tr(" %", "%")}`, tr("du territoire en tendance significative", "of the territory with a significant trend")]; }
  $("chiffres").innerHTML = [
    [`${r.b * 10 > 0 ? "+" : ""}${fmt(r.b * 10, 2)} ${u}`, tr("tendance par décennie", "trend per decade") + nonsig],
    [`${fmt(ext[0], 1)} ${u}`, `${RECORD} (${ext[1]})`],
    c3
  ].map(([b, t]) => `<div class="chiffre"><b>${b}</b><small>${t}</small></div>`).join("");
}

// ---------- couche mortalite observee : barres par essence ----------
function grapheMort() {
  const svg = $("graphe"), O = E.O, dep = E.depSel, d = dep ? E.depPaths.find(x => x.code === dep) : null;
  const esp = Object.keys(ESP_NOM).filter(k => k !== "TOUT");
  const lignes = esp.map(k => ({ k, fr: O.france[k], v: dep && O.par_dep[k][dep] ? O.par_dep[k][dep] : null }));
  const W = 520, H = 300, g = 130, dd = 46, h = (H - 30) / esp.length, max = Math.max(20, ...lignes.map(l => Math.max(l.fr, l.v && l.v.pct || 0)));
  const X = v => g + v / max * (W - g - dd);
  svg.replaceChildren();
  const el = (t, a) => { const e = document.createElementNS(NS, t); Object.entries(a).forEach(([k, v]) => e.setAttribute(k, v)); svg.append(e); return e; };
  lignes.forEach((l, i) => {
    const y = 8 + i * h;
    el("text", { x: g - 8, y: y + h * .55, "text-anchor": "end" }).textContent = ESP_NOM[l.k];
    const val = d ? (l.v && l.v.pct != null ? l.v.pct : null) : l.fr;
    if (val != null) el("rect", { x: g, y: y + h * .2, width: X(val) - g, height: h * .55, fill: couleur(PAL.mort, .25 + .7 * val / max), rx: 3 });
    if (d) el("line", { x1: X(l.fr), x2: X(l.fr), y1: y + h * .08, y2: y + h * .87, stroke: "currentColor", "stroke-width": 2 });
    el("text", { x: (val != null ? X(val) : g) + 6, y: y + h * .6 }).textContent = val != null ? fmt(val, 1) + tr(" %", "%") : (l.v ? tr(`trop peu (${l.v.n})`, `too few (${l.v.n})`) : tr("absente", "absent"));
  });
  el("text", { x: g, y: H - 6 }).textContent = d ? tr("barre : département · trait : moyenne France", "bar: département · line: France average") : tr("moyenne France", "France average");
  $("titre-graphe").textContent = d ? tr(`Mortalité observée : ${d.nom} (${d.code})`, `Observed mortality: ${d.nom} (${d.code})`) : tr("Mortalité observée : France", "Observed mortality: France");
  $("aide-graphe").textContent = tr("Part des placettes 2015-2023 où au moins un arbre de l'essence est mort. ", "Share of 2015-2023 plots where at least one tree of the species died. ") + (d ? "" : tr("Cliquez sur un département pour le comparer à la moyenne.", "Click a département to compare it with the average."));
  const o = dep ? O.par_dep.TOUT[dep] : null;
  $("chiffres").innerHTML = [
    [`${fmt(O.france.TOUT, 1)}${tr(" %", "%")}`, tr("France, toutes essences", "France, all species")],
    [o && o.pct != null ? `${fmt(o.pct, 1)}${tr(" %", "%")}` : "n.d.", d ? tr(`${d.nom}, toutes essences`, `${d.nom}, all species`) : tr("choisissez un département", "choose a département")],
    [`${(o ? o.n : O.effectifs.TOUT).toLocaleString(LOC)}`, o ? tr("placettes × essences dans le département", "plot × species pairs in the département") : tr("placettes × essences (France)", "plot × species pairs (France)")]
  ].map(([b, t]) => `<div class="chiffre"><b>${b}</b><small>${t}</small></div>`).join("");
}

// ---------- courbe de reponse : essence choisie dans le panneau mortalite ----------
function courbe() {
  const zone = $("courbe-zone"), k = E.courbeEsp;
  if (!k || !E.C) { zone.hidden = true; return; }
  zone.hidden = false;
  const I = E.D.indices[E.indice], L = (E.C[E.indice] || {})[k] || [], svg = $("courbe");
  $("courbe-titre").textContent = tr(`${ESP_NOM[k]} : mortalité prédite selon ${E.indice}`, `${ESP_NOM[k]}: predicted mortality vs ${E.indice}`);
  svg.replaceChildren();
  const el = (t, a) => { const e = document.createElementNS(NS, t); Object.entries(a).forEach(([q, v]) => e.setAttribute(q, v)); svg.append(e); return e; };
  if (!L.length) { el("text", { x: 260, y: 120, "text-anchor": "middle" }).textContent = tr(`${E.indice} n'est retenu pour cette essence avec aucune base : pas de courbe.`, `${E.indice} is not retained for this species with any dataset: no curve.`); return; }
  const ok = L.filter(c => !c.instable), xs = ok.flatMap(c => c.x), ys = ok.flatMap(c => c.y);
  const W = 520, H = 250, m = { g: 46, d: 110, h: 12, b: 32 };
  const x0 = Math.min(...xs), x1 = Math.max(...xs), y0 = 0, y1 = Math.max(...ys) * 1.1 || 1;
  const X = v => m.g + (v - x0) / (x1 - x0) * (W - m.g - m.d), Y = v => H - m.b - (v - y0) / (y1 - y0) * (H - m.h - m.b);
  for (let i = 0; i <= 4; i++) { const v = y0 + i * (y1 - y0) / 4; el("line", { x1: m.g, x2: W - m.d, y1: Y(v), y2: Y(v), stroke: "currentColor", "stroke-opacity": .12 }); el("text", { x: m.g - 6, y: Y(v) + 4, "text-anchor": "end" }).textContent = fmt(v, 1); }
  for (let i = 0; i <= 4; i++) { const v = x0 + i * (x1 - x0) / 4; el("text", { x: X(v), y: H - 14, "text-anchor": "middle" }).textContent = fmt(v, 1); }
  el("text", { x: (m.g + W - m.d) / 2, y: H - 1, "text-anchor": "middle" }).textContent = `${E.indice}${I.unite ? " (" + I.unite + ")" : ""}`;
  el("text", { x: 4, y: 10 }).textContent = tr("% d'arbres morts", "% dead trees");
  ok.forEach((c, i) => {
    el("path", { d: c.x.map((v, j) => (j ? "L" : "M") + X(v).toFixed(1) + "," + Y(c.y[j]).toFixed(1)).join(""), fill: "none", stroke: COL_BASE[c.base], "stroke-width": 2.2 });
    el("line", { x1: W - m.d + 12, x2: W - m.d + 30, y1: m.h + 8 + i * 18, y2: m.h + 8 + i * 18, stroke: COL_BASE[c.base], "stroke-width": 3 });
    el("text", { x: W - m.d + 36, y: m.h + 12 + i * 18 }).textContent = c.base;
  });
  if (ok.length < L.length) el("text", { x: W - m.d + 12, y: m.h + 12 + ok.length * 18 }).textContent = tr(`${L.length - ok.length} instable(s) masquée(s)`, `${L.length - ok.length} unstable hidden`);
}

// ---------- lien avec la mortalite forestiere (modeles du stage) ----------
function mortalite() {
  const M = E.M, box = $("mortalite"); if (!M) return;
  const L = M.indices[E.indice], max = Math.max(5, ...L.map(x => x.ir_max)), coul = STYLE[E.indice].coul;
  box.replaceChildren(...L.map(x => {
    const row = document.createElement("div"); row.className = "mrow" + (E.courbeEsp === x.code ? " choisie" : "");
    row.title = tr("Cliquer pour voir la courbe de réponse", "Click to see the response curve"); row.onclick = () => { E.courbeEsp = E.courbeEsp === x.code ? null : x.code; mortalite(); };
    const sens = x.n_retenu === 0 ? ["nul", tr("non retenu", "not retained")] : x.ir_moy < 0.1 ? ["nul", tr("négligeable", "negligible")] :
      x.sens === "+" ? ["plus", tr(`▲ hausse = plus de mortalité (${x.n_sens}/5 bases)`, `▲ increase = more mortality (${x.n_sens}/5 datasets)`)] : ["moins", tr(`▼ hausse = moins de mortalité (${x.n_sens}/5 bases)`, `▼ increase = less mortality (${x.n_sens}/5 datasets)`)];
    row.innerHTML = `<span>${ESP_NOM[x.code] || x.nom}</span>
      <span class="mbar" title="${x.par_base.map(b => `${b.base}${tr(" :", ":")} ${fmt(b.ir, 1)}${tr(" %", "%")}`).join(" · ")}">
        <span class="moy" style="width:${100 * x.ir_moy / max}%;background:${coul}"></span>
        <span class="eten" style="left:${100 * x.ir_min / max}%;width:${Math.max(0.5, 100 * (x.ir_max - x.ir_min) / max)}%"></span></span>
      <span><span class="mval">${fmt(x.ir_moy, 1)}${tr(" %", "%")}</span> <span class="sens ${sens[0]}">${sens[1]}</span></span>`;
    return row;
  }));
  const NOTES = EN ? {
    SPEI6: "SPEI6 was almost never retained by the internship models. As the repository's ERRATA explains, the report version aggregated it as a March-August mean, which may have smoothed out droughts: this result does not show that drought does not matter.",
    WG10P: "Drought-duration index: it weighs less than 1% on average for each species, and its weight varies between climate datasets (up to 2.7% for Scots pine with a single dataset).",
    CWN: "In the models, fewer cold spells go with higher mortality, especially for silver fir and Scots pine. Yet their number is falling (see Trend mode): a decline that points towards higher mortality.",
    TXx: "This is most often the dominant extreme: for black pine and Norway spruce, it is the top index with all 5 datasets."
  } : {
    SPEI6: "Le SPEI6 n'a presque jamais été retenu par les modèles du stage. Comme l'indique l'ERRATA du dépôt, la version du rapport l'agrégeait par une moyenne de mars à août, ce qui a pu lisser les sécheresses : ce résultat ne permet donc pas de conclure que la sécheresse ne compte pas.",
    WG10P: "Indice de durée de la sécheresse : il pèse moins de 1 % en moyenne pour chaque essence, et son poids varie d'une base climatique à l'autre (jusqu'à 2,7 % pour le pin sylvestre avec une seule base).",
    CWN: "Dans les modèles, moins de vagues de froid va avec plus de mortalité, surtout pour le sapin pectiné et le pin sylvestre. Or leur nombre diminue (voir le mode Tendance) : une baisse qui va dans le sens d'une hausse de la mortalité.",
    TXx: "C'est l'extrême le plus souvent dominant : pour le pin noir et l'épicéa, il est le premier indice avec les 5 bases."
  };
  $("mortalite-note").textContent = (NOTES[E.indice] ? NOTES[E.indice] + " " : "") +
    tr("Ordre de grandeur : les extrêmes climatiques pèsent en moyenne 8,2 % de l'explication de la mortalité, contre environ 92 % pour la structure du peuplement et le sol. Survolez une barre pour le détail par base, cliquez sur une essence pour sa courbe de réponse.", "For scale: climate extremes account for 8.2% of the explained mortality on average, against about 92% for stand structure and soil. Hover over a bar for the detail by dataset, click a species for its response curve.");
  courbe();
}

// ---------- controles ----------
function majTout() {
  const mo = E.couche === "mortalite";
  document.querySelectorAll("#choix-couche button").forEach(b => b.classList.toggle("actif", b.dataset.couche === E.couche));
  $("ctl-climat").hidden = mo; $("ctl-mort").hidden = !mo; $("mortalite-zone").hidden = mo || !E.M; $("mort-obs-info").hidden = !mo;
  $("ctl-annee").hidden = mo || E.mode !== "annee"; $("ctl-periode").hidden = mo || E.mode !== "periode";
  if (mo) { $("definition").textContent = ""; dessiner(); grapheMort(); return; }
  $("periode-aide").textContent = AIDE_PERIODE[E.D.indices[E.indice].agg];
  $("definition").textContent = E.D.indices[E.indice].def;
  document.querySelectorAll("#choix-indice button").forEach(b => b.classList.toggle("actif", b.dataset.ind === E.indice));
  document.querySelectorAll("#choix-mode button").forEach(b => b.classList.toggle("actif", b.dataset.mode === E.mode));
  dessiner(); graphe(); mortalite();
}
let anim = null;
$("annee").oninput = e => { E.annee = +e.target.value; $("annee-txt").textContent = E.annee; dessiner(); graphe(); };
$("lecture").onclick = () => {
  if (anim) { clearInterval(anim); anim = null; $("lecture").textContent = "▶"; return; }
  $("lecture").textContent = "❚❚"; if (E.annee >= 2024) E.annee = 1978;
  anim = setInterval(() => { E.annee++; $("annee").value = E.annee; $("annee-txt").textContent = E.annee; dessiner(); graphe();
    if (E.annee >= 2024) { clearInterval(anim); anim = null; $("lecture").textContent = "▶"; } }, 450);
};
$("periode").onchange = e => { E.periode = +e.target.value; dessiner(); graphe(); };
document.querySelectorAll("#choix-mode button").forEach(b => b.onclick = () => { E.mode = b.dataset.mode; majTout(); });
document.querySelectorAll("#choix-couche button").forEach(b => b.onclick = () => { E.couche = b.dataset.couche; majTout(); });
$("essence").onchange = e => { E.esp = e.target.value; dessiner(); grapheMort(); };

(async () => {
  [E.D, E.deps] = await Promise.all([fetch("donnees.json").then(r => r.json()), fetch("departements.geojson").then(r => r.json())]);
  [E.M, E.O, E.C] = await Promise.all(["mortalite.json", "mortalite_observee.json", "courbes.json"].map(f => fetch(f).then(r => r.json()).catch(() => null)));
  if (EN) Object.entries(IND_EN).forEach(([k, v]) => { if (E.D.indices[k]) Object.assign(E.D.indices[k], v); });
  if (!E.O) $("choix-couche").hidden = true;
  else Object.keys(ESP_NOM).forEach(k => $("essence").append(new Option(tr(`${ESP_NOM[k]} (${fmt(E.O.france[k], 1)} % en France)`, `${ESP_NOM[k]} (${fmt(E.O.france[k], 1)}% in France)`), k)));
  Object.entries(E.D.indices).forEach(([k, I]) => {
    const b = document.createElement("button"); b.dataset.ind = k; b.title = I.def;
    b.append(Object.assign(document.createElement("b"), { textContent: k }), Object.assign(document.createElement("small"), { textContent: I.nom.split(/ ?: /)[1] || "" }));
    b.onclick = () => { E.indice = k; majTout(); }; $("choix-indice").append(b);
  });
  PERIODES.forEach(([a, b], i) => $("periode").append(new Option(`${a}-${b}`, i)));
  E.periode = PERIODES.length - 1; $("periode").value = E.periode;
  preparer(); majTout();
})().catch(e => { document.querySelector("main").insertAdjacentHTML("afterbegin", `<p class="panneau">${tr("Données introuvables", "Data not found")} (${e.message}).</p>`); });
