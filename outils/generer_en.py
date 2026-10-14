# Genere en.html (version anglaise) a partir de index.html et du dictionnaire traduction_en.py.
# Usage : python3 outils/generer_en.py   (depuis la racine du portfolio)
import sys
from pathlib import Path
from bs4 import BeautifulSoup, Comment
sys.path.insert(0, str(Path(__file__).parent))
from traduction_en import T
T.update({"Master 2": "MSc", "21 989": "21,989", "8,5 °C": "8.5 °C", "Version française": "Version française"})
soup = BeautifulSoup(Path("index.html").read_text(), "html.parser")
manquants = []
for t in soup.find_all(string=True):
    if t.parent.name in ("script", "style") or isinstance(t, Comment): continue
    s = str(t).replace("\u00a0:", " :").replace("\u00a0;", " ;").replace("\u00a0?", " ?"); c = s.strip()
    if not c: continue
    if t.parent.name == "sup":                      # ordinaux : 1er -> 1st, 2e -> 2nd, 4e -> 4th
        prev = str(t.parent.previous_sibling or "").rstrip()
        n = prev[-1:] if prev else ""
        t.replace_with({"1": "st", "2": "nd", "3": "rd"}.get(n, "th")); continue
    if c in T:
        t.replace_with(s.replace(c, T[c]).replace(" :", ":").replace(" ;", ";").replace(" ?", "?"))  # typographie anglaise
    elif not all(ch in "·.▲N−°0123456789 ,:;()-/" for ch in c) and c not in ("R", "GitHub", "NetCDF", "SPEI", "JavaScript", "terra", "Climpact", "GLM", "ERRATA", "Sentinel-2", "NDVI", "QGIS", "Orfeo ToolBox", "Poster", "GPS", "Diagnostic", "Swift", "Python", "API OpenAlex", "Zotero", "Jérémy Genty", "SAFRAN 8 km", "Anoplophora glabripennis", "A. chinensis", "Melampsora larici-populina", "genty.jeremy36@gmail.com", "html", "Classification", "Contact", "Code", "Portfolio"):
        manquants.append(c)
for el in soup.find_all(True):
    for a in ("alt", "title", "aria-label", "content"):
        v = el.get(a)
        if v and v.strip() in T: el[a] = T[v.strip()]
soup.html["lang"] = "en"
for a in soup.find_all("a", href="en.html"):
    a["href"] = "index.html"; a["hreflang"] = "fr"; a["title"] = "Version française"
    for img in a.find_all("img"):
        img["src"] = "images/drapeau_fr.svg"; img["alt"] = "Version française"
for a in soup.find_all("a", href="carte/"): a["href"] = "carte/en.html"   # carte en anglais
alt = soup.find("link", rel="alternate"); alt["href"] = "index.html"; alt["hreflang"] = "fr"
Path("en.html").write_text(str(soup))
print("en.html ecrit ;", len(manquants), "textes sans traduction :", manquants)
