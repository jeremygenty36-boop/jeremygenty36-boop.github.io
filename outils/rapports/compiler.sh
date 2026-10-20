#!/bin/sh
# Compile un ou tous les rapports et copie le PDF dans documents/.
# Usage : ./compiler.sh [cavitation|restauration|courpain|rouille]
set -e
cd "$(dirname "$0")"
declare_cible() { case $1 in
  cavitation) echo Cavitation_peuplier_Genty_2024 ;;
  restauration) echo Restauration_carriere_Sandillon_Bellin_Genty ;;
  courpain) echo Habitats_carriere_Courpain_Genty_Beaubois ;;
  rouille) echo Rouille_peuplier_phytopathologie_Genty_Delmas ;;
esac; }
for r in ${1:-cavitation restauration courpain rouille}; do
  ( cd "$r"
    xelatex -interaction=nonstopmode -halt-on-error "$r" >/dev/null
    bibtex "$r" >/dev/null
    xelatex -interaction=nonstopmode -halt-on-error "$r" >/dev/null
    xelatex -interaction=nonstopmode -halt-on-error "$r" | grep -E '^!|undefined' || true )
  cp "$r/$r.pdf" "../../documents/$(declare_cible "$r").pdf"
  echo "$r -> documents/$(declare_cible "$r").pdf"
done
