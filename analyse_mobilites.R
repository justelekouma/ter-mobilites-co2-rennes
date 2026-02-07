## Objectif

Analyse des flux domicile–travail vers Rennes, des parts modales associées, et calcul plus fin des émissions de CO₂ à partir des distances par commune (option 3), à partir des données INSEE (2022) + distances (fichier Excel).

------------------------------------------------------------------------

## Étape 0 : Setup (packages, paramètres, dossiers)

```{r}
packages <- c("readr","dplyr","stringr","janitor","tidyr","tibble","ggplot2","readxl","scales")
to_install <- setdiff(packages, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install)

library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(tibble)
library(ggplot2)
library(readxl)
library(scales)

# Paramètres
rennes_com <- "35238"
top_n_communes <- 100
seuil_navetteurs <- 200

# Hypothèses de conversion "navetteurs -> km annuels"
jours_travailles_an <- 220     # hypothèse standard (jours travaillés/an)
aller_retour <- 2              # A/R domicile-travail

# Dossier d'export
dir.create("data_travaillees", showWarnings = FALSE, recursive = TRUE)

```

------------------------------------------------------------------------

## Étape 1 : Import des données INSEE (flux + parts modales)

```{r}
flux <- read_delim(
  "data/base-flux-mobilite-domicile-lieu-travail-2022.csv",
  delim = ";",
  show_col_types = FALSE
) |> clean_names()

nav <- read_delim(
  "data/TD_NAV2BV1_2022.csv",
  delim = ";",
  show_col_types = FALSE
) |> clean_names()

# Vérification colonnes
names(flux)
names(nav)

```

------------------------------------------------------------------------

## Étape 2 : Nettoyage et mise en forme (codes INSEE, types)

```{r}
flux <- flux |>
  mutate(
    codgeo = str_pad(as.character(codgeo), 5, "left", "0"),
    dclt   = str_pad(as.character(dclt),   5, "left", "0")
  )

nav <- nav |>
  mutate(
    codgeo   = str_pad(as.character(codgeo), 5, "left", "0"),
    trans_19 = as.integer(trans_19),
    sexe     = as.integer(sexe),
    iltduu   = as.integer(iltduu)
  )

head(flux)
head(nav)

```

------------------------------------------------------------------------

## Étape 3 : Flux domicile–travail vers Rennes (navetteurs par commune d’origine)

```{r}
flux_to_rennes <- flux |>
  filter(dclt == rennes_com) |>
  group_by(codgeo, libgeo) |>
  summarise(
    navetteurs_vers_rennes = sum(nbflux_c22_actocc15p, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(navetteurs_vers_rennes))

head(flux_to_rennes, 20)


```

------------------------------------------------------------------------

## Étape 4 : Parts modales à Rennes (NAV2B)

```{r}
nav_rennes <- nav |>
  filter(nivgeo == "COM", codgeo == rennes_com) |>
  group_by(trans_19) |>
  summarise(nb = sum(nb, na.rm = TRUE), .groups = "drop") |>
  mutate(part = nb / sum(nb)) |>
  arrange(desc(part))

mode_dict <- tibble(
  trans_19 = c(1,2,3,4,5,6,7,8,9,10),
  mode = c(
    "Marche","Vélo","Deux-roues motorisé",
    "Voiture (conducteur)","Voiture (passager)",
    "Transports en commun","Train",
    "Autre","Non renseigné","Travail à domicile"
  )
)

nav_rennes <- nav_rennes |>
  left_join(mode_dict, by = "trans_19") |>
  relocate(mode, .before = trans_19)

nav_rennes

```

## Étape 4bis : Contrôles qualité rapides

```{r}
# Somme des parts modales (doit être ~ 1)
sum(nav_rennes$part)

# Vérif variables clés non manquantes
stopifnot(!anyNA(flux_to_rennes$codgeo))
stopifnot(!anyNA(flux_to_rennes$navetteurs_vers_rennes))
stopifnot(!anyNA(nav_rennes$part))

```

------------------------------------------------------------------------

## Étape 5 : Définition de la zone autour de Rennes (top & seuil)

```{r}
zone_top <- flux_to_rennes |>
  filter(codgeo != rennes_com) |>
  slice_max(navetteurs_vers_rennes, n = top_n_communes, with_ties = FALSE)

zone_seuil <- flux_to_rennes |>
  filter(codgeo != rennes_com) |>
  filter(navetteurs_vers_rennes >= seuil_navetteurs)

nrow(zone_top)
nrow(zone_seuil)

head(zone_top, 10)

```

------------------------------------------------------------------------

## Étape 6 : Regroupement des modes (pour une utilisation macro)

```{r}
nav_rennes_grouped <- nav_rennes |>
  mutate(groupe = case_when(
    trans_19 %in% c(4,5) ~ "Voiture",
    trans_19 %in% c(6,7) ~ "Transports collectifs",
    trans_19 %in% c(1,2,3) ~ "Modes doux / deux-roues",
    trans_19 %in% c(10) ~ "Travail à domicile",
    TRUE ~ "Autre / NR"
  )) |>
  group_by(groupe) |>
  summarise(nb = sum(nb), part = sum(part), .groups = "drop") |>
  arrange(desc(part))

nav_rennes_grouped

```

------------------------------------------------------------------------

## Étape 7 : Import des distances (Excel) + jointure aux communes

```{r}
dist_raw <- read_excel("data/TER_distances_communes_rennes.xlsx") |> clean_names()
names(dist_raw)

# Détection automatique des colonnes (robuste)
possible_code <- intersect(names(dist_raw), c("codgeo","code_insee","insee","code_commune","codgeo","com"))
possible_dist <- intersect(names(dist_raw), c("distance_km","distance","dist_km","km","distance_rennes_km","distance_vers_rennes_km"))

if (length(possible_code) == 0) stop("❌ Colonne code commune introuvable dans le fichier distances. Ajoute une colonne 'codgeo' ou 'code_insee'.")
if (length(possible_dist) == 0) stop("❌ Colonne distance introuvable dans le fichier distances. Ajoute une colonne 'distance_km' (en km).")

code_col <- possible_code[1]
dist_col <- possible_dist[1]

dist <- dist_raw |>
  transmute(
    codgeo = str_pad(as.character(.data[[code_col]]), 5, "left", "0"),
    distance_km = as.numeric(.data[[dist_col]])
  ) |>
  filter(!is.na(codgeo)) |>
  distinct(codgeo, .keep_all = TRUE)

summary(dist$distance_km)
head(dist, 10)

# Zone de calcul : communes (seuil) + Rennes
zone_base <- flux_to_rennes |>
  filter(codgeo %in% c(zone_seuil$codgeo, rennes_com)) |>
  left_join(dist, by = "codgeo")

# Contrôle : quelles communes n'ont pas de distance ?
missing_dist <- zone_base |> filter(is.na(distance_km)) |> select(codgeo, libgeo, navetteurs_vers_rennes)
n_missing <- nrow(missing_dist)
n_missing
head(missing_dist, 20)

# Si quelques distances manquent, on peut:
# - soit les compléter manuellement dans l'Excel
# - soit les exclure du calcul CO2 (ici on les exclut)
zone_base_ok <- zone_base |> filter(!is.na(distance_km))

nrow(zone_base_ok)

```

------------------------------------------------------------------------

## Étape 8 : Facteurs d’émission (kgCO2e / passager-km) + calcul CO₂

```{r}
# Parts par groupe (sécurisation : si un groupe n'existe pas, on met 0)
get_part <- function(g) {
  x <- nav_rennes_grouped |> filter(groupe == g) |> pull(part)
  if (length(x) == 0) return(0)
  x[1]
}

p_voiture <- get_part("Voiture")
p_tc      <- get_part("Transports collectifs")
p_doux    <- get_part("Modes doux / deux-roues")
p_tele    <- get_part("Travail à domicile")
p_autre   <- get_part("Autre / NR")

c(p_voiture=p_voiture, p_tc=p_tc, p_doux=p_doux, p_tele=p_tele, p_autre=p_autre, somme=p_voiture+p_tc+p_doux+p_tele+p_autre)

# Facteurs d'émission (kgCO2e / passager-km) - ordres de grandeur (à documenter dans le rapport)
# On définit 3 scénarios "bas/central/haut" pour refléter l'incertitude.
fe <- tibble(
  scenario = c("bas","central","haut"),
  fe_voiture_kg_pkm = c(0.12, 0.18, 0.25),
  fe_tc_kg_pkm      = c(0.03, 0.06, 0.10),
  fe_doux_kg_pkm    = c(0.00, 0.00, 0.00),
  fe_autre_kg_pkm   = c(0.10, 0.15, 0.20),
  fe_tele_kg_pkm    = c(0.00, 0.00, 0.00)
)

fe

# Calcul des km annuels + émissions
# km_an = navetteurs * distance_km * 2 * jours_travailles_an
# Emissions = km_an * [sum(parts * facteurs)]
zone_co2_dist <- zone_base_ok |>
  mutate(km_an = navetteurs_vers_rennes * distance_km * aller_retour * jours_travailles_an) |>
  crossing(fe) |>
  mutate(
    fe_moy_kg_pkm = p_voiture*fe_voiture_kg_pkm + p_tc*fe_tc_kg_pkm + p_doux*fe_doux_kg_pkm + p_autre*fe_autre_kg_pkm + p_tele*fe_tele_kg_pkm,
    co2_kg_an = km_an * fe_moy_kg_pkm,
    co2_t_an = co2_kg_an / 1000
  ) |>
  arrange(desc(co2_t_an))

head(zone_co2_dist, 20)

# Résumé par scénario (tonnes CO2e / an)
resume_co2_dist <- zone_co2_dist |>
  group_by(scenario) |>
  summarise(
    navetteurs_total = sum(navetteurs_vers_rennes, na.rm = TRUE),
    km_total_an = sum(km_an, na.rm = TRUE),
    co2_total_t_an = sum(co2_t_an, na.rm = TRUE),
    .groups = "drop"
  )

resume_co2_dist

```

------------------------------------------------------------------------

## Étape 9 : Scénario de report modal (–10 points voiture → TC) sur le calcul distance-based

```{r}
# Report modal : on déplace 10 points de la voiture vers les transports collectifs
delta <- 0.10

p_voiture_new <- max(p_voiture - delta, 0)
p_tc_new      <- min(p_tc + delta, 1)

# On renormalise légèrement si dépassement (cas rares)
s <- p_voiture_new + p_tc_new + p_doux + p_tele + p_autre
p_voiture_new <- p_voiture_new / s
p_tc_new <- p_tc_new / s
p_doux_new <- p_doux / s
p_tele_new <- p_tele / s
p_autre_new <- p_autre / s

c(old_voiture=p_voiture, old_tc=p_tc, new_voiture=p_voiture_new, new_tc=p_tc_new)

zone_co2_dist_report <- zone_base_ok |>
  mutate(km_an = navetteurs_vers_rennes * distance_km * aller_retour * jours_travailles_an) |>
  crossing(fe) |>
  mutate(
    fe_moy_kg_pkm_avant = p_voiture*fe_voiture_kg_pkm + p_tc*fe_tc_kg_pkm + p_doux*fe_doux_kg_pkm + p_autre*fe_autre_kg_pkm + p_tele*fe_tele_kg_pkm,
    fe_moy_kg_pkm_apres = p_voiture_new*fe_voiture_kg_pkm + p_tc_new*fe_tc_kg_pkm + p_doux_new*fe_doux_kg_pkm + p_autre_new*fe_autre_kg_pkm + p_tele_new*fe_tele_kg_pkm,
    co2_t_an_avant = (km_an * fe_moy_kg_pkm_avant) / 1000,
    co2_t_an_apres = (km_an * fe_moy_kg_pkm_apres) / 1000,
    gain_t_an = co2_t_an_avant - co2_t_an_apres
  )

resume_report <- zone_co2_dist_report |>
  group_by(scenario) |>
  summarise(
    co2_total_avant_t_an = sum(co2_t_an_avant, na.rm = TRUE),
    co2_total_apres_t_an = sum(co2_t_an_apres, na.rm = TRUE),
    gain_total_t_an = sum(gain_t_an, na.rm = TRUE),
    gain_relatif = gain_total_t_an / co2_total_avant_t_an,
    .groups = "drop"
  )

resume_report

```

------------------------------------------------------------------------

## Étape 10 : Visualisations (flux, parts modales, émissions distance-based)

```{r}
# Top 20 communes (hors Rennes) les plus contributrices vers Rennes
top20_flux <- flux_to_rennes |>
  filter(codgeo != rennes_com) |>
  slice_max(navetteurs_vers_rennes, n = 20, with_ties = FALSE)

ggplot(top20_flux, aes(x = reorder(libgeo, navetteurs_vers_rennes), y = navetteurs_vers_rennes)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 des communes d'origine des navetteurs vers Rennes (2022)",
    x = "Commune d'origine",
    y = "Navetteurs vers Rennes"
  )

# Parts modales Rennes
ggplot(nav_rennes, aes(x = reorder(mode, part), y = part)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Parts modales des déplacements domicile–travail à Rennes (2022)",
    x = "Mode de transport",
    y = "Part"
  )

# Top 20 émissions (distance-based) — scénario central
top20_co2 <- zone_co2_dist |>
  filter(scenario == "central", codgeo != rennes_com) |>
  slice_max(co2_t_an, n = 20, with_ties = FALSE)

ggplot(top20_co2, aes(x = reorder(libgeo, co2_t_an), y = co2_t_an)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 des communes contributrices en CO2 (distance-based) — scénario central",
    x = "Commune d'origine",
    y = "CO2 (tonnes/an)"
  )

# Relation distance vs émissions (central) — utile pour discussion
scat <- zone_co2_dist |>
  filter(scenario == "central", codgeo != rennes_com)

ggplot(scat, aes(x = distance_km, y = co2_t_an)) +
  geom_point() +
  labs(
    title = "Distance (km) vs émissions estimées (t/an) — scénario central",
    x = "Distance vers Rennes centre (km)",
    y = "CO2 estimé (tonnes/an)"
  )

```

## Étape 11 : Exports des résultats

```{r}
# Exports INSEE (déjà utiles)
write_csv(flux_to_rennes, "data_travaillees/flux_vers_rennes.csv")
write_csv(nav_rennes, "data_travaillees/parts_modales_rennes.csv")
write_csv(nav_rennes_grouped, "data_travaillees/parts_modales_rennes_groupes.csv")

write_csv(zone_top, "data_travaillees/zone_autour_rennes_top100.csv")
write_csv(zone_seuil, "data_travaillees/zone_autour_rennes_seuil200.csv")

# Exports distances + émissions
write_csv(dist, "data_travaillees/distances_communes_vers_rennes.csv")
write_csv(zone_base_ok, "data_travaillees/zone_base_avec_distances.csv")

write_csv(zone_co2_dist, "data_travaillees/co2_par_commune_distancebased_scenarios.csv")
write_csv(resume_co2_dist, "data_travaillees/resume_co2_distancebased_scenarios.csv")

write_csv(resume_report, "data_travaillees/resume_report_modal_distancebased.csv")

list.files("data_travaillees")

```

------------------------------------------------------------------------

## Conclusion

Ce TER met en évidence :

les principaux flux domicile–travail vers Rennes (navetteurs par commune d’origine),

la structure modale des déplacements (parts modales INSEE à Rennes),

une estimation plus fine des émissions grâce à l’ajout des distances par commune, permettant de passer d’une approche “par scénario annuel” à une approche “km × facteur d’émission”,

un scénario de report modal (–10 points voiture → TC) pour quantifier un levier potentiel de réduction.

Remarque méthodologique : les parts modales utilisées dans le calcul CO₂ sont celles observées à Rennes (NAV2B) et appliquées à la zone de navetteurs vers Rennes comme approximation (faute de parts modales par commune d’origine dans le jeu de données utilisé).
