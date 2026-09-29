###############################################################################
# generer_donnees_fictives.R
#
# Generates 100% SYNTHETIC data with the same structure (column names and
# types) as the files used by the original monitoring app. No value comes from
# the real survey: geography, household numbers, names, observations and
# progress rates are all randomly generated.
#
# Usage (from the repository root):
#   Rscript data-raw/generer_donnees_fictives.R
#
# Outputs (written to the repository root, next to app.R):
#   data0.rds, MEMBRE.rds, PARCELLE.rds, CHEPTEL.rds, MARAICHAGE.rds,
#   PREVISION.rds, ABANDONNEES.rds and their *_NT.rds counterparts,
#   TAUX_PROGRESSION.xlsx, www/icon.png
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(openxlsx)
})

set.seed(2026)

# Output directory = repository root (parent of data-raw/)
args     <- commandArgs(trailingOnly = FALSE)
this     <- sub("^--file=", "", args[grep("^--file=", args)])
out_dir  <- if (length(this)) normalizePath(file.path(dirname(this), "..")) else getwd()
message("Writing synthetic data to: ", out_dir)

###############################################################################
# 1. Fictitious geography: 5 regions > 3 provinces > 4 communes > villages
#    One row of data0 = one field enumerator (860 enumerators in total).
###############################################################################
N_ENQ   <- 860
regions <- paste("Région", LETTERS[1:5])

geo_com <- expand.grid(r = 1:5, p = 1:3, c = 1:4) |>
  arrange(r, p, c) |>
  mutate(REG  = regions[r],
         PROV = paste0("Province ", LETTERS[r], p),
         COM  = paste0("Commune ", LETTERS[r], p, "-", c))

# Distribute the 860 enumerators (one village each) across the 60 communes
n_by_com <- as.vector(rmultinom(1, N_ENQ, prob = runif(nrow(geo_com), 0.6, 1.4)))
n_by_com[n_by_com == 0] <- 1
data0 <- geo_com[rep(seq_len(nrow(geo_com)), n_by_com), c("REG", "PROV", "COM")] |>
  as_tibble() |>
  slice_head(n = N_ENQ) |>
  mutate(VIL = sprintf("Village %03d", row_number()))

saveRDS(data0, file.path(out_dir, "data0.rds"))

###############################################################################
# 2. Helpers
###############################################################################
pick_geo <- function(n) data0[sample.int(nrow(data0), n, replace = TRUE), ]
fake_menage <- function(n) as.numeric(sample(1:40, n, replace = TRUE))
fake_chef   <- function(n) sprintf("Chef de ménage %04d", sample(1:9999, n, replace = TRUE))
fake_membre <- function(n) sprintf("Membre %04d", sample(1:9999, n, replace = TRUE))
with_na <- function(x, idx) { x[idx] <- NA; x }

cultures <- c("Maïs", "Sorgho", "Mil", "Riz", "Niébé", "Arachide", "Sésame",
              "Coton", "Soja", "Voandzou", "Fonio", "Patate douce")
maraich  <- c("Oignon", "Tomate", "Chou", "Laitue", "Aubergine", "Piment",
              "Gombo", "Carotte", "Poivron", "Concombre")
especes  <- c("Bovins", "Ovins", "Caprins", "Porcins", "Asins", "Volaille",
              "Pintades", "Camelins")

obs_membre <- c(
  "Âge du membre incompatible avec son lien de parenté avec le chef",
  "Chef de ménage âgé de moins de 15 ans",
  "Membre enregistré deux fois dans le même ménage",
  "Sexe du conjoint identique à celui du chef de ménage",
  "Niveau d'instruction supérieur au niveau possible pour l'âge déclaré")
obs_parcelle <- c(
  "Superficie de la parcelle hors des bornes attendues",
  "Culture principale identique à la culture secondaire",
  "Responsable de parcelle absent de la liste des membres",
  "Coordonnées GPS de la parcelle manquantes",
  "Date de semis postérieure à la date de visite")
obs_cheptel <- c(
  "Effectif du troupeau anormalement élevé pour le ménage",
  "Nombre de naissances supérieur à l'effectif des femelles",
  "Espèce non renseignée",
  "Responsable des animaux absent de la liste des membres")
obs_maraich <- c(
  "Superficie maraîchère supérieure à la superficie totale déclarée",
  "Production déclarée sans superficie",
  "Culture maraîchère hors saison pour la zone",
  "Rendement hors des bornes attendues")
obs_prev <- c(
  "Production prévue nulle pour une parcelle semée",
  "Rendement prévu hors des bornes attendues",
  "Prévision saisie avant le passage de l'agent sur la parcelle")
obs_aband <- c(
  "Parcelle déclarée abandonnée mais production saisie",
  "Motif d'abandon non renseigné")
obs_nt <- c(
  "Ménage non encore visité",
  "Formulaire non transmis au serveur",
  "Ménage absent lors du passage de l'enquêteur",
  "Visite reportée : chef de ménage indisponible",
  "Formulaire commencé mais non finalisé")

obs <- function(n, pool) paste0(sample(pool, n, replace = TRUE),
                                " (contrôle n°", sample(1:99, n, replace = TRUE), ")")

###############################################################################
# 3. Inconsistency tables (Incoherence tab) and "not reached" tables (NT)
###############################################################################
# MEMBRE
n <- 300; g <- pick_geo(n); na <- sample.int(n, 60)
MEMBRE <- g |> mutate(MENAGE = with_na(fake_menage(n), na),
                      NOM_CHEF = with_na(fake_chef(n), na),
                      NOM_MEMBRE = with_na(fake_membre(n), na),
                      OBSERVATION = obs(n, obs_membre))
n <- 400; g <- pick_geo(n)
MEMBRE_NT <- g |> mutate(MENAGE = fake_menage(n), OBSERVATION = obs(n, obs_nt))

# PARCELLE
n <- 350; g <- pick_geo(n); na <- sample.int(n, 60)
PARCELLE <- g |> mutate(MENAGE = with_na(fake_menage(n), na),
                        NOM_CHEF = with_na(fake_chef(n), na),
                        NOM_MEMBRE = with_na(fake_membre(n), na),
                        NUM_PARCELLE = with_na(as.numeric(sample(1:15, n, TRUE)), na),
                        CULTURE_1 = with_na(sample(cultures, n, TRUE), na),
                        CULTURE_2 = with_na(sample(cultures, n, TRUE), c(na, sample.int(n, 10))),
                        LIEU_DIT_PARCELLE = with_na(sprintf("Lieu-dit %03d", sample(1:500, n, TRUE)), na),
                        OBSERVATION = obs(n, obs_parcelle))
n <- 700; g <- pick_geo(n); na <- sample.int(n, 120)
PARCELLE_NT <- g |> mutate(MENAGE = with_na(fake_menage(n), na),
                           NOM_CHEF = with_na(fake_chef(n), na),
                           OBSERVATION = obs(n, obs_nt))

# CHEPTEL (NOM_CHEF is an all-NA logical column in the original structure)
n <- 450; g <- pick_geo(n)
CHEPTEL <- g |> mutate(MENAGE = fake_menage(n),
                       NOM_CHEF = NA,
                       NOM_MEMBRE = fake_membre(n),
                       ESPECE = with_na(sample(especes, n, TRUE), sample.int(n, 400)),
                       OBSERVATION = obs(n, obs_cheptel))
n <- 500; g <- pick_geo(n)
CHEPTEL_NT <- g |> mutate(MENAGE = fake_menage(n), NOM_CHEF = fake_chef(n),
                          OBSERVATION = obs(n, obs_nt))

# MARAICHAGE
n <- 300; g <- pick_geo(n)
MARAICHAGE <- g |> mutate(MENAGE = fake_menage(n), NOM_CHEF = fake_chef(n),
                          CULTURE = sample(maraich, n, TRUE),
                          OBSERVATION = obs(n, obs_maraich))
n <- 400; g <- pick_geo(n)
MARAICHAGE_NT <- g |> mutate(MENAGE = fake_menage(n), NOM_CHEF = fake_chef(n),
                             OBSERVATION = obs(n, obs_nt))

# PREVISION
n <- 80; g <- pick_geo(n)
PREVISION <- g |> mutate(MENAGE = fake_menage(n), NOM_CHEF = fake_chef(n),
                         NUM_PARCELLE = as.numeric(sample(1:15, n, TRUE)),
                         OBSERVATION = obs(n, obs_prev))
n <- 2000; g <- pick_geo(n)
PREVISION_NT <- g |> mutate(MENAGE = fake_menage(n),
                            NOM_CHEF = with_na(fake_chef(n), sample.int(n, 10)),
                            NUM_PARCELLE = with_na(as.numeric(sample(1:20, n, TRUE)), sample.int(n, 200)),
                            OBSERVATION = obs(n, obs_nt))

# ABANDONNEES
n <- 20; g <- pick_geo(n)
ABANDONNEES <- g |> mutate(MENAGE = fake_menage(n), NOM_CHEF = fake_chef(n),
                           NUM_PARCELLE = as.numeric(sample(1:15, n, TRUE)),
                           OBSERVATION = obs(n, obs_aband))
n <- 1200; g <- pick_geo(n)
ABANDONNEES_NT <- g |> mutate(MENAGE = fake_menage(n), NOM_CHEF = fake_chef(n),
                              OBSERVATION = obs(n, obs_nt))

tables <- list(MEMBRE = MEMBRE, MEMBRE_NT = MEMBRE_NT,
               PARCELLE = PARCELLE, PARCELLE_NT = PARCELLE_NT,
               CHEPTEL = CHEPTEL, CHEPTEL_NT = CHEPTEL_NT,
               MARAICHAGE = MARAICHAGE, MARAICHAGE_NT = MARAICHAGE_NT,
               PREVISION = PREVISION, PREVISION_NT = PREVISION_NT,
               ABANDONNEES = ABANDONNEES, ABANDONNEES_NT = ABANDONNEES_NT)
for (nm in names(tables)) {
  saveRDS(as_tibble(tables[[nm]]) |> arrange(REG, PROV, COM, VIL),
          file.path(out_dir, paste0(nm, ".rds")))
}

###############################################################################
# 4. TAUX_PROGRESSION.xlsx : one sheet per form + LABEL sheet
#    Rows: NATIONAL, then region, province, commune and village levels.
#    Village rates are random; upper levels are the mean of their children.
###############################################################################
forms_cols <- list(
  MEMBRE      = c("Menage_touche", "Membre_du_menage_touche"),
  CHEPTEL     = c("Menage_touche", "Resp_annimaux_touche"),
  PARCELLE    = c("Menage_touche", "Resp_parcelle_touche_pluvial"),
  MARAICHAGE  = c("Menage_touche", "Resp_parcelle_touche_maraichage"),
  PREVISION   = c("Menage_touche", "Parcelle_prevision_touche"),
  ABANDONNEES = c("Menage_touche", "Menage_toucheBIS"))

make_progress <- function(cols) {
  # Region-level "speed" so that the dashboard shows contrasts between regions
  speed <- setNames(runif(length(regions), 45, 95), regions)
  vil <- data0 |>
    mutate(v1 = pmin(100, pmax(0, rnorm(n(), speed[REG], 15))),
           v2 = pmin(100, pmax(0, v1 - runif(n(), 0, 20))))
  agg <- function(d, ...) d |> group_by(...) |>
    summarise(v1 = mean(v1), v2 = mean(v2), .groups = "drop")
  nat <- tibble(REG = "NATIONAL", v1 = mean(vil$v1), v2 = mean(vil$v2))
  out <- bind_rows(nat,
                   agg(vil, REG),
                   agg(vil, REG, PROV),
                   agg(vil, REG, PROV, COM),
                   vil) |>
    transmute(REG, PROV, COM, VIL,
              !!cols[1] := round(v1, 1), !!cols[2] := round(v2, 1))
  out
}

labels <- tibble(
  Variable = c("Menage_touche", "Membre_du_menage_touche", "Resp_annimaux_touche",
               "Resp_parcelle_touche_pluvial", "Resp_parcelle_touche_maraichage",
               "Parcelle_prevision_touche", "Menage_toucheBIS"),
  Label = c("Ménage touché", "Membre du ménage touché", "Responsable d'animaux touché",
            "Responsable de parcelle touché (pluvial)",
            "Responsable de parcelle touché (maraîchage)",
            "Parcelle de prévision touchée", "Ménage touché (parcelles abandonnées)"))

wb <- createWorkbook()
addWorksheet(wb, "LABEL"); writeData(wb, "LABEL", labels)
for (frm in names(forms_cols)) {
  addWorksheet(wb, frm)
  writeData(wb, frm, make_progress(forms_cols[[frm]]))
}
saveWorkbook(wb, file.path(out_dir, "TAUX_PROGRESSION.xlsx"), overwrite = TRUE)

###############################################################################
# 5. Neutral PWA icon (no institutional logo)
###############################################################################
dir.create(file.path(out_dir, "www"), showWarnings = FALSE)
png(file.path(out_dir, "www", "icon.png"), width = 192, height = 192, bg = "transparent")
par(mar = c(0, 0, 0, 0))
plot.new(); plot.window(c(-1, 1), c(-1, 1), asp = 1)
symbols(0, 0, circles = 0.98, inches = FALSE, add = TRUE, bg = "#00734D", fg = NA)
text(0, 0.12, "EPA", col = "white", cex = 3.2, font = 2)
text(0, -0.42, "DEMO", col = "#EAF9F1", cex = 1.6, font = 2)
invisible(dev.off())

message("Done: synthetic data generated.")
