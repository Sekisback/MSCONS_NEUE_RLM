# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#
# MSCONS Genrator für neue RLM aus S1                                       ----
#
# Author : Sascha Kornberger
# Datum  : 23.04.2025
# Version: 1.0.1
#
# History:
# 1.0.1  Funktion: Anpassung der sender und empfaegr auf 9903606000005
# 1.0.0  Funktion: Initiale Freigabe
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# OPTIONS ----------------------------------------------------------------------
# Deaktiviert die die Meldungen der Quellpakete während der Installation.
options(install.packages.check.source = "no")

# Verhindert die wissenschaftliche Notation für Zahlen.
options(scipen = 999)


# ERZEUGEN DER MSCONS ----
erzeuge_mscons <- function(empfaenger_vnb, marktlokation, start_datum, obis) {

  # BENOETIGTE PAKETE ------------------------------------------------------------
  ## Liste der Pakete ----
  pakete <- c("lubridate", "glue", "fs", "stringr", "readr", "purrr")
  
  ## Installiere fehlende Pakete ohne Rückfragen
  installiere_fehlende <- pakete[!pakete %in% installed.packages()[, "Package"]]
  if (length(installiere_fehlende) > 0) {
    install.ypackages(
      installiere_fehlende,
      repos = "https://cran.r-project.org",
      quiet = TRUE
    )
  }
  
  ## Lade alle Pakete
  invisible(lapply(pakete, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
  
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- -#
  # ----                           FUNKTIONEN                                 ----
  # --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- -#
  
  ## EEG interner BDEW COde aus der ZFA
  sender_msb     <- "9903606000005"
  empfaenger     <- "9903606000005"
  
  
  ## Startdatum = -1 Tag ----
  if (is.character(start_datum)) {
    datum_file  <- dmy(paste(start_datum))
    start_datum <- dmy_hm(paste(start_datum, "22:00")) - days(1)
  }
  ## Enddatum = +8 Tage ----
  end_datum   <- start_datum + days(8)
  
  ## Obis-Formatierung ----
  obis <- gsub(":", "?:", obis, fixed = TRUE)
  
  ## Erzeuge Referenz-ID ----
  ref_id <- format(Sys.time(), "%M%S%OS3") |> gsub("\\.", "", x = _)

  ## Erstellungszeitpunkt ----
  tag    <- format(Sys.time(), "%y%m%d")
  dtmtag <- format(Sys.time(), "%Y%m%d")
  zeit   <- format(Sys.time(), "%H%M")
  
  ## Segmentanzahl berechnen ----
  tage <- as.numeric(difftime(end_datum, start_datum, units = "days"))
  segment_count <- tage * 288 + 14
  
  ## Zeitreihe erzeugen ----
  zeitreihe <- seq(from = start_datum, to = end_datum - minutes(15), by = "15 mins")
  eintraege <- map_chr(zeitreihe, function(t) {
    t_start <- format(t, "%Y%m%d%H%M")
    t_ende  <- format(t + minutes(15), "%Y%m%d%H%M")
    glue("QTY+Z18:0,000'DTM+163:{t_start}?+00:303'DTM+164:{t_ende}?+00:303'")
  })
  
  ## MSCONS zusammenbauen ----
  kopf <- glue_collapse(glue(
    "UNA:+,? '",
    "UNB+UNOC:3+{sender_msb}:500+{empfaenger}:500+{tag}:{zeit}+{ref_id}++TL'",
    "UNH+1+MSCONS:D:04B:UN:2.4c'",
    "BGM+Z48+{ref_id}-1+9'",
    "DTM+137:{dtmtag}{zeit}?+00:303'",
    "RFF+Z13:13025'",
    "NAD+MS+{sender_msb}::293'",
    "NAD+MR+{empfaenger}::293'",
    "UNS+D'",
    "NAD+DP'",
    "LOC+172+{marktlokation}'",
    "DTM+163:{format(start_datum, '%Y%m%d%H%M')}?+00:303'",
    "DTM+164:{format(end_datum, '%Y%m%d%H%M')}?+00:303'",
    "LIN+1'",
    "PIA+5+{obis}:SRW'"
    ), sep = ""
  )
  
  daten <- paste(eintraege, collapse = "")
  ende <- glue("UNT+{segment_count}+1'UNZ+1+{ref_id}'")
  mscons_text <- glue_collapse(c(kopf, daten, ende), sep = "")
  
  ## Datei speichern ----
  
  # Speicherpfad zusammensetzen – mit Fallback auf getwd()
  if (all(c("main_path", "folder", "wilken_out") %in% ls(envir = .GlobalEnv))) {
    pfad <- file.path(main_path, folder, wilken_out, "MSCONS")
  } else {
    pfad <- file.path(getwd(), "MSCONS")
  }
  
  # Ordner erstellen, falls nicht vorhanden
  if (!dir.exists(pfad)) dir.create(pfad, recursive = TRUE)
  
  # Dateiname definieren
  dateiname <- glue("MSCONS_S1_{marktlokation}_{empfaenger_vnb}_{format(datum_file, '%Y%m%d')}_{ref_id}.txt")
  
  # Vollständiger Pfad + Dateiname
  voller_pfad <- file.path(pfad, dateiname)
  
  # Datei speichern
  write_file(mscons_text, file = voller_pfad)
}


# Test Parameter ----
# 
# empfaenger_vnb <- "9900473000002"
# marktlokation  <- "10181928253"
# start_datum    <- "01.06.2025"
# obis           <- "1-1:1.29.0"
# 
# # Funktion aufrufen ----
# erzeuge_mscons(empfaenger_vnb, marktlokation, start_datum, obis)
