# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
#
# MSCONS Genrator für neue RLM aus S1                                       ----
#
# Author : Sascha Kornberger
# Datum  : 23.04.2025
# Version: 1.0.0
#
# History:
# 1.0.0  Funktion: Initiale Freigabe
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --

# OPTIONS ----------------------------------------------------------------------
# Deaktiviert die die Meldungen der Quellpakete während der Installation.
options(install.packages.check.source = "no")

# Verhindert die wissenschaftliche Notation für Zahlen.
options(scipen = 999)

erzeuge_mscons <- function(sender_gln, empfaenger_gln, zaehlpunkt_id, start_datum, obis) {

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
  
  ## Startdatum = -1 Tag ----
  if (is.character(start_datum)) {
    start_datum <- dmy_hm(paste(start_datum, "22:00")) - days(1)
  }
  ## Enddatum = +8 Tage ----
  end_datum   <- start_datum + days(8)
  
  ## Obis-Formatierung ----
  obis <- gsub(":", "?:", obis, fixed = TRUE)
  
  ## Erzeuge Referenz-ID ----
  zeit_ref <- format(Sys.time(), "%M%S%OS3") |> gsub("\\.", "", x = _)
  ref_id <- paste0("S1", zeit_ref)
  
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
    "UNB+UNOC:3+{sender_gln}:500+{empfaenger_gln}:500+{tag}:{zeit}+{ref_id}++TL'",
    "UNH+1+MSCONS:D:04B:UN:2.4c'",
    "BGM+Z48+{ref_id}-1+9'",
    "DTM+137:{dtmtag}{zeit}?+00:303'",
    "RFF+Z13:13025'",
    "NAD+MS+{sender_gln}::293'",
    "NAD+MR+{empfaenger_gln}::293'",
    "UNS+D'",
    "NAD+DP'",
    "LOC+172+{zaehlpunkt_id}'",
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
  dateiname <- glue("MSCONS_TL_{sender_gln}_{empfaenger_gln}_{dtmtag}_{ref_id}.txt")
  write_file(mscons_text, file = dateiname)
}


# Parameter vorbereiten
sender_gln     <- "9905336000008"
empfaenger_gln <- "9904029000002"
zaehlpunkt_id  <- "10181928253"
start_datum    <- "01.06.2025"
obis           <- "1-1:1.29.0"

# Funktion aufrufen
erzeuge_mscons(sender_gln, empfaenger_gln, zaehlpunkt_id, start_datum, obis)
