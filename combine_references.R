# Pfad zum Arbeitsverzeichnis setzen
work_dir <- "C:/Users/johan/OneDrive/Desktop/SpoWi/MetabolicAnalytics-Apps.github.io"
setwd(work_dir)

# Dateipfade definieren
ma_file <- file.path(work_dir, "references_MA.bib")
ip_file <- file.path(work_dir, "references_IP.bib")
all_file <- file.path(work_dir, "references_all.bib")

# Inhalt der Dateien einlesen
ma_content <- readLines(ma_file, warn = FALSE)
ip_content <- readLines(ip_file, warn = FALSE)

# Inhalte zusammenfügen
all_content <- c(ma_content, "", ip_content)  # Leere Zeile als Trenner

# Zusammengeführten Inhalt in neue Datei schreiben
writeLines(all_content, all_file)

cat("BIB-Dateien wurden erfolgreich zu 'references_all.bib' zusammengeführt.\n")