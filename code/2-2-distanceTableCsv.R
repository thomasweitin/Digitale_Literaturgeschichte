################################ DISTANZTABELLEN ###############################
#------------------------------------------------------------------------------#

### Wandelt die von Stylo ausgegebenen txt-Distanztabellen in csv-Dateien um,
### die in spaeteren Analysen und Anwendungen besser verwendet werden koennen.

#------------------------------------------------------------------------------#
## Durchfuehrung --------------------------------------------------------------#
#------------------------------------------------------------------------------#

path_results <- "C:\\Rivalinnen\\results\\Stilometrie"
setwd(path_results)

files <- list.files(path = ".", pattern = "distance_table.+.txt", 
                    full.names = TRUE, recursive = TRUE)
# Findet alle Dateien in einem Ordner, deren Name die mit "pattern" festgelegte
# Sequenz enthaelt.
# recursive = FALSE -> nur der Ordner selbst wird durchsucht
# recursive = TRUE -> alle Subordner werden auch durchsucht

for (i in 1:length(files)){
  name <- gsub("(.+).txt", "\\1", files[i])
  filename <- paste0(name, ".csv")
  file <- read.table(files[i], sep = " ", check.names = FALSE)
  # check.names = FALSE verhindert, dass die Spaltennamen kontrolliert und
  # eventuell veraendert werden
  write.table(file, filename, col.names = NA)
  print(paste0(i, " von ", length(files)))
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#