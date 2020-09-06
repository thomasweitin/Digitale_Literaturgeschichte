################################## DELTA MEANS #################################
#------------------------------------------------------------------------------#

### Erstellt aus Distanztabellen Listen von Deltameans, die in der 
### stilometrischen Netzwerkdarstellung als Werte fuer die Knoten verwendet 
### werden. Deltameans aus Distanztabellen innerhalb aller Subdirectories, die 
### in dem durch den Pfad definierten Ordner werden, werden in einem Schritt 
### erstellt.

#------------------------------------------------------------------------------#
## Durchfuehrung --------------------------------------------------------------#
#------------------------------------------------------------------------------#

path_results <- "C:\\Rivalinnen\\results\\Stilometrie"
# Pfad zum Ordner, in dem die Distanztabellen liegen
setwd(path_results)

list_distance_tables <- list.files(path = ".", pattern = "distance_table.+.csv",  
                                   recursive = TRUE, full.names = TRUE)         
# Sucht nach allen zuvor umgewandelten csv-Distanztabellen
len <- length(list_distance_tables)
 
for (i in 1:len){
  distance_table <- read.table(list_distance_tables[i], 
                             sep = " ", header = TRUE, row.names = 1,
                             check.names = FALSE)
  # check.names = FALSE verhindert, dass die Spaltennamen kontrolliert und
  # eventuell veraendert werden
  delta_mean <- colMeans(distance_table, na.rm = FALSE)
  filename <- gsub("(.+/)(.+)", "\\1delta_mean.csv", list_distance_tables[i], 
                   perl = TRUE)
  write.table(delta_mean, filename)
  print(paste0(i, " von ", length(list_distance_tables)))
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
