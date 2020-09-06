################################# FUNKTIONEN ###################################
#------------------------------------------------------------------------------#

# cosineDelta(x)
# 
# @param x -> Spalte von zscores
# @return y -> Distanzwert
#
# Ergebnisse zwischen 0 und 2
# 0 --> groeßte Aehnlichkeit
# 2 --> kleinste Aehnlichkeit

cosineDelta = function(x){
  x = scale(x)
  y = as.dist(x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
  z = 1 - y
  return(z)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# myFileRename(from, to)
#
# @param from -> aktueller Pfad
# @param to -> gewünschter Pfad (path_result_corpus)
#
# Funktion für die Umspeicherung der Ergebnisdateien in die entsprechenden 
# Ergebnisordner:
# Zuerst werden die Ergebnisse in path_corpora gespeichert. 
# Dann werden die Dateien unter dem in path_result_corpus gespeicherten Pfad 
# endgespeichert.

myFileRename <- function(from, to) {
  if (is.character(from) == FALSE) {
    message("Der angegebene Pfad ist kein Charakter!")
  }
  if (is.character(to) == FALSE) {
    message("Der angegebene Pfad ist kein Charakter!")
  }
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive = TRUE,
                                                  showWarnings = FALSE)
  file.rename(from = from,  to = to)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#