################################# STILOMETRIE ##################################
#------------------------------------------------------------------------------#

### Fuehrt stilometrische Analysen für mehrere Korpora mit mehreren 
### Parameter-Settings durch.
###
### Ordnerstrukturen:
###
### corpora
###     |_Novellenschatz
### results
###     |_[Ergebnisordner werden automatisch erstellt]

#------------------------------------------------------------------------------#
## Benoetigte Packages --------------------------------------------------------#
#------------------------------------------------------------------------------#

library(stylo)

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

source("C:\\Rivalinnen\\code\\0-generalFunctions.R")

path_results <- "C:\\Rivalinnen\\results\\Stilometrie\\Novellenschatz"

if(!dir.exists(path_results)){
  dir.create(path_results, recursive = TRUE)
}

setwd(path_results)

path_corpora <- "C:\\Rivalinnen\\corpora"

#------------------------------------------------------------------------------#
## Parameter ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

corpus <- "Novellenschatz"
path_corpus <- paste0(path_corpora, "\\", corpus)
MFW <- 500
distance_measure <- "dist.delta"

#------------------------------------------------------------------------------#
## Stilometrische Analyse -----------------------------------------------------#
#------------------------------------------------------------------------------#

batch <- stylo(gui = FALSE,
               corpus = path_corpus,
               distance.measure = distance_measure,
               corpus.format = "plain",
               corpus.lang = "German",
               analyzed.features = "w",
               ngram.size = 1,
               preserve.case = FALSE,
               encoding = "UTF-8",
               mfw.min = MFW,
               mfw.max = MFW,
               mfw.incr = 0,
               culling.min = 20,
               culling.max = 20,
               culling.incr = 0,
               analysis.type = "CA",
               write.pdf.file = TRUE,
               write.jpg.file = FALSE,
               write.svg.file = TRUE,
               save.distance.tables = TRUE,
               save.analyzed.features = TRUE,
               save.analyzed.freqs = TRUE,
               display.on.screen = FALSE)

batch_zscores <- batch$table.with.all.zscores
all.zs <- t(batch_zscores)
if (ncol(batch_zscores) >= MFW){
  zs <- batch_zscores[,1:MFW]
} else {
  zs <- batch_zscores
}

filename_zscore <- paste0(corpus, "_", MFW, "MFW", "_zscores.csv")

write.csv(zs, file = filename_zscore)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#