################################ CONSENSUS TREE ################################
#------------------------------------------------------------------------------#

### Fuehrt stilometrische Analysen für mehrere Korpora mit mehreren 
### Parameter-Settings durch.
###
### Ordnerstrukturen:
###
### corpora
###     |_Buchholz
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

path_results <- "C:\\Rivalinnen\\results\\Stilometrie"
path_result_corpus <- paste0(path_results, "\\Buchholz")

if(!dir.exists(path_result_corpus)){
  dir.create(path_result_corpus, recursive = TRUE)
}

path_corpus <- "C:\\Rivalinnen\\corpora\\Buchholz"

#------------------------------------------------------------------------------#
## Stilometrische Analysen ----------------------------------------------------#
#------------------------------------------------------------------------------#

setwd(path_result_corpus)
distance_measure <- "cosineDelta"

consensus <- stylo(gui = FALSE,
            corpus = path_corpus,
            distance.measure = distance_measure,
            corpus.format = "plain",
            corpus.lang = "German",
            analyzed.features = "w",
            ngram.size = 1,
            preserve.case = FALSE,
            encoding = "UTF-8",
            mfw.min = 100,
            mfw.max = 3000,
            mfw.incr = 100,
            culling.min = 20,
            culling.max = 20,
            culling.incr = 0,
            analysis.type = "BCT",
            consensus.strength = 0.5,
            sample.size = 10000,
            sampling = "normal.sampling",
            write.pdf.file = TRUE,
            write.jpg.file = FALSE,
            write.svg.file = TRUE,
            save.distance.tables = TRUE,
            save.analyzed.features = TRUE,
            save.analyzed.freqs = TRUE,
            display.on.screen = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#