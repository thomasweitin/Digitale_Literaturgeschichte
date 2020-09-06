############################ KLASSIFIKATION (STYLO) ############################
#------------------------------------------------------------------------------#

### Fuehrt Klassifikationen fuer mehrere Aufteilungen in Trainings- und 
### Testdaten mit den Funktionen des stylo-Packages durch.
###
### Ordnerstrukturen:
###
### corpora
###     |_Romankorpus_Classifier
###         |_training_set
###         |_testing_set
### results
###     |_[Ergebnisordner werden automatisch erstellt]

#------------------------------------------------------------------------------#
## Benoetigte Packages --------------------------------------------------------#
#------------------------------------------------------------------------------#

library(stylo)

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

path_sets <- "C:\\Rivalinnen\\corpora\\Romankorpus_Classifier"
path_results <- "C:\\Rivalinnen\\results\\Klassifikation\\Romankorpus_Classifier_STYLO" 

if (!dir.exists(path_results)) {
  dir.create(path_results, recursive = TRUE)                                     
}

# Die verschiedenen Aufteilungen in seed-Ordner werden aufgelistet
seeds <- list.dirs(path_sets, full.names = TRUE,
                   recursive = FALSE)

# Falls zusaetzliche Order im designierten Ordner liegen, werden sie mit einer     
# Regex ausgeschlossen
seeds <- seeds[grepl("classify_seed", seeds) ]                                   

#------------------------------------------------------------------------------#      
## Klassifikation -------------------------------------------------------------#
#------------------------------------------------------------------------------#

for (i in 1:length(seeds)) {
  # Erstellung des Ergebnisordners
  seed <- gsub(".+?/(.+?seed.+?)", "\\1", seeds[i])
  path <- paste0(path_results, "\\", seed)
  if (!dir.exists(path)) {
    dir.create(path)
  }
  setwd(path)
  # Definierung der Trainings- und Testsets
  training <- paste0(seeds[i], "\\training_set")
  testing <- paste0(seeds[i], "\\testing_set")
  classify <- classify(gui = FALSE,
               training.corpus.dir = training,
               test.corpus.dir = testing,
               corpus.format="plain",
               corpus.lang="German",
               analyzed.features="w",
               ngram.size=1,
               mfw.min = 100,
               mfw.max = 3000,
               mfw.incr = 100,
               culling.min = 20,
               culling.max = 20,
               culling.inc = 0,
               classification.method = "svm",
               culling.of.all.samples = TRUE,
               z.scores.of.all.samples = FALSE,
               reference.wordlist.of.all.samples = FALSE,
               svm.kernel="linear",
               svm.degree=3,
               svm.coef0=0,
               svm.cost=1,
               k.value=1,
               l.value=0)
  
  # Zusaetzliche Ausgabe der verwendeten Features
  classify_results <- classify$features.actually.used
  write(classify_results, "features_used.txt")
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#