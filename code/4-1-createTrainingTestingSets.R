############################ TRAININGS- UND TESTSETS ###########################
#------------------------------------------------------------------------------#

### Erstellt mehrere zufaellig aufgeteilte Test- und Trainingssets fuer 
### Klassifikationen. Dabei wird sichergestellt, dass fuer die Untersuchungen 
### relevante Texte immer in den Testsets sind.
###
### Ordnerstrukturen:
###
### corpora
###     |_Romankorpus_gesamt
###         |_male
###         |_female
###     |_Romankorpus_Classifier                                           
###         |_[Ordner werden automatisch erstellt]

#------------------------------------------------------------------------------#
## Durchfuehrung --------------------------------------------------------------#
#------------------------------------------------------------------------------#

path_result <- "C:\\Rivalinnen\\corpora\\Romankorpus_Classifier" 
path_corpora <-  "C:\\Rivalinnen\\corpora" 

if(!dir.exists(path_result)){
  dir.create(path_result)
}

num_seeds <- 4
for (i in 1:num_seeds) {
  set.seed(i)
  # seed -> es werden immer dieselben zufaellig generierten Zahlen ausgegeben
  overall_dir <-  paste0(path_result, "\\Romankorpus_classify_seed-", i)          
  training_set <- paste0(path_result, "\\Romankorpus_classify_seed-", i,         
                         "\\training_set")
  testing_set <- paste0(path_result, "\\Romankorpus_classify_seed-", i,          
                        "\\testing_set")
  split_dir <- c(overall_dir, training_set, testing_set)
  
  for (dir in split_dir){
    if(!dir.exists(dir)){
      dir.create(dir)
    }
  }
  
  ##############################################################################
  
  # Aufteilung aller Texte von Autoren auf Trainings- und Testset
  corpus_m <- paste0(path_corpora, "\\Romankorpus_gesamt\\male")                  
  
  filelist_m <- list.files(path = corpus_m, pattern = NULL,
                           full.names = TRUE, recursive = FALSE)
  
  current.count_m <- length(filelist_m)
  # Die Texte werden im Verhaeltnis 80-20 auf die Sets verteilt
  # trunc = ohne Kommastellen
  countTesting_m <- trunc((current.count_m/100) * 20) 
  
  sampling_ids_m <- sample.int(current.count_m, countTesting_m, replace = FALSE)
  
  for (i in 1:current.count_m) {
    if (i %in% sampling_ids_m) {
      file.copy(filelist_m[i], testing_set)
    } else {
      file.copy(filelist_m[i], training_set)
    }
  }
  
  ##############################################################################
  
  # Aufteilung aller Texte von Autorinnen auf Trainings- und Testset
  corpus_f <- paste0(path_corpora, "\\Romankorpus_gesamt\\female")              
  
  filelist_f <- list.files(path = corpus_f, pattern = NULL,
                           full.names = TRUE, recursive = FALSE)
  
  current.count_f <- length(filelist_f)
  # Die Texte werden im Verhaeltnis 80-20 auf die Sets verteilt
  # trunc = ohne Kommastellen
  countTesting_f <- round((current.count_f/100) * 20) 
  
  sampling_ids_f <- sample.int(current.count_f, countTesting_f, replace = FALSE)
  
  # Durch eine Iteration der Ids werden die Ids der Texte gefunden, die 
  # besonders relevant sind
  for (i in 1:current.count_f) {
    if (grepl("w_La-Roche_Sternheim-Teil-1_1771", filelist_f[i], fixed = TRUE)) {
      id_Sternheim1 <- i
    } 
    if (grepl("w_La-Roche_Sternheim-Teil-2_1771", filelist_f[i], fixed = TRUE)) {
      id_Sternheim2 <- i
    }
    if (grepl("w_Wobeser_Elisa_1795", filelist_f[i], fixed = TRUE)) {
      id_Elisa <- i
    }
    if (grepl("w_Unger_Bekenntnisse_1806", filelist_f[i], fixed = TRUE)) {
      id_Bekenntnisse <- i
    }
  }
  
  important_case_studies <- c(id_Sternheim1, id_Sternheim2, id_Elisa, id_Bekenntnisse)
  
  # Falls die relevanten Texte den Trainingsdaten zugeordnet worden sind,
  # werden sie durch andere Texte ersetzt.
  for (case in important_case_studies) {
    if (case %in% sampling_ids_f == FALSE) {
      x <- min(which(sampling_ids_f %in% important_case_studies == FALSE))
      sampling_ids_f <- replace(sampling_ids_f, x, case)
    }
  }
  
  for (i in 1:current.count_f) {
    if (i %in% sampling_ids_f) {
      file.copy(filelist_f[i], testing_set)
    } else {
      file.copy(filelist_f[i], training_set)
    }
  }
}

################################################################################
################################################################################