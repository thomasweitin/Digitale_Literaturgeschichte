################## TRAININGS- UND TESTSETS (OPPOSE / CLASSIFY) #################
#------------------------------------------------------------------------------#

### Erstellt mehrere zufaellig aufgeteilte Oppose und Classify Sets fuer 
### Klassifikationen. Dabei wird sichergestellt, dass fuer die Untersuchungen 
### relevante Texte immer in den Testsets des Classify Sets sind.
###
### Ordnerstrukturen:
###
### corpora
###     |_Romankorpus_gesamt
###         |_male
###         |_female
###     |_Romankorpus_Oppose
###         |_[Ergebnisordner werden automatisch erstellt]

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

`%nin%` = Negate(`%in%`)

oppose_dir <- "C:\\Rivalinnen\\corpora\\Romankorpus_Oppose" 

if(!dir.exists(oppose_dir)){
  dir.create(oppose_dir)
}

#------------------------------------------------------------------------------#
## Datenaufteilung: Texte von Autorinnen --------------------------------------#
#------------------------------------------------------------------------------#

corpus_f <- "C:\\Rivalinnen\\corpora\\Romankorpus_gesamt\\female"
filelist_f <- list.files(path = corpus_f, pattern = NULL, full.names = TRUE, 
                         recursive = FALSE)
len_filelist_f <- length(filelist_f)

for (i in 1:len_filelist_f) {
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

proportion_oppose_f <- trunc(len_filelist_f/2)
proportion_classify_f <- len_filelist_f - proportion_oppose_f
proportion_classify_train_f <- trunc(proportion_classify_f/100 * 80)
proportion_classify_test_f <- proportion_classify_f - proportion_classify_train_f
proportion_primary_f <- trunc(proportion_oppose_f/100 * 80)
proportion_test_f <- proportion_oppose_f - proportion_primary_f

#------------------------------------------------------------------------------#
## Datenaufteilung: Texte von Autoren -----------------------------------------#
#------------------------------------------------------------------------------#

corpus_m <-"C:\\Rivalinnen\\corpora\\Romankorpus_gesamt\\male"

filelist_m <- list.files(path = corpus_m, pattern = NULL, full.names = TRUE,
                         recursive = FALSE)
len_filelist_m <- length(filelist_m)
proportion_oppose_m <- trunc(len_filelist_m/2)
proportion_classify_m <- len_filelist_m - proportion_oppose_m
proportion_classify_train_m <- trunc(proportion_classify_m/100 * 80)
proportion_classify_test_m <- proportion_classify_m - proportion_classify_train_m
proportion_secondary_m <- trunc(proportion_oppose_m/100 * 80)
proportion_test_m <- proportion_oppose_m - proportion_secondary_m

#------------------------------------------------------------------------------#
## Datenaufteilung: Seeds -----------------------------------------------------#
#------------------------------------------------------------------------------#

for (i in 1:4) {
  seed <- i
  set.seed(seed)
  # Erstellung der Ordner
  oppose_dir_i <- paste0(oppose_dir, "\\seed", seed)
  oppose <- paste0(oppose_dir_i, "\\oppose")
  classify <- paste0(oppose_dir_i, "\\classify")
  primary_dir <- paste0(oppose, "\\primary_set")
  secondary_dir <- paste0(oppose, "\\secondary_set") 
  test_dir <- paste0(oppose, "\\test_set")
  testing_set <- paste0(classify, "\\testing")
  training_set <- paste0(classify, "\\training")
  split_dir <- c(oppose_dir_i, oppose, classify, primary_dir, secondary_dir, 
                 test_dir, testing_set, training_set)
  for (dir in split_dir){
    if(!dir.exists(dir)){
      dir.create(dir)
    }
  }
  
  ##############################################################################
  
  # Texte von Autorinnen
  
  # Die vier besonders relevanten Texte sind die Testdaten fuer das Subset der
  # Texte von Autorinnen. Da damit der Anteil der Testdaten schon abgedeckt ist,
  # muessen keine zusaetzlichen Texte von Frauen zu den Testdaten hinzugefuegt 
  # werden.
  classify_test_ids_f <- filelist_f[important_case_studies]
  for (i in 1:length(classify_test_ids_f)) {
    file.copy(classify_test_ids_f[i], testing_set)
  }
  spare_ids_f <- which(filelist_f %nin% classify_test_ids_f)
  spare_f <- filelist_f[spare_ids_f]
  
  # Trainingsdaten
  classify_train_f <- sample.int(length(spare_f), proportion_classify_train_f, 
                                 replace = FALSE)
  classify_train_ids_f <- spare_f[classify_train_f]
  
  for (i in 1:length(classify_train_ids_f)) {
    file.copy(classify_train_ids_f[i], training_set)
  }
  
  spare_ids_f <- which(spare_f %nin% classify_train_ids_f)
  spare_f <- spare_f[spare_ids_f]
  
  # Oppose
  oppose_primary <- sample.int(length(spare_f), proportion_primary_f, 
                               replace = FALSE)
  oppose_primary_ids_f <- spare_f[oppose_primary]
  
  for (i in 1:length(oppose_primary_ids_f)) {
    file.copy(oppose_primary_ids_f[i], primary_dir)
  }
  
  spare_ids_f <- which(spare_f %nin% oppose_primary_ids_f)
  spare_f <- spare_f[spare_ids_f]
  
  for (i in 1:length(spare_f)) {
    file.copy(spare_f[i], test_dir)
  }
  
  ##############################################################################
  
  # Texte von Autoren
  # Testdaten
  classify_test_m <- sample.int(length(filelist_m), proportion_classify_test_m, 
                                replace = FALSE)
  classify_test_ids_m <- filelist_m[classify_test_m]
  
  for (i in 1:length(classify_test_ids_m)) {
    file.copy(classify_test_ids_m[i], testing_set)
  }
  
  spare_ids_m <- which(filelist_m %nin% classify_test_ids_m)
  spare_m <- filelist_m[spare_ids_m]
  
  # Trainingsdaten
  classify_train_m <- sample.int(length(spare_m), proportion_classify_train_m, 
                                 replace = FALSE)
  classify_train_ids_m <- spare_m[classify_train_m]
  
  for (i in 1:length(classify_train_ids_m)) {
    file.copy(classify_train_ids_m[i], training_set)
  }
  
  spare_ids_m <- which(spare_m %nin% classify_train_ids_m)
  spare_m <- spare_m[spare_ids_m]
  
  # Oppose
  oppose_secondary <- sample.int(length(spare_m), proportion_secondary_m, 
                                 replace = FALSE)
  oppose_secondary_ids_m <- spare_m[oppose_secondary]
  
  for (i in 1:length(oppose_secondary_ids_m)) {
    file.copy(oppose_secondary_ids_m[i], secondary_dir)
  }
  
  spare_ids_m <- which(spare_m %nin% oppose_secondary_ids_m)
  spare_m <- spare_m[spare_ids_m]
  
  for (i in 1:length(spare_m)) {
    file.copy(spare_m[i], test_dir)
  }
}


################################################################################
################################################################################