############################### CLASSIFY / OPPOSE ##############################
#------------------------------------------------------------------------------#

### Fuehrt zuerst eine Oppose-Funktion durch, die Woerter in zwei Listen 
### zurueckgibt, die in der ersten von zwei Gruppen von Texten besonders 
### ueber- und unterrepraesentiert sind. 
###
### Ordnerstrukturen:
###
### corpora
###     |_Romankorpus_Oppose
###         |_seed1
###             |_classify
###             |_oppose
### results
###     |_[Ergebnisordner werden automatisch erstellt]

#------------------------------------------------------------------------------#
## Benoetigte Packages --------------------------------------------------------#
#------------------------------------------------------------------------------#

library(stylo)
source("C:\\Rivalinnen\\code\\0-generalFunctions.R")

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

path_results <- "C:\\Rivalinnen\\results\\Klassifikation\\Romankorpus_Classifier_Oppose"

if (!dir.exists(path_results)) {
  dir.create(path_results, dir)
}

path_corpora <- "C:\\Rivalinnen\\corpora\\Romankorpus_Oppose"

#------------------------------------------------------------------------------#
## Funktionen -----------------------------------------------------------------#
#------------------------------------------------------------------------------#
                                                                                
call_oppose <- function(){
  oppose(gui = FALSE,
         oppose.method = "craig.zeta",
         primary.corpus.dir = "primary_set", 
         secondary.corpus.dir = "secondary_set",
         test.corpus.dir = "test_set",
         text.slice.length = 3000, 
         text.slice.overlap = 1000, 
         corpus.format="plain",
         corpus.lang = "German", 
         encoding = "UTF-8",
         write.pdf.file = TRUE,
         write.png.file = TRUE)
}

call_classify <- function(){ 
  classify( gui = FALSE,
            training.corpus.dir = "training",
            test.corpus.dir = "testing",
            corpus.format = "plain",
            corpus.lang = "German",
            analyzed.features = "w",
            mfw.min = wordcount,
            mfw.max = wordcount,
            ngram.size = 1,
            use.existing.wordlist = TRUE,
            features = wordlist,
            classification.method = "svm",
            culling.of.all.samples = TRUE,
            z.scores.of.all.samples = FALSE,
            reference.wordlist.of.all.samples = FALSE,
            distance.measure="delta",
            svm.kernel = "linear",
            svm.degree = 3,
            svm.coef0 = 0,
            svm.cost = 1,
            k.value = 1,
            l.value = 0)
}



seeds <- 4
for (i in 1:seeds) {
  #----------------------------------------------------------------------------#
  ## Oppose -------------------------------------------------------------------#
  #----------------------------------------------------------------------------#
  
  oppose_dir <- paste0(path_corpora, "\\seed", i, "\\oppose")
  path_seeds <-  paste0(path_results,"\\seed", i)
  wd <- paste0(path_seeds, "\\oppose")       
  setwd(oppose_dir)
  
  if (!dir.exists(wd)) {
    dir.create(wd, recursive = TRUE)
  }
  
  oppose_results <- call_oppose()
  
  scores_avoided <- oppose_results$words.avoided.scores
  write.csv(scores_avoided, "scores_avoided.csv")
  scores_preferred <- oppose_results$words.preferred.scores
  write.csv(scores_preferred, "scores_preferred.csv")
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(wd, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
  
  #----------------------------------------------------------------------------#
  ## Classify mit Oppose (preferred) ------------------------------------------#
  #----------------------------------------------------------------------------#
  
  wordlist <- as.vector(oppose_results$words.preferred)
  wordcount <- length(wordlist)
  
  classify_dir <- paste0(path_corpora, "\\seed", i, "\\classify")
  setwd(classify_dir)
  
  results_preferred <- call_classify()
  analyzed_features <- results_preferred$features.actually.used
  write(analyzed_features, "analyzed_features.txt")
  
  path_results_preferred <- paste0(path_results, "\\seed", i, "\\results_preferred")
  
  if (!dir.exists(path_results_preferred)) {
    dir.create(path_results_preferred, recursive = TRUE)
  }
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(path_results_preferred, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
  
  #----------------------------------------------------------------------------#
  ## Classify mit Oppose (avoided) --------------------------------------------#
  #----------------------------------------------------------------------------#
  
  wordlist <- as.vector(oppose_results$words.avoided)
  wordcount <- length(wordlist)
  
  results_avoided <- call_classify()
  analyzed_features <- results_avoided$features.actually.used
  write(analyzed_features, "analyzed_features.txt")
  
  path_results_avoided <- paste0(path_results, "\\seed", i, "\\results_avoided")
  
  if (!dir.exists(path_results_avoided)) {
    dir.create(path_results_avoided, recursive = TRUE)
  }
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(path_results_avoided, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
  
  #----------------------------------------------------------------------------#
  ## Classify mit Oppose (merged) ---------------------------------------------#
  #----------------------------------------------------------------------------#
  
  wordlist <- as.vector(c(oppose_results$words.avoided, 
                          oppose_results$words.preferred))
  wordcount <- length(wordlist)

  results_merged <- call_classify()
  analyzed_features <- results_merged$features.actually.used
  write(analyzed_features, "analyzed_features.txt")
  
  path_results_merged <- paste0(path_results, "\\seed", i, "\\results_merged")
  
  if (!dir.exists(path_results_merged)) {
    dir.create(path_results_merged, dir)
  }
  
  files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                      full.names = TRUE)
  for (file in files){
    file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
    myFileRename(from = file,
                 to = paste0(path_results_merged, "\\" ,file_name))
    # myFileRename --> generalFunctions.R
  }
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#