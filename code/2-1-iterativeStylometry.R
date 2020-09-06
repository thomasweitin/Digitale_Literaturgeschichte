############################# ITERATIVE STILOMETRIE ############################
#------------------------------------------------------------------------------#

### Fuehrt stilometrische Analysen fuer mehrere Korpora mit mehreren 
### Parameter-Settings durch.
###
### Ordnerstrukturen:
###
### corpora
###     |_Romankorpus_18_Jahrhundert
###     |_Romankorpus_1770-1830
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

if(!dir.exists(path_results)){
  dir.create(path_results)
}

path_corpora <- "C:\\Rivalinnen\\corpora"
setwd(path_corpora)

#------------------------------------------------------------------------------#
## Parameter ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# PARAMETER-EINSTELLUNGEN
# 
# in Listen gespeichert:  x[1] = MFW
#                         x[2] = Pronomen loeschen?
#                         x[3] = welches Distanzmaß ("dist.delta"/
#                                 "cosine")?
#                         x[4] = ab welcher MFW-Position (1, 100)?

param_3000MFW_burrows <- list(3000, FALSE, "dist.delta", 1)
param_3000MFW_burrows_noPronouns <- list(3000, TRUE, "dist.delta", 1)
param_3000MFW_burrows_startat100 <- list(3000, FALSE, "dist.delta", 100)
param_3000MFW_cosine <- list(3000, FALSE, "cosineDelta", 1)
# cosineDelta --> generalFunctions.R

# LISTE VON PARAMETER-EINSTELLUNGEN

# Zugriff mit Index: parameter_settings[1]

parameter_settings <- list(param_3000MFW_burrows, 
                           param_3000MFW_burrows_noPronouns, 
                           param_3000MFW_burrows_startat100, 
                           param_3000MFW_cosine)

# Falls nur einige Parameter verwendet werden sollen, kann eine andere Liste 
# angelegt werden.
# z.B.:
# parameter_settings_cos <- list(param_500MFW_cosine, param_1000MFW_cosine)

#------------------------------------------------------------------------------#
## Korpora --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

# KORPORA

# corp1 etc -> komplette Pfade zu den Korpora, die bearbeitet werden sollen

corpus_1 <- paste0(path_corpora, "\\Romankorpus_1770-1830")
corpus_2 <- paste0(path_corpora, "\\Romankorpus_18_Jahrhundert")

# LISTE VON KORPORA

# Zugriff mit Index: corpora[1]

corpora <- list(corpus_1, corpus_2)
len <- length(corpora)

#------------------------------------------------------------------------------#
## Ergebnisordner erstellen ---------------------------------------------------#
#------------------------------------------------------------------------------#

results_dir <- c("\\burrows-delta", "\\cosine-delta")

for (dir in results_dir){
  if(!dir.exists(paste0(path_results, dir))){
    dir.create(paste0(path_results, dir))
  }
}

#------------------------------------------------------------------------------#
## Stilometrische Analysen ----------------------------------------------------#
#------------------------------------------------------------------------------#

# STYLO-LOOP

for (i in 1:len){
  corpus <- as.character(corpora[i])
  # Kuerzung der Pfade auf den eigentlichen Korpusnamen, die dann in 
  # path_result_corpus weiterverwendet werden
  corpus_name <- gsub("(.+?corpora\\\\)(.+?)?", "\\2", 
                      corpus)
  # Erstellung des path_result_corpus aus den uebergebenen Parameter-Einstellungen
    for (setting in parameter_settings){
      MFW <- as.numeric(setting[1])
      # Booleans in der Liste -> werden als Strings abgespeichert
      # Work-around: String-Identitaet abfragen; daraus TRUE/FALSE ableiten
      pronounsTF <- if (setting[2] == "TRUE"){
        pronounsTF <- TRUE
      } else {
        pronounsTF <- FALSE
      }
      distance_measure <- setting[[3]]
      start_at <- as.numeric(setting[4])
      # Wenn "dist.delta" && keine Pronomenloeschung & kein Cut-off fuer MFW:
      if (distance_measure == "dist.delta" & 
          pronounsTF == FALSE & 
          start_at == 1){
        path_result_corpus <- paste0(path_results, 
                                     "\\burrows-delta\\basic\\", 
                                     corpus_name, "_", MFW,
                                     "MFW_", "burrows-delta")
      }
      # Wenn "dist.delta" && Pronomenloeschung && kein Cut-off fuer MFW:
      else if (distance_measure == "dist.delta" &
               pronounsTF == TRUE &
               start_at == 1){
        path_result_corpus <- paste0(path_results, 
                                     "\\burrows-delta\\advanced\\", 
                                     corpus_name, "_", MFW, 
                                     "MFW_", "burrows-delta", 
                                     "_withoutpronouns")
      }
      # Wenn "dist.delta" && keine Pronomenloeschung && Cut-off fuer MFW:
      else if (distance_measure == "dist.delta" & 
               pronounsTF == FALSE &
               start_at == 100){
        path_result_corpus <- paste0(path_results,
                                     "\\burrows-delta\\advanced\\", 
                                     corpus_name, "_", MFW, 
                                     "MFW_", "burrows-delta", "_startat100")
      }
      # Sonst: cosine
      else {
        path_result_corpus <- paste0(path_results, 
                                     "\\cosine-delta\\basic\\", 
                                     corpus_name, "_", MFW, 
                                     "MFW_", "cosine-delta")
      }
      
      # Stylo in Variable schreiben
      batch <- stylo(gui = FALSE, corpus.dir = corpus_name, 
            corpus.format = "plain", analysis.type = "CA",
            corpus.lang = "German", encoding = "UTF-8", 
            mfw.min = MFW, mfw.max = MFW, mfw.incr = 0, 
            culling.min = 20, culling.max = 20, culling.incr = 0, 
            analyzed.features = "w",
            delete.pronouns = pronounsTF,
            start.at = start_at,
            ngram.size = 1, distance.measure = distance_measure,
            using.existing.wordlist = FALSE, save.distance.tables = TRUE, 
            save.analyzed.features = TRUE, save.analyzed.freqs = TRUE,
            write.png.file = TRUE, write.pdf.file = TRUE,
            plot.custom.height = 15, plot.custom.width = 15,
            display.on.screen = FALSE)
      
      # Zscores aus batch in eine Datei schreiben 
      # Ueberpruefen, ob genug MFWs vorhanden sind
      batch_zscores <- batch$table.with.all.zscores
      all.zs <- t(batch_zscores)
      if (ncol(batch_zscores) >= MFW){
        zs <- batch_zscores[,1:MFW]
      } else {
        zs <- batch_zscores
      }
      
      filename_zscore <- paste0(corpus, "_", MFW, "MFW", "_zscores.csv")
      
      write.csv(zs, file = filename_zscore)
      
      # Alle Dateien, die in path_corpora geschrieben werden & die 
      # auf .csv/.txt/.pdf/.png enden, werden in path_result_corpus geschrieben
      files <- list.files(path = ".", pattern = ".(csv|txt|pdf|png)", 
                          full.names = TRUE)
      for (file in files){
        file_name <- gsub("(.+/)(.+?(csv|txt))", "\\2", file)
        myFileRename(from = file,
                     to = paste0(path_result_corpus, "\\" ,file_name))
        # myFileRename --> generalFunctions.R
      }
      message("----------------------------------------------------------------",
              "\n",
              "----------------------------------------------------------------",
              "\n",
              
                  "corpus: ", corpus_name, "\n",
                  " MFW = ", MFW, "\n",
                  " distance measure = ", distance_measure, "\n",
                  " deleted pronouns: ", pronounsTF, "\n",
                  " start at position: ", start_at, 
                  "\n is done!", 
              "\n",
              "----------------------------------------------------------------",
              "\n",
              "----------------------------------------------------------------")
    }
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#