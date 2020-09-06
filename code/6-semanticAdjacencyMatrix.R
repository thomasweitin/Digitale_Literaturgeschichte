############################### SEMANTIC NETWORKS ##############################
#------------------------------------------------------------------------------#

### In diesem Skript werden Adjazenzmatrizen fuer semantische Netzwerke aus 
### Topic-Modeling-Daten erstellt.  
### Um eine Beziehung zu definieren, wird zunaechst ein Schwellenwert 
### festgelegt, der 1 oder 10 Prozent der hoechsten Werte beinhaltet. 
###
### Notwendige Vorarbeit - Ordner mit Topics-In-Docs-Dateien erstellen, 
### ueberpruefen, ob irgendwo in den Dateien NAs oder Leerstellen gibt.
###
### Ordnerstrukturen:
###
### results
###   |_TopicModeling
###       |_Romankorpus_18_Jahrhundert
###           |_100Topics
###               |_M1
###                 |_TM_500chunksize_100numtopics_10000iteration_100twords_topics-in-docs.csv
###               |_Netzwerke
###                 |_matrix_1_prozent
###                   |_1_prozent_Romankorpus_18_Jahrhundert_adjacency_matrix.csv
###                 |_matrix_10_prozent
###                   |_10_prozent_Romankorpus_18_Jahrhundert_adjacency_matrix.csv

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

# Working directory: Ordner mit Topics-In-Docs-Dateien, z.b. hier: 
corpus <- "Romankorpus_18_Jahrhundert"
#corpus <- "Romankorpus_1770-1830"

topics <- 100
path_results <-  "C:\\Rivalinnen\\results\\TopicModeling"
path_tm <- paste0(path_results, "\\", corpus, "\\", topics, "Topics\\M1")
setwd(path_tm)
files <- dir(pattern = ".+topics-in-docs.csv", full.names = TRUE)

#------------------------------------------------------------------------------#
## Definition der Schwellenwerte ----------------------------------------------#
#------------------------------------------------------------------------------#

# Prozentwert für den Schwellenwert definieren: 1 oder 10 
percent <- 1
#percent <- 10

# Alle Dateien einlesen und als Vektor darstellen, Schwellenwert errechnen
topics_in_docs<- list()
all_values<- list()
percent_fronteer <- list()
value <- list()

for (i in 1:length(files)) {
  topics_in_doc <- read.csv(files[i], sep= " ",
                             dec = ".", stringsAsFactors = FALSE, row.names = 1,
                            header = TRUE)
  ncols <- ncol(topics_in_doc)
  topics_in_doc <- topics_in_doc * 100
  topics_in_docs[[i]] <- topics_in_doc
  all_values[[i]] <- sort(
    unlist(c(topics_in_docs[[i]][1:dim(topics_in_docs[[i]])[1],
                                 1:dim(topics_in_docs[[i]])[2]])), 
                     decreasing = T)
  percent_fronteer[[i]] <- dim(topics_in_docs[[i]])[1] * percent
  value[[i]] <- all_values[[i]][percent_fronteer[[i]]]
}

# Werte in den Tabellen ersetzen: 0, wenn es unter dem Schwellenwert ist, 
# 1 wenn darueber
for (m in 1:length(files)){
  for(i in 1:dim(topics_in_docs[[m]])[1]){
     for(j in 1:dim(topics_in_docs[[m]])[2]){
        if (topics_in_docs[[m]][i,j] < value[[m]]){
              topics_in_docs[[m]][i,j] <- 0 
        } else {
              topics_in_docs[[m]][i,j] <- 1 
        }
      }
    }
  }

#------------------------------------------------------------------------------#
## Erstellung der Adjazenzmatrix ----------------------------------------------#
#------------------------------------------------------------------------------#

# Leere Listen fuer Matrizen erstellen: adj = Adjazenzmatrix, 
# Endung "_m" = Matrix, "_t" = transponierte Matrix.
topics_in_docs_m <- list()
topics_in_docs_t <- list()
adj <- list()

# Mit Loop einzelne Matrizen einlesen, transponieren und multiplizieren. 
# Auf Pfade achten!
for(i in 1:length(files)){ # i <- 1
    topics_in_docs_m[[i]] <- as.matrix(topics_in_docs[[i]])
    topics_in_docs_t[[i]] <- t(topics_in_docs_m[[i]])
    adj[[i]] <- topics_in_docs_m[[i]] %*% topics_in_docs_t[[i]]
    corpus <- gsub(".+TopicModeling\\\\(.+)\\\\\\d+Topics\\\\M.", "\\1", 
                   path_tm, perl = TRUE)
    dir.create(paste0(path_tm, "\\Netzwerke"), showWarnings = FALSE)
    dir.create(paste0(path_tm, "\\Netzwerke\\matrix_", percent, "_prozent"), 
               showWarnings = FALSE)
    write.table(adj[[i]], paste0(path_tm, "\\Netzwerke\\matrix_", percent, "_prozent\\", 
                               percent, "_prozent_", corpus, 
                               "_adjacency_matrix.csv"),
                sep = " ", row.names = TRUE, col.names = NA)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#