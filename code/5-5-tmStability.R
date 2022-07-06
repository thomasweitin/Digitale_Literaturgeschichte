########################## TOPIC MODELING STABILITAET ##########################
#------------------------------------------------------------------------------#

### Berechnet die Stabilitaet von zwei Topic Models, indem zuerst die 
### passendsten Topic-Paare aus Modell 1 und Modell 2 identifiziert und
### dann die prozentualen Ueberlappungen von Topic Woertern berechnet werden.
###
### Ordnerstrukturen:
###
### results
###   |_TopicModeling
###       |_Romankorpus_18_Jahrhundert
###           |_100Topics
###               |_M1
###               |_M2
###               |_Stability
###       |_Romankorpus_1770-1830
###           |_100Topics
###               |_M1
###               |_M2
###               |_Stability

#------------------------------------------------------------------------------#
## Vorereinstellungen ---------------------------------------------------------#
#------------------------------------------------------------------------------#

topics <- 100

corpus <- "Romankorpus_18_Jahrhundert"
#corpus <- "Romankorpus_1770-1830"

path_results <-  "C:\\Rivalinnen\\results\\TopicModeling"

# Einlesen der Wortgewichte des Topic Models und des Kontrollmodells             
wordweightA <- read.csv(paste0(path_results, "\\", corpus, "\\", topics, "Topics\\M1\\TM_500chunksize_100numtopics_10000iteration_100twords__wordweight.csv"),   
                        sep = " ")
wordweightB <- read.csv(paste0(path_results, "\\", corpus, "\\", topics, "Topics\\M2\\TM_500chunksize_100numtopics_10000iteration_100twords__wordweight.csv"),   
                        sep = " ")

path_stability <- paste0(path_results, "\\", corpus, "\\", 
                         paste0(topics, "Topics"), "\\Stability")

if (dir.exists(path_stability) == FALSE) {
  dir.create(path_stability)
}

#------------------------------------------------------------------------------#
## Berechnung der Uebereinstimmung der Topic Woerter --------------------------#
#------------------------------------------------------------------------------#

max_intersect <- vector("list", length = topics)                                
id1 <- 0

# Fuer jedes Topic aus Modell 1 werden die Ueberlappungen in den Topic Woertern
# zu jedem Topic aus Modell 2 berechnet. Da die Topic Woerter in den ungeraden
# Spalten angefuehrt werden, werden nur diese anvisiert.
for (i in seq(1, ncol(wordweightA), by = 2)) { # i <- 1
  id1 <- id1 + 1
  print(id1)
  max <-  0
  col_max <- 0
  df <- data.frame(matrix(NA, nrow = 100, ncol = 2))
  id2 <- 0
  for (j in seq(1, ncol(wordweightB), by = 2)) { # j <- 1
    id2 <- id2 + 1
    k1 <- as.character(wordweightA[,i])
    k2 <- as.character(wordweightB[,j])
    intersect <- length(intersect(k1, k2))
    df[id2,1] <- intersect
  }
  df[,2] <- rank(df[,1])
  ordered <- df[order(df[,1], decreasing = TRUE),]
  max_intersect[[id1]] <- ordered
}

#------------------------------------------------------------------------------#
## Zuordnung der Topics aus dem Kontrollmodell zum Topic Model ----------------#
#------------------------------------------------------------------------------#

ranks <- seq(1, topics)                                                          
corresponding_topics <- data.frame(matrix(NA, nrow = topics, ncol = 2))

# Jedem Topic aus dem Modell 1 wird zuerst Topic 1 aus dem Kontrollmodell
# zugeordnet. Wird in der Iteration durch die Topics ein passenderes Topic (i.e.
# mit mehr ueberlappenden Topic Woertern) gefunden, wird die Zuordnung 
# aktualisiert. Falls das Topic aus dem Kontrollmodell bereits an ein anderes 
# Topic vergeben ist, wird die bereits bestehende Zuordnung ueberprueft und
# eventuell ersetzt. 
for (x in 1:length(ranks)) { # x <- 1
  for (y in 1:length(ranks)) {  # y <- 2
    replace <- FALSE
    message("Modell 1: Topic ", y, " wird ausgewaehlt.")
    new_entry <- max_intersect[[y]][x,]
    model2_topic <- as.numeric(rownames(new_entry))
    model2_topic_overlap <- new_entry[,1]
    message("Modell 2: Topic ", model2_topic, " wird ausgewaehlt (Overlap = ", 
            model2_topic_overlap, ").")
    # Wurde dem Topic aus Model 1 bereits ein Topic zugordnet?
    if (any(is.na(corresponding_topics[y,])) == TRUE) {
      message("Modell 1: Es gibt noch keine bestehende Zuordnung.")
      replace <- TRUE
    } else {
      message("Modell 1: Die bestehende Zuordnung wird ueberprueft.")
      existing_entry <- corresponding_topics[y,]
      existing_model2_topic <- existing_entry[,1]
      existing_model2_topic_overlap <- existing_entry[,2]
      if (model2_topic_overlap > existing_model2_topic_overlap) {
        replace <- TRUE
      }
    }
    if (replace) {
      # Falls nicht, wurde das ausgewaehlte Topic aus Modell 2 bereits einem
      # anderen Topic zugeordnet?
      if (model2_topic %in% corresponding_topics[,1] == FALSE) {
        message("Modell 2: Es gibt noch keine bestehende Zuordnung.")
        # Falls nicht, werden die Topics einander zugeordnet
        corresponding_topics[y,] <- c(model2_topic, model2_topic_overlap)
        message("Topic ", model2_topic, " aus dem Kontrolltopic wurde Topic ", 
                y, " zugeordnet!")
      } else {
        # Falls es schon eine bestehende Zuordnung gibt, wird diese ueberprueft
        message("Modell 2: Die bestehende Zuordnung wird ueberprueft.")
        existing_entry <- corresponding_topics[which(corresponding_topics[,1] 
                                                     == model2_topic),]
        existing_model2_topic <- existing_entry[,1]
        existing_model2_topic_overlap <- existing_entry[,2]
        # Falls die neue Zuordnung mehr Ueberlappungen ergibt, wird die schon
        # bestehende Zuordnung geloescht und die neue Zuordnung eingetragen.
        if (model2_topic_overlap > existing_model2_topic_overlap) {
          corresponding_topics[which(corresponding_topics[,1] 
                                     == model2_topic),] <- c(NA, NA)
          corresponding_topics[y,] <- c(model2_topic, model2_topic_overlap)
          message("Topic ", model2_topic, " aus dem Kontrolltopic wurde Topic ", 
                  y, " zugeordnet!")
        }
      }
    }
    message("\n\n\n---\n\n\n")
  }
}

corresponding_topics <- cbind(Row.Names = rownames(corresponding_topics), 
                              corresponding_topics)
rownames(corresponding_topics) <- NULL
names(corresponding_topics) <- c("Topic aus Model 1",
                                 "Topic aus Model 2", 
                                 "Übereinstimmende Topic Wörter")

sum <- sum(corresponding_topics[,3])
keyword_stability <- (sum / (topics * 100)) * 100
corresponding_topics1 <- rbind(corresponding_topics, c(NA, keyword_stability))

write.csv(corresponding_topics1, paste0(path_stability, 
                                        "\\absolut_intersect.csv"))

corresponding_topics[,3] <- (corresponding_topics[,3]/100)*100
write.csv(corresponding_topics, paste0(path_stability, 
                                       "\\percent_intersect.csv"))


################################################################################
################################################################################