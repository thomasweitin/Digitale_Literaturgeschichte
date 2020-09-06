############################ KLASSIFIKATION (CARET) ############################
#------------------------------------------------------------------------------#

### Fuehrt Klassifikationen fuer mehrere Aufteilungen in Trainings- und 
### Testdaten mit den Funktionen des caret-Packages durch.
###
### Ordnerstrukturen:
###
### corpora
###     |_Romankorpus_gesamt
### results
###     |_[Ergebnisordner werden automatisch erstellt]

#------------------------------------------------------------------------------#
## Benoetigte Packages --------------------------------------------------------#
#------------------------------------------------------------------------------#

library(caret)
library(stylo)
library(kernlab)

#------------------------------------------------------------------------------#
## Parameter ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

path_results <- "C:\\Rivalinnen\\results\\Klassifikation\\Romankorpus_Classifier_CARET"

if (!dir.exists(path_results)) {
  dir.create(path_results, dir)
}

corpus_dir <- "C:\\Rivalinnen\\corpora\\Romankorpus_gesamt\\all"
MFW <- 3000
start_at <- 1
distance_measure <- "dist.delta"
ngram_size <- 1
culling <- 20
seeds <- c(100, 101, 102, 103)

#------------------------------------------------------------------------------#
## Stylo-Ausfuehrung  ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Die Analysen werden in ein Batch abgespeichert, um spaeter zusaetzliche
# Informationen extrahieren zu koennen.
batch <- stylo(gui = FALSE, corpus.dir = corpus_dir, 
               corpus.format = "plain", 
               corpus.lang = "German", encoding = "UTF-8", 
               mfw.min = MFW, mfw.max = MFW, mfw.incr = 0, 
               culling.min = culling, culling.max = culling, culling.incr = 0, 
               analyzed.features = "w",
               delete.pronouns = FALSE,
               start.at = start_at,
               ngram.size = ngram_size, distance.measure = distance_measure)

# Frequenztabellen fuer 3,000MFW
freq_df <- as.data.frame(batch$table.with.all.freqs)

all_predictions <- data.frame(MFW = numeric(),
                 accuracy = numeric(), precision = numeric(),
                 recall = numeric(), f1 = numeric(),
                 incorrect = character(), stringsAsFactors = FALSE)

# Fuer jede zufaellige Aufteilung in Trainings- und Testdaten wird die
# Frequenztabelle so durchiteriert, dass Klassifikationen in 100er-Schritten
# von 3,000 bis 100MFW durchgefuehrt werden.
for (s in 1:length(seeds)) {                                                    
  set.seed(seeds[s])
  for (i in 1:30) {
    x <- (i-1)*100
    cut_off <- 3000 - x
    freq_df_cutoff <- freq_df[,1:cut_off]
    
    freq_df_cutoff$gender <- rownames(freq_df_cutoff)
    freq_df_cutoff$gender <- gsub("(w|m)_.+", "\\1", freq_df_cutoff$gender)      
    
    # Die besonders relevanten Texte werden immer als Teil der Testdaten 
    # deklariert
    test_cases <- c("w_La-Roche_Sternheim-Teil-1_1771", 
                    "w_La-Roche_Sternheim-Teil-2_1771",
                    "w_Wobeser_Elisa_1795",
                    "w_Unger_Bekenntnisse_1806")
    test_cases_df <- freq_df_cutoff[test_cases,]
    # Alle Texte, die nicht unbedingt in den Testdaten sein muessen, werden
    # in ein neues Dataframe geschrieben
    freq_without <- freq_df_cutoff[!(row.names(freq_df_cutoff) 
                                     %in% test_cases), ]
    # Dieses Dataframe wird nun entsprechend in Trainings- und Testdaten 
    # aufgeteilt
    sample <- sample.int(n = nrow(freq_without), 
                         size = floor(.83*nrow(freq_without)), replace = FALSE)
    train <- freq_without[sample, ]
    test  <- freq_without[-sample, ]
    # Die Testfaelle werden zum Testset hinzugefuegt
    test <- rbind(test, test_cases_df)
    # Initiierung der Cross-Validation
    ctrl <- trainControl(method = "cv", number = 10,
                         savePred = TRUE, classProb = TRUE)
    # Initiierung des Classifiers
    mod <- train(gender ~ ., data = train, 
                 method = "svmLinear", trControl = ctrl)
    # Anwendung auf die Testdaten
    predictions <- predict.train(object = mod, test, type="raw")
    # Erstellung einer Confusion Matrix & Extrahierung der relevanten Metriken
    conf <- confusionMatrix(predictions, as.factor(test$gender))
    accuracy <- conf$overall[[1]]
    precision <- conf$byClass[[5]]
    recall <- conf$byClass[[6]]
    f1 <- 2*((precision*recall)/(precision + recall))
    # Zusammenfassung der Klassifikationen
    test$gender_predicted <- predictions
    results_prediction <- test[,c("gender", "gender_predicted")]
    j <- which(!(results_prediction$gender == results_prediction$gender_predicted))
    df <- results_prediction[j, ]
    texts <- rownames(df)
    texts_r <- paste(texts, sep = ",", collapse = " ")
    results <- c(cut_off, accuracy, precision, recall, f1, texts_r)
    all_predictions[i,] <- results
    print(i)
    
  }
  write.table(all_predictions, 
              paste0(path_results, "\\all_predictions_all_values_seed", 
                     seeds[s], ".csv"), col.names = NA, sep = " ")              
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#