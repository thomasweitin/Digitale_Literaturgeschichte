################################## Abbildung 25 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 25: Die beiden Topics, die in der 
### Stabilitaetsberechnung aus den Kontrollmodellen dem Topic N. 26 aus dem 
### Topic Model zum Romankorpus und dem Topic N. 21 zum Romankorpus Goethezeit
### zugeordnet wurden. Reihenfolge wie in Abbildung 15.

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(wordcloud)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung25"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}
corpus_18Jhd <- "Romankorpus_18_Jahrhundert"
corpus_1770_1830 <- "Romankorpus_1770-1830"
tm_results_18Jhd<- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                          corpus_18Jhd, "\\100Topics\\M2")
tm_results_1770_1830<- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                              corpus_1770_1830, "\\100Topics\\M2")

# Der Pfad zur Wordweight Datei wird als String definiert
file_18Jhd<- paste0(tm_results_18Jhd,
                    "\\TM_500chunksize_100numtopics_10000iteration_100twords__wordweight.csv")

wordweights_18Jhd<- read.csv(file_18Jhd, sep = " ")

file_1770_1830 <- paste0(tm_results_1770_1830,
                         "\\TM_500chunksize_100numtopics_10000iteration_100twords__wordweight.csv")

wordweights_1770_1830 <- read.csv(file_1770_1830, sep = " ")

#------------------------------------------------------------------------------#
## Herausfiltern des Topics ---------------------------------------------------#
#------------------------------------------------------------------------------#

# Wie viele Topic-Woerter sollen abgebildet werden? Das wordweights Dataframe 
# wird entsprechend abgeschnitten.
limit <- 100
wordweights_18Jhd <- wordweights_18Jhd[1:limit,]
wordweights_1770_1830 <- wordweights_1770_1830[1:limit,]

# Struktur der Wordweight Datei:
# Spalten mit ungerader Ordnungszahl --> Topic-Woerter
# Spalten mit gerader Ordnungszahl --> Wortgewichte

# Nummer der Topics, das abgebildet werden soll
topic_no <- c(19, 96)

#------------------------------------------------------------------------------#
## Wordcloud ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

clouds <- vector(length = length(topic_no), mode='list')

for (i in 1:length(topic_no)) {
  if (i == 1) {
    wordweights <- wordweights_18Jhd
  }
  else {
    wordweights <- wordweights_1770_1830
  }
  index <- topic_no[i]*2
  df <- wordweights[,(index-1):index]
  colnames(df) <- c("Topic_Wörter", "Wortgewichte")
  png_file <- paste0(output_dir, "\\Wordcloud_Topic", topic_no[i], "_", 
                     limit,".png")
  png(png_file, width = 2500, height = 2500, bg = NA, res = 320)
  wordcloud(df$Topic_Wörter,
            df$Wortgewichte,
            scale = c(6.8,.01),
            min.freq = 1, max.words = 100, rot.per=0,
            random.order=FALSE)
  graphics.off()
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#