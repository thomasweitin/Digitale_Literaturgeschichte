################################## Abbildung 17 #################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 17: Topic N. 92 aus einem Topic Model 
### zum Romankorpus 18. Jahrhundert (86 Texte) mit 100 Topics auf der 
### Basis von 10.000 Iterationen, Chunking in Einheiten von je 500 Woertern,
### stopwords removed.

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(wordcloud)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung17"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}

corpus <- "Romankorpus_18_Jahrhundert"
tm_results<- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                    corpus, "\\100Topics\\M1")

# Der Pfad zur Wordweight Datei wird als String definiert
file <- paste0(tm_results,
               "\\TM_500chunksize_100numtopics_10000iteration_100twords__wordweight.csv")

wordweights <- read.csv(file, sep = " ")

#------------------------------------------------------------------------------#
## Herausfiltern des Topics ---------------------------------------------------#
#------------------------------------------------------------------------------#

# Wie viele Topic-Woerter sollen abgebildet werden? Das wordweights Dataframe 
# wird entsprechend abgeschnitten.
limit <- 25
wordweights <- wordweights[1:limit,]

# Struktur der Wordweight Datei:
# Spalten mit ungerader Ordnungszahl --> Topic-Woerter
# Spalten mit gerader Ordnungszahl --> Wortgewichte

# Nummer des Topics, das abgebildet werden soll
topic_no <- 92
index <- (topic_no*2)

# Auswahl des Topics
df <- wordweights[,(index-1):index]
colnames(df) <- c("Topic_Wörter", "Wortgewichte")

#------------------------------------------------------------------------------#
## Wordcloud ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

png_file <- paste0(output_dir, "\\Wordcloud_Topic", topic_no, "_", limit,".png")
png(png_file, width = 2500, height = 2500, bg = NA, res = 320)

wordcloud(df$Topic_Wörter,
          df$Wortgewichte,
          scale = c(8,.05),
          min.freq = 1, max.words = 100, rot.per=0,
          random.order=FALSE)


graphics.off()

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#