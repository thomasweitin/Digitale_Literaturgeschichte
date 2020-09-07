################################# Abbildung 19 #################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 19: Topic N. 15 und N. 42 und N. 100 
### aus dem gleichen Topic Model zum Romankorpus 18. Jahrhundert wie
### Abbildungen 8 und 9.

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(wordcloud)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung19"

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

# Nummer der Topics, das abgebildet werden soll
topic_no <- c(15, 42, 100)

#------------------------------------------------------------------------------#
## Wordcloud ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

for (i in 1:length(topic_no)) {
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