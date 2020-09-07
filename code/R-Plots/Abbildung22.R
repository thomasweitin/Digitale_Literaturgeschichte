################################## Abbildung 22 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 22: Topic N. 42 und N. 55 mit der 
### Verteilung der Topic-Gewichte aus dem gleichen Topic Model zum Romankorpus
### 18. Jahrhundert wie Abbildungen 8-12.

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(wordcloud)
library(ggplot2)
library(ggpubr)
library(gghighlight)
library(dplyr)

#------------------------------------------------------------------------------#
## WORDCLOUDS -----------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung22"

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
limit <- 100
wordweights <- wordweights[1:limit,]

# Struktur der Wordweight Datei:
# Spalten mit ungerader Ordnungszahl --> Topic-Woerter
# Spalten mit gerader Ordnungszahl --> Wortgewichte

# Nummer der Topics, das abgebildet werden soll
topic_no <- c(42, 55)

#------------------------------------------------------------------------------#
## Wordcloud ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

clouds <- vector(length = length(topic_no), mode='list')

for (i in 1:length(topic_no)) {
  index <- topic_no[i]*2
  df <- wordweights[,(index-1):index]
  colnames(df) <- c("Topic_Wörter", "Wortgewichte")
  png_file <- paste0(output_dir, "\\Wordcloud_Topic", topic_no[i], "_", 
                     limit,".png")
  png(png_file, width = 2500, height = 2500, bg = NA, res = 300)
  wordcloud(df$Topic_Wörter,
            df$Wortgewichte,
            scale = c(5.7,.01),
            min.freq = 1, max.words = 100, rot.per=0,
            random.order=FALSE)
  graphics.off()
}

#------------------------------------------------------------------------------#
## PROFILE --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Der Pfad zur Topics-in-docs Datei wird als String definiert
file <- paste0(tm_results, "\\TM_500chunksize_100numtopics_10000iteration_100twords_topics-in-docs.csv")

# Die Topics-in-docs Datei wird eingelesen

# Wir haben in topicModeling.R den Seperator als "X" definiert, weshalb die 
# Datei jetzt auch wieder mit demselben Seperator eingelesen wird.
topicswithnames <- read.table(file, header = TRUE, row.names = 1, sep = " ")
text_titles <- rownames(topicswithnames)

# Einlesen der Metadatentabelle
metadata <- read.table("C:\\Rivalinnen\\200817_Metadatentabelle_gesamt.csv",
                       sep = ";", quote = "", header = TRUE)

metadata$gender <- gsub("m", "männlich", metadata$gender)
metadata$gender <- gsub("w", "weiblich", metadata$gender)

#------------------------------------------------------------------------------#
## Profile --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

topic42 <- topicswithnames$Topic42
df_topic42 <- data.frame(as.factor(text_titles), topic42)
colnames(df_topic42) <- c("Texte", "Topic")

topic55 <- topicswithnames$Topic55
df_topic55 <- data.frame(as.factor(text_titles), topic55)
colnames(df_topic55) <- c("Texte", "Topic")

all_dfs <- list(df_topic42, df_topic55)
names(all_dfs) <- c("Topic42", "Topic55")

# Welche Texte sollen hervorgehoben werden? 
# In diesem Fall sollen Texte nach Autorengender unterschieden werden.

for (i in 1:length(all_dfs)) {
  df <- all_dfs[[i]]
  df <- merge(df, metadata, by.x = "Texte", by.y = "file_name", all.x = TRUE)
  df <- select(df, Texte, Topic, gender)
  title <- names(all_dfs[i])
  title_plot <- gsub("Topic(\\d+)", "Topic \\1", names(all_dfs[i]))
  plot <- ggplot(df, aes(x=Texte, y=Topic)) +
          geom_bar(stat = "identity", aes(fill = gender), alpha = 8/10) +
          scale_fill_grey(start = 0.8, end = 0.2) +
          theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.title = element_text(hjust = 0.5, size=26),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = c(0.9,0.9),
          legend.title=element_blank()) +
    ggtitle(title_plot)
  
  filename <- gsub("Topic(\\d+)", 
                   "\\Abbildung22_Topic\\1.png", title)
  filename_complete <- paste0(output_dir, "\\", filename)
  ggsave(filename_complete, width = 50, height = 30, bg = "transparent",
         dpi = 320, units = "cm", limitsize = FALSE)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#