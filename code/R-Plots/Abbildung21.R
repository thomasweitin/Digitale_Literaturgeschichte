################################## Abbildung 21 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 21: Topic N.1, N. 12 und N. 36 mit 
### Verteilung der Topic-Gewichte aus dem gleichen Topic Model zum Romankorpus
### 18. Jahrhundert wie Abbildungen 8-11.

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(wordcloud)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(gghighlight)

#------------------------------------------------------------------------------#
## WORDCLOUDS -----------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung21"

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
topic_no <- c(1, 12, 36)

#------------------------------------------------------------------------------#
## Wordcloud ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

for (i in 1:length(topic_no)) {
  index <- topic_no[i]*2
  df <- wordweights[,(index-1):index]
  colnames(df) <- c("Topic_Wörter", "Wortgewichte")
  png_file <- paste0(output_dir, "\\Wordcloud_Topic", topic_no[i], "_", 
                     limit,".png")
  png(png_file, width = 2500, height = 2500, bg = NA, res = 300)
  wordcloud(df$Topic_Wörter,
            df$Wortgewichte,
            scale = c(6.8,.01),
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

#------------------------------------------------------------------------------#
## Profile --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

topic1 <- topicswithnames$Topic1
df_topic1 <- data.frame(as.factor(text_titles), topic1)
colnames(df_topic1) <- c("Texte", "Topic")

topic12 <- topicswithnames$Topic12
df_topic12 <- data.frame(as.factor(text_titles), topic12)
colnames(df_topic12) <- c("Texte", "Topic")

topic36 <- topicswithnames$Topic36
df_topic36 <- data.frame(as.factor(text_titles), topic36)
colnames(df_topic36) <- c("Texte", "Topic")

all_dfs <- list(df_topic1, df_topic12, df_topic36)
names(all_dfs) <- c("Topic1", "Topic12", "Topic36")

# Welche Texte sollen hervorgehoben werden? 
# In diesem Fall werden Texte hervorgehoben, die mit den verschiedenen Autoren-
# namen beginnen. Im Profil von Topic 1 sollen Texte von Tieck hervorgehoben 
# werden, im Profil von Topic 12 die Texte von Wieland, etc.
pattern <- c("Tieck", "Wieland", "Paul")

all_plots <- list()

for (i in 1:length(all_dfs)) {
  df <- all_dfs[[i]]
  title <- names(all_dfs[i])
  title_plot <- gsub("Topic(\\d+)", "Topic \\1", names(all_dfs[i]))
  labelled <- text_titles[grepl(pattern[i], text_titles)]
  indeces <- which(as.character(df$Texte) %in% labelled)
  df_labelled <- df[indeces,]
  plot <- ggplot(df, aes(x=Texte, y=Topic)) +
    geom_bar(stat = "identity", alpha = 8/10) +
    scale_color_grey() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          plot.title = element_text(hjust = 0.5, size=26),
          legend.position = "none") +
    ggtitle(title_plot) +
    gghighlight(Texte %in% labelled) +
    geom_text_repel(data=df_labelled, 
                    aes(label=Texte),
                    size=6)
  
  all_plots[[i]] <- plot
  filename <- gsub("Topic(\\d+)", 
                   "\\Abbildung21_Topic\\1.png", title)
  filename_complete <- paste0(output_dir, "\\", filename)
  ggsave(filename_complete, width = 50, height = 30, dpi = 320, 
         bg = "transparent", units = "cm", limitsize = FALSE)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#