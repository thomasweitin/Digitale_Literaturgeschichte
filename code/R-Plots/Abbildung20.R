################################## Abbildung 20 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 20: Verteilung des Topic-Gewichts fuer 
### die Topics N. 15, N. 42, N. 92 und N. 100 im gleichen Topic Model zum
### Romankorpus 18. Jahrhundert wie Abbildungen 8-10.

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(ggrepel)
library(ggpubr)
library(gghighlight)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung20"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}

# Pfad zu Topic Modeling Ergebnissen wird als String vordefiniert
corpus <- "Romankorpus_18_Jahrhundert"
tm_results <- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                     corpus, "\\100Topics\\M1")

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

topic15 <- topicswithnames$Topic15
df_topic15 <- data.frame(as.factor(text_titles), topic15)
colnames(df_topic15) <- c("Texte", "Topic15")

indeces <- which(startsWith(as.character(df_topic15$Texte), "Ehrmann"))
df_labelled <- df_topic15[indeces,]
labelled <- as.character(df_labelled$Texte)

plot15 <- ggplot(df_topic15, aes(x=Texte, y=Topic15)) + 
  geom_bar(stat = "identity", alpha = 8/10) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5, size=26)) +
  gghighlight(Texte %in% labelled) +
  geom_text_repel(data=df_labelled, 
                  aes(label=Texte),
                  size=6) +
  ggtitle("Topic 15")

filename_means <-  paste0(output_dir, "\\Abbildung20_Topic15.png")
ggsave(filename_means, width = 50, height = 30, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#

topic42 <- topicswithnames$Topic42
df_topic42 <- data.frame(as.factor(text_titles), topic42)
colnames(df_topic42) <- c("Texte", "Topic42")

plot42 <- ggplot(df_topic42, aes(x=Texte, y=Topic42)) + 
  geom_bar(stat = "identity", alpha = 8/10) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5, size=26)) +
  ggtitle("Topic 42")

filename_means <-  paste0(output_dir, "\\Abbildung20_Topic42.png")
ggsave(filename_means, width = 50, height = 30, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#

topic92 <- topicswithnames$Topic92
df_topic92 <- data.frame(as.factor(text_titles), topic92)
colnames(df_topic92) <- c("Texte", "Topic92")

indeces <- which(startsWith(as.character(df_topic92$Texte), "La-Roche"))
df_labelled <- df_topic92[indeces,]
labelled <- as.character(df_labelled$Texte)

plot92 <- ggplot(df_topic92, aes(x=Texte, y=Topic92)) + 
  geom_bar(stat = "identity", alpha = 8/10) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5, size=26)) +
  gghighlight(Texte %in% labelled) +
  geom_text_repel(data=df_labelled, 
                  aes(label=Texte),
                  size=6) +
  ggtitle("Topic 92")

filename_means <-  paste0(output_dir, "\\Abbildung20_Topic92.png")
ggsave(filename_means, width = 50, height = 30, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#

topic100 <- topicswithnames$Topic100
df_topic100 <- data.frame(as.factor(text_titles), topic100)
colnames(df_topic100) <- c("Texte", "Topic100")

indeces <- which(startsWith(as.character(df_topic100$Texte), "Naubert"))
df_labelled <- df_topic100[indeces,]
labelled <- as.character(df_labelled$Texte)

plot100 <- ggplot(df_topic100, aes(x=Texte, y=Topic100)) + 
  geom_bar(stat = "identity", alpha = 8/10) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(hjust = 0.5, size=26)) +
  gghighlight(Texte %in% labelled) +
  geom_text_repel(data=df_labelled, 
                  aes(label=Texte),
                  size=6) +
  ggtitle("Topic 100")

filename <-  paste0(output_dir, "\\Abbildung20_Topic100.png")
ggsave(filename, width = 50, height = 30, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#

multiplot <- ggarrange(plot15, plot42, plot92, plot100,
                       ncol = 2, nrow = 2)

filename <-  paste0(output_dir, "\\Abbildung20_allTopics.png")
ggsave(filename, width = 50, height = 30, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#