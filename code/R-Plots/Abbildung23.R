################################## Abbildung 23 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 23: Topic N. 59 mit der Verteilung der 
### Topic-Gewichte aus dem Topic Model zum Romankorpus Goethezeit (1770-1830)

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(wordcloud)
library(reshape)
library(RColorBrewer)
library(ggplot2)

#------------------------------------------------------------------------------#
## WORDCLOUD ------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung23"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}

corpus <- "Romankorpus_1770-1830"
tm_results<- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                    corpus, "\\100Topics\\M1")

# Der Pfad zur Wordweight Datei wird als String definiert
file <- paste0(tm_results,
               "\\TM_500chunksize_100numtopics_10000iteration_100twords__wordweight.csv")

# Einlesen der Wordweight Datei
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

# Nummer des Topics, das abgebildet werden soll
topic_no <- 59
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
          scale = c(5.7,.01),
          min.freq = 1, max.words = 100, rot.per=0,
          random.order=FALSE)


graphics.off()

#------------------------------------------------------------------------------#
## BOXPLOT --------------------------------------------------------------------#
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

#------------------------------------------------------------------------------#
## Aufteilung der Daten in Gender-Subsets -------------------------------------#
#------------------------------------------------------------------------------#

# Extrahieren der Dateinamen von Texten, die von Frauen verfasst wurden
metadata_female <- subset(metadata, gender == "w")
metadata_female_filename <- metadata_female$file_name

# Extrahieren der Dateinamen von Texten, die von Männern verfasst wurden
metadata_male <- subset(metadata, gender == "m")
metadata_male_filename <- metadata_male$file_name

# Die topics-in-docs Datei wird anhand der Gender-Subsets aufgeteilt
topicswithnames_female <- topicswithnames[rownames(topicswithnames) %in% 
                                            metadata_female_filename,]
topicswithnames_male <- topicswithnames[rownames(topicswithnames) %in% 
                                          metadata_male_filename,]

mdata_female <- melt(as.matrix(topicswithnames_female))
mdata_female$gender <- as.factor("weiblich")
mdata_male <- melt(as.matrix(topicswithnames_male))
mdata_male$gender <- as.factor("männlich")
data <- rbind(mdata_female, mdata_male)

topic <- subset(data, X2 == paste0("Topic", as.character(topic_no)))

#------------------------------------------------------------------------------#
## Boxplot in Farbe -----------------------------------------------------------#
#------------------------------------------------------------------------------#

plot <- ggplot(topic, aes(x = gender, y = value, group = gender)) + 
  geom_boxplot(aes(fill = gender), outlier.shape=NA, alpha = 8/10) +
  scale_fill_manual(values = c("sienna4", "slateblue4")) +
  geom_jitter(position=position_jitter(0.2)) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))

filename_all <-  paste0(output_dir, "\\", "boxplot_vis_", topic_no, ".png")
ggsave(filename_all, width = 20, height = 15, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
## Boxplot in S/W -------------------------------------------------------------#
#------------------------------------------------------------------------------#

plot <- ggplot(topic, aes(x = gender, y = value, fill = gender)) + 
  geom_boxplot(outlier.shape=NA, alpha = 8/10) +
  scale_fill_grey() +
  geom_jitter(position=position_jitter(0.2)) +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))

filename_all <-  paste0(output_dir, "\\", "boxplot_vis_SW_", topic_no, ".png")
ggsave(filename_all, width = 20, height = 15, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#