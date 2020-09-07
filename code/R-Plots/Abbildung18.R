################################## Abbildung 18 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 18: Topic-Gewichte nach Subsets, 
### male/female für das Romankorpus 18. Jahrhundert

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(reshape)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung18"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}

# Pfad zu Topic Modeling Ergebnissen wird als String vordefiniert
corpus <- "Romankorpus_18_Jahrhundert"
tm_results <- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", corpus, "\\100Topics\\M1")

# Die Topics-in-docs Datei wird eingelesen
file <- paste0(tm_results, "\\TM_500chunksize_100numtopics_10000iteration_100twords_topics-in-docs.csv")

# Wir haben in topicModeling.R den Seperator als "X" definiert, weshalb die 
# Datei jetzt auch wieder mit demselben Seperator eingelesen wird.
topicswithnames <- read.table(file, header = TRUE, row.names = 1, sep = " ")

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

# Die durchschnittlichen Topic-Gewichte für die Subsets werden in zwei 
# Dateframe geschrieben
mean_female <- as.data.frame(colMeans(topicswithnames_female))
mean_female$Topic <- rownames(mean_female)
rownames(mean_female) <- NULL
colnames(mean_female) <- c("Mean", "Topic")

mean_male <- as.data.frame(colMeans(topicswithnames_male))
mean_male$Topic <- rownames(mean_male)
rownames(mean_male) <- NULL
colnames(mean_male) <- c("Mean", "Topic")

#------------------------------------------------------------------------------#
## Vorbereitung fuer Visualisierung -------------------------------------------#
#------------------------------------------------------------------------------#

# Strings werden in Faktoren umgewandelt
mean_female$gender <- as.factor("weiblich")
mean_male$gender <- as.factor("männlich")

# Die Subset-Dataframes werden zusammengefügt
data_mean <- rbind(mean_female, mean_male)
data_mean$Topic <- as.numeric(gsub("Topic(\\d+)", "\\1", data_mean$Topic))

#------------------------------------------------------------------------------#
## Visualisierung (in Farbe) --------------------------------------------------#
#------------------------------------------------------------------------------#

plot <- ggplot(data_mean, aes(x = Topic, Mean, group = gender)) + 
  geom_line(aes(color = gender)) +
  scale_colour_manual(values = c("sienna4", "slateblue4")) +
  theme_minimal() +
  theme(legend.position = c(0.9,0.9),
        legend.title=element_blank(),
        axis.title.y=element_blank()) +
  scale_x_continuous(name="Topics 1-100", 
                     limits=c(0, 100), 
                     breaks=seq(0,100,25),
                     minor_breaks = seq(0, 100, 1)) 

filename <-  paste0(output_dir, "\\Abbildung18.png")
ggsave(filename, width = 30, height = 21, dpi = 320, 
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
## Visualisierung (S/W) -------------------------------------------------------#
#------------------------------------------------------------------------------#

plot <- ggplot(data_mean, aes(x = Topic, Mean, group = gender)) + 
  geom_line(aes(linetype = gender)) +
  theme_minimal() +
  theme(legend.position = c(0.9,0.9),
        legend.title=element_blank(),
        axis.title.y=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  scale_x_continuous(name="Topics 1-100", 
                     limits=c(0, 100), 
                     breaks=seq(0,100,25),
                     minor_breaks = seq(0, 100, 1)) 

filename <-  paste0(output_dir, "\\Abbildung18_SW.png")
ggsave(filename, width = 30, height = 21, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#