################################## Abbildung 27 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 27: Vergleich der Topic Gewichte in 
### Goethes Wilhelm Meisters Lehrjahre und Unger/Buchholz' Bekenntnisse einer
### schoenen Seele. Von ihr selbst geschrieben. Zum Vergleich die durch-
### schnittlichen Topic Gewichte der Subsets male/female. Grundlage ist das 
### Topic Model zum Romankorpus Goethezeit.

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)
library(reshape)
library(tibble)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung27"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}

# Pfad zu Topic Modeling Ergebnissen wird als String vordefiniert
corpus <- "Romankorpus_1770-1830"
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

# Einzeltexte, die geplottet werden sollen
selected_rows <- topicswithnames[c("Goethe_Lehrjahre_1795", 
                                   "Unger_Bekenntnisse_1806"),]
data_plot <- as.data.frame(t(selected_rows))
df <- rownames_to_column(data_plot, "Topics")
colnames(df) <- c("Topics", "Goethe, Wilhelm Meisters Lehrjahre (1795)", 
                  "Unger/Buchholz, Bekenntnisse einer schönen Seele (1806)")
df$Topics <- factor(df$Topics, levels=df$Topics)
df$Topics <- as.numeric(gsub("Topic(\\d+)", "\\1", df$Topics))

data <- melt(df, id = "Topics") 
colnames(data) <- c("Topics", "Text", "Anteil")

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
mean_female$gender <- as.factor("Mittelwert der Texte von Autorinnen")
mean_male$gender <- as.factor("Mittelwert der Texte von Autoren")

# Die Subset-Dataframes werden zusammengefügt
data_mean <- rbind(mean_female, mean_male)
data_mean$Topic <- as.numeric(gsub("Topic(\\d+)", "\\1", data_mean$Topic))
data_mean$Type <- 0.7
data_mean$Alpha <- 0.1

df_means <- data.frame(Topics=factor(nrow(data_mean)), 
                       Text=factor(nrow(data_mean)), 
                       Anteil=numeric(nrow(data_mean)), 
                       Type=numeric(nrow(data_mean)),
                       Alpha=numeric(nrow(data_mean)))
df_means$Topics <- data_mean$Topic
df_means$Text <- data_mean$gender
df_means$Anteil <- data_mean$Mean
df_means$Type <- data_mean$Type
df_means$Alpha <- data_mean$Alpha

data$Type <- 0.5
data$Alpha <- 1

df_all <- rbind(df_means, data)

#------------------------------------------------------------------------------#
## Visualisierung (in Farbe) --------------------------------------------------#
#------------------------------------------------------------------------------#

plot <- ggplot(df_all, aes(x = Topics, Anteil)) + 
  geom_line(aes(color = Text, size = Type, alpha = Alpha)) +
  scale_colour_manual(values = c("sienna1", "slateblue", "slateblue4", "sienna4")) +
  theme_minimal() +
  theme(legend.position = c(0.25,0.9),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.y=element_blank()) +
  scale_x_continuous(name="Topics 1-100", 
                     limits=c(0, 100), 
                     breaks=seq(0,100,25),
                     minor_breaks = seq(0, 100, 1)) +
  guides(size = FALSE) +
  guides(alpha = FALSE)


filename <-  paste0(output_dir, "\\Abbildung27.png")
ggsave(filename, width = 30, height = 21, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
## Visualisierung (S/W) -------------------------------------------------------#
#------------------------------------------------------------------------------#

plot <- ggplot(df_all, aes(x = Topics, Anteil, linetype = Text, 
                           size = Type, color = Text)) +
  geom_line() +
  scale_linetype_manual(values=c("solid", "solid", "dotted", "twodash")) +
  scale_color_manual(values= c("gray88", "gray77", "grey22", "gray11")) +
  theme_minimal() +
  theme(legend.position = c(0.25,0.9),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.y=element_blank()) +
  scale_x_continuous(name="Topics 1-100", 
                     limits=c(0, 100), 
                     breaks=seq(0,100,25),
                     minor_breaks = seq(0, 100, 1)) +
  guides(size = FALSE) +
  guides(alpha = FALSE)

filename <-  paste0(output_dir, "\\Abbildung27_SW.png")
ggsave(filename, width = 30, height = 21, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#