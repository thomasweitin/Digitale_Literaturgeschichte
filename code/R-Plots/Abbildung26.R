################################## Abbildung 26 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 26: Topic N. 26 aus dem Topic Model zum
### Romankorpus 18. Jahrhundert und Topic N. 21 zum Romankorpus Goethezeit
### (siehe Abbildung 15), gefolgt von den dazugehörigen Topics aus dem 
### Kontrollmodellen.

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
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung26"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}
# Pfad zu Topic Modeling Ergebnissen wird als String vordefiniert
corpus_18Jhd <- "Romankorpus_18_Jahrhundert"
tm_results_18Jhd_M1 <- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                              corpus_18Jhd, "\\100Topics\\M1")
tm_results_18Jhd_M2 <- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                              corpus_18Jhd, "\\100Topics\\M2")

corpus_1770_1830 <- "Romankorpus_1770-1830"
tm_results_1770_1830_M1 <- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                              corpus_1770_1830, "\\100Topics\\M1")
tm_results_1770_1830_M2 <- paste0("C:\\Rivalinnen\\results\\TopicModeling\\", 
                              corpus_1770_1830, "\\100Topics\\M2")


# Die Topics-in-docs Dateien werden eingelesen
file_18Jhd_M1 <- paste0(tm_results_18Jhd_M1, 
                        "\\TM_500chunksize_100numtopics_10000iteration_100twords_topics-in-docs.csv")
file_18Jhd_M2 <- paste0(tm_results_18Jhd_M2, 
                        "\\TM_500chunksize_100numtopics_10000iteration_100twords_topics-in-docs.csv")
file_1770_1830_M1 <- paste0(tm_results_1770_1830_M1, 
                        "\\TM_500chunksize_100numtopics_10000iteration_100twords_topics-in-docs.csv")
file_1770_1830_M2 <- paste0(tm_results_1770_1830_M2, 
                        "\\TM_500chunksize_100numtopics_10000iteration_100twords_topics-in-docs.csv")

# Wir haben in topicModeling.R den Seperator als "X" definiert, weshalb die 
# Datei jetzt auch wieder mit demselben Seperator eingelesen wird.
topicswithnames_18Jhd_M1 <- read.table(file_18Jhd_M1, header = TRUE, 
                                       row.names = 1, sep = " ")
text_titles_18Jhd_M1 <- rownames(topicswithnames_18Jhd_M1)

topicswithnames_18Jhd_M2 <- read.table(file_18Jhd_M2, header = TRUE, 
                                       row.names = 1, sep = " ")
text_titles_18Jhd_M2 <- rownames(topicswithnames_18Jhd_M2)

topicswithnames_1770_1830_M1 <- read.table(file_1770_1830_M1, header = TRUE, 
                                       row.names = 1, sep = " ")
text_titles_1770_1830_M1 <- rownames(topicswithnames_1770_1830_M1)

topicswithnames_1770_1830_M2 <- read.table(file_1770_1830_M2, header = TRUE, 
                                       row.names = 1, sep = " ")
text_titles_1770_1830_M2 <- rownames(topicswithnames_1770_1830_M2)

#------------------------------------------------------------------------------#
## Profile --------------------------------------------------------------------#
#------------------------------------------------------------------------------#

topic26 <- topicswithnames_18Jhd_M1$Topic26
df_topic26 <- data.frame(as.factor(text_titles_18Jhd_M1), topic26)
colnames(df_topic26) <- c("Texte", "Topic")

topic19 <- topicswithnames_18Jhd_M2$Topic19
df_topic19 <- data.frame(as.factor(text_titles_18Jhd_M2), topic19)
colnames(df_topic19) <- c("Texte", "Topic")

topic21 <- topicswithnames_1770_1830_M1$Topic21
df_topic21 <- data.frame(as.factor(text_titles_1770_1830_M1), topic21)
colnames(df_topic21) <- c("Texte", "Topic")

topic96 <- topicswithnames_1770_1830_M2$Topic96
df_topic96 <- data.frame(as.factor(text_titles_1770_1830_M2), topic96)
colnames(df_topic96) <- c("Texte", "Topic")


all_dfs <- list(df_topic26, df_topic21, df_topic19, df_topic96)
names(all_dfs) <- c("M1_Topic26", "M1_Topic21", "M2_Topic19", "M2_Topic96")

labelled <- c("Unger_Bekenntnisse_1806", "Goethe_Lehrjahre_1795", 
              "Fischer_Honigmonathe_1802", "Ehrmann_Amalie_1787",
              "Klinger_Faust_1791")

plot_titles <- c("Modell 18. Jahrhundert - Topic 26", 
                 "Modell Goethezeit - Topic 21",
                 "Kontrollmodell 18. Jahrhundert",
                 "Kontrollmodell Goethezeit")

all_plots <- list()

for (i in 1:length(all_dfs)) {
  df <- all_dfs[[i]]
  title <- names(all_dfs[i])
  new_title <- gsub("M(\\d)_Topic(\\d+)", "Modell \\1 - Topic \\2", title)
  plot_title <- plot_titles[i]
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
    ggtitle(plot_title) +
    gghighlight(Texte %in% labelled) +
    geom_text_repel(data=df_labelled, 
                    aes(label=Texte),
                    size=6)

  all_plots[[i]] <- plot
  filename <- gsub("Modell (\\d) - Topic (\\d+)", 
                   "\\Abbildung26_Modell\\1_Topic\\2.png", new_title)
  filename_complete <- paste0(output_dir, "\\", filename)
  ggsave(filename_complete, width = 50, height = 30, dpi = 320,
         bg = "transparent", units = "cm", limitsize = FALSE)
}


#------------------------------------------------------------------------------#

multiplot <- ggarrange(all_plots[[1]], all_plots[[2]], 
                       all_plots[[3]], all_plots[[4]],
                       ncol = 2, nrow = 2)

filename <-  paste0(output_dir, "\\Abbildung26_allTopics.png")
ggsave(filename, width = 50, height = 30, dpi = 320,
       bg = "transparent", units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#