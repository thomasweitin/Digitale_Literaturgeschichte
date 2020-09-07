################################## Abbildung 16 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 16: Top 70 Zeta-Scores der Kontrast-
### analyse mit Texten aus den Korpora 18. Jahrhundert und Goethezeit.
###
### Ordnerstruktur:
###
### path
###   |_ primary_set (Texte von Frauen)
###   |_ secondary_set (Texte von Maennern)
###   |_ test_set (Texte von Frauen & Maennern)   

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(ggplot2)

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung16"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}

# Ordner, in dem primary_set, secondary_set & test_set liegen
path <- "C:\\Rivalinnen\\corpora\\Romankorpus_Oppose\\oppose_abb16"
setwd(path)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

oppose_avoided <- read.csv("scores_avoided.csv")

oppose_preferred <- read.csv("scores_preferred.csv")

#------------------------------------------------------------------------------#
## Datenaufbereitung ----------------------------------------------------------#
#------------------------------------------------------------------------------#

# Die von Autorinnen vermiedenen Woerter werden in ein Dataframe gespeichert, 
# das dann auf die Top 70 Woerter begrenzt wird.
# Umwandlung in Faktoren als Vorbereitung für das Plotting.
avoided <- as.data.frame(oppose_avoided)
names(avoided) <- c("Wörter", "Zeta")
avoided <- avoided[1:70,]
avoided$Rang <- as.factor(seq(1,70,1))

# Die von Autorinnen bevorzugten Woerter werden in ein Dataframe gespeichert, 
# das dann auf die Top 70 Woerter begrenzt wird.
# Umwandlung in Faktoren als Vorbereitung für das Plotting.
preferred <- as.data.frame(oppose_preferred)
names(preferred) <- c("Wörter", "Zeta")
preferred <- preferred[1:70,]
preferred$Rang <- as.factor(seq(1,70,1))

# Kombination beider Dataframes
all_values <- rbind(avoided, preferred)

#------------------------------------------------------------------------------#
## Plot -----------------------------------------------------------------------#
#------------------------------------------------------------------------------#

plot <- ggplot(all_values, aes(y=Rang, x=Zeta, label=Wörter)) +
  geom_text(hjust = "outward") +
  geom_vline(xintercept = 0, linetype="dashed") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.text.y = element_text(angle = 270),
        axis.title.y = element_text(angle = 270),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_y_discrete(limits = rev(unique(all_values$Rang)),
                   breaks = seq(0,70,10),
                   name = "Rang",
                   expand = c(0.05,0)) +
  scale_x_continuous(limits = c(-1, 1),
                     breaks=seq(-1,1,0.5),
                     position = "top",
                     name = NULL,
                     sec.axis = sec_axis(~.,
                                         breaks = c(-0.5,0.5),
                                         labels = c("Avoided", "Preferred"))) 
         
filename <-  paste0(output_dir, "\\Abbildung16.png")
ggsave(filename, width = 15, height = 25, dpi = 320, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#