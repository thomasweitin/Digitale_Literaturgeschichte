################################## Abbildung 15 ################################
#------------------------------------------------------------------------------#

### Skript zur Erstellung von Abbildung 15: Clustering mit dem 
### Autorschaftstestkorpus für die Bekenntnisse einer schönen Seele. Von ihr 
### selbst geschrieben (1806). 100-3000 MFW, Cosine Delta, 20 % Culling, 
### Consensus Strength 0,5. 

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#

library(ape)
library(ggtree)
library(treeio)
library(tibble)

#------------------------------------------------------------------------------#
## Einlesen der Daten ---------------------------------------------------------#
#------------------------------------------------------------------------------#

# Output-Ordner festlegen
output_dir <- "C:\\Rivalinnen\\plots\\Abbildung15"

if(!dir.exists(paste0(output_dir))){
  dir.create(paste0(output_dir), recursive = TRUE)
}

# Input-Ordner festlegen
results_dir <- "C:\\Rivalinnen\\results\\Stilometrie\\Buchholz"

# Die iterativ erstellten Distanztabellen werden eingelesen 
files <- list.files(results_dir, pattern = "distance_table_", 
                    recursive = FALSE, full.names = TRUE)

# Fuer jede Distanztabelle wird ein Dendrogramm erstellt
dendrograms <- list()
for (i in 1:length(files)) {
  dist <- read.csv(files[i], sep = " ")
  dendrogram <- as.phylo(hclust(as.dist(dist), method = "ward.D"))
  dendrograms[[i]] <- dendrogram
}

# Damit Texte nach Autorschaft eingefaerbt werden koennen, wird eine 
# Gruppierung erstellt
names <- gsub("_.*","", names(dist))
unique_labels <- unique(as.data.frame(names))
labels_cleaned <- merge(as.data.frame(names), unique_labels, by = 'names')

col <- c("sienna4", "slateblue4", "seagreen4", "deepskyblue1", "dimgrey")

nas <- rep(NA, 88)
names <- c(names, nas)

consensus <- consensus(dendrograms, p = 0.5)
n <- Nnode2(consensus)
data <- tibble(node = 1:n, authors = names)

#------------------------------------------------------------------------------#
## Plotting -------------------------------------------------------------------#
#------------------------------------------------------------------------------#

tree <- treedata(phylo = consensus, data = data)

# Anpassung der Visualisierung
t <- ggtree(tree, layout = "circular", size = 0.1)
t <- open_tree(t, angle = 180)
t + geom_tiplab(size = 0.7, aes(color = data$authors)) +
  scale_color_manual(values = col) +
  # Aeste, die von Interesse sind, werden farblich unterlegt
  geom_hilight(c(210), fill = "lightgrey") + 
  geom_hilight(c(169), fill = "lightgrey") + 
  theme(legend.position = "none")

filename <-  paste0(output_dir, "\\Consensus_circular-layout.png")
ggsave(filename, width = 15, height = 15, dpi = 960, bg = "transparent",
       units = "cm", limitsize = FALSE)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#