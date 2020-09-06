################################# NER-STOPLIST #################################
#------------------------------------------------------------------------------#

### Erstellt eine erweiterte Stopword-Liste, in der Stopwoerter mit Named 
### Entities kombiniert werden.
###
### Ordnerstrukturen:
###
### data
###     |_NER
###         |_Romankorpus_18_Jahrhundert_NER
###         |_Romankorpus_1770-1830_NER
###     |_NER_stoplists [wird automatisch erstellt]                              
###         |_Romankorpus_1770-1830_stoplist.txt
###         |_Romankorpus_1770-1830_stoplist_gesamt.txt
###         |_Romankorpus_18_Jahrhundert_stoplist.txt
###         |_Romankorpus_18_Jahrhundert_stoplist_gesamt.txt
###     |_wordlists
###         |_180315_stoppwords.txt
###         |_leipzig10000.txt

#------------------------------------------------------------------------------#
## Benoetigte Packages ---------------------------------------------------------#
#------------------------------------------------------------------------------#

library(xml2)
library(XML)
library(plyr)

#------------------------------------------------------------------------------#
## Voreinstellungen -----------------------------------------------------------#
#------------------------------------------------------------------------------#

# Ordnerstruktur: Alle XML-Dateien befinden sich in einem Ordner. 
# Die Dateinamen sollten kein Sonderzeichen und Umlaute beinhalten
path_data <- "C:\\Rivalinnen\\data"
path <- "C:\\Rivalinnen\\data\\NER"

#corpus <- "Romankorpus_18_Jahrhundert"
corpus <- "Romankorpus_1770-1830"

setwd(paste0(path, "\\", corpus, "_NER"))
files <- dir(full.names = TRUE)

#------------------------------------------------------------------------------#
## Einlesen und Filtern der XML-Dateien ---------------------------------------#
#------------------------------------------------------------------------------#

# Einlesen der Dateien und Umwandlung in ein Dataframe
tables <- list()

for(i in 1:length(files)){
  xmldata <- xmlParse(files[i])
  tables[[i]] <- xmlToDataFrame(xmldata, nodes = getNodeSet(xmldata, "//token")) 
  print(i) # um zu sehen, wie weit der Loop ist, dauert lange
}

# Im NER-Output interessieren uns die Kategorien "Person" und "Location". 
# Diese nehmen wir aus den Dataframes raus:  
subset_person <- list()
subset_loc <- list()

for(i in 1:length(files)){
  subset_person[[i]] <- subset(as.data.frame(tables[[i]]), NER == "PERSON" )
  subset_loc[[i]] <- subset(as.data.frame(tables[[i]]), NER == "LOCATION" )
}

# Wir verwenden am Ende eine Stopliste fuer alle Texte, daher erstellen wir aus 
# der Dataframeliste ein einziges Dataframe: 
persons <- ldply(subset_person, data.frame) 
# siehe: https://stackoverflow.com/questions/2851327/convert-a-list-of-data-frames-into-one-data-frame/2851434#2851434
locations <- ldply(subset_loc, data.frame)
pers_locs <- rbind(persons, locations)

# Dem Dataframe wird eine Spalte "Leipzig" hinzugefuegt. Diese braucht man, 
# da wir spaeter die Namen mit der Leipziger Liste der 1,000 haeufigsten 
# Woerter vergleichen werden. Der Wert der Spalte ist zunaechst "OO":
pers_locs$leipzig <- "OO"

#------------------------------------------------------------------------------#
## Abgleich mit der Leipziger Liste -------------------------------------------#
#------------------------------------------------------------------------------#

# Filtern und exportieren: Ordner festlegen
stoplists_path <- paste0(path_data, "\\NER_stoplist")

if(!dir.exists(stoplists_path)){
  dir.create(stoplists_path)
}

# Uebergeordneter Outputordner
setwd(stoplists_path) 

# Wir nehmen nur die Woerter, die sowohl in der Spalte "POS" die Bezeichnung 
# Named Entity haben UND nicht in der Liste der haeufigsten Woerter der 
# Deutschen Sprache vorkommen: 
words_leipzig <- scan("C:\\Rivalinnen\\data\\wordlists\\leipzig10000.txt", 
                      what= "character")
# sehr wichtig hier: Lemmas, nicht words
pers_locs$leipzig[which(pers_locs$lemma %in% words_leipzig)] <- "LL"   
pers_locs_ne_all <- subset(subset(pers_locs, POS == "NE"), leipzig =="OO")
write(sort(tolower(unique(pers_locs_ne_all$word))), 
      paste0(corpus, "_stoplist.txt"))

#------------------------------------------------------------------------------#
## Kombination mit anderen Stopword-Listen ------------------------------------#
#------------------------------------------------------------------------------#

# Stopword-Liste von Katharina Herget integrieren: 
stoplist_kh <- scan(
  "C:\\Rivalinnen\\data\\wordlists\\180315_stoppwords.txt", 
  what="character", encoding = "unknown")
stoplist_ner <- scan(paste0(corpus, "_stoplist.txt"), what="character", 
                     encoding = "unknown")

stoplist_complete <- append(stoplist_kh, stoplist_ner)
stoplist_complete <- unique(stoplist_complete)

write(stoplist_complete, paste0(corpus, "_stoplist_gesamt.txt"))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
