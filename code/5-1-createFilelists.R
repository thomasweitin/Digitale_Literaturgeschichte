################################### FILELISTS ##################################
#------------------------------------------------------------------------------#

### Erstellt Filelists fuer alle Korpora. Die Filelists werden fuer die 
### Filelist-Funktion des PoS/NER-Taggers benoetigt.
###
### Ordnerstrukturen:
###
### data
###     |_filelists
###       |_ filelist_A.txt
###       |_ filelist_B.txt
###       |_ ...

#------------------------------------------------------------------------------#
## Durchfuehrung --------------------------------------------------------------#
#------------------------------------------------------------------------------#

path_corpora <- "C:\\Rivalinnen\\corpora"
output_dir <- "C:\\Rivalinnen\\data\\filelists\\"
NER_dir <- "C:\\Rivalinnen\\data\\NER\\"

if(!dir.exists(output_dir)){
    dir.create(output_dir)
}

if(!dir.exists(NER_dir)){
  dir.create(NER_dir)
}

corpora <- list.dirs(path_corpora, recursive = FALSE)

for (i in 1:length(corpora)){
  dir_path <- paste0(corpora[i])
  corpus <- gsub("(.+corpora/)(.+)(_short)*",
                 "\\2", dir_path,
                 perl = TRUE)
  filelist <- list.files(path = dir_path, full.names = TRUE, recursive = FALSE)
  file <- paste0(output_dir, "filelist_", corpus, ".txt")
  write(filelist, file)
}

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#