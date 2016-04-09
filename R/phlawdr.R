no.files<- system("ls -1 *seq", intern=TRUE)

phlawdr<-function(file.names, gene.table, species.vector){
  all.sequences <- data.table() #SET UP SOMEHOW
  for(file.index in sequence(length(file.names))){
  current.file<-readLines(file.names[file.index])#read in ith file
  accession.line <- grep("ACCESSION", current.file)#identify lines with accession numbers
  definition.start.line <- grep("DEFINITION", current.file)#identify lines with definition start
  definition.stop.line<-accession.line-1#identify lines with definition stop
  species.line <- grep("ORGANISM", current.file)#identify lines with species identification
  seq.start.line <- grep("^ORIGIN      ", current.file) + 1#identify where sequences start
  seq.stop.line <- which(current.file == "//") - 1#identify where sequences stop
  if(length(accession.line) != length(seq.stop.line)) {
    stop("File is not being parsed properly!")
  }
  for (record.index in sequence(length(accession.line))) {
    accession<-gsub("ACCESSION","", current.file[accession.line])#drop ACCESSION from line leaving just Accession #
    species<-gsub(paste("\\s+","ORGANISM","\\s+"),"", current.file[species.line])#drop ORGANISM from line leaving just the name of the species
    species<-gsub(" ", "_", species)#Kill white space between genus_species
    if(species %in% species.vector) {
      sequence <- paste(gsub("[[:digit:] ]", "", current.file[seq.start.line[record.index]:seq.stop.line[record.index]]), collapse="")#Grab the sequence and kill the number of lines
      #gene.name <- MatchGeneName(__DEFINITIONSTRING___, gene.table)#Make this work
      if(!is.na(gene.name)) {
        all.sequences <- data.table.append(species, gene.name, accession.number, sequence) #NOT THE REAL FN NAME. OR, APPEND TO A CSV FILE YOU CREATE WHEN YOU START THIS OVERALL FN
      }
    }
  }  
 }
}