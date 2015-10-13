
######################################################################################################################################
######################################################################################################################################
### MDLS test -- algorithm for conducting the heuristic search for obtaining the Maximum Defining Label Set (MDLS)
######################################################################################################################################
######################################################################################################################################

#written by Jeremy M. Beaulieu

#This is detailed in the SOM of Sanderson et al. 2011. Note that the user has obtained the distribution of scripts from this paper.

## Example function call:
# mdls.routine(gene.names=gene.names, "camp.subtrees")

## Requires:
# That the 'mdls.pl' perl script is in the working directory. Otherwise, the function will fail.

## Inputs:
# Must provide a vector of gene names that are in the same order as the subtrees in your subtree file:
# gene.names = c("its", "atpbrbcl", "rpl16", "rps16", "trnk", "trnlf", "trnsg", "atpb", "matk", "ndhf", "psba", "rbcl")
# Must provide the file name for the subtrees file:
# subtrees.filename = "camp.subtree"

## Outputs:
# A single binary tree that represents the maximum defining label set across k partitions.
# A list of the taxon names from the binary tree.

library(ape)

mdls.routine <- function(gene.names, subtrees.filename=NULL){
	cat("Initializing...", "\n")
	system("mkdir mdls_files")
	subtree.set <- read.tree(subtrees.filename)
	while(length(gene.names) > 2){
		pairwise.comp <- t(combn(1:length(gene.names), 2))
		for(i in 1:dim(pairwise.comp)[1]){
			set1 <- subtree.set[[pairwise.comp[i,1]]]
			set2 <- subtree.set[[pairwise.comp[i,2]]]
			common.taxa <- set1$tip.label[set1$tip.label %in% set2$tip.label]
			new.set1 <- root(set1, common.taxa[length(common.taxa)], resolve.root=TRUE)
			new.set2 <- root(set2, common.taxa[length(common.taxa)], resolve.root=TRUE)
			write.tree(new.set1, file=paste("mdls_files/", gene.names[pairwise.comp[i,1]], "_", gene.names[pairwise.comp[i,2]], ".tre", sep=""))
			write.tree(new.set2, file=paste("mdls_files/", gene.names[pairwise.comp[i,1]], "_", gene.names[pairwise.comp[i,2]], ".tre", sep=""), append=TRUE)
		}
		
		paired.subsets <- system(paste("ls -1 ", "mdls_files/", "*.tre", sep=""), intern=TRUE)
		for(j in 1:length(paired.subsets)){
			system(paste("./mdls.pl", paired.subsets[j], ">", paste(paired.subsets[j], ".out", sep="")))
		}
		
		retained.taxa.list <- c()
		paired.subsets.out <- system(paste("ls -1 ", "mdls_files/", "*.out", sep=""), intern=TRUE)
		for(k in 1:length(paired.subsets.out)){
			tmp <- readLines(paired.subsets.out[k])
			split.tmp <- unlist(strsplit(tmp[grep("Solution*", tmp)], " "))
			retained.taxa.list <- rbind(retained.taxa.list, as.numeric(split.tmp[4]))
		}
		
		largest.taxon.set <- which.max(retained.taxa.list)
		tmp <- readLines(paired.subsets.out[largest.taxon.set])
		solution.line = grep("Solution*", tmp)
		newtree.line = tmp[solution.line-1]
		new.tree <- read.tree(text = newtree.line)
		genes.combined <- unlist(strsplit(paired.subsets.out[largest.taxon.set], "\\_|\\/|\\."))[3:4]
		
		cat(paste("Genes", genes.combined[1], "and", genes.combined[2], "found to retain the largest taxon set."), "\n")
		
		to.drop.from.set = which(gene.names %in% genes.combined)
		subtree.set = subtree.set[-to.drop.from.set]
		gene.names = gene.names[-to.drop.from.set]
		genes.combined.name = paste(genes.combined[1], genes.combined[2], sep="")
		gene.names = c(gene.names, genes.combined.name)
		subtree.set[[length(subtree.set)+1]] = new.tree
		system("rm mdls_files/*.out")
		system("rm mdls_files/*.tre")
		
		cat("A single binary tree comprised of these two subtrees replaces their individual subtrees in the set.", "\n")
		cat("Performing new heuristic search.", "\n")
	}
	#Now we break out of the while loop and just do the last two and return the final tree file:
	
	pairwise.comp <- t(combn(1:length(gene.names), 2))
	for(i in 1:dim(pairwise.comp)[1]){
		set1 <- subtree.set[[pairwise.comp[i,1]]]
		set2 <- subtree.set[[pairwise.comp[i,2]]]
		common.taxa <- set1$tip.label[set1$tip.label %in% set2$tip.label]
		new.set1 <- root(set1, common.taxa[length(common.taxa)], resolve.root=TRUE)
		new.set2 <- root(set2, common.taxa[length(common.taxa)], resolve.root=TRUE)
		write.tree(new.set1, file=paste("mdls_files/", gene.names[pairwise.comp[i,1]], "_", gene.names[pairwise.comp[i,2]], ".tre", sep=""))
		write.tree(new.set2, file=paste("mdls_files/", gene.names[pairwise.comp[i,1]], "_", gene.names[pairwise.comp[i,2]], ".tre", sep=""), append=TRUE)
	}
	paired.subsets <- system(paste("ls -1 ", "mdls_files/", "*.tre", sep=""), intern=TRUE)
	system(paste("./mdls.pl", paired.subsets[1], ">", paste(paired.subsets[1], ".out", sep="")))
	paired.subsets.out <- system(paste("ls -1 ", "mdls_files/", "*.out", sep=""), intern=TRUE)
	tmp <- readLines(paired.subsets.out)
	solution.line = grep("Solution*", tmp)
	newtree.line = tmp[solution.line-1]
	new.tree <- read.tree(text = newtree.line)
	system("rm mdls_files/*.out")
	system("rm mdls_files/*.tre")
	write.tree(new.tree, file="mdls_files/mdlspruned.tre")
	write.table(new.tree$tip.label, file="mdls_files/mdls.taxonset", quote=FALSE, row.names=FALSE, col.names="Genus_species", sep="\t")
	cat("Done.", "\n")
}

#gene.names = c("its", "atpbrbcl", "rpl16", "rps16", "trnk", "trnlf", "trnsg", "atpb", "matk", "ndhf", "psba", "rbcl")
#mdls.routine(gene.names=gene.names, "camp.subtrees")
