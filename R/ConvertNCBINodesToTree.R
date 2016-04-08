#' From an NCBI taxonomy download, return a tree in igraphy graph format
#' @param taxdir The path to the folder that has the unziped nodes.dmp from NCBI
#' @return An igraph directed graph. It has labels for vertices that match the node ids from NCBI
#' @export
#' @details
#' This is essentially a wrapper for CHNOSZ' functions
ConvertNCBINodesToTree <- function(taxdir) {
	taxon.nodes <- CHNOSZ::getnodes(taxdir)
	tree.nodes <- as.matrix(taxon.nodes[,c(1,2)])
	tree <- igraph::graph_from_edgelist(tree.nodes)
	V(tree)$label <- as.character(V(tree))
	return(tree)
}

#' Create a data.frame with NCBI taxon id, name, and what kind of object this is
#' @param taxdir The path to the folder that has the unziped nodes.dmp from NCBI
#' @param name.type NCBI includes common names, scientific names, etc. Specify NULL if you want to return all names
#' @return A data.frame with taxon ids and names
#' @export
#' @details
#' This is essentially a wrapper for CHNOSZ' functions
GetNCBITaxa <- function(taxdir, name.type="scientific name") {
	local.names <- CHNOSZ::getnames(taxdir)
	if(!is.null(name.type)) {
		local.names <- local.names[which(name.type == local.names$type), ]
	}
	local.names[,2] <- as.character(local.names[,2])
	return(local.names)
}

#' Go from an NCBI id to the ids of its descendants (all the descendants)
#' @param node.id The id for the focal taxon (maybe from GetIDFromName())
#' @param igraph.tree The igraph tree (maybe from ConvertNCBINodesToTree())
#' @return A vector of ids
#' @export
GetDescendantIDs <- function(node.id, igraph.tree) {
	return(ego(igraph.tree, order=1e6, nodes=node.id, mode="in")[[1]])	
}

#' Convert from a taxon name to its NCBI id
#' @param taxon.name A character string with the taxon to match
#' @param ncbi.names The data.frame with names (maybe from GetNCBITaxa)
#' @return The id for the focal taxon
#' @export
GetIDFromName <- function(taxon.name, ncbi.names) {
	matching.id <- ncbi.names$id[which(taxon.name == ncbi.names$name)]
	return(matching.id)
}

#' Convert from an NCBI id to a taxon name
#' @param taxon.id An NCBI taxon id
#' @param ncbi.names The data.frame with names (maybe from GetNCBITaxa)
#' @return The name for the focal taxon
#' @export
GetNameFromID <- function(taxon.id, ncbi.names) {
	matching.name <- ncbi.names$name[which(taxon.id == ncbi.names$id)]
	if(length(matching.name) > 1) {
		warning(paste("Found more than one name for", taxon.id, ": ", paste(matching.name, collapse=" and ")))
	}
	return(matching.name[1])
}


#' Go from a taxon name to the names of all its descendants
#' @param taxon.name A character string with the taxon to match
#' @param igraph.tree The igraph tree (maybe from ConvertNCBINodesToTree())
#' @param ncbi.names The data.frame with names (maybe from GetNCBITaxa)
#' @return A vector of taxon names
#' @export
#' @details
#' Note this currently gives a list of ALL named descendant taxa. So if you have a family
#' it'll give the genera as well as the species
GetDescendantNamesFromName <- function(taxon.name, igraph.tree, ncbi.names) {
	descendant.ids <- GetDescendantIDs(GetIDFromName(taxon.name, ncbi.names), igraph.tree)
	return(sapply(descendant.ids, GetNameFromID, ncbi.names=ncbi.names))
}
