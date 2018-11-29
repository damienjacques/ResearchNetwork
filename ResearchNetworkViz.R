################################################
# Interactive network viz from bibtex file
# Author: Damien Jacques
# Last update: November 29, 2018
################################################

library(bib2df)
library(igraph)
library(networkD3)

# Load bibliography
bib <- bib2df("/home/ubuntu/Dropbox/Research/Bibliography/Biblio")

# Correct author list
unique.author <- unique(sub("^(\\S*\\s+\\S+).*", "\\1",   unlist(bib$AUTHOR)))
unique.author <- unique.author[-which(is.na(unique.author))]
unique.author <- unique.author[-which(unique.author == "others")] 

# Build adjacency matrix
all.pairs <- matrix(ncol = length(unique.author),
                    nrow = length(unique.author),
                    dimnames = list(unique.author, unique.author), 0)

for (i in 1:nrow(bib)) {
  print(i)
  authors <- sub("^(\\S*\\s+\\S+).*", "\\1",   bib$AUTHOR[[i]])
  for (k in 1:length(authors)) {
    all.pairs[which(row.names(all.pairs) == authors[k]), which(colnames(all.pairs) %in% authors[-k])] <-  all.pairs[which(row.names(all.pairs) == authors[k]), which(colnames(all.pairs) %in% authors[-k])]  + 1
  }
}

# Make graph
author.adj <- graph.adjacency(all.pairs, mode = 'undirected', weighted = TRUE)

# Use igraph to make the graph and find membership
wc <- cluster_walktrap(author.adj)
members <- membership(wc)

# Convert to object suitable for networkD3
author.adj_d3 <- igraph_to_networkD3(author.adj, group = members)

# Create force directed network plot
forceNetwork(Links = author.adj_d3$links, Nodes = author.adj_d3$nodes,
             Source = 'source', Target = 'target', NodeID = 'name',
             Group = 'group', fontSize = 24,
             zoom = T, Value = "value", radiusCalculation = "d.nodesize", opacity = 1)
