install.packages("igraph")

library(igraph)
g = read.graph(file = "F:/2017 fall third semester/BIA 658/week6/polbooks/polbooks.gml", format = "gml")
plot(g)
plot(g, layout=layout.fruchterman.reingold, vertex.size=5,vertex.label=NA)
plot(g, layout=layout.circle, vertex.size=5,vertex.label=NA)
vcount(g)
g

assortativity_degree(graph = g, directed = TRUE)
assortativity_nominal(graph = g, types = as.factor(V(g)$value), directed = F)



