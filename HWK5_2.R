library(igraph)
g = read.graph(file = "F:/2017 fall third semester/BIA 658/week6/R-code-2017-10-10-1/R-code-2017-10-10/4_lesmiserables/lesmiserables.gml", format = "gml")
g
vcount(g)
ecount(g)
# plot
plot(g)
plot(g, layout=layout.fruchterman.reingold, vertex.size=5, vertex.label=NA)
## "NA" not including the NA
plot(g, layout=layout.circle, vertex.size=5, vertex.label=NA)

# degree and degree distribution
degree(g)
## only get the sequence of the degree

## which.max to get the highest degree
V(g)[which.max(degree(g))]$label
V(g)[degree(g) == max(degree(g))]$label
## from this code, we can get which get the highest degree
V(g)[12]$label

hist(degree(g))
table(degree(g))
## relative propbility of the distribution from the following code
sum(degree.distribution(g))
barplot(degree.distribution(g))





shared_neighbors_number <- function(i){
  a = ends(g, i)[1]
  b = ends(g, i)[2]
  source_neighbors = neighbors(g, a)
  target_neighbors = neighbors(g, b)
  num_overlap_neighbors = length(intersection(source_neighbors, target_neighbors))
  print(num_overlap_neighbors)
}

x <- sapply(E(g),shared_neighbors_number)
x # the number of shared neighbors
y <- E(g)$value # tie strength
y

lm = lm(y~x)
lm

summary(lm)


# scatterplot

#scatter.smooth(x, y, main="number of shared neighbors--tie strength")
plot(x, y, main="number of shared neighbors--tie strength")
abline(lm,lwd=2)



