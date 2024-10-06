library(igraph)
library(stats)
gdata <- read.csv("asoiaf-all-edges.csv")

g <- graph_from_data_frame(d = gdata, directed = FALSE)
E(g)$weight <- gdata$weight
print(g)
is_directed(g)
save(g, file = "got_graph.csv")

vcount(g) ## 796 vertices


ecount(g) ## 2823 edges 

vertices <- V(g)
print(vertices)

vertex_names <- V(g)$name
print(vertex_names)
vertice_list <- as_data_frame(g, what = "vertices")
print(vertice_list)

edges <- E(g)

print(edges)

edge_list <- as_data_frame(g, what = "edges")
print(edge_list)

graph_diameter <- diameter(g, directed = FALSE, weights = E(g)$weight)
graph_diameter
diameter_path <- get_diameter(g, directed = FALSE, weights = E(g)$weight)

vertex_color <- rep(rgb(0, 1, 0, alpha=0.35), vcount(g))
edge_color <- rep("darkgray", ecount(g))
edge_width <- rep(1, ecount(g))
vertex_size <- 3

vertex_color[diameter_path] <- "red"
diameter_edges <- E(g, path=diameter_path)
edge_color[diameter_edges] <- "red"
edge_width[diameter_edges] <- 4.5

set.seed(12345)  
layout <- layout_with_fr(g)  


# Plot the network with highlighted diameter path
plot(g, layout = layout, vertex.size = vertex_size, edge.width = edge_width, vertex.label=NA,
     vertex.color = vertex_color, edge.color = edge_color, main="Game of Thrones Characters (full) Graph")

### Finding the number of triangles present in the graph ###
vertex_triangles <- count_triangles(g)
total_triangles <- sum(vertex_triangles) / 3
print(paste("Total number of triangles in the graph:", total_triangles))

#### Finding the vertices with the biggest degree ####
edges_degree_greater12 <- E(g)[weight > 12]
edges_degree_greater12_num <- length(edges_degree_greater12)
print(paste("Number of edges with weight more than 12:", edges_degree_greater12_num))


degrees <- degree(g, mode = "all", loops = FALSE)
degrees_norm <- degree(g, mode = "all", normalized = TRUE)

# Sort the vertices by their degree in descending order
sorted_degrees <- sort(degrees, decreasing = TRUE)
top_10_characters <- names(head(sorted_degrees, 10))

print("Top 10 characters by degree:")
for (i in 1:10) {
  cat(top_10_characters[i], ": ", sorted_degrees[i], "\n")
}

### showing normalized degree ###
sorted_degrees2 <- sort(degrees_norm, decreasing = TRUE)
top_10_characters2 <- names(head(sorted_degrees2, 10))

print("Top 10 characters by normalized degree:")
for (i in 1:10) {
  cat(top_10_characters2[i], ": ", sorted_degrees2[i], "\n")
}


### Finding the 10 vertices with the biggest weighted degree ###
weighted_degrees<- strength(g, mode = "all", loops = FALSE, weights=E(g)$weight)
sorted_degrees3 <- sort(weighted_degrees, decreasing = TRUE)

# Get the names of the top 10 characters
top_10_characters3 <- names(head(sorted_degrees3, 10))

# Print the top 10 characters and their weighted degrees
print("Top 10 characters by weighted degree:")
for (i in 1:10) {
  cat(top_10_characters3[i], ": ", sorted_degrees3[i], "\n")
}


#### Finding the 10 vertices with the biggest local clustering coefficient ####
local_clustering <- transitivity(g, type = "local")

sorted_local_clust <- sort(local_clustering, decreasing = TRUE, index.return = TRUE)
top_10_characters4 <- names(sorted_local_clust$x[1:10])

print("Top 10 characters by local clustering coefficient:")
for (i in 1:10) {
  cat(top_10_characters4[i], ": ", sorted_local_clust$x[i], "\n")
}

#### Calculating the global clustering coefficient ####
global_clustering <- transitivity(g, type = "global")
print(paste("Global clustering coefficient of the graph:", global_clustering))


####### Comparing global clustering coefficient with the corresponding measure for random networks #######
num_random_networks <- 10000
num_nodes <- vcount(g)
num_edges <- ecount(g)
random_clustering_coeffs <- numeric(num_random_networks)

for (i in 1:num_random_networks) {
  random_graph <- sample_gnm(num_nodes, num_edges, directed = FALSE, loops = FALSE)
  random_clustering_coeffs[i] <- transitivity(random_graph, type = "global")
}
mean_random_clustering <- mean(random_clustering_coeffs)
sd_random_clustering <- sd(random_clustering_coeffs)

cat("Mean clustering coefficient for random networks:", mean_random_clustering, "\n")
cat("Standard deviation of clustering coefficients for random networks:", sd_random_clustering, "\n")

global_clustering
z_score <- (global_clustering - mean_random_clustering) / sd_random_clustering
cat("Z-score of your network's clustering coefficient:", z_score, "\n")

###### Plot the Graph #####
layout <- layout_with_fr(g) 
set.seed(12345)
vertex_color <- rgb(0, 1, 0, alpha=0.45)
par(mar=c(3.1,1.8,2.1,1.1))
plot(g, layout = layout, vertex.size = 3, edge.width = 1, vertex.label=NA,
     vertex.color = vertex_color, edge.color = "darkgray", main="Game of Thrones Characters (full) Graph")
par(c("mar", "mai"))

#### Creating the subgraph ####
vertex_degree <- degree(g)
subgraph_vertices <- V(g)[vertex_degree >= 9]
subgraph <- induced_subgraph(g, subgraph_vertices)

#### Plotting the subgraph ####
set.seed(12345) 
layout_subgraph <- layout_with_fr(subgraph)
plot(subgraph, layout = layout_subgraph, vertex.size = 3, edge.width = 1, vertex.label=NA,
     vertex.color = "green", edge.color = "darkgray", main="Game of Thrones Characters with high degree Subgraph")


#### Calculating the density of the full graph and the subgraph ####
density_full_graph <- edge_density(g)
density_subgraph <- edge_density(subgraph)

cat("Edge density of the entire graph:", density_full_graph, "\n")
cat("Edge density of the subgraph:", density_subgraph, "\n")

#### Finding the top 15 characters based on closeness centrality ####
closeness_centrality <- closeness(g, normalized = TRUE)
sorted_closeness <- sort(closeness_centrality, decreasing = TRUE)
top_15_closeness <- names(head(sorted_closeness, 15))
cat("Top 15 characters by closeness centrality:\n")
for (i in 1:15) {
  cat(top_15_closeness[i], " ", sorted_closeness[top_15_closeness[i]], "\n", sep="")
}


#### Finding the top 15 characters based on betweenness centrality ####
betweenness_centrality <- betweenness(g,normalized = TRUE)
sorted_betweenness <- sort(betweenness_centrality, decreasing = TRUE)
top_15_betweenness <- names(head(sorted_betweenness, 15))
cat("Top 15 characters by betweenness centrality:\n")
for (i in 1:15) {
  cat(top_15_betweenness[i], " ", sorted_betweenness[top_15_betweenness[i]], "\n", sep="")
}

vertex_color <- ifelse(betweenness_centrality >= 0.05, "red", rgb(0, 1, 0, alpha=0.47))

set.seed(12345)  
layout <- layout_with_fr(g)  

par(mar=c(3.1,1.8,2.1,1.1))
a <- ifelse(V(g)$name == "Jon-Snow", "Jon Snow", "")


# Plot the network with vertex colors based on betweenness centrality
plot.igraph(g, layout = layout, vertex.size = ifelse(V(g)$name == "Jon-Snow", 6, 3), edge.width = 1, vertex.label=a, label.font=2, vertex.label.font=2 ,
     vertex.color = vertex_color, edge.color = "darkgray", vertex.label.degree = ifelse(V(g)$name == "Jon-Snow", (4.65*(pi/4)), 0),vertex.label.dist = ifelse(V(g)$name == "Jon-Snow", 1.5, 0),vertex.label.color = "firebrick4",vertex.label.cex=1.35,main="Game of Thrones Characters (full) Graph")
legend("topleft", legend = c("Betweenness > 0.05", "Betweenness =< 0.05"),
       col = c("red", rgb(0, 1, 0)), pch = 19, pt.cex = 2,
       bty = "n", text.col = "black", cex = 1.2)






#### Ranking the characters using pagerank ####
page_rank_values <- page_rank(g, directed = FALSE)$vector
sorted_pagerank <- sort(page_rank_values, decreasing = TRUE)
sorted_pagerank

top_15_pagerank <- names(head(sorted_pagerank, 15))

cat("Top 15 characters by PageRank value:\n")
for (i in 1:15) {
  cat(top_15_pagerank[i], sorted_pagerank[top_15_pagerank[i]], "\n", sep=" ")
}

min_size <- 3
max_size <- 9
vertex_sizes <- (page_rank_values - min(page_rank_values)) / (max(page_rank_values) - min(page_rank_values)) * (max_size - min_size) + min_size

vertex_color <- rgb(0, 1, 0, alpha=0.6)
set.seed(12345)  
layout <- layout_with_fr(g)  


# Plot the network with vertex sizes based on PageRank values
plot(g, layout = layout, vertex.size = vertex_sizes, edge.width = 1, vertex.label=NA,
     vertex.color = vertex_color, edge.color = "darkgrey", main="Game of Thrones Characters Graph\n(vertex size based on Pagerank value)")

