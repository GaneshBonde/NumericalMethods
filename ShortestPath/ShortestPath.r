library(igraph)
options(repr.plot.width = 12, repr.plot.height =8)

df <- data.frame("n1" = c('A','A','A','A','B','C','F','E','D','A','P','P','B'),
                 "n2" = c('B','C','E','F','C','F','D','D','A','P','Q','R','R'),
                 "weight"= c(2,3,2,5,3,3,1,2,3,2,4,2,2))
df

graph_df <- graph_from_data_frame(df, directed=T)
plot(graph_df,vertex.size=30 , vertex.label.cex=1.5)

graph_df <- set_edge_attr(graph_df,"label", value = E(graph_df)$weight)
plot(graph_df,vertex.size=30 , vertex.label.cex=1.5)

# Compute shortest path using dijkstra algorithm with mode 'out'
distances(graph_df, v = c('A'), to= c('F','D'), mode = c('out'), algorithm = c('dijkstra'))

# Compute shortest path using bellman ford algorithm with mode 'out'
distances(graph_df, v = c('P'), to= c('Q','B'), mode = c('out'), algorithm = c('bellman-ford'))

# Compute shortest path using dijkstra algorithm with mode 'in'
distances(graph_df, v = c('A'), to= c('F','D'), mode = c('in'), algorithm = c('dijkstra'))

# Compute shortest path using bellman ford algorithm with mode 'in'
distances(graph_df, v = c('P'), to= c('Q','B'), mode = c('in'), algorithm = c('bellman-ford'))

# Compute shortest path using dijkstra algorithm with mode 'all'
distances(graph_df, v = c('A'), to= c('F','D'), mode = c('all'), algorithm = c('dijkstra'))

# Compute shortest path using bellman ford algorithm with mode 'all'
distances(graph_df, v = c('P'), to= c('Q','B'), mode = c('all'), algorithm = c('bellman-ford'))

#Compute the shotest path with mode 'out'
path <- shortest_paths(graph_df, from = 'F' , to = c('A'),  mode= c('out'))
path$vpath

#Compute the shotest path with mode 'in'
path <- shortest_paths(graph_df, from = 'F' , to = c('A'),  mode= c('in'))
path$vpath

#Compute the shotest path with mode 'all'
path <- shortest_paths(graph_df, from = 'F' , to = c('A'),  mode= c('all'))
path$vpath

#Compute the shotest path with mode 'out'
path <- shortest_paths(graph_df, from = 'F' , to = c('A', 'E'),  mode= c('out'))
path$vpath


