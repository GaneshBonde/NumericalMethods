install.packages('igraph')

library(igraph)
options(repr.plot.width = 12, repr.plot.height = 8)

g1 <- graph(edge = c(1,2, 2,3, 2,4, 3,1, 4,1), n=4 , directed = F)
plot(g1, vertex.label.cex = 1.5 , vertex.size = 40)

g2 <- graph(edge = c(1,2, 2,1, 2,3, 2,4, 3,1, 4,1), n=4 , directed = T)
plot(g2, vertex.label.cex = 1.5 , vertex.size = 50)

g3 <- graph(edge = c(1,2, 2,3, 3,7, 7,5, 5,1, 5,6, 7,4), n=7 , directed = T)
plot(g3, vertex.label.cex = 1.5 , vertex.size = 30)

g4 <- graph(edge = c(1,4, 2,4, 3,4, 7,5, 5,1 ), n=9 , directed = T)
plot(g4, vertex.label.cex = 1.5 , vertex.size = 30)

g5 <- graph( c('IND','USA',  'IND','USA', 'USA','AUS', 
               'AUS','IND',  'IND','CHN', 'CHN','IND',
               'JPN','IND',  'USA','CHN', 'CHN', 'USA') , directed=T )
plot(g5, vertex.label.cex = 1.5 , vertex.size = 50)

g6 <- graph( c('Ford','Suzuki',  'Suzuki','Tata', 'Tata','Nissan', 
               'Nissan','Toyota',  'Nissan','Nissan', 'Toyota','Ford'),
                isolates = c('Honda' , 'Volvo') )
plot(g6,  vertex.size = 50, edge.arrow.size = 1, vertex.color = 'green' , vetex.frame.color = 'red',
     vertex.label.color = 'black' , vertex.label.cex = 1.5 , vertex.label.dist = 4 , edge.curved = .2 )

E(g6)

V(g6)

g6[]

g6[4,]

V(g6)$name

V(g6)$country = c ("USA", "Japan", "India", "Japan", "Japan", "Japan", "Sweden")
vertex_attr(g6)

E(g6)$relationship = c("Research", "Alliance", "Patnership" , "Research" , "Autoparts", "Alliance")
edge_attr(g6)

g6 <- set_graph_attr(g6, "about", "Car manufacturing companies")
g6 <- set_graph_attr(g6, "data", "relationship")

graph_attr(g6)

plot(graph_from_literal(Subaru--Suzuki,
                        Suzuki---Tata,
                        Tata--Tesla,
                        Tesla------Subaru,
                        Tesla--Suzuki),
    vertex.size=65, vertex.label.cex = 1.5, vertex.color = 'yellow')

plot(graph_from_literal(Subaru--+Suzuki,
                        Suzuki+---Tata,
                        Tata--+Tesla,
                        Tesla+------Subaru,
                        Tesla+--Suzuki),
    vertex.size=65, vertex.label.cex = 1.5, vertex.color = 'yellow')

empty_g <- make_empty_graph(30)
plot(empty_g, vertex.size = 10, vertex.label = NA)

full_g <- make_full_graph(20)
plot(full_g, vertex.size = 10, vertex.label = NA)

star_g <- make_star(30)
plot(star_g, vertex.size = 10, vertex.label = NA)

tree_g <- make_tree(30,children =3 , mode = 'undirected')
plot(tree_g, vertex.size=10, vertex.label= NA)

# nodes pointing towards child nodes
tree_g <- make_tree(30,children =3 , mode = 'out')
plot(tree_g, vertex.size=10, vertex.label= NA)

# nodes pointing away from child nodes
tree_g <- make_tree(30,children =3 , mode = 'in')
plot(tree_g, vertex.size=10, vertex.label= NA)

ring_g <- make_ring(30)
plot(ring_g, vertex.size=10, vertex.label= NA)

# erdos renyi model graph
erdos_renyi_g <- sample_gnm(n=50 , m=35)
plot(erdos_renyi_g, vertex.size=10, vertex.label= NA)


