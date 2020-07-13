# QUESTION 1.1

edges = read.csv("edges.csv")

users = read.csv("users.csv")

str(users)

str(edges) 

# QUESTION 1.2

table(users$locale, users$school)

# QUESTION 1.3

table(users$gender, users$school)

# QUESTION 2.1

install.packages("igraph")

library(igraph)

# QUESTION 2.2

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

# QUESTION 2.3

table(degree(g) >= 10)

# QUESTION 2.4

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
 summary(degree(g))

# QUESTION 3.1

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

# QUESTION 3.2

V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)

# QUESTION 3.3

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)

# QUESTION 4

rglplot(g, vertex.label=NA)
plot(g, edge.width=2, vertex.label=NA)

