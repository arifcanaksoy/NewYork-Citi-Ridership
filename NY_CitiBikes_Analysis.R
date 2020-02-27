setwd("/Users/Can/Desktop/IE/SocialNA")

# Import the dataset, citibike NewYork Bike Ridership for January, 2020
df_journeys <- read.csv("202001-citibike-tripdata.csv", 1)
str(df_journeys)

#Instead of analysing all, 1.25M records 1.000 rows seem to be enough
set.seed(42)
df_journeys1 <- df_journeys[sample(1:nrow(df_journeys), 10000, replace = FALSE),]

# after column transformations
write.csv(df_journeys1, "/Users/Can/Desktop/IE/SocialNA/journeys_new.csv", row.names = FALSE)

# based on birth.year column, first age is calculated...
for (i in 1:nrow(df_journeys1)) {
  df_journeys1$age[i] <- 2020 - df_journeys1$birth.year[i]
}
#... and then ageGroup field is created to make further analysis users
df_journeys1$ageGroup <- cut(df_journeys1$age, breaks=c(0,30,59,140), labels=c("young","middleage","eldery"))

#df_journeys1$ageGroup <- cut(df_journeys1$age, breaks=c(0,39,59,100), labels=c("young","middleage","eldery"))
#Compute distance between starting points and Ending points.
install.packages("geosphere")
library(geosphere)
library(dplyr)

for (i in 1:nrow(df_journeys1)) {
  df_journeys1$geodistance[i] <-  distm(c(df_journeys1$start.station.longitude[i], df_journeys1$start.station.latitude[i]), c(df_journeys1$end.station.longitude[i], df_journeys1$end.station.latitude[i]), fun = distHaversine)
}

### START of NETWORK ANALYSIS ###

#Create the graph of bikesharing 
# load packages for network exploration
library(igraph)
library(ggplot2)
## Visualizing the Network
install.packages("ggraph")
library(ggraph)
str(df_journeys1)

# for trips based on usertype
journey_df_user <- df_journeys1 %>% 
  group_by(usertype, start.station.id, end.station.id) %>% 
  summarize(weights = n())

journey_g_user <- graph_from_data_frame(journey_df_user[, c("start.station.id", "end.station.id")])

journey_g_user$name <- "New York BikeShare Network based on Users"

E(journey_g_user)$usertype <- journey_df_user$usertype
E(journey_g_user)$weight <- journey_df_user$weights
V(journey_g_user)$strength <- strength(journey_g_user)
V(journey_g_user)$id <- names(V(journey_g_user))

# graph of the ride networks per usertype
ggraph(journey_g_user, layout = "graphopt") + 
  geom_edge_link(aes(color = as.factor(usertype), alpha = weight)) +
  ggtitle("Graph of New York BikeShare Network based on Usertypes")
# note that the distance of graph is not the real distance between the stops

#-----

# for trips based on gender

df_journeys1$gender[df_journeys1$gender == 0] <- "unknown"
df_journeys1$gender[df_journeys1$gender == 1] <- "male"
df_journeys1$gender[df_journeys1$gender == 2] <- "female"


journey_df_gender <- df_journeys1 %>% 
  group_by(gender, start.station.id, end.station.id) %>% 
  summarize(weights = n())

journey_g_gender <- graph_from_data_frame(journey_df_gender[, c("start.station.id", "end.station.id")])

journey_g_gender$name <- "New York BikeShare Network based on Gender"

E(journey_g_gender)$gender <- journey_df_gender$gender
E(journey_g_gender)$weight <- journey_df_gender$weights
V(journey_g_gender)$strength <- strength(journey_g_gender)
V(journey_g_gender)$id <- names(V(journey_g_gender))


# graph of the ride networks per usertype

ggraph(journey_g_gender, layout = "graphopt") + 
  geom_edge_link(aes(color = as.factor(gender), alpha = weight)) +
  ggtitle("Graph of New York BikeShare Network based on Gender") + geom_node_point(aes(alpha=0.2, size= strength))
# note that the distance of graph is not the real distance between the stops

# for trips based on ageGroup: generation
journey_df_generation <- df_journeys1 %>% 
  group_by(ageGroup, start.station.id, end.station.id) %>% 
  summarize(weights = n())

journey_g_generation <- graph_from_data_frame(journey_df_generation[, c("start.station.id", "end.station.id")])

journey_g_generation$name <- "New York BikeShare Network based on Generation"

E(journey_g_generation)$ageGroup <- journey_df_generation$ageGroup
E(journey_g_generation)$weight <- journey_df_generation$weights
V(journey_g_generation)$degrees <- degree(journey_g_generation)
V(journey_g_generation)$strength <- strength(journey_g_generation)
V(journey_g_generation)$id <- names(V(journey_g_generation))
V(journey_g_generation)$betweenness <- betweenness(journey_g_generation)

data_frame(
  "Degree Centrality" = V(journey_g_generation)$id[which.max(V(journey_g_generation)$degrees)],
  "Strength Centrality" = V(journey_g_generation)$id[which.max(V(journey_g_generation)$strength)],
  "Betweenness Centrality" = V(journey_g_generation)$id[which.max(V(journey_g_generation)$betweenness)]
)

# graph of the ride networks per usertype
ggraph(journey_g_generation, layout = "graphopt") + 
  geom_edge_link(aes(color = as.factor(ageGroup), alpha = weight)) +
  ggtitle("Graph of New York BikeShare Network based on Generation")
# note that the distance of graph is not the real distance between the stops
# when we look at generation, there seems no preference on certain path


# for overall trips
journey_df <- df_journeys1 %>% 
  group_by(start.station.id, end.station.id) %>% 
  summarize(weights = n())

journey_g <- graph_from_data_frame(journey_df[, c("start.station.id", "end.station.id")])

## Quick Exploration of the graph

# explore the set of nodes and print the number of nodes
V(journey_g)
gorder(journey_g)

# explore the set of ties and print the number of ties
E(journey_g)
gsize(journey_g)
# means that our graph is still pretty dense...


# add the name attribute "NewYork Citi Bikesharing Network" to the network and print it
journey_g$name <- "New York Citi Bikesharing Network"

# add node attribute id and print the node `id` attribute
V(journey_g)$id <- names(V(journey_g))
V(journey_g)$degree <- degree(journey_g, mode = "all")
V(journey_g)$strength <- strength(journey_g)

# add edge weights
E(journey_g)$weight <- journey_df$weights


#in case we need adjacency matrix:
A <- as_adjacency_matrix(journey_g, attr = "weight", names = FALSE)

# degree centrality measures the importance of a station by the number of edges (degree): "station 402"
sort(degree(journey_g), decreasing = T)
# betwenness means the highest frequency of crossroads: again "station 402"
sort(betweenness(journey_g), decreasing = T)
# strength takes weights into consideration in addition to number of edges: "station 402"
sort(strength(journey_g), decreasing = T)

# Station 402 is the hot spot!!



# start with partial look, just as an example
subgraf <- induced_subgraph(journey_g, 1:20)
plot(subgraf, vertex.label = NA, edge.arrow.width = 0.4, edge.arrow.size = 0.2,
     margin = 0, vertex.size = 6, edge.width = log(E(subgraf)$weight + 2))
# the ones whose arrow goes back, means ride is ended up where it started, and which seems pretty common

# overall look of the ride networks
ggraph(journey_g, layout = "with_kk") + 
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(aes(size = strength, alpha = strength)) + 
  ggtitle("Graph of New York BikeShare Network")
# note that the distance of graph is not the real distance between the stops

# Calculate the median betweenness
E(journey_g)$betweenness <- edge_betweenness(journey_g)
median_betweenness = median(E(journey_g)$betweenness)

ggraph(journey_g, layout = "with_kk") + 
  # Filter ties for betweenness greater than the median
  geom_edge_link(aes(alpha = betweenness, filter = betweenness > median_betweenness)) + 
  theme(legend.position="none")

# overall look of the ride networks
ggraph(journey_g, layout = "kk") + 
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(
    aes(color = as.factor(strength)),show.legend = T) +
  ggtitle("Graph of Newyork BikeShare Network") 
  

# Investigate rides of usertypes: Subscriber - Customer

subscribers <- journey_df_user %>% 
  # Filter for rows where usertype is Subscriber
  filter(usertype == "Subscriber")

# Count the number of subscriber trips
n_subscriber_trips <- nrow(subscribers)

subscriber_trip_graph <- subscribers %>% 
  # Group by from_station_id and to_station_id
  group_by(start.station.id, end.station.id) %>% 
  # Calculate summary statistics
  summarize(
    # Set weights as proportion of total trips 
    weights = n() / n_subscriber_trips
  ) %>%
  # Make a graph from the data frame
  graph_from_data_frame()

# Now for Customers
customers <- journey_df_user %>% filter(usertype == "Customer")
n_customer_trips <- nrow(customers)
customer_trip_graph <- customers %>% 
  group_by(start.station.id, end.station.id) %>% 
  summarize(weights = n() / n_customer_trips) %>%
  graph_from_data_frame()


# Check out which one of customer group is using the service more
gsize(subscriber_trip_graph)
gsize(customer_trip_graph)
#it seems that subscribers beats the casual customers


# Induce a subgraph from customer_trip_graph of the first 12 vertices
twelve_customer_trip_graph <- induced_subgraph(customer_trip_graph,1:12)


#Compare graph distance vs. geographic distance.

# on graph:
farthest_vertices(journey_g)

get_diameter(journey_g)


# Get the to stations coordinates
st_to <- df_journeys %>%
  filter(start.station.id == 3850) %>%
  sample_n(1) %>%
  select(start.station.longitude, start.station.latitude)
# Get the from stations coordinates
st_from <- df_journeys %>%
  filter(start.station.id == 3522) %>%
  sample_n(1) %>%
  select(start.station.longitude, start.station.latitude)

# find the geographic distance
farthest_dist <- distm(st_from, st_to, fun = distHaversine)
farthest_dist

#Compare Subscriber vs. Non-Subscriber Distances
farthest_vertices(subscriber_trip_graph)
farthest_vertices(customer_trip_graph)

#Farthest Real Distance for Subscribers
# Get the to stations coordinates
st_to <- df_journeys %>%
  filter(start.station.id == 3499) %>%
  sample_n(1) %>%
  select(start.station.longitude, start.station.latitude)

# Get the from stations coordinates
st_from <- df_journeys %>%
  filter(start.station.id == 3838) %>%
  sample_n(1) %>%
  select(start.station.longitude, start.station.latitude)
st_from
# find the geographic distance
farthest_distance_subscriber <- distm(st_from, st_to, fun = distHaversine)
farthest_distance_subscriber


#Farthest Real Distance for Customer
st_to <- df_journeys1 %>%
  filter(start.station.id == 3749) %>%
  sample_n(1) %>%
  select(start.station.longitude, start.station.latitude)
st_to

# Get the from stations coordinates
st_from <- df_journeys %>%
  filter(start.station.id == 3113) %>%
  sample_n(1) %>%
  select(start.station.longitude, start.station.latitude)
st_from
# find the geographic distance
farthest_distance_customer <- distm(st_from, st_to, fun = distHaversine)
farthest_distance_customer
#So, customer's farthest distance tend to be narrower than subscriber's



# Most Traveled To and From Stations with Weights
trip_strng <- data_frame(
  # Find the "out" strength distribution
  trip_out = strength(journey_g, mode = "out"), 
  # ... and the "in" strength distribution
  trip_in = strength(journey_g, mode = "in"),
  # Calculate the ratio of out / in
  ratio = trip_out / trip_in
)

trip_strng_filtered <- trip_strng %>%
  # Filter for rows where trips in and out are both over 10
  filter(trip_out > 10, trip_in > 10) 

# Plot histogram of filtered ratios
hist(trip_strng_filtered$ratio)


#Visualize central vertices

g519 <- make_ego_graph(journey_g, 1, nodes = "519", mode= "out")[[1]]

# Plot ego graph
ggraph(
  g519, 
  edge.width = E(g519)$weight
  # Use geographic coordinates
)

#Weighted Measures of Centrality: eigen centrality and closeness

# This calculates weighted eigen-centrality 
ec_weight <- eigen_centrality(journey_g, directed = TRUE)$vector

# Calculate unweighted eigen-centrality 
ec_unweight <- eigen_centrality(journey_g, directed = FALSE, weights = NA)$vector

# This calculates weighted closeness
close_weight <- closeness(journey_g)
close_weight
# Calculate unweighted closeness
close_unweight <- closeness(journey_g, weights = NULL)

# Get vertex names
vertex_names <- names(V(journey_g))
vertex_names

# Complete the data frame to see the results for usertypes
data_frame(
  "Weighted Eigen Centrality" = vertex_names[which.min()],
  "Unweighted Eigen Centrality" = vertex_names[which.min(ec_unweight)],
  "Weighted Closeness" = vertex_names[which.min(close_weight)],
  "Unweighted Closeness" = vertex_names[which.min(close_unweight)]
)

# Calculate the minimum number of cuts
ud_cut <- min_cut(journey_g, value.only = FALSE)

# See the result
ud_cut

#Unweighted Clustering Randomizations
# Calculate global transitivity
actual_global_trans <- transitivity(journey_g, type = "GLOBAL")

# See the result
actual_global_trans


# Calculate the order
n_nodes <- gorder(journey_g)
n_nodes
# Calculate the edge density
edge_dens <- edge_density(journey_g)
edge_dens

# Run the simulation
simulated_global_trans <- rep(NA, 1000)
for(i in 1:1000) {
  # Generate an Erdos-Renyi simulated graph
  simulated_graph <- erdos.renyi.game(n_nodes, edge_dens, directed = TRUE)
  # Calculate the global transitivity of the simulated graph
  simulated_global_trans[i] <- transitivity(simulated_graph, type = "GLOBAL")
}

# Plot a histogram of simulated global transitivity
hist(
  simulated_global_trans, xlim = c(0.01, 0.2),
    main = "Unweighted clustering randomization"
)

# Add a vertical line at the actual global transitivity
abline(v = actual_global_trans, col = "red")



# Find the mean local weighted clustering coeffecient using transitivity()
actual_mean_weighted_trans <- mean(transitivity(journey_g, type = "weighted"))

# Calculate the order
n_nodes <- gorder(journey_g)

# Calculate the edge density
edge_dens <- edge_density(journey_g)

# Get edge weights
edge_weights <- E(journey_g)$weights
  

# Run the simulation
  # From previous step
  actual_mean_weighted_trans <- mean(transitivity(journey_g, type = "weighted"))
n_nodes <- gorder(journey_g)
edge_dens <- edge_density(journey_g)
edge_weights <- E(journey_g)$weight
simulated_mean_weighted_trans <- rep(NA, 100)
for(i in 1:100) {
  simulated_graph <- erdos.renyi.game(n_nodes, edge_dens, directed = TRUE)
  n_simulated_edges <- gsize(simulated_graph)
  E(simulated_graph)$weight <- sample(edge_weights, n_simulated_edges, replace = TRUE)
  simulated_mean_weighted_trans[i] <- mean(transitivity(simulated_graph, type = "weighted"))
}

# Plot a histogram of simulated mean weighted transitivity
hist(
  simulated_mean_weighted_trans, xlim = c(0.01, 0.2),
   
  main = "Mean weighted clustering randomization"
)

# Add a vertical line at the actual mean weighted transitivity
abline(v = actual_mean_weighted_trans, col = "red")


#find out start.stations which has highest avg. trip distance 
library(tidyr)
journey_df_distance <- df_journeys1 %>% drop_na() %>%
  group_by(start.station.id, end.station.id) %>% 
  summarize(dist = mean(geodistance), weight = n())

journey_g_dist <- graph_from_data_frame(journey_df_distance[, c("start.station.id", "end.station.id")])
V(journey_g_dist)$id <- 1:vcount(journey_g_dist)
E(journey_g_dist)$weight<- journey_df_distance$weight
E(journey_g_dist)$dist <- journey_df_distance$dist
arrange(journey_df_distance, desc(dist))
#top 10 start.station.id's with highest dist
is_far <- E(journey_g_dist)$dist > 6302
E(journey_g_dist)$fardist <- top_n(journey_df_distance,dist, 10)
ggraph(journey_g_dist, layout = "with_kk") + 
  geom_edge_link(aes(alpha=weight, color = is_far)) +
geom_node_text(aes(label=id), repel=0.25) +
  ggtitle("Graph of New York BikeShare Network")

