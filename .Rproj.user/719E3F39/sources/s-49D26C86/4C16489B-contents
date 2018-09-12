#'dijkstra function
#'
#'This function implements the Dijkstras algorithm to find the shortest path
#'from the initial node to every other node in the graph. MOre detail about the
#'algorithm can be found here:
#'\url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#'
#'@param wiki_graph A data.frame that shows the nodes and their dependencies
#'  within a graph.
#'@param init_node An integer that states the number of the source node in the
#'  graph.
#'
#'@return The output is a numeric vector.
#'
#'@examples wiki_graph <-
#'  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'  v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'  w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'
#'dijkstra(wiki_graph,1) #0 7 9 20 20 11
#'
#'@export

dijkstra <- function(wiki_graph, init_node){

if((!is.data.frame(wiki_graph))& (!is.integer(init_node))){stop("wrong input!")}
if(any(names(wiki_graph) != c("v1", "v2", "w"))){stop("wrong column name!")}
if((init_node > max(wiki_graph[,1]))|init_node < min(wiki_graph[,1])){stop("Wrong node number!")}
#This is a vector and we will update it to get the final result!
output <- rep(max(wiki_graph[,3])^max(wiki_graph[,1]),max(wiki_graph[,1]))

output[init_node] <- 0

# We should find the nodes that are related directly to the init_node
for_visit <- wiki_graph[which(wiki_graph[,1]==init_node),2:3]

# We update output for all of the directly related nodes
for(i in 1:length(for_visit[,1])){output[for_visit[i,1]] <- for_visit[i,2]}

#for each directly related node we will find their own directly related node
#and we will update the output based on the condition number 1
for(j in for_visit[,1]){
    for_shorter <- wiki_graph[which(wiki_graph[,1]==j),2:3]
      for(a in 1:length(for_shorter[,1])){

      #condition number 1
      if(output[for_shorter[a,1]] > (output[j] + for_shorter[a,2])){
        output[for_shorter[a,1]] <-  output[j] + for_shorter[a,2]
      }
    }
  }
output
}

