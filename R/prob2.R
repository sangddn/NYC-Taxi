find_most_connected <- function(month, year, num_nodes) {
  
  # Retrieve data from S3
  str_glue(
    'aws s3 cp s3://hw13-prob1-results/', 
    year, '-', month, '.csv ', 'dynamic/matx.csv'
  ) %>%
    system
  
  # Read in data and convert into an igraph object
  dat <- fread('dynamic/matx.csv', header = TRUE) %>% 
    as.matrix %>% 
    graph_from_adjacency_matrix
  
  # Create a matrix of possible combinations.
  # Excluding sets with repeated numbers (e.g., 1, 1, 2).
  # Order does not matter (e.g., 1, 2, 3 = 2, 3, 1).
  combs <- gtools::combinations(gorder(dat), num_nodes)
  
  # Build a vector of numbers of connections in each combination
  connectivity <- mclapply(
      1:nrow(combs), # Number of combinations
      function(x) {
        # Create subgraph from big graph
        subgr <- subgraph(dat, combs[x,])
          # combs[x,] = combination being considered = vertices of new graph
  
        # Return number of edges of subgraph (ie number of connections)
        return(gsize(subgr))
      },
      mc.cores = 8
    ) %>%
    unlist

  most_connected <- combs[which.max(connectivity),]
  out <- append(most_connected, max(connectivity))
  
  return(out)
}