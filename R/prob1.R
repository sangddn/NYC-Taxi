save_csv_files <- function() {
  # Downloads csv files from S3 bucket nyc-tlc in 2019 and 2020 to local folder.
  
  # Copy yellow files with names having patterns "2019" or "2020" to folder raw_data.
  system('aws s3 cp "s3://nyc-tlc/trip data/" raw_data/. --recursive --exclude "*" --include "yellow*2019*" --include "yellow*2020*"')
}

# Create folders.
create_folders <- function()
  system('mkdir raw_data processed_data dynamic')

# Create S3 bucket.
create_results_bucket <- function()
  system('aws s3 create-bucket -bucket hw13-prob1-results')

processed_files <- function() {
  system('aws s3 ls s3://hw13-prob1-results > dynamic/processed_files.txt')
  processed <- readLines('dynamic/processed_files.txt')
  
  return(processed)
}

map_matrices <- function() {
  # Converts all csv files in folder raw_data into matrices and saves them to an S3 bucket.
  
  # Convert each csv file into a matrix
  csvfiles <- list.files(
    'raw_data', 
    full.names = TRUE
  )
  
  processed <- processed_files()
  
  for(csvfile in csvfiles) {
    month <- str_sub(csvfile, 26, 32) # Example: 2019-01
    
    # Detect if the file has already been processed. 
    # If yes, skip the loop.
    if(str_detect(processed, month)) 
      next 
      
    dat <- fread(csvfile)[
        , .(PULocationID, DOLocationID) # Keep what matters
      ]
    
    # Build matrix
    out_matrix <- dat %>%
      graph.data.frame %>%
      get.adjacency %>%
      as.matrix
    
    # Reorder matrix
    out_matrix <- out_matrix[, order( as.numeric( colnames(out_matrix) ) )]
    out_matrix <- out_matrix[ order( as.numeric( rownames(out_matrix) ) ), ]
    
    # Write matrix
    out_path <- str_glue('processed_data/', month, '.csv')
    
    fwrite(
      out_matrix,
      out_path
    )
    
    # Push to S3
    str_glue(
      'aws s3 cp ', 
      out_path, 
      ' s3://hw13-prob1-results'
    ) %>% 
      system
  }
  
}