find_three_most_connected_all_months <- function() {
  
  months <- processed_files %>% 
    str_sub(32, 38) # Vector example: "2019-01" "2019-02" "2019-03"
  
  # Set a plan to instruct the next future function to run on 6 cores
  future::plan(multicore, workers = 6)
  out <- furrr::future_map_dfr(
      months, 
      function(month) {
        mon <- str_sub(month, 6, 7)
        yr <- str_sub(month, 1, 4)
        
        most_connected <- find_most_connected(mon, yr, 3)
        
        out <- data.frame(
            time = month, 
            r1 = most_connected[1],
            r2 = most_connected[2],
            r3 = most_connected[3],
            ntrips = most_connected[4],
            stringsAsFactors = FALSE
          )

        return(out)
      }
    )
  
  out_path <- 'processed_data/three_most_connected.csv'
  fwrite(out, out_path)
  
  str_glue('aws s3 cp processed_data/three_most_connected.csv s3://hw13-prob1-results/.') %>%
    system
  
}