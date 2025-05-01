## Libraries ----
if (
  !require(
    "pacman"
  )
) {
  install.packages(
    "pacman"
  )
}

pacman::p_load(jsonlite)

## OEIS Function ----
oeis <-
  function(seq_id) {
    
    url <- 
      paste0("https://oeis.org/search?q=", seq_id, "&fmt=json")
    
    raw_json <- tryCatch({
      readLines(url, warn = FALSE)
    }, error = function(e) return(NULL))
    
    if (is.null(raw_json)) return(NULL)
    
    json_string <-
      paste(raw_json, collapse = "")
    
    df <- tryCatch({
      fromJSON(json_string)
    }, error = function(e) return(NULL))
    
    if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
    
    # Convert to list
    entry <- as.list(df[1, ])
    
    # Convert "data" field to numeric vector
    entry$data <- as.numeric(strsplit(entry$data, ",")[[1]])
    
    return(entry)
}

