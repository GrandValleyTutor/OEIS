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

pacman::p_load(tidyverse,
               jsonlite)

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

## A333448 ----
A333448 <- oeis("A333448")
names(A333448)
head(A333448$data)

A333448_data <- A333448$data

## A045572 ----
A045572 <- oeis("A045572")
names(A045572)
head(A045572$data)

A045572_data <- A045572$data

## A383504 ----

# Not published yet
# A383504 <- oeis("A383504")
# names(A383504)
# head(A383504$data)

A383504_data <-
  c(
  1, 3, 7, 9, 11, 13, 17, 19, 21, 23,
  27, 29, 31, 33, 37, 39, 41, 43, 47, 49,
  51, 53, 57, 59, 61, 63, 67, 69, 71, 73,
  77, 79, 81, 83, 87, 89, 91, 93, 97, 99,
  101, 103, 107, 109, 111, 113, 117, 119, 121
)

## Work ----
min_len <- 
  min(length(A045572_data),
      length(A333448_data),
      length(A383504_data))

df <-
  tibble(A045572 = A045572_data[1:min_len],
         A333448 = A333448_data[1:min_len],
         A383504 = A383504_data[1:min_len]
    
  )

df
