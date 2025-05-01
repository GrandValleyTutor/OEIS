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
head(A333448$data)

A333448_data <- A333448$data

## A045572 ----
A045572 <- oeis("A045572")
head(A045572$data)

A045572_data <- A045572$data

## A383504 ----

# Not published yet
# A383504 <- oeis("A383504")
# names(A383504)
# head(A383504$data)

generate_A383504 <- 
  function(N) {
    a <- integer(N)  # pre-allocate vector of zeros
    last_nonzero <- 0
    
    for (n in 1:N) {
      if (n %% 2 == 0 || n %% 5 == 0) {
        a[n] <- 0
      } else {
        a[n] <- last_nonzero + 1
        last_nonzero <- a[n]
      }
    }
    
    return(a)
}

A383504_data <- 
  generate_A383504(121)

## GDR Definitions ----

# A045572(A383504(n)) = A383504(A045572(n)) = n
# A045572(4)=9 and A383504(9)=4

# D(n) = A333448(A383504(n))
# D(9) = A333448(A383504(9)) = A333448(4) = 1

## Work ----
min_len <- 
  min(length(A045572_data),
      length(A333448_data),
      length(A383504_data))

df <-
  tibble(A045572 = A045572_data[1:min_len],
         A383504 = A383504_data[1:min_len],
         A333448 = A333448_data[1:min_len]
  )

df
