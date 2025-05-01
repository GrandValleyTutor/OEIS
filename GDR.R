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

A333448_data <- A333448$data

## A045572 ----
A045572 <- oeis("A045572")

A045572_data <- A045572$data

## A383504 ----

# Not published yet
# Probably will be A383504ii version
# A383504 <- oeis("A383504")
# A383504_data <- A383504$data

## A383504i ----

generate_A383504i <- 
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

A383504i_data <- 
  generate_A383504i(121)

## A383504ii ----

## constructed 
generate_A383504ii <- function(N, A333448_vals) {
  if (length(A333448_vals) < N) {
    stop("A333448_vals must have at least N values.")
  }
  
  # Step 1: Generate the first N values of A045572
  A045572 <- integer(0)
  k <- 1
  while (length(A045572) < N) {
    if (k %% 10 %in% c(1, 3, 7, 9)) {
      A045572 <- c(A045572, k)
    }
    k <- k + 1
  }
  
  # Step 2: Create A383504ii[n] = A333448[i] if A045572[i] == n
  max_n <- max(A045572)
  A383504ii <- integer(max_n)
  for (i in seq_len(N)) {
    n <- A045572[i]
    A383504ii[n] <- A333448_vals[i]
  }
  
  return(A383504ii)
}

A383504ii_data <- generate_A383504ii(50, A333448_data)
A383504ii_data[9]     # 9 is A045572[4], so this should return A333448[4] = 1
A383504ii_data[13]    # should return A333448[6] = 4
A383504ii_data[10]    # not in A045572 → 0
A383504ii_data[27]

## Explicit
# Patched correct A333448 values for small n < 12
A333448_patch <- c(1, 1, 5, 1, 10, 4, 12, 2, 19, 7, 19, 3)

A333448_explicit <- function(n) {
  if (n <= length(A333448_patch)) {
    return(A333448_patch[n])
  }
  term1 <- 1 - 2 * (floor((n + 1) / 4) + n)
  term2 <- 1 - (1 + (floor(16 * 9^n / 205) %% 9)) / 10
  a_n <- 1/10 - term1 * term2
  return(as.integer(a_n))
}

A383504ii_explicit <- function(n) {
  gcd <- function(a, b) if (b == 0) a else Recall(b, a %% b)
  if (gcd(n, 10) != 1) stop("n must be coprime to 10")
  
  i <- sum(sapply(1:n, function(k) gcd(k, 10) == 1))
  A333448_explicit(i)
}

A383504ii_explicit(9)
A383504ii_explicit(13)
A383504ii_explicit(10)
A383504ii_explicit(27)



## GDR Definitions ----

# A383504ii(n) = A333448(N) iff [A045572(N)=n and gcd(n,10)=1]

# A045572(A383504i(n)) = A383504i(A045572(n)) = n
# A045572(4)=9 and A383504(9)=4

# D(n) = A333448(A383504i(n))
# D(9) = A333448(A383504i(9)) = A333448(4) = 1

# D(n) = A383504ii(n)
# D(9) = A383504ii(9) = 1

## Work ----
min_len <- 
  min(length(A045572_data),
      length(A333448_data),
      length(A383504i_data),
      length(A383504ii_data))

df <-
  tibble(A045572 = A045572_data[1:min_len],
         A383504i = A383504i_data[1:min_len],
         A383504ii = A383504ii_data[1:min_len],
         A333448 = A333448_data[1:min_len]
  )

df |> view()
