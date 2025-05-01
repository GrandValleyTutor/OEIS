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
               jsonlite,
               gmp
               )

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

##
A333448 <- oeis("A333448")

A333448_oeis <- A333448$data

## 
packageDescription("gmp")
# help(package = "gmp")
help("pow.bigz")
help("div.bigz")
help("mul.bigz")
help("mod.bigz")
help("bigq")
help("round.bigq")

A333448_explicit <- function(n) {
  # OEIS supplies the first 12 values explicitly
  patch <- c(1, 1, 5, 1, 10, 4, 12, 2, 19, 7, 19, 3)
  if (n <= 12) return(patch[n])
  
  b <- A045572_explicit2(n)              # value of A045572 at index n
  m <- (b - 1) %/% 10          # integer part of (b - residue)/10
  r <- b %% 10                 # residue class of b (1,3,7,9)
  
  switch(as.character(r),
         "1" = 9 * m + 1,
         "3" = 3 * m + 1,
         "7" = 7 * m + 5,
         "9" =       m + 1)
}

A333448_explicit_ <-
  sapply(1:67, A333448_explicit)

# Compare
A333448_oeis[1:67]
A333448_explicit_[1:67]
all.equal(A333448_oeis, A333448_explicit_)

## A045572 ----

##
A045572 <- oeis("A045572")

A045572_oeis <- A045572$data

##
A045572_explicit <- function(n) {
  term1 <- 10 * floor((n - 1) / 4)
  term2 <- 2 * floor((4 * ((n - 1) %% 4) + 1) / 3)
  return(term1 + term2 + 1) # Credit Carl R. White https://oeis.org/A045572
}

A045572_explicit_ <-
  sapply(1:62, A045572_explicit)

##
A045572_explicit2 <- function(n) {
  q <- (n - 1) %/% 4           # how many complete blocks of 4 we passed
  r <- (n - 1) %% 4            # position inside the block
  
  # the 4 coprime residues mod 10 are 1,3,7,9 in that order
  residues <- c(1, 3, 7, 9)
  10 * q + residues[r + 1]
}

A045572_explicit_2 <-
  sapply(1:62, A045572_explicit2)

# Compare
A045572_oeis[1:62]
A045572_explicit_[1:62]
A045572_explicit_2[1:62]
all.equal(A045572_oeis,A045572_explicit_)
all.equal(A045572_oeis,A045572_explicit_2)

## A383504i ----
# Not published yet

##
A383504i_implicit <- 
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

A383504i_implicit_ <- 
  A383504i_implicit(67)

##
A383504i_explicit <- function(n) {
  
  N <- 1000
  A045572_vec <- sapply(1:N, A045572_explicit)
  
  if (gcd(n, 10) == 1) {
    # Find position of n in A045572 sequence
    which(A045572_vec == n)[1]
  } else {
    0
  }
}

A383504i_explicit_ <- 
  sapply(1:67, A383504i_explicit)

# Compare
A383504i_implicit_[1:67]
A383504i_explicit_[1:67]
all.equal(A383504i_implicit_, A383504i_explicit_)

## A383504ii ----
# Not published yet

##
A383504ii_implicit <-
  function(N, A333448_vals) {
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

A383504ii_implicit_ <-
  A383504ii_implicit(67, A333448_oeis)

##
A383504ii_explicit <- function(n) {
  n   <- as.integer(n)
  out <- integer(length(n))
  
  copr <- (n %% 2 != 0) & (n %% 5 != 0)       # gcd(n,10)=1 ?
  if (any(copr)) {
    res <- n[copr] %% 10
    pos <- match(res, c(1, 3, 7, 9)) - 1      # 0,1,2,3
    N   <- 4 * (n[copr] %/% 10) + pos + 1     # index in coprime order
    out[copr] <- vapply(N, function(k) as.integer(A333448(k)), integer(1))
  }
  out
}

A383504ii_explicit_ <- 
  sapply(1:length(A383504ii_implicit_), A383504ii_explicit)

# Compare 
A383504ii_implicit_[1:67]
A383504ii_explicit_[1:67]
all.equal(A383504ii_implicit_[1:67],A383504ii_explicit_[1:67])

## *GDR Definitions ----

# A383504ii(n) = A333448(N) iff [A045572(N)=n and gcd(n,10)=1]

# A045572(A383504i(n)) = A383504i(A045572(n)) = n when gcd(n,10)=1
# A383504i(n)=0 when gcd(n,10)>1
# A045572(4)=9 and A383504i(9)=4

# D(n) = A333448(A383504i(n))
# D(9) = A333448(A383504i(9)) = A333448(4) = 1

# D(n) = A383504ii(n)
# D(9) = A383504ii(9) = 1

## Visual Comparisons ----
min_len <- 
  min(length(A333448_oeis),
      length(A045572_oeis),
      length(A383504i_explicit_),
      length(A383504i_implicit_),
      length(A383504ii_explicit_),
      length(A383504ii_implicit_))

df <-
  tibble(A045572_oeis = A045572_oeis[1:min_len],
         # A045572_explicit_ = A045572_explicit_[1:min_len],
         # A045572_explicit_2 = A045572_explicit_2[1:min_len],
         A383504i_implicit = A383504i_implicit_[1:min_len],
         A383504i_explicit = A383504i_explicit_[1:min_len],
         A383504ii_implicit = A383504ii_implicit_[1:min_len],
         A383504ii_explicit = A383504ii_explicit_[1:min_len],
         A333448_oeis = A333448_oeis[1:min_len],
         # A333448_explicit = A333448_explicit_[1:min_len]
  )

df |> view()
