parse_cell <- function(x) {
  tryCatch(
    {
      x %>% fromJSON() # ]%>% do.call(rbind, .)
    },
    error = function(cond) {
      return (NA)
    },
    warning = function(cond) {
      return (NA)
    }
  )
}

parse_col <- function(x) {
  x %>%
    map(parse_cell) %>%
    map(as_data_frame) %>%
    bind_rows()
}


parse_qrt_csv <- function(x) {
  library(jsonlite)
  library(purrr)
  library(dplyr)
  
  d <- read_csv(x, col_names = TRUE)[-1, ]
  
  # Parsing all JSON columns
  a <- d %>%
    select(contains("ExitTest_1_TEXT")) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  # Fix [] in names
  names(a) <- sub("\\[", ".", names(a))
  names(a) <- sub("\\]", "", names(a))
  
  # Repeat all other columns
  # ExitTest_[0-9]_TEXT.[0-9].
  # [0-9]_stimulus_[0-9]
  b <- d %>% select(-contains("ExitTest_1_TEXT"))
  b <- do.call(rbind, replicate(nrow(a) / nrow(d), b, simplify = FALSE))
  
  # Return binded columns
  cbind(b, a)
}


parse_csv <- function(x) {
  library(jsonlite)
  library(purrr)
  library(dplyr)
  
  d <- read_csv(x, col_names = TRUE)[c(-1, -2), ]
  
  # Parsing all JSON columns
  a <- d %>%
    select(matches("[0-9]_stimulus|[0-9]_grp_decision|[0-9]_item|[0-9]_decision")) %>%
    select(-contains("Topics")) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  # Repeat all other columns
  b <- d %>% select(-matches("[0-9]_stimulus|[0-9]_grp_decision|[0-9]_item|[0-9]_decision"))
  b <- do.call(rbind, replicate(nrow(a) / nrow(d), b, simplify = FALSE))
  
  # Return binded columns
  cbind(b, a)
}


parse_csv2 <- function(x) {
  library(jsonlite)
  library(purrr)
  library(dplyr)
  
  d <- read_csv(x, col_names = TRUE)[c(-1, -2), ]
  
  # Parsing all JSON columns
  a <- d %>%
    select(matches("[0-9]_stimulus|[0-9]_confidence|[0-9]_decision|[0-9]_ind_stimulus|[0-9]_ind_confidence|[0-9]_ind_decision|[0-9]_wait|[0-9]_grp_stimulus|[0-9]_grp_confidence|[0-9]_grp_decision")) %>%
    select(-contains("Topics")) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  # Repeat all other columns
  b <- d %>% select(-matches("[0-9]_stimulus|[0-9]_confidence|[0-9]_decision|[0-9]_ind_stimulus|[0-9]_ind_confidence|[0-9]_ind_decision|[0-9]_wait|[0-9]_grp_stimulus|[0-9]_grp_confidence|[0-9]_grp_decision"))
  b <- do.call(rbind, replicate(nrow(a) / nrow(d), b, simplify = FALSE))
  
  # Return binded columns
  cbind(b, a)
}


parse_csv3 <- function(x) {
  library(jsonlite)
  library(purrr)
  library(dplyr)
  
  d <- read_csv(x, col_names = TRUE)[c(-1, -2), ]
  
  # Parsing all JSON columns
  a <- d %>%
    select(matches("[0-9]_grp_stimulus")) %>%
    select(-contains("Topics")) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  # Repeat all other columns
  b <- d %>% select(-matches("[0-9]_grp_stimulus"))
  b <- do.call(rbind, replicate(nrow(a) / nrow(d), b, simplify = FALSE))
  
  # Return binded columns
  cbind(b, a)
}

# parse_group_java_csv <- function(x) {
#   library(jsonlite)
#   library(purrr)
#   library(dplyr)
#   
#   d <- readLines(x)[-2] %>%
#     textConnection() %>%
#     read.csv(header = TRUE, stringsAsFactors = FALSE)
#   
#   # Parsing all JSON columns
#   a <- d %>%
#     select(matches("[0-9]_stimulus|[0-9]_grp_decision|[0-9]_item")) %>%
#     map(parse_col) %>%
#     map(as_data_frame) %>%
#     bind_rows()
#   
#   # Fix [] in names
#   names(a) <- sub("\\[", ".", names(a))
#   names(a) <- sub("\\]", "", names(a))
#   
#   # Repeat all other columns
#   b <- d %>% select(-matches("[0-9]_stimulus|[0-9]_grp_decision|[0-9]_item"))
#   b <- do.call(rbind, replicate(nrow(a) / nrow(d), b, simplify = FALSE))
#   
#   # Return binded columns
#   cbind(b, a)
# }
