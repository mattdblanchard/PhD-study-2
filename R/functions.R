# this script contains commonly used functions for S2 data
# to calculate POST using Simon's code
# Takes a data frame (x) with columns named "d1", "d2" ... for decisions (scored 0 and 1) and "c1", "c2" ... (for confidence ratings)
computePost <- function(x, group = c(T)) {
  dcs <- getCol(x, "d", group)
  crs <- getCol(x, "c", group)

  # Fit logistic regression model
  posts <- apply(x, 1, function(i) {
    # Return NA if all decisions or confidence ratings are the same
    if (allWithin(i[dcs], na.rm = TRUE) | allWithin(i[crs], na.rm = TRUE)) {
      return (NA)
    }

    fit  <- glm(i[dcs] ~ i[crs], family = binomial())
    -coef(fit)[[1]] / coef(fit)[[2]]
  })

  # Set any POST to NA if it's outside acceptable range (0, 100)
  posts[is.na(posts) | posts < 0 | posts > 100] <- NA

  posts
}

# Helper functions..

getCol <- function(x, i, group = c(T)) {
  grep(paste0(i, "[0-9]"), names(x), value = T)[group]
}

allWithin <- function(x, tol = 0, na.rm = FALSE) {
  abs(max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) < tol
}