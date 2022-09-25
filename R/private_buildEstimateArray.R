buildEstimateArray <- function(labeltext, lower, upper, mean) {
  if (missing(lower) &&
      missing(upper) &&
      missing(mean)) {
    if (missing(labeltext)) {
      stop(
        "You need to provide the labeltext or",
        " the mean/lower/upper arguments"
      )
    }

    mean <- labeltext
    labeltext <- rownames(mean)
  }

  if (missing(lower) &&
      missing(upper)) {
    assert(
      check_matrix(mean, ncols = 3),
      check_array(mean, d = 3),
      check_integer(dim(mean)[2], lower = 3, upper = 3)
    )
  }

  if (missing(labeltext)) {
    labeltext <- rownames(mean)
  }

  if (is.null(labeltext)) {
    stop(
      "You must provide labeltext either in the direct form as an argument",
      " or as rownames for the mean argument."
    )
  }
  # Assume that lower and upper are contained within
  # the mean variable
  if (missing(lower) &&
      missing(upper)) {
    if (NCOL(mean) != 3) {
      stop("If you do not provide lower/upper arguments your mean needs to have 3 columns")
    }

    # If the mean can in this case be eithe 2D-matrix
    # that generates a regular forest plot or
    # it can be a 3D-array where the 3:rd level
    # constitutes the different bands
    all <- prFpConvertMultidimArray(mean)
    mean <- all$mean
    lower <- all$lower
    upper <- all$upper
  }

  if (NCOL(mean) != NCOL(lower) ||
      NCOL(lower) != NCOL(upper) ||
      NCOL(mean) == 0) {
    stop(
      "Mean, lower and upper contain invalid number of columns",
      " Mean columns:", ncol(mean),
      " Lower bound columns:", ncol(lower),
      " Upper bound columns:", ncol(upper)
    )
  }

  if (NCOL(mean) == 1) {
    estimates <- array(cbind(mean, lower, upper), dim = c(NROW(mean), 3, 1))
  } else {
    estimates <- array(dim = c(NROW(mean), 3, NCOL(mean)))
    for (i in 1:NCOL(mean)) {
      estimates[,,i] <- cbind(mean[,i], lower[,i], upper[,i])
    }
  }

  list(labeltext = labeltext,
       estimates = estimates)
}
