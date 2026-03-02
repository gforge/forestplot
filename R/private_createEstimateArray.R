createEstimateArray <- function(labeltext, lower, upper, mean) {
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

  # ---------------------------------------------------------------------------
  # Input validation: ensure coherency of mean/lower/upper values.
  # 'lower' cannot exceed 'upper'; if all three are provided the 'mean'
  # should lie between the bounds. Doing this early prevents cryptic errors
  # during plotting (e.g. missing boxes or Inf warnings when negative values
  # are used).
  # ---------------------------------------------------------------------------
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
    estimates <- array(NA, dim = c(NROW(mean), 3, 1))
    estimates[, , 1] <- cbind(mean, lower, upper) |> as.matrix()
  } else {
    estimates <- array(dim = c(NROW(mean), 3, NCOL(mean)))
    for (i in seq_len(NCOL(mean))) {
      estimates[, , i] <- cbind(mean[, i], lower[, i], upper[, i])
    }
  }

  # validate input consistency ------------------------------------------------
  # lower should never exceed upper; means should lie between bounds when not NA.
  # this also covers situations where a CI crosses zero in a strange way.
  # We vectorize over the third dimension (bands).
  vals <- estimates
  # dims: rows x 3 x bands
  # avoid drop=TRUE; keep array shape so which(...,arr.ind=TRUE) returns a
  # matrix even when there is only a single band or single row.
  lower_vals <- vals[, 2, , drop = FALSE]
  upper_vals <- vals[, 3, , drop = FALSE]
  mean_vals <- vals[, 1, , drop = FALSE]

  # helper to produce informative messages
  bad_lower <- which(lower_vals > upper_vals, arr.ind = TRUE)
  if (is.matrix(bad_lower) && nrow(bad_lower) > 0) {
    stop(
      "Invalid confidence interval: lower bound exceeds upper bound for ",
      "row ", bad_lower[1, 1],
      if (ncol(bad_lower) > 1) paste0(", band ", bad_lower[1, 2]) else "",
      ". Please check your 'lower' and 'upper' arguments."
    )
  }

  # check mean outside interval when all three present
  out_of_range <- which(
    (!is.na(mean_vals)) &
      (!is.na(lower_vals)) &
      (!is.na(upper_vals)) &
      (mean_vals < lower_vals | mean_vals > upper_vals),
    arr.ind = TRUE
  )
  if (is.matrix(out_of_range) && nrow(out_of_range) > 0) {
    stop(
      "Estimate outside confidence interval for row ", out_of_range[1, 1],
      if (ncol(out_of_range) > 1) paste0(", band ", out_of_range[1, 2]) else "",
      ". Ensure each mean lies between its lower and upper limits."
    )
  }

  d <- dimnames(estimates)
  d[[2]] <- c("mean", "lower", "upper")
  dimnames(estimates) <- d
  list(
    labeltext = labeltext,
    estimates = estimates
  )
}
