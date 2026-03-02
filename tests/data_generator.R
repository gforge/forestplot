make_forest_metadata_inventors_vs_mello <- function(seed = 1) {
  set.seed(seed)

  inventors <- c(
    "Alfred Nobel",
    "Gustaf Dalen",
    "Baltzar von Platen",
    "Carl Munters",
    "Hakan Lans",
    "Nils Bohlin",
    "Rune Elmqvist",
    "John Ericsson",
    "Anders Celsius",
    "Christopher Polhem"
  )

  mello <- c(
    "Loreen",
    "Carola",
    "Herreys",
    "Charlotte Perrelli",
    "Mans Zelmerlow",
    "Benjamin Ingrosso",
    "The Mamas",
    "Tusse",
    "John Lundvik",
    "Cornelia Jakobs"
  )

  mk_studies <- function(names, group) {
    k <- length(names)
    year <- sample(1998:2024, k, replace = TRUE)
    author <- paste(names, year)

    n1 <- sample(40:220, k, replace = TRUE)
    n2 <- pmax(30, n1 + sample(-30:30, k, replace = TRUE))

    # small systematic difference between groups (for demo realism)
    mu_log_or <- if (group == "Inventors") -0.20 else 0.05
    true_or <- exp(rnorm(k, mu_log_or, 0.35))

    base_risk <- runif(k, 0.05, 0.25)
    p2 <- base_risk
    odds2 <- p2 / (1 - p2)
    odds1 <- true_or * odds2
    p1 <- odds1 / (1 + odds1)

    ai <- rbinom(k, n1, p1)
    ci <- rbinom(k, n2, p2)

    # introduce some Not Estimated rows
    ne_idx <- sample(seq_len(k), size = max(1, floor(k * 0.15)))
    ai[ne_idx] <- 0
    ci[ne_idx] <- 0
    n1[ne_idx] <- 0
    n2[ne_idx] <- 0

    cc <- 0.5
    estimable <- !(n1 == 0 & n2 == 0)

    a <- ai + cc
    b <- pmax(n1 - ai, 0) + cc
    c_ <- ci + cc
    d <- pmax(n2 - ci, 0) + cc

    log_or <- rep(NA_real_, k)
    se <- rep(NA_real_, k)

    log_or[estimable] <- log((a[estimable] / b[estimable]) /
      (c_[estimable] / d[estimable]))

    se[estimable] <- sqrt(
      1 / a[estimable] + 1 / b[estimable] +
        1 / c_[estimable] + 1 / d[estimable]
    )

    est <- exp(log_or)
    lb <- exp(log_or - 1.96 * se)
    ub <- exp(log_or + 1.96 * se)

    w_raw <- ifelse(estimable, 1 / (se^2), NA_real_)

    rb <- function() sample(c("+", "-", "?"), k, replace = TRUE)
    rb_mat <- replicate(6, rb())
    colnames(rb_mat) <- paste0("rb.", letters[1:6])

    data.frame(
      group = group,
      type = "study",
      author = author,
      ai = ai, n1i = n1,
      ci = ci, n2i = n2,
      weights_raw = w_raw,
      weights = NA_real_,
      orci = ifelse(!estimable,
        "Not Estimated",
        sprintf("%.2f [%.2f, %.2f]", est, lb, ub)
      ),
      rb_mat,
      est = est, lb = lb, ub = ub,
      cicol = "",
      stringsAsFactors = FALSE
    )
  }

  dt <- rbind(
    mk_studies(inventors, "Inventors"),
    mk_studies(mello, "Melodifestival Winners")
  )

  # normalize weights to 100 across all estimable studies
  s <- sum(dt$weights_raw, na.rm = TRUE)
  if (s > 0) dt$weights <- 100 * dt$weights_raw / s

  mk_subtotal <- function(g) {
    x <- dt[dt$group == g & !is.na(dt$est), ]
    w <- x$weights_raw
    mu <- sum(w * log(x$est)) / sum(w)
    se_mu <- sqrt(1 / sum(w))

    est <- exp(mu)
    lb <- exp(mu - 1.96 * se_mu)
    ub <- exp(mu + 1.96 * se_mu)

    data.frame(
      group = g, type = "subtotal", author = paste0("Subtotal (", g, ")"),
      ai = NA, n1i = sum(dt$n1i[dt$group == g], na.rm = TRUE),
      ci = NA, n2i = sum(dt$n2i[dt$group == g], na.rm = TRUE),
      weights_raw = NA_real_,
      weights = sum(dt$weights[dt$group == g], na.rm = TRUE),
      orci = sprintf("%.2f [%.2f, %.2f]", est, lb, ub),
      rb.a = "", rb.b = "", rb.c = "", rb.d = "", rb.e = "", rb.f = "",
      est = est, lb = lb, ub = ub, cicol = "",
      stringsAsFactors = FALSE
    )
  }

  sub_inv <- mk_subtotal("Inventors")
  sub_mel <- mk_subtotal("Melodifestival Winners")

  # total
  x <- dt[!is.na(dt$est), ]
  w <- x$weights_raw
  mu <- sum(w * log(x$est)) / sum(w)
  se_mu <- sqrt(1 / sum(w))

  total <- data.frame(
    group = "Total", type = "total", author = "Total",
    ai = NA, n1i = sum(dt$n1i, na.rm = TRUE),
    ci = NA, n2i = sum(dt$n2i, na.rm = TRUE),
    weights_raw = NA_real_, weights = 100,
    orci = sprintf(
      "%.2f [%.2f, %.2f]",
      exp(mu), exp(mu - 1.96 * se_mu), exp(mu + 1.96 * se_mu)
    ),
    rb.a = "", rb.b = "", rb.c = "", rb.d = "", rb.e = "", rb.f = "",
    est = exp(mu), lb = exp(mu - 1.96 * se_mu), ub = exp(mu + 1.96 * se_mu),
    cicol = "",
    stringsAsFactors = FALSE
  )

  header <- function(g) {
    data.frame(
      group = g, type = "header", author = g,
      ai = NA, n1i = NA, ci = NA, n2i = NA,
      weights_raw = NA_real_, weights = NA_real_,
      orci = "",
      rb.a = "", rb.b = "", rb.c = "", rb.d = "", rb.e = "", rb.f = "",
      est = NA_real_, lb = NA_real_, ub = NA_real_, cicol = "",
      stringsAsFactors = FALSE
    )
  }

  out <- rbind(
    header("Inventors"),
    dt[dt$group == "Inventors", ],
    sub_inv,
    header("Melodifestival Winners"),
    dt[dt$group == "Melodifestival Winners", ],
    sub_mel,
    total
  )

  # final column order similar to forestploter metadata
  out <- out[, c(
    "author", "ai", "n1i", "ci", "n2i", "weights", "orci",
    "rb.a", "rb.b", "rb.c", "rb.d", "rb.e", "rb.f",
    "est", "lb", "ub", "cicol", "group", "type"
  )]

  out
}
