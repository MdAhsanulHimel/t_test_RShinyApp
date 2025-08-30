library(ggplot2)

t_test_flex <- function(
    x,
    y = NULL,
    side = c("both","left","right"),
    sig  = c(0.1, 0.05, 0.01),
    plot = TRUE,
    paired = FALSE,
    var.equal = FALSE,
    mu0 = 0
) {
  side <- match.arg(side)
  sig  <- match.arg(as.character(sig), choices = c("0.1","0.05","0.01"))
  sig  <- as.numeric(sig)
  
  # Clean inputs
  x <- as.numeric(x); x <- x[is.finite(x)]
  if (!is.null(y)) { y <- as.numeric(y); y <- y[is.finite(y)] }
  
  # Determine test type
  test_type <- if (is.null(y)) {
    "one-sample"
  } else if (paired) {
    "paired"
  } else {
    "two-sample"
  }
  
  # Helpers ---------------------------------------------------------------
  tcrit_value <- function(df, side, alpha) {
    switch(side,
           "both"  = qt(1 - alpha/2, df),
           "right" = qt(1 - alpha, df),
           "left"  = qt(alpha, df))
  }
  decide <- function(tval, tcrit, side) {
    switch(side,
           "both"  = abs(tval) >= tcrit,
           "right" = tval >= tcrit,
           "left"  = tval <= tcrit)
  }
  pval_fun <- function(tval, df, side) {
    switch(side,
           "both"  = 2 * min(pt(tval, df), 1 - pt(tval, df)),
           "right" = 1 - pt(tval, df),
           "left"  = pt(tval, df))
  }
  fmt <- function(x, digits=6) ifelse(is.na(x), "NA", formatC(x, digits=digits, format="fg"))
  
  # Compute stats ---------------------------------------------------------
  out <- list()
  if (test_type == "one-sample") {
    n1 <- length(x); m1 <- mean(x); s1 <- stats::sd(x)
    tval <- (m1 - mu0) / (s1 / sqrt(n1))
    df <- n1 - 1
    tcrit <- tcrit_value(df, side, sig)
    pval <- pval_fun(tval, df, side)
    
    # Hypotheses text
    if (side == "right") {
      H0 <- sprintf("H0: μ ≤ %.6g", mu0)
      H1 <- sprintf("H1: μ > %.6g", mu0)
    } else if (side == "left") {
      H0 <- sprintf("H0: μ ≥ %.6g", mu0)
      H1 <- sprintf("H1: μ < %.6g", mu0)
    } else {
      H0 <- sprintf("H0: μ = %.6g", mu0)
      H1 <- sprintf("H1: μ ≠ %.6g", mu0)
    }
    
    reject <- decide(tval, tcrit, side)
    
    out <- within(list(), {
      hypothesis <- paste("Hypothesis:", H0, "|", H1)
      mean1 <- m1; n1 <- n1; s1 <- s1
      df <- df
      t_calculated <- tval
      t_critical <- tcrit
      p_value <- pval
      decision <- if (reject) "Reject the null hypothesis." else "Do not reject the null hypothesis."
    })
    
  } else if (test_type == "paired") {
    if (length(x) != length(y)) stop("For paired t-test, x and y must have the same length.")
    d <- x - y
    n <- length(d); md <- mean(d); sd_d <- stats::sd(d)
    tval <- (md - mu0) / (sd_d / sqrt(n))
    df <- n - 1
    tcrit <- tcrit_value(df, side, sig)
    pval <- pval_fun(tval, df, side)
    
    if (side == "right") {
      H0 <- sprintf("H0: μ_d ≤ %.6g", mu0)
      H1 <- sprintf("H1: μ_d > %.6g", mu0)
    } else if (side == "left") {
      H0 <- sprintf("H0: μ_d ≥ %.6g", mu0)
      H1 <- sprintf("H1: μ_d < %.6g", mu0)
    } else {
      H0 <- sprintf("H0: μ_d = %.6g", mu0)
      H1 <- sprintf("H1: μ_d ≠ %.6g", mu0)
    }
    
    reject <- decide(tval, tcrit, side)
    
    out <- within(list(), {
      hypothesis <- paste("Hypothesis:", H0, "|", H1)
      mean1 <- mean(x); mean2 <- mean(y)
      n1 <- length(x); n2 <- length(y)
      s1 <- stats::sd(x); s2 <- stats::sd(y)
      df <- df
      t_calculated <- tval
      t_critical <- tcrit
      p_value <- pval
      decision <- if (reject) "Reject the null hypothesis." else "Do not reject the null hypothesis."
    })
    
  } else {  
    # two-sample
    n1 <- length(x); n2 <- length(y)
    m1 <- mean(x);  m2 <- mean(y)
    s1 <- stats::sd(x); s2 <- stats::sd(y)
    
    if (var.equal) {
      sp2 <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
      se <- sqrt(sp2 * (1/n1 + 1/n2))
      df <- n1 + n2 - 2
      tval <- (m1 - m2) / se
      tcrit <- tcrit_value(df, side, sig)
      pooled_variance <- sp2
    } else {
      se2 <- s1^2 / n1 + s2^2 / n2
      tval <- (m1 - m2) / sqrt(se2)
      df <- se2^2 / ( (s1^2/n1)^2/(n1 - 1) + (s2^2/n2)^2/(n2 - 1) )
      tcrit <- tcrit_value(df, side, sig)
      pooled_variance <- NA
    }
    pval <- pval_fun(tval, df, side)
    
    if (side == "right") {
      H0 <- "H0: μ₁ ≤ μ₂"; H1 <- "H1: μ₁ > μ₂"
    } else if (side == "left") {
      H0 <- "H0: μ₁ ≥ μ₂"; H1 <- "H1: μ₁ < μ₂"
    } else {
      H0 <- "H0: μ₁ = μ₂"; H1 <- "H1: μ₁ ≠ μ₂"
    }
    
    reject <- decide(tval, tcrit, side)
    
    out <- within(list(), {
      hypothesis <- paste("Hypothesis:", H0, "|", H1)
      mean1 <- m1; mean2 <- m2
      n1 <- n1; n2 <- n2
      s1 <- s1; s2 <- s2
      pooled_variance <- pooled_variance
      df <- df
      t_calculated <- tval
      t_critical <- tcrit
      p_value <- pval
      decision <- if (reject) "Reject the null hypothesis." else "Do not reject the null hypothesis."
    })
  }
  
  # Build report ----------------------------------------------------------
  lines <- c(
    out$hypothesis,
    if (!is.null(out$mean1)) sprintf("Mean of first group = %s", fmt(out$mean1)) else NULL,
    if (!is.null(out$mean2)) sprintf("Mean of second group = %s", fmt(out$mean2)) else NULL,
    if (!is.null(out$n1)) sprintf("n1 = %s", out$n1) else NULL,
    if (!is.null(out$n2)) sprintf("n2 = %s", out$n2) else NULL,
    if (!is.null(out$s1)) sprintf("s1^2 = %s", fmt(out$s1^2)) else NULL,
    if (!is.null(out$s2)) sprintf("s2^2 = %s", fmt(out$s2^2)) else NULL,
    if (!is.null(out$pooled_variance) && !is.na(out$pooled_variance)) sprintf("Pooled variance = %s", fmt(out$pooled_variance)) else NULL,
    sprintf("df = %s", fmt(out$df)),
    sprintf("Calculated t statistic, t = %s", fmt(out$t_calculated)),
    sprintf("Critical t statistic, t = %s%s",
            fmt(out$t_critical),
            if (side == "both") " (two-sided; use ±t*)" else ""),
    sprintf("P-value, p = %s", fmt(out$p_value)),
    paste0("Decision: ", out$decision)
  )
  report <- paste(lines, collapse = "\n")
  out$report <- report
  
  # Plotting the t distribution ----------------------------------------------
  if (plot) {
    dfp <- out$df
    xmax <- max(5, abs(out$t_calculated) + 3, abs(out$t_critical) + 3)
    xs <- seq(-xmax, xmax, length.out = 2000)
    dat <- data.frame(x = xs, y = dt(xs, df = dfp))
    
    if (side == "both") {
      tcrit <- out$t_critical
      dat_left  <- subset(dat, x <= -tcrit)
      dat_right <- subset(dat, x >=  tcrit)
    } else if (side == "right") {
      tcrit <- out$t_critical
      dat_tail <- subset(dat, x >= tcrit)
    } else {
      tcrit <- out$t_critical
      dat_tail <- subset(dat, x <= tcrit)
    }
    
    p <- ggplot(dat, aes(x = x, y = y)) + geom_line()
    if (side == "both") {
      p <- p +
        geom_area(data = dat_left, aes(y = y), alpha = 0.25, fill = "blue") +
        geom_area(data = dat_right, aes(y = y), alpha = 0.25, fill = "blue") +
        geom_vline(xintercept = c(-tcrit, tcrit), linetype = "dotted")
    } else if (side == "right") {
      p <- p +
        geom_area(data = dat_tail, aes(y = y), alpha = 0.25, fill = "blue") +
        geom_vline(xintercept = tcrit, linetype = "dotted")
    } else {
      p <- p +
        geom_area(data = dat_tail, aes(y = y), alpha = 0.25, fill = "blue") +
        geom_vline(xintercept = tcrit, linetype = "dotted")
    }
    
    p <- p +
      geom_vline(xintercept = out$t_calculated, linetype = "dashed", linewidth = 0.7) +
      labs(
        title = sprintf("t distribution (df = %.2f) · side = %s · α = %.3f", dfp, side, sig),
        x = "t", y = "Density",
        subtitle = sprintf(
          "Observed t = %.3f, Critical t = %s, p = %s",
          out$t_calculated, fmt(out$t_critical), 
          fmt(ifelse(out$p_value <= 0.001,"<0.001", round(out$p_value, 3)))
        )
      ) +
      theme_minimal()
    print(p)
  }
  
  cat(report, sep = "\n")
  invisible(out)
}


# test code
# t_test_flex(c(110, 112, 111, 109, 113, 115, 112, 114, 110, 111, 113, 112, 114, 109, 110),
#             c(116, 118, 117, 115, 119, 120, 118, 117, 116, 119, 118, 117, 121, 116, 120),
#             side = "left", var.equal = F, sig = 0.1)
# 
