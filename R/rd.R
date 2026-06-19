rd_indicators <- function(fun) {
  switch(
    fun,
    link_daily = {
      land    <- allowed_indicators_by_catalogue$`derived-era5-land-daily-statistics`
      slevels <- allowed_indicators_by_catalogue$`derived-era5-single-levels-daily-statistics`

      # Pad shorter vector with empty strings
      max_len <- max(length(land), length(slevels))
      land    <- c(sprintf("\\code{%s}", land),    rep("", max_len - length(land)))
      slevels <- c(sprintf("\\code{%s}", slevels), rep("", max_len - length(slevels)))

      df <- data.frame(
        Land = land,
        "Single Levels" = slevels,
        check.names = FALSE
      )
      rd_table(df)
    },
    link_monthly = {
      land    <- allowed_indicators_by_catalogue$`reanalysis-era5-land-monthly-means`
      slevels <- allowed_indicators_by_catalogue$`reanalysis-era5-single-levels-monthly-means`

      max_len <- max(length(land), length(slevels))
      land    <- c(sprintf("\\code{%s}", land),    rep("", max_len - length(land)))
      slevels <- c(sprintf("\\code{%s}", slevels), rep("", max_len - length(slevels)))

      df <- data.frame(
        Land = land,
        "Single Levels" = slevels,
        check.names = FALSE
      )
      rd_table(df)
    }
  )
}


rd_list <- function(x, code = FALSE) {
  x <- lapply(x, function(item) sprintf(" \\item{%s}", item))
  sprintf("\\itemize{\n%s\n}", paste(x, collapse = "\n"))
}


rd_table <- function(df, header = TRUE, ...) {
  stopifnot(is.data.frame(df))
  align <- function(x) if (is.numeric(x))
    "r"
  else "l"
  col_align <- vapply(df, align, character(1))
  cols <- lapply(df, format, ...)
  if (header)
    cols <- lapply(
      names(cols),
      function(x) c(sprintf("\\strong{%s}", x), cols[[x]])
    )
  cols[[1]] <- sprintf("  %s", cols[[1]])
  contents <- do.call("paste", c(cols, list(
    sep = " \\tab ",
    collapse = "\\cr\n  "
  )))

  paste(
    "\\tabular{",
    paste(col_align, collapse = ""),
    "}{\n  ",
    contents,
    "\n}\n",
    sep = ""
  )
}
