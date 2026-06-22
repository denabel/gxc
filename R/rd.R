rd_indicators <- function(fun) {
  switch(
    fun,
    link_daily = {
      land    <- allowed_indicators_by_catalogue$`derived-era5-land-daily-statistics`
      slevels <- allowed_indicators_by_catalogue$`derived-era5-single-levels-daily-statistics`
      dwd     <- allowed_indicators_by_catalogue$`dwd-hyras-daily`

      max_len <- max(length(land), length(slevels), length(dwd))
      land    <- c(sprintf("\\code{%s}", land),    rep("", max_len - length(land)))
      slevels <- c(sprintf("\\code{%s}", slevels), rep("", max_len - length(slevels)))
      dwd     <- c(sprintf("\\code{%s}", dwd),     rep("", max_len - length(dwd)))

      df <- data.frame(
        "ERA5 Land"      = land,
        "ERA5 Single Levels" = slevels,
        "DWD HYRAS"      = dwd,
        check.names      = FALSE
      )
      rd_table(df)
    },
    link_monthly = {
      land    <- allowed_indicators_by_catalogue$`reanalysis-era5-land-monthly-means`
      slevels <- allowed_indicators_by_catalogue$`reanalysis-era5-single-levels-monthly-means`
      dwd     <- allowed_indicators_by_catalogue$`dwd-monthly`

      max_len <- max(length(land), length(slevels), length(dwd))
      land    <- c(sprintf("\\code{%s}", land),    rep("", max_len - length(land)))
      slevels <- c(sprintf("\\code{%s}", slevels), rep("", max_len - length(slevels)))
      dwd     <- c(sprintf("\\code{%s}", dwd),     rep("", max_len - length(dwd)))

      df <- data.frame(
        "ERA5 Land"          = land,
        "ERA5 Single Levels" = slevels,
        "DWD"                = dwd,
        check.names          = FALSE
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
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  cols <- lapply(df, format, ...)
  if (header)
    cols <- lapply(
      names(cols),
      function(x) c(sprintf("\\strong{%s}", x), cols[[x]])
    )
  cols[[1]] <- sprintf("  %s", cols[[1]])
  contents <- do.call("paste", c(cols, list(
    sep      = " \\tab ",
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
