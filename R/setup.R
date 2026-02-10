.make_lnk <- function(.data, ...) {
  args <- list(...)
  for (att in names(args)) {
    attr(.data, att) <- args[[att]]
  }
  class(.data) <- union("gxc_lnk", class(.data))
  .data
}


.check_lnk <- function(x, stage = NULL, name = obj_name(x)) {
  .check_class(x, "gxc_lnk", name = name)

  if (!is.null(stage)) {
    required <- switch(
      stage,
      baseline = c("indicator", "catalogue", "statistic", "time_zone")
    )

    has_req <- required %in% names(attributes(x))
    if (!all(has_req)) {
      cli::cli_abort(c(
        "Cannot add a baseline when no EOD indicator is present.",
        "i" = "Before adding a baseline, first run `link_eod()`."
      ))
    }
  }
}
