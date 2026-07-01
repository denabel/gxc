# tests/testthat/test-catalogue.R
#
# Smoke tests: checks that every indicator in every catalogue can be
# downloaded and returns non-empty, distinct values.
#
# Requires fixtures to be built first via setup-fixtures.R.

local_key(service = "ecmwfr")
local_key(service = "dwd")

pt <- test_pts(seq = FALSE)

for (cat in names(allowed_indicators_by_catalogue)) {
  fn         <- if (grepl("daily", cat)) "link_daily"   else "link_monthly"
  service    <- if (grepl("dwd",   cat)) "dwd"          else "ecmwfr"
  indicators <- allowed_indicators_by_catalogue[[cat]]

  for (ind in indicators) {
    test_that(sprintf("'%s' / '%s' returns data", cat, ind), {
      skip_on_cran()
      options(".__gxc_fail_on_request__." = TRUE)
      on.exit(options(".__gxc_fail_on_request__." = NULL), add = TRUE)

      cache <- test_cache()
      local_test_index(cache, service = service)

      result <- do.call(fn, list(pt, indicator = ind, catalogue = cat,
                                 cache = TRUE, path = cache, verbose = FALSE))

      expect_s3_class(result, "sf")
      expect_true(nrow(result) > 0)
      expect_false(all(is.na(result$.study)))
      expect_equal(result$.indicator[[1]], ind)
    })
  }

  if (length(indicators) > 1) {
    test_that(sprintf("'%s' returns distinct values across indicators", cat), {
      skip_on_cran()
      options(".__gxc_fail_on_request__." = TRUE)
      on.exit(options(".__gxc_fail_on_request__." = NULL), add = TRUE)

      cache <- test_cache()
      local_test_index(cache, service = service)

      study_values <- vapply(indicators, function(ind) {
        res <- do.call(fn, list(pt, indicator = ind, catalogue = cat,
                                cache = TRUE, path = cache, verbose = FALSE))
        res$.study[[1]]
      }, numeric(1))

      expect_gt(length(unique(na.omit(study_values))), 1)
    })
  }
}
