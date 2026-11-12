# Tests never download: a missing fixture fails immediately instead of
# waiting in the CDS request queue
options(".__gxc_fail_on_request__." = TRUE)
withr::defer(options(".__gxc_fail_on_request__." = NULL), teardown_env())
