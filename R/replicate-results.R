#' Replicate Results
#'
#' Replicates the results by running the package demo. The results are saved
#' to a folder results in the working directory.
#'
#' By default (\code{mode = "debug"}) the analyses are run in debug mode
#' which quickly generates non-convergent results (this is useful for testing
#' everything works before commiting to the full analyses).
#'
#' To run the analyses with sufficient iterations for convergence
#' with an R-hat value of 1.1. set \code{mode = "report"}. To run the
#' analyses with the same settings as used for the paper set
#' \code{mode = "paper"}.
#'
#' @param mode A string specifying the mode for the analyses.
#' @param res A count indicating the nominal resolution in ppi.
#' @param parallel A flag indicating whether to run the chains in parallel.
#' @export
replicate_results <- function(mode = "debug", res = 150L, parallel = TRUE) {

  assert_that(is.string(mode))
  assert_that(is.count(res))
  assert_that(is.flag(parallel))

  requireNamespace("foreach")
  requireNamespace("doParallel")

  if (!mode %in% c("debug", "report", "paper"))
    stop("mode must be 'debug', 'report' or 'paper'")

  th <- ggplot2::theme_get()
  on.exit(ggplot2::theme_set(th), add = TRUE)
  ggplot2::theme_set(ggplot2::theme_bw(base_size = 8))
  ggplot2::theme_update(panel.grid = ggplot2::element_blank())

  op <- jaggernaut::opts_jagr(mode = mode)
  on.exit(jaggernaut::opts_jagr(op), add = TRUE)

  if (parallel) {
    nworkers <- foreach::getDoParWorkers()
    if (nworkers < jaggernaut::opts_jagr()$nchains) {
      on.exit(doParallel::stopImplicitCluster(), add = TRUE)
      if (nworkers > 1) {
        on.exit(doParallel::registerDoParallel(nworkers), add = TRUE)
      }
      doParallel::stopImplicitCluster()
      doParallel::registerDoParallel(jaggernaut::opts_jagr()$nchains)
    }
  }
  jaggernaut::opts_jagr(parallel = parallel)

  res <- options(res = res)
  on.exit(options(res = res$res), add = TRUE)

  utils::demo("ferox", ask = FALSE)

  saveRDS(mode, "results/mode.rds")
  invisible()
}
