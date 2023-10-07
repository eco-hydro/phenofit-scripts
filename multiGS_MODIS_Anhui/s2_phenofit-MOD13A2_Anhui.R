source("main_pkgs.R", encoding = "UTF-8")
infile = "data-raw/MOD09A1_anhui9_EVI_8day_2015.rda"
load(infile)
data$dates = data$date
n = nrow(data$VI)

## Parameter setting
# If provided, LCs should have the same length as `data$VI`
LCs <- NULL
nptperyear <- 46
opt_old <- phenofit::get_options()

# Different parameters for different land covers (LCs) refered by TIMESAT
# Name of `list_options` corresponds to `LCs` code
list_options <- list(
  "1" = list(),
  "2" = list(),
  default = list(
    wFUN = wTSM, wmin = 0.2,
    verbose = FALSE,
    season = list(
      # rFUN = "smooth_wWHIT", lambda = 0.5,
      rFUN = "smooth_wHANTS", nf = 6,
      maxExtendMonth = 12, r_min = 0.0
    ),
    # fine fitting parameters
    fitting = list(
      nextend = 1, minExtendMonth = 0, maxExtendMonth = 0.5,
      methods = c("AG", "Zhang", "Beck", "Elmore", "Gu")[3], # ,"klos",, 'Gu'
      iters = 1,
      minPercValid = 0
    )
  )
)
do.call(phenofit::set_options, list_options$default)


#' main_phenofit
#' 
#' @return 
#' - `t`: info of running time
#' - `res`: extracted phenological metrics
main_phenofit <- function(n, n_run = 10, step = 1000, outfile = NULL, .parallel = FALSE) {
  `%dof%` <- ifelse(.parallel, foreach::`%dopar%`, foreach::`%do%`)
  inds <- 1:n

  t <- system.time({
    res <- foreach(i = inds %>% set_names(., .), icount(n_run)) %dof% {
      runningId(i, step)

      if (!is.null(LCs)) {
        LC <- LCs[i]
        do.call(set_options, list_options[[LC]])
      }
      tryCatch({
        d <- get_input(i, data)
        r <- phenofit_point(d, plot = FALSE, period = c(2015, 2020))$pheno
      }, error = function(e) {
        message(sprintf('%s', e$message))
      })
    }
  })
  if (is.null(outfile)) {
    res
  } else save(res, t, file = outfile)
}


library(parallel)
ncluster = 10
InitCluster(ncluster, kill = FALSE)
cl = getOption("cl")

clusterExport(cl, c("data", "LCs", "n", "list_options"))

.tmp = clusterEvalQ(cl, {
    library(Ipaper)
    library(phenofit)
    source("main_pkgs.R", encoding = "UTF-8")

    do.call(phenofit::set_options, list_options$default)
})

res = main_phenofit(n, n_run=n, step=100, .parallel = TRUE)
str(res[1:3], 2) # only show the first three

mkdir("OUTPUT")
save(res, file = "OUTPUT/pheno_Anhui_MODIS_V0.1.rda")

