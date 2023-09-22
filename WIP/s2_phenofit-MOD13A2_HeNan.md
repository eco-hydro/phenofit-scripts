
``` r
source("main_pkgs.R", encoding = "UTF-8")
# terra 1.7.39
# 
# 载入程辑包：'terra'
# The following object is masked from 'package:data.table':
# 
#     shift
# The following object is masked from 'package:grid':
# 
#     depth
# The following object is masked from 'package:Ipaper':
# 
#     clamp
# The following objects are masked from 'package:magrittr':
# 
#     extract, inset

infile <- "data/MOD13A2_Henan_2015_2020_V2.rda"
load(infile)
data$dates = data$date
n = nrow(data$VI)

## Parameter setting
# If provided, LCs should have the same length as `data$VI`
LCs <- NULL
nptperyear <- 23
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
```

## 运行说明

### (a) 非并行版本

> 注意：**该版本是非并行的版本**

- `runningId`：`step`次循环打印一次进度，请根据你的样本大小设置合适的值。

- `n_run`：只运行前`n_run`个点。先确保程序在测试的站点上可以跑通，然后再运行所有（设置`n_run=n`）。

- `outfile`：如果指定outfile，则保存数据，否则返回`res`

> global variables: `n`

``` r
# the following function used many global variables, please make sure run the above
# script before

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

# outfile <- "OUTPUT/phenofit_V0.3.5_wHANTS_MODIS_Henan/pheno_Henan_MODIS_V6.1.rda"
res = main_phenofit(n, n_run = 10, 1)
# [] running 1 ...
# [] running 2 ...
# [] running 3 ...
# [] running 4 ...
# [] running 5 ...
# [] running 6 ...
# [] running 7 ...
# [] running 8 ...
# [] running 9 ...
# [] running 10 ...
str(res[1:3], 2) # only show the first three
# List of 3
#  $ 1:Classes 'data.table' and 'data.frame':   6 obs. of  16 variables:
#   ..$ meth      : Factor w/ 1 level "Beck": 1 1 1 1 1 1
#   ..$ flag      : chr [1:6] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:6], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:6] 112 132 117 124 128 118
#   ..$ TRS5.eos  : num [1:6] 282 280 277 274 305 286
#   ..$ DER.sos   : num [1:6] 112 133 117 125 128 119
#   ..$ DER.pos   : num [1:6] 178 218 190 181 251 211
#   ..$ DER.eos   : num [1:6] 282 279 278 274 305 286
#   ..$ UD        : num [1:6] 96 90 86 103 85 80
#   ..$ SD        : num [1:6] 128 174 146 146 171 157
#   ..$ DD        : num [1:6] 256 253 243 237 288 258
#   ..$ RD        : num [1:6] 309 309 318 316 323 317
#   ..$ Greenup   : num [1:6] 131 196 165 48 128 185
#   ..$ Maturity  : num [1:6] 171 218 190 181 251 211
#   ..$ Senescence: num [1:6] 195 240 215 204 283 236
#   ..$ Dormancy  : num [1:6] 282 272 278 274 313 287
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 2:Classes 'data.table' and 'data.frame':   6 obs. of  16 variables:
#   ..$ meth      : Factor w/ 1 level "Beck": 1 1 1 1 1 1
#   ..$ flag      : chr [1:6] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:6], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:6] 112 132 117 124 128 118
#   ..$ TRS5.eos  : num [1:6] 282 280 277 274 305 286
#   ..$ DER.sos   : num [1:6] 112 133 117 125 128 119
#   ..$ DER.pos   : num [1:6] 178 218 190 181 251 211
#   ..$ DER.eos   : num [1:6] 282 279 278 274 305 286
#   ..$ UD        : num [1:6] 96 90 86 103 85 80
#   ..$ SD        : num [1:6] 128 174 146 146 171 157
#   ..$ DD        : num [1:6] 256 253 243 237 288 258
#   ..$ RD        : num [1:6] 309 309 318 316 323 317
#   ..$ Greenup   : num [1:6] 131 196 165 48 128 185
#   ..$ Maturity  : num [1:6] 171 218 190 181 251 211
#   ..$ Senescence: num [1:6] 195 240 215 204 283 236
#   ..$ Dormancy  : num [1:6] 282 272 278 274 313 287
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 3:Classes 'data.table' and 'data.frame':   6 obs. of  16 variables:
#   ..$ meth      : Factor w/ 1 level "Beck": 1 1 1 1 1 1
#   ..$ flag      : chr [1:6] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:6], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:6] 116 128 114 126 127 115
#   ..$ TRS5.eos  : num [1:6] 278 283 275 270 301 289
#   ..$ DER.sos   : num [1:6] 116 130 115 126 127 115
#   ..$ DER.pos   : num [1:6] 181 213 178 183 238 207
#   ..$ DER.eos   : num [1:6] 279 282 275 270 301 290
#   ..$ UD        : num [1:6] 97 86 90 102 88 89
#   ..$ SD        : num [1:6] 135 170 139 149 165 142
#   ..$ DD        : num [1:6] 250 252 238 234 281 267
#   ..$ RD        : num [1:6] 310 315 318 312 322 314
#   ..$ Greenup   : num [1:6] 116 129 28 42 127 115
#   ..$ Maturity  : num [1:6] 181 191 154 183 238 203
#   ..$ Senescence: num [1:6] NA 235 203 205 301 NA
#   ..$ Dormancy  : num [1:6] 279 269 275 270 371 289
#   ..- attr(*, ".internal.selfref")=<externalptr>
```

### (b) 并行版本

#### windows版本

- `ncluster`：并行核数，根据自己的电脑配置设定，让电脑以80%的能力去跑，以免影响办公软件。

> windows并行较为复杂，需要对每个cluster执行加载包、加载函数、加载数据命令，如下：

``` r
library(parallel)
ncluster = 4
InitCluster(ncluster, kill = FALSE)
cl = getOption("cl")

clusterExport(cl, c("data", "LCs", "n", "list_options"))

.tmp = clusterEvalQ(cl, {
    library(Ipaper)
    library(phenofit)
    source("main_pkgs.R", encoding = "UTF-8")
    
    do.call(phenofit::set_options, list_options$default)
})
```

``` r
res = main_phenofit(n, n_run=10, step=1, .parallel = TRUE)
str(res[1:3], 2) # only show the first three
# List of 3
#  $ 1:Classes 'data.table' and 'data.frame':   6 obs. of  16 variables:
#   ..$ meth      : chr [1:6] "Beck" "Beck" "Beck" "Beck" ...
#   ..$ flag      : chr [1:6] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:6], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:6] 112 132 117 124 128 118
#   ..$ TRS5.eos  : num [1:6] 282 280 277 274 305 286
#   ..$ DER.sos   : num [1:6] 112 133 117 125 128 119
#   ..$ DER.pos   : num [1:6] 178 218 190 181 251 211
#   ..$ DER.eos   : num [1:6] 282 279 278 274 305 286
#   ..$ UD        : num [1:6] 96 90 86 103 85 80
#   ..$ SD        : num [1:6] 128 174 146 146 171 157
#   ..$ DD        : num [1:6] 256 253 243 237 288 258
#   ..$ RD        : num [1:6] 309 309 318 316 323 317
#   ..$ Greenup   : num [1:6] 131 196 165 48 128 185
#   ..$ Maturity  : num [1:6] 171 218 190 181 251 211
#   ..$ Senescence: num [1:6] 195 240 215 204 283 236
#   ..$ Dormancy  : num [1:6] 282 272 278 274 313 287
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 2:Classes 'data.table' and 'data.frame':   6 obs. of  16 variables:
#   ..$ meth      : chr [1:6] "Beck" "Beck" "Beck" "Beck" ...
#   ..$ flag      : chr [1:6] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:6], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:6] 112 132 117 124 128 118
#   ..$ TRS5.eos  : num [1:6] 282 280 277 274 305 286
#   ..$ DER.sos   : num [1:6] 112 133 117 125 128 119
#   ..$ DER.pos   : num [1:6] 178 218 190 181 251 211
#   ..$ DER.eos   : num [1:6] 282 279 278 274 305 286
#   ..$ UD        : num [1:6] 96 90 86 103 85 80
#   ..$ SD        : num [1:6] 128 174 146 146 171 157
#   ..$ DD        : num [1:6] 256 253 243 237 288 258
#   ..$ RD        : num [1:6] 309 309 318 316 323 317
#   ..$ Greenup   : num [1:6] 131 196 165 48 128 185
#   ..$ Maturity  : num [1:6] 171 218 190 181 251 211
#   ..$ Senescence: num [1:6] 195 240 215 204 283 236
#   ..$ Dormancy  : num [1:6] 282 272 278 274 313 287
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 3:Classes 'data.table' and 'data.frame':   6 obs. of  16 variables:
#   ..$ meth      : chr [1:6] "Beck" "Beck" "Beck" "Beck" ...
#   ..$ flag      : chr [1:6] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:6], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:6] 116 128 114 126 127 115
#   ..$ TRS5.eos  : num [1:6] 278 283 275 270 301 289
#   ..$ DER.sos   : num [1:6] 116 130 115 126 127 115
#   ..$ DER.pos   : num [1:6] 181 213 178 183 238 207
#   ..$ DER.eos   : num [1:6] 279 282 275 270 301 290
#   ..$ UD        : num [1:6] 97 86 90 102 88 89
#   ..$ SD        : num [1:6] 135 170 139 149 165 142
#   ..$ DD        : num [1:6] 250 252 238 234 281 267
#   ..$ RD        : num [1:6] 310 315 318 312 322 314
#   ..$ Greenup   : num [1:6] 116 129 28 42 127 115
#   ..$ Maturity  : num [1:6] 181 191 154 183 238 203
#   ..$ Senescence: num [1:6] NA 235 203 205 301 NA
#   ..$ Dormancy  : num [1:6] 279 269 275 270 371 289
#   ..- attr(*, ".internal.selfref")=<externalptr>
# mkdir("OUTPUT")
# save(res, "OUTPUT/pheno_Henan_MODIS_V7.1.rda")
```

#### linux版本

``` r
ncluster <- 4
InitCluster(ncluster, kill = FALSE)

res <- main_phenofit(100, 1, ..parallel = TRUE)
str(res[1:3], 2) # only show the first three
```

> 请自行比较哪个版本的并行更简单！
