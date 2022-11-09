
``` r
source("main_pkgs.R", encoding = "UTF-8")
# terra 1.6.17
# 
# Attaching package: 'terra'
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

infile <- "data/MOD13A2_Henan_2015_2020.rda"

if (!file.exists(infile)) {
  indir <- path.mnt("I:/Research/phenology/gee_whittaker/data-raw/NorthChina")
  file_vi <- glue("{indir}/MOD13A2_Henan_2016_2020_EVI.tif")
  file_qc <- glue("{indir}/MOD13A2_Henan_2016_2020_SummaryQA.tif")
  file_doy <- glue("{indir}/MOD13A2_Henan_2016_2020_DayOfYear.tif")

  # dates = terra::rast(infile) %>% names() %>%
  #     substr(1, 10) %>% as.Date("%Y_%m_%d")
  dates <- modis_date("2015-01-01", "2020-12-31", 16) # make sure dates are in line with that in tiff
  l <- read_rast(file_vi, dates) %>% rast2mat()
  l_qc <- read_rast(file_qc) %>% rast2mat()
  l_doy <- read_rast(file_doy) %>% rast2mat()

  I_grid <- l$I_grid
  d_coord <- l$d_coord[I_grid, ]
  data <- listk(
    VI = l$mat[I_grid, ] / 1e4,
    QC = l_qc$mat[I_grid, ],
    DOY = l_doy$mat[I_grid, ],
    dates = l$dates, qcFUN = qc_summary
  )
  save(data, d_coord, file = infile)
} else {
  load(infile)
  n <- nrow(data$QC)
}

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

``` r
# the following function used many global variables, please make sure run the above
# script before

#' main_phenofit
#' 
#' @return 
#' - `t`: info of running time
#' - `res`: extracted phenological metrics
main_phenofit <- function(n_run = 10, step = 1000, outfile = NULL, .parallel = FALSE) {
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
res = main_phenofit(10, 1)
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
#   ..$ TRS5.sos  : num [1:6] 114 101 113 102 113 121
#   ..$ TRS5.eos  : num [1:6] 272 283 277 282 269 271
#   ..$ DER.sos   : num [1:6] 114 102 113 103 113 121
#   ..$ DER.pos   : num [1:6] 141 173 158 175 166 182
#   ..$ DER.eos   : num [1:6] 273 283 279 281 269 271
#   ..$ UD        : num [1:6] 108 79 100 76 94 99
#   ..$ SD        : num [1:6] 119 124 126 129 132 142
#   ..$ DD        : num [1:6] 245 248 241 242 230 239
#   ..$ RD        : num [1:6] 304 321 321 325 314 306
#   ..$ Greenup   : num [1:6] 103 75 96 72 90 95
#   ..$ Maturity  : num [1:6] 125 128 130 134 136 147
#   ..$ Senescence: num [1:6] NA 239 230 232 218 231
#   ..$ Dormancy  : num [1:6] NA 326 327 331 320 312
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 2:Classes 'data.table' and 'data.frame':   7 obs. of  16 variables:
#   ..$ meth      : Factor w/ 1 level "Beck": 1 1 1 1 1 1 1
#   ..$ flag      : chr [1:7] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:7], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:7] 114 108 117 60 119 119 125
#   ..$ TRS5.eos  : num [1:7] 280 283 278 82 272 272 276
#   ..$ DER.sos   : num [1:7] 113 108 117 62 104 119 125
#   ..$ DER.pos   : num [1:7] 158 190 162 80 187 177 201
#   ..$ DER.eos   : num [1:7] 280 284 278 82 272 271 276
#   ..$ UD        : num [1:7] 106 76 105 49 99 96 102
#   ..$ SD        : num [1:7] 121 140 129 72 137 142 147
#   ..$ DD        : num [1:7] 258 248 244 79 240 232 254
#   ..$ RD        : num [1:7] 303 323 316 85 306 317 298
#   ..$ Greenup   : num [1:7] 101 70 101 44 NA 92 98
#   ..$ Maturity  : num [1:7] 126 146 134 79 143 147 151
#   ..$ Senescence: num [1:7] 253 238 235 NA 232 221 250
#   ..$ Dormancy  : num [1:7] 306 330 321 NA 312 323 301
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 3:Classes 'data.table' and 'data.frame':   7 obs. of  16 variables:
#   ..$ meth      : Factor w/ 1 level "Beck": 1 1 1 1 1 1 1
#   ..$ flag      : chr [1:7] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:7], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:7] 116 106 113 108 124 116 NA
#   ..$ TRS5.eos  : num [1:7] 281 279 291 272 268 208 NA
#   ..$ DER.sos   : num [1:7] 116 105 113 110 125 115 NA
#   ..$ DER.pos   : num [1:7] 150 193 172 187 178 172 NA
#   ..$ DER.eos   : num [1:7] 281 278 292 272 267 217 NA
#   ..$ UD        : num [1:7] 108 76 98 72 102 96 NA
#   ..$ SD        : num [1:7] 123 134 128 145 146 135 NA
#   ..$ DD        : num [1:7] 250 251 260 234 230 201 NA
#   ..$ RD        : num [1:7] 316 308 326 314 311 217 NA
#   ..$ Greenup   : num [1:7] 103 72 94 66 98 90 NA
#   ..$ Maturity  : num [1:7] 128 139 132 152 151 140 NA
#   ..$ Senescence: num [1:7] NA 246 253 224 219 NA NA
#   ..$ Dormancy  : num [1:7] NA 311 330 321 317 202 NA
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

.tmp = clusterEvalQ(cl, {
    library(Ipaper)
    library(phenofit)
    source("main_pkgs.R", encoding = "UTF-8")
})
clusterExport(cl, c("data", "LCs"))
```

``` r
res = main_phenofit(10, 1, .parallel = TRUE)
str(res[1:3], 2) # only show the first three
# List of 3
#  $ 1:Classes 'data.table' and 'data.frame':   24 obs. of  16 variables:
#   ..$ meth      : chr [1:24] "AG" "AG" "AG" "AG" ...
#   ..$ flag      : chr [1:24] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:24], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:24] 114 103 114 102 114 121 114 103 114 102 ...
#   ..$ TRS5.eos  : num [1:24] 274 282 273 286 270 270 274 281 272 284 ...
#   ..$ DER.sos   : num [1:24] 113 108 113 108 119 123 114 103 114 102 ...
#   ..$ DER.pos   : num [1:24] 151 138 154 145 145 165 145 180 159 182 ...
#   ..$ DER.eos   : num [1:24] 277 287 273 290 271 272 274 282 272 284 ...
#   ..$ UD        : num [1:24] 105 78 99 74 93 96 106 85 99 73 ...
#   ..$ SD        : num [1:24] 123 127 129 131 135 146 121 122 129 131 ...
#   ..$ DD        : num [1:24] 237 251 226 251 224 232 241 257 231 248 ...
#   ..$ RD        : num [1:24] 314 317 325 326 323 314 310 308 321 324 ...
#   ..$ Greenup   : num [1:24] 99 68 91 62 84 85 101 81 95 68 ...
#   ..$ Maturity  : num [1:24] 128 138 138 145 145 162 127 125 134 136 ...
#   ..$ Senescence: num [1:24] NA 236 163 231 161 180 NA 252 217 238 ...
#   ..$ Dormancy  : num [1:24] NA 330 346 342 345 332 NA 311 327 330 ...
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 2:Classes 'data.table' and 'data.frame':   24 obs. of  16 variables:
#   ..$ meth      : chr [1:24] "AG" "AG" "AG" "AG" ...
#   ..$ flag      : chr [1:24] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:24], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:24] 114 111 205 104 122 125 114 111 117 104 ...
#   ..$ TRS5.eos  : num [1:24] 281 280 271 266 269 278 280 278 274 270 ...
#   ..$ DER.sos   : num [1:24] 113 118 204 107 126 130 113 111 117 106 ...
#   ..$ DER.pos   : num [1:24] 134 154 223 197 160 160 160 188 161 185 ...
#   ..$ DER.eos   : num [1:24] 286 283 264 257 270 282 280 278 274 268 ...
#   ..$ UD        : num [1:24] 109 83 200 55 97 101 105 84 105 61 ...
#   ..$ SD        : num [1:24] 118 141 209 153 147 149 121 139 129 147 ...
#   ..$ DD        : num [1:24] 250 246 239 218 229 252 259 247 240 227 ...
#   ..$ RD        : num [1:24] 317 317 304 316 314 307 303 312 313 316 ...
#   ..$ Greenup   : num [1:24] 103 71 195 34 86 91 100 79 100 53 ...
#   ..$ Maturity  : num [1:24] 125 154 215 182 159 160 126 144 134 157 ...
#   ..$ Senescence: num [1:24] 141 227 230 NA 176 240 254 240 230 215 ...
#   ..$ Dormancy  : num [1:24] 331 333 317 337 333 318 306 316 318 324 ...
#   ..- attr(*, ".internal.selfref")=<externalptr> 
#  $ 3:Classes 'data.table' and 'data.frame':   24 obs. of  16 variables:
#   ..$ meth      : chr [1:24] "AG" "AG" "AG" "AG" ...
#   ..$ flag      : chr [1:24] "2015_1" "2016_1" "2017_1" "2018_1" ...
#   ..$ origin    : Date[1:24], format: "2015-01-01" "2016-01-01" ...
#   ..$ TRS5.sos  : num [1:24] 116 105 112 109 129 112 116 107 113 109 ...
#   ..$ TRS5.eos  : num [1:24] 280 273 292 268 262 275 280 273 289 270 ...
#   ..$ DER.sos   : num [1:24] 115 102 110 108 133 110 116 106 113 110 ...
#   ..$ DER.pos   : num [1:24] 148 212 168 197 168 192 149 199 172 186 ...
#   ..$ DER.eos   : num [1:24] 284 275 296 261 257 276 280 273 290 269 ...
#   ..$ UD        : num [1:24] 108 79 99 73 102 87 108 81 96 72 ...
#   ..$ SD        : num [1:24] 124 132 126 146 155 137 124 132 129 146 ...
#   ..$ DD        : num [1:24] 245 256 258 223 213 244 246 253 256 231 ...
#   ..$ RD        : num [1:24] 320 290 329 315 317 307 318 295 327 313 ...
#   ..$ Greenup   : num [1:24] 103 67 92 57 91 76 103 77 92 66 ...
#   ..$ Maturity  : num [1:24] 129 141 131 168 167 148 129 136 133 154 ...
#   ..$ Senescence: num [1:24] NA 250 241 199 182 226 NA 250 248 221 ...
#   ..$ Dormancy  : num [1:24] NA 297 344 333 340 321 NA 297 332 320 ...
#   ..- attr(*, ".internal.selfref")=<externalptr>
```

#### linux版本

``` r
ncluster <- 4
InitCluster(ncluster, kill = FALSE)

res <- main_phenofit(10, 1, ..parallel = TRUE)
str(res[1:3], 2) # only show the first three
```

> 请自行比较哪个版本的并行更简单！
