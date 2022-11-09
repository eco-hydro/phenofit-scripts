library(sp)
library(terra)


prj84 = sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
df2sp <- function (d, formula = ~lon + lat, prj) {
    if (missing(prj))
        prj <- prj84
    coordinates(d) <- formula
    proj4string(d) <- prj
    return(d)
}

# 注意：tiff文件是否有日期
#' read sentinel2 tiff file
read_rast <- function(file, dates = NULL) {
    r = rast(file)
    # guess date from band names
    # Note that might be failed to guess date
    if (is.null(dates)) {
        dates = names(r) %>% gsub("_", "", .) %>% substr(1, 8) %>% as.Date("%Y%m%d")
    }
    terra::time(r) = dates
    names(r) = dates
    r
}

#' convert `rast` to 2d data, with the dimension of [`ngrid`, `ntime`].
#'
#' @param r rast object
#' @param bareval the VI value of bare land. Pixels with mutli-annual mean of VI
#' less than `bareval` will be eliminated.
#'
#' @return
#' - `mat`: matrix (with the dimension of [`ngrid`, `ntime`]), Vegetation index (VI).
#' - `x_mean`: vector (with the length of `ngrid`), mutli-annual mean of VI.
#' - `dates`: Date vector (with the length of `ntime`), corresponding dates of VI.
#' - `d_coords`: A data.table, with the coordinates of each pixel.
#' - `I_grid`: Integer vector, pixels with `x_mean` greater than 0.1. If not, this pixel will be
#'    eleminated in phenology extraction.
#' - `grid`: SpatiaPixelsDataFrame object.
#'
#' @export
rast2mat <- function(r, backval = 0.1) {
    d_coord = rast_coord(r[[1]])
    arr = rast_array(r)
    mat = array_3dTo2d(arr)

    range = ext(r) %>% as.vector() # [xmin, xmax, ymin, ymax]
    cellsize = res(r)
    grid <- make_grid(range, cellsize)

    x_mean = mat %>% rowMeans(na.rm = TRUE)
    I_grid = which(!(x_mean <= backval | is.na(x_mean)))
    d_coord = grid@coords %>% as.data.table() %>% set_colnames(c("lon", "lat")) %>% cbind(I = 1:nrow(.), .)
    listk(mat, dates = terra::time(r), x_mean,
        d_coord, I_grid, grid, r = r)
}
