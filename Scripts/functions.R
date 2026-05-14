#' Multisacle TPI
#'
#' This function compuates multiscale topographic position index, sensu Theobald et al. (2015).
#'
#' @param dem RasterLayer representing elevation, in meters.
#'
#' @return A raster layer of mTPI
#' @references DM Theobald, D Harrison-Atlas, WB Monahan, CM Albano, Ecologically-relevant maps
#'   of landforms and physiographic diversity for climate adaptation planning. PloS One e0143619 (2015)
#' @export
mtpi <- function(dem){
  message("... computing elevational position ...")
  average <- function(x){
    x <- na.omit(x)
    sum(x) / length(x)
  }
  f <- function(dem, radius){
    w <- focalWeight(dem, radius / 111.1, fillNA = T) # 111.1 km/deg latitude (appx)
    w <- w / w
    if(length(w) == 1) return(dem)
    focal(dem, w, average, pad = T)
  }
  tpi <- (dem * 3 - f(dem, .5) - f(dem, .225) - f(dem, .1)) / 3
  setNames(tpi, "mTPI")
}

#' Northness and eastness
#'
#' Calculate northness and eastness based on slope and aspect rasters.
#'
#' @param terr RasterBrick with layers named "slope" and "aspect", e.g. generated with raster::terrain() function.
#'
#' @return A RasterBrick of with northness and eastness variables.
#' @export
northeast <- function(terr){
  message("... computing northness & eastness ...")
  northness <- sin(terr$slope) * cos(terr$aspect)
  eastness <- sin(terr$slope) * sin(terr$aspect)
  ne <- stack(northness, eastness)
  ne <- setNames(ne, c("northness", "eastness"))
  ne
}

#' Windward exposure
#'
#' Calculate windward exposure based on terrain rasters. Wind data are sourced from Kling & Ackerly (2020).
#'
#' @param ne RasterBrick with layers named "northness" and "eastness", e.g. generated with northeast() function.
#' @param terr Raster object with a layer named "slope", e.g. generated with raster::terrain() function.
#'
#' @return A RasterLayer of windward exposure
#' @references MM Kling, DD Ackerly, Global wind patterns and the vulnerability of wind-dispersed species to climate change. Nature Climate Change 10, 868–875 (2020).
#' @export
windex <- function(ne, terr){
  message("... computing windward exposure ...")
  
  wind <- stack(topo_data("wind.tif"))
  wind <- setNames(wind, c("wspeed", "wdir", "waniso", "wu", "wv"))
  wind <- projectRaster(wind, terr[[1]])
  
  anglediff <- function(x, y){
    z <- abs(x - y)
    z[z > pi] <- abs(2*pi - z[z > pi])
    z
  }
  windiff <- pi - anglediff(raster::atan2(wind$wv, wind$wu), # downwind direction,
                            raster::atan2(ne$northness, ne$eastness)) # downslope direction
  windward <- cos(windiff) * sin(terr$slope) * wind$waniso / 0.14896 # 0.14896 is mean FIA anisotropy
  setNames(windward, "windward_exposure")
}

#' Format macroclimate data
#'
#' Prepare a set of macroclimate rasters (biovariables bio12, bio1, bio5, and bio6), resampled and cropped to match resolution of elevation data.
#'
#' @param dem RasterLayer representing elevation, in meters.
#' @param interpolation Interpolation method. Either "bilinear" (default) or "ngb".
#'
#' @return A RasterBrick of macroclimate variables
#' @export
macroclimate <- function(dem, interpolation = "bilinear"){
  message("... preparing macroclimate ...")
  
  # load CHELSA rasters
  f <- list.files(topo_data(), pattern = "CHELSA", full.names = T)
  clim <- stack(f[!grepl("xml", f)])
  names(clim) <- paste0("bio", c(1, 12, 5, 6))
  
  # resample (over an expanded area to avoid edge effects)
  clim <- crop(clim, extent(dem) * 3)
  climate <- crop(resample(clim, dem, method = interpolation), dem)
  climate
}

tensor_splines <- function(x, y, xbounds = NULL, ybounds = NULL,
                           knots = 2, degree = 3){
  k = seq(0, 1, length.out = knots)
  k = k[2:(length(k)-1)]
  if(knots == 2) k <- NULL
  
  if(is.null(xbounds)) xbounds <- range(x)
  if(is.null(ybounds)) xbounds <- range(y)
  B1 <- splines::bs(x, knots = k, Boundary.knots = xbounds, degree=degree, intercept = TRUE)
  B2 <- splines::bs(y, knots = k, Boundary.knots = xbounds, degree=degree, intercept = TRUE)
  B <- rep(1, nrow(B1))
  for(i in 1:ncol(B1)) for(j in 1:ncol(B2)) B <- cbind(B, B1[,i] * B2[,j])
  B <- B[,2:ncol(B)]
  B
}

get_deltas <- function(md, # model metadata
                       g){ # predictor data
  
  message("... computing deltas ...")
  
  log10inc <- function(x){ # change in log10 precip
    y <- rnorm(1)
    z <- rnorm(1)
    ((y*10^(z+x)) - (y*10^z)) / (y*10^z)
  }
  
  vars <- md$vars[[1]]
  mod_vars <- md$mod_vars[[1]]
  topo_vars <- md$topo_vars[[1]]
  subplots <- md$subplots[[1]]
  d <- readRDS(topo_data(basename(md$data_file)))
  
  dmd <- d$md
  dmv <- d$mv
  spid <- d$spid
  dmd$sp_id <- as.integer(factor(dmd$species))
  
  g <- g %>%
    dplyr::select(id, bio1, bio12) %>%
    mutate(bio1 = (bio1 - dmv$bio1_mean) / dmv$bio1_sd,
           bio12 = (log10(bio12+1) - dmv$bio12_mean) / dmv$bio12_sd)
  
  n_bases <- (md$s_knots + md$s_degree - 1) ^ 2
  
  deltas <- read.csv(topo_data("samples_full.csv"), comment.char = "#") %>%
    select(contains("delta")) %>%
    mutate(i = 1:nrow(.)) %>%
    gather(param, value, -i) %>%
    mutate(param = str_remove(param, "delta\\."),
           param = str_replace_all(param, "\\.", "_")) %>%
    separate(param, c("param", "var")) %>%
    rename(clim_var = var, basis = param) %>%
    mutate(basis = as.integer(basis),
           topo_var = topo_vars[ceiling(basis / n_bases)],
           basis = basis %% n_bases,
           basis = ifelse(basis == 0, n_bases, basis),
           clim_var = vars[as.integer(clim_var)]) %>%
    select(-i)
  
  
  
  delt <- function(gs){
    
    # generate basis functions
    z <- tensor_splines(ecdf(dmd[[mod_vars[1]]])(gs[[mod_vars[1]]]),
                        ecdf(dmd[[mod_vars[2]]])(gs[[mod_vars[2]]]),
                        knots = md$s_knots, degree = md$s_degree,
                        xbounds = 0:1, ybounds = 0:1) %>%
      as.data.frame() %>% as_tibble()
    
    # compute deltas from bases
    result <- select(gs, id)
    for(cv in unique(deltas$clim_var)){
      for(tv in unique(deltas$topo_var)){
        dlt <- filter(deltas, topo_var == tv, clim_var == cv)$value
        result[[paste(cv, tv, sep = "_")]] <- apply(z, 1, function(x) sum(x * dlt))
      }
    }
    return(result)
  }
  
  # process in chunks to stay within memory
  nslice <- 10
  gg <- g %>%
    mutate(slice = rep(1:nslice, each = ceiling(nrow(.)/nslice))[1:nrow(.)]) %>%
    split(.$slice) %>%
    map_dfr(delt)
  return(gg)
}

microclimate <- function(md, d, deltas, macro){
  message("... compiling bioclimate estimates ...")
  
  ddd <- readRDS(topo_data(basename(md$data_file)))
  mv <- ddd$mv
  
  # standardize
  d <- d %>%
    select(id, x, y,
           northness, eastness, windward = wind, tpi,
           bio5, bio6, bio12) %>%
    mutate(bio5 = (bio5 - mv$bio5_mean) / mv$bio5_sd,
           bio6 = (bio6 - mv$bio6_mean) / mv$bio6_sd,
           bio12 = (log10(bio12 + 1) - mv$bio12_mean) / mv$bio12_sd,
           northness = (northness - mv$northness_mean) / mv$northness_sd,
           eastness = (eastness - mv$eastness_mean) / mv$eastness_sd,
           windward = (windward - mv$windward_mean) / mv$windward_sd,
           tpi = (tpi - mv$tpi_mean) / mv$tpi_sd)
  
  # add deltas
  deltas <- select(deltas, -id)
  for(i in 1:ncol(deltas)){
    id <- str_split(colnames(deltas)[i], "_")[[1]]
    d[[id[1]]] <- d[[id[1]]] + deltas[,i] * d[[id[2]]]
  }
  
  # destandardize
  d <- d %>%
    mutate(h = bio5 * mv$bio5_sd + mv$bio5_mean,
           c = bio6 * mv$bio6_sd + mv$bio6_mean,
           m = 10 ^ (bio12 * mv$bio12_sd + mv$bio12_mean) - 1)
  
  d <- d %>%
    select(h:m) %>%
    as.matrix()
  topo <- macro[[1:3]]
  topo[] <- d
  names(topo) <- c("high_temp", "low_temp", "moisture")
  topo
}


#' Topoclimate estimates
#'
#' This function produces estimates of biologically effective microclimate given a user-suppled digital elevation model (DEM). It is based on a model that leverages North American tree species occurrences as microclimate indicators. Estimates provided are for the posterior mode (penalized maximum likelihood) for model parameters.
#'
#' @param dem RasterLayer representing elevation, in meters. A spatial resolution of 10-30 m is recommended. The dataset must be in the continental US or Canada. DEMs covering areas larger than a small landscape may encounter computational challenges.
#' @param include_inputs Whether to return intermediate inputs in addition to topoclimate result (default FALSE)
#'
#' @return A raster stack of topoclimate variables, including "high_temperature" (in deg C), "low_temperature" (in deg C), and "moisture" (in mm).
#' @references Kling, Baer, & Ackerly (2024). A tree's view of the terrain. Ecography e07131, https://doi.org/10.1111/ecog.07131
#' @export
bioclimate <- function(dem, include_inputs = FALSE){
  
  message("Calculating biologically effective topoclimate")
  
  # enforce lat-lon crs
  if(!isLonLat(dem)){
    message("... projecting DEM to lon-lat for compatibility ...")
    dem <- projectRaster(dem, crs = "+proj=longlat +datum=NAD83 +no_defs")
  }
  
  # prep predictor data
  terr <- terrain(dem, c("slope", "aspect"))
  ne <- northeast(terr)
  wind <- windex(ne, terr)
  tpi <- mtpi(dem)
  macro <- macroclimate(dem)
  d <- stack(setNames(tpi, "tpi"), ne, setNames(wind, "wind"), macro) %>%
    rasterToPoints() %>% as.data.frame() %>% as_tibble() %>%
    mutate(id = 1:nrow(.))
  
  # calculate topoclimate estimates
  md <- readRDS(topo_data("model_metadata.rds"))[2,]
  deltas <- get_deltas(md, d)
  topo <- microclimate(md, d, deltas, macro)
  
  if(include_inputs) topo <- stack(topo, ne, wind, tpi,terr,
                                   setNames(macro, paste0("macro_", names(macro))))
  return(topo)
}


topo_data <- function(path = NULL){
  if(is.null(path)) return(system.file("extdata", package = "topoclimate.pred"))
  return(system.file("extdata", path, package = "topoclimate.pred"))
}

