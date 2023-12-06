## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = T, message = F, warning = F---------------------------------------
#install.packages("phenthau")
library(PHENTHAUproc)

# for presentation, additional packages are used:
library(terra)
library(tidyterra)
library(ggplot2)
library(geomtextpath)
library(grDevices)

## ----echo = T, message = F----------------------------------------------------
# local data for PHENTHAUproc 
freiburg_path <- system.file("extdata", "freiburg.csv", package  = "PHENTHAUproc", mustWork = TRUE)
freiburg <- utils::read.csv(freiburg_path)

head(freiburg)

## ----echo = T, message = F----------------------------------------------------
# spatial data for PHENTHAUproc
tmean_path <- system.file("extdata", "fva_tmean.nc", package  = "PHENTHAUproc", mustWork = TRUE)
tmax_path <- system.file("extdata", "fva_tmax.nc", package  = "PHENTHAUproc", mustWork = TRUE)
tmin_path <- system.file("extdata", "fva_tmin.nc", package  = "PHENTHAUproc", mustWork = TRUE)

fva <-  list(
  tmean = terra::rast(tmean_path),
  tmax = terra::rast(tmax_path),
  tmin = terra::rast(tmin_path)
)

## ----echo = T, message = F----------------------------------------------------
local <- phenthau(freiburg)
regional <- phenthau(fva)

## ----echo = T, message = T----------------------------------------------------
plot_station_step(local)
plot_station(local, main = "PHENTHAUproc\nFreiburg")

## ----echo = T, message = F----------------------------------------------------
stages <- regional$stages

plot_stages(stages,
            time = "2020-06-05",
            main = "Stages - 5. June",
            axes = F)

## ----echo = T, message = T----------------------------------------------------
# return mortality from list
mo <- regional$mortality

# get_legend returns a dataframe with ID, category and colors for a spatial PHENTHAUproc output
leg <- get_legend("mortality")

# set levels and colors for mortality
levels(mo) <- leg[,c("ID", "category")]
terra::coltab(mo) <- leg[,c("ID", "colors")]

# plot mortality FVA 2020
terra::plot(mo,
            plg = list(title = "%"),
            main = "Mortality",
            all_levels = T,
            axes = F)

## ----echo = T, message = F----------------------------------------------------
# show possible models and parametrisation: parameter()
leafunfolding <- phenology(fva,
                           model = "leafunfolding",
                           parametrisation = "quercus_robur_type1")

# plot_date can be used to plot time serial numbers as raster values in SpatRaster and add the date format to the legend.
plot_date(leafunfolding,
          col = hcl.colors(50, "Greens 2"), 
          main = "leaf unfolding",
          axes = F)

## ----echo = T, message = F, warning = F---------------------------------------
custers <- phenology(fva, model = "hatch", parametrisation = "custers")
meurisse <- phenology(fva, model = "hatch", parametrisation = "meurisse")
wagenhoff <- phenology(fva, model = "hatch", parametrisation = "wagenhoff")

## ----echo = F, message = F, warning = F---------------------------------------
hatch <- c(custers, meurisse, wagenhoff)
names(hatch) <- c("custers", "meurisse", "wagenhoff")

mi <- min(terra::minmax(hatch)["min",])
ma <- max(terra::minmax(hatch)["max",])
me <- round(mean(c(mi, ma)))

ggplot() +
  geom_spatraster(data = hatch) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_whitebox_c(
    palette = "deep",
    labels = lubridate::as_date,
    breaks = c(mi, me, ma),
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = 20,
      draw.ulim = 1,
      draw.llim = 1
    )
  ) +
  theme_bw() +
  labs(fill = "hatchday") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

