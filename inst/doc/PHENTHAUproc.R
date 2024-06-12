## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = T, message = F, warning = F---------------------------------------
library(PHENTHAUproc)

# Packages used for demonstration:
library(terra)
library(tidyterra)
library(ggplot2)
library(grDevices)

## ----echo = T, message = F----------------------------------------------------
# local data for PHENTHAUproc 
freiburg <- load_test("hour")

head(freiburg)

## ----echo = T, message = F----------------------------------------------------
# spatial data for PHENTHAUproc
fva <-  load_test("SpatRaster")
fva

## ----echo = T, message = F----------------------------------------------------
# The parameter default method is "dailymeanminmax". If you want to calculate with hourly data, you have to change the parameter manually.
regional <- phenthau(fva)
local <- phenthau(freiburg, params = parameter("hour", year = 2020))

# we can create a parameter list
params <- parameter("dailymeanminmax", year = 2020)

# and change single parameter
params$budswelling$ldt <- 5 # change lower development threshold for budswelling from Default to 5

regional_manipulated <- phenthau(fva, params = params)

rm(params, regional_manipulated)

## ----echo = T, out.width = "100%", fig.dim = c(8, 6), message = T-------------
plot_station_step(local)

## ----echo = T, out.width = "50%", fig.align = "center", fig.cap = "OPM larval stages 5. June 2020", message = F----
stages <- regional$stages

plot_stages(stages,
            time = "2020-06-05",
            main = "OPM 5. June 2020",
            axes = F,
            box = T)

## ----echo = T, out.width = "50%", fig.cap = "Starvation related Mortality 2020" ,fig.align = "center", message = T----
# return mortality from list
mort <- regional$mortality

# get_legend returns a dataframe with ID, category and colors for a spatial PHENTHAUproc output
legend <- get_legend("mortality")

# set levels and colors for mortality
levels(mort) <- legend[,c("ID", "category")]
terra::coltab(mort) <- legend[,c("ID", "colors")]

# plot mortality FVA 2020
terra::plot(mort,
            plg = list(title = "%"),
            main = "Mortality 2020",
            all_levels = T,
            axes = F,
            box = T)

## ----echo = T, message = F, warning = F---------------------------------------
# show possible models and parametrisation: parameter()
leafunfolding <- phenology(fva,
                           model = "leafunfolding",
                           parametrisation = "quercus_robur_clone256_type1")


## ----echo = F, out.width = "100%", fig.dim = c(8, 6), fig.align = "center", message = F, warning = F----

mi <- min(terra::minmax(leafunfolding)["min",])
ma <- max(terra::minmax(leafunfolding)["max",])
me <- round(mean(c(mi, ma)))

ggplot() +
  geom_spatraster(data = leafunfolding) +
  scale_fill_whitebox_c(
    palette = "gn_yl",
    labels = lubridate::as_date,
    breaks = c(mi, me, ma),
    guide = guide_colorbar(
      title.position = "top",
      ticks.colour = "white",
      ticks.linewidth = 0.3
    )
  ) +
  theme_bw() +
  labs(title = "Leafunfolding 2020", subtitle = "4*4 pixel cutout centered at FVA", fill = "leafunfolding") +
  theme(legend.position = "right")

## ----echo = T, message = F, warning = F---------------------------------------
custers <- phenology(fva, model = "hatch", parametrisation = "custers")
meurisse <- phenology(fva, model = "hatch", parametrisation = "meurisse")
wagenhoff <- phenology(fva, model = "hatch", parametrisation = "wagenhoff")

## ----echo = F, out.width = "100%", fig.dim = c(8, 6), fig.align = "center", message = F, warning = F----
hatch <- c(custers, meurisse, wagenhoff)
names(hatch) <- c("custers", "meurisse", "wagenhoff")

mi <- min(terra::minmax(hatch)["min",])
ma <- max(terra::minmax(hatch)["max",])
me <- round(mean(c(mi, ma)))

ggplot() +
  geom_spatraster(data = hatch) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_whitebox_c(
    palette = rev("viridi"),
    direction = -1,
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
  labs(title = "Hatchmodels 2020",
       subtitle = "4*4 pixel cutout centered at FVA",
       fill = "hatchday") +
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


