
# library(tint)
# # invalidate cache when the package version changes
# knitr::opts_chunk$set(tidy = FALSE, cache.extra = packageVersion('tint'))
# options(htmltools.dir.version = FALSE)

#Default Rmd options
knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      dev = "cairo_pdf",
                      warning = FALSE,
                      fig.align = 'center') #allows for inserting R code into captions

#Plotting and data libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ecodata)
library(here)
library(kableExtra)
library(ggrepel)
library(stringr)
library(patchwork)
library(grid)
# library(ggiraph)
library(vegan)
library(RColorBrewer)
library(rpart)

#GIS libraries
library(sf)
library(rgdal)
library(raster)
library(rnaturalearth)

#Data directories
image.dir <- here("images")
gis.dir <- here("gis")

#GIS directory
#gis.dir <- here::here("inst","extdata","gridded")

#General inline text input for report
#Council
council <- "New England Fishery Management Council"
council_abbr <- "NEFMC"

#Region identifiers
epu <- "New England"
epu_abbr <- c("GOM","GB")
region <- "New England"
region_abbr <- "NE" #Some commercial data organized by "MA" or "NE" regions, not by EPU 
