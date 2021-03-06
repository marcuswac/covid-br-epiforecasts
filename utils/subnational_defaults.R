
# Knitr options -----------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, eval = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      fig.height = 12,
                      fig.width = 12,
                      dpi = 300)

# Load packages -----------------------------------------------------------
library(EpiNow2)
library(covidregionaldata)
library(RtD3)
library(dplyr)
library(magrittr)
library(rnaturalearth)
library(sf)
library(rmarkdown)
library(here)
library(stringr)
library(ggplot2)

# Load summary data -------------------------------------------------------
summary_table_file <- here::here("covid-rt-estimates", "subnational",
                                              folder, "cases",
                                              "summary", "summary_table.csv")
warning(paste("XXXXX PATH", summary_table_file))
summary_table <- data.table::fread(summary_table_file)
#summary_table <- data.table::fread(here::here("covid-rt-estimates", "subnational", 
#                                              folder, "cases", 
#                                              "summary", "summary_table.csv"))
latest_date <- readRDS(here::here("covid-rt-estimates", "subnational", folder,
                                  "cases", "summary", "latest_date.rds"))

# Build widget ------------------------------------------------------------
if (!exists("region_var")) {
  region_var <- "region"
}

  # Read in each summary folder
#base_path <- "https://raw.githubusercontent.com/epiforecasts/covid-rt-estimates/master/"
base_path <- here("covid-rt-estimates/")
warning(paste("YYYYY", base_path))

read_in_rt <- function(data = "cases") { 
  RtD3::readInEpiNow2(path = paste0(base_path, "subnational/", folder, "/", data, "/summary"),
                                               region_var = region_var)
  }

if (folder %in% "united-kingdom") {
  rtData <- list("Cases" = read_in_rt(),
                 "Admissions" = read_in_rt("admissions"),
                 "Deaths" = read_in_rt("deaths"))

}else {
  rtData <- list("Cases" = RtD3::joinRtData(
          read_in_rt(),
          RtD3::readInEpiNow2(path = paste0(base_path, "national/cases/summary"),
                              region_var = "country", regions = region)),
               "Deaths" = RtD3::readInEpiNow2(path = paste0(base_path, "national/deaths/summary"),
                                              region_var = "country", regions = region))
}

widget <- RtD3::summaryWidget(
  geoData = geoData,
  rtData = rtData,
  activeArea = region,
  downloadUrl = paste0("https://github.com/epiforecasts/covid-rt-estimates/master/subnational/", folder)
)

widget_caption <- paste0("*Figure 1: The results of the latest reproduction number estimates
                         (based on estimated confirmed cases with a date of infection on 
                         the ", latest_date , ") can be summarised by whether confirmed 
                         cases are likely increasing or decreasing. This represents the 
                         strength of the evidence that the reproduction number in each region 
                         is greater than or less than 1, respectively (see the
                         [methods](https://epiforecasts.io/covid/methods.html) for details).
                         Click on a subnational area (or search) to see subnational level estimates. 
                         This interactive visualisation is powered by RtD3[@rtd3].*")
