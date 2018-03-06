library(lubridate)
library(cobs)
library(plyr)  # for rbind.fill
library(tidyverse)
library(RODBC)

options(width=120)

source("functions/helpers.R")
source("functions/manual.R")
source("functions/monotone.R")
source("functions/offset.R")
source("functions/plot.R")
source("functions/vertauscher.R")
source("functions/smoothing.R")
source("functions/msaccess.R")

config <- list(
  correction = "complete",              # "complete" or "update", corresponds to all data or all new data (i.e. where Abgenommen==FALSE)
  QS_threshold_for_manual_flag = 1,     # quality scores below this threshold will result in flagging this machine for manual inspection
  min_obs_for_monotone_correction = 3,  # counters with less than this number of observations will not be corrected for negative slopes, but flagged for manual inspection
  min_obs_for_offset = 5,               # offset correction will be skipped for counters with less than this number of observations
  offset_relative_drop = 0.20,          # only consider points as offsets that drop by more than this percentage
  offset_absolute_drop = 1000,          # only consider points as offsets that drop by more than this absolute number
  offset_if_SSQ_ratio_below = 0.01,     # an offset is found if the residual SSQ of the corrected model is less than this ratio times the uncorrected residual SSQ
  smoothing_min_obs = 4,                # minimum observations before smoothing is attempted. do not set this too low
  smoothing_degree = 1,                 # degree of the smoothing spline. 1=piecewise linear, 2=piecewise quadratic, etc.
  smoothing_penalty = 1000,             # smoothing parameter for the spline. 0 = "no penalty", and the higher, the straighter the smooth will be
  
  # minimum slope for counters (otherwise it will be deactivated):
  min_slope = c(SIS_NC_Aufgewertet=0,
                SIS_Resonator_Aufgewertet=0,
                SIS_Strahl_Aufgewertet=0),
  
  # maximum slope for counters (otherwise it will be deactivated):
  max_slope = c(SIS_NC_Aufgewertet=0.99,
                SIS_Resonator_Aufgewertet=0.99,
                SIS_Strahl_Aufgewertet=0.95)
)

# some constants definitions:
ZAEHLER <- c("SIS_NC_Aufgewertet", "SIS_Resonator_Aufgewertet", "SIS_Strahl_Aufgewertet")
MANUAL_TOO_FEW_OBSERVATIONS <- 2
MANUAL_BAD_QUALITY_SCORE <- 1
