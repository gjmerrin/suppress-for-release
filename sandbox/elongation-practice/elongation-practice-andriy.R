# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched_output/0-greeter.md"
# )

# This script inputs the raw data and prepares it for tuning.
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. 
# This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")       # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions

# ---- load-packages -----------------------------------------------------------
library(magrittr) # pipes
requireNamespace("dplyr")
requireNamespace("readr")
requireNamespace("testit")

# ---- declare-globals ---------------------------------------------------------
# declare the location of the data sources to be used in this script
path_input <- "data-unshared/EPI_WAVES 1-7 MEASURES_RECODE.SAV"
path_save  <- "./sandbox/elongation-practice/"

(
  select_variables <- c(
  "SUBNO"
  ,"FEMALE"
  ,"RACE"
  ,paste0("AGE_",0:6)
  ,paste0("BULLYING_",0:6)
  )
)

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script

# ---- load-data ---------------------------------------------------------------
ds0  <- foreign::read.spss(path_input) %>% as.data.frame() #%>% tibble::as_tibble()
ds0 %>% class()

# ---- inspect-data -----------------------------------------------------------
# ds0 %>% dplyr::glimpse(50)

# ---- tweak-data -------------------------------------------------------------
# let us subset only the needed columns from the raw data to make workflow lighter
# (names(ds0) <- tolower(names(ds0)))

ds1 <- ds0 %>% 
  dplyr::select_(.dots = select_variables) %>% 
  dplyr::filter(SUBNO %in% 1003) 
  # dplyr::filter(SUBNO %in% 1001:1003) 

ds1 %>% dplyr::glimpse(50)

# ---- manual-elongation ----------------------
# 
# # approach 1
# ds2_wide <- ds1 %>% 
#   tidyr::gather(key = "key", value = "value", 
#                 AGE_0, AGE_1, AGE_2, AGE_3, AGE_4, AGE_5, AGE_6) %>% 
#   dplyr::mutate(
#     age = value,
#     wave = gsub(pattern = "AGE_(\\d+)",replacement = "\\1", x = key)
#   ) %>% 
#   dplyr::arrange(SUBNO)
# 
# View(ds2_wide)
# 
# # approach 2
# (varnames <- names(ds1))
# (variables_static <- c("SUBNO","FEMALE","RACE"))
# (variables_dynamic <- setdiff(varnames, variables_static))
# 
# ds2_wide <- ds1 %>% 
#   tidyr::gather("key","value", variables_dynamic) %>% # use with caution
#   dplyr::mutate(
#     
#     spread_var = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\1", x = key)
#     ,wave     = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\2", x = key)
#   ) %>% 
#   dplyr::select(-key) %>% 
#   tidyr::spread(key = "spread_var", value = "value")
# 
# # alt 2
# ds2_wide <- ds1 %>% 
#   tidyr::gather("key","value", variables_dynamic) %>% # use with caution
#   dplyr::mutate(
#     wave = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\2", x = key)
#     ,key = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\1", x = key)
#   ) %>% 
#   tidyr::spread("key", "value")


# ---- elongate-function ----------------------
# approach 3
# involves creating a function with targeting facility

elongate_1 <- function(
  d # input data set in wide form
  # ,variables_static
  # ,variables_dynamic
){
  # values for testing and development
  # d <- ds1 
  
  # create vectors to store variable names
  (varnames <- names(d))
  (variables_static <- c("SUBNO","FEMALE","RACE"))
  (variables_dynamic <- setdiff(varnames, variables_static))
  
  # perform the transformations
  d1 <- d %>% 
    # elongate with respect to wave
    tidyr::gather("key","value", variables_dynamic) %>% # use with caution
    dplyr::mutate(
      wave = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\2", x = key)
      ,key = gsub(pattern = "(\\w+)_(\\d+)",replacement = "\\1", x = key)
    ) %>% 
    # widen with respect to elongated measures
    tidyr::spread("key", "value")
  return(d1)
} # how to use
ds2_long <- ds1 %>% elongate_1() # note ds1 is in wide with respect to wave

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

