### Code for analysing HSY data for Espoo

library(tidyverse)
library(arrow)

df <- readxl::read_xlsx("~/Downloads/tietokanta_2022.xlsx")
df2 <- read_parquet("/Users/jouni/devel/dvctest/hsy/pks_khk_paastot.parquet")
df2 <- read_parquet("/Users/jouni/Library/Caches/dvc-pandas/espoo/6f02644cf70f5a9039966fc4d1736eee/espoo/hsy/pks_khk_paastot.parquet")
