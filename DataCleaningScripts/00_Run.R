rmarkdown::render(
  input=here::here("DataCleaningScripts", "RECS_2015_DataPrep.Rmd")
)

rmarkdown::render(
  input=here::here("DataCleaningScripts", "RECS_2020_DataPrep.Rmd")
)

rmarkdown::render(
  input=here::here("DataCleaningScripts", "ANES_2020_DataPrep.Rmd")
)
