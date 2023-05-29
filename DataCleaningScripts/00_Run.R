rmarkdown::render(
  input=here::here("DataCleaningScripts", "RECS_2015_DataPrep.Rmd"),
  envir=new.env()
)

rmarkdown::render(
  input=here::here("DataCleaningScripts", "RECS_2020_DataPrep.Rmd"),
  envir=new.env()
)

rmarkdown::render(
  input=here::here("DataCleaningScripts", "ANES_2020_DataPrep.Rmd"),
  envir=new.env()
)

rmarkdown::render(
  input=here::here("DataCleaningScripts", "LAPOP_2021_DataPrep.Rmd"),
  envir=new.env()
)

rmarkdown::render(
  input=here::here("DataCleaningScripts", "NCVS_2021_DataPrep.Rmd"),
  envir=new.env()
)
