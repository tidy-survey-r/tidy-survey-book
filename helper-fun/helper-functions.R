read_osf <- function(filename){
  #' Downloads file from Tidy Survey Analysis in R
  #' Reads in file
  #' Deletes file from computer
  
  osf_dl_del_later <- !dir.exists("osf_dl")
  
  if (osf_dl_del_later) {
    osf_dl_del_later <- TRUE
    dir.create("osf_dl")
  }
  
  dat_det <-
    osf_retrieve_node("https://osf.io/gzbkn/?view_only=8ca80573293b4e12b7f934a0f742b957") %>%
    osf_ls_files() %>%
    dplyr::filter(name == filename) %>%
    osf_download(conflicts = "overwrite", path = "osf_dl")
  
  out <- dat_det %>%
    dplyr::pull(local_path) %>%
    readr::read_rds()
  
  if (osf_dl_del_later) {
    unlink("osf_dl", recursive = TRUE)
  } else{
    unlink(dplyr::pull(dat_det, local_path))
  }
  
  return(out)
}