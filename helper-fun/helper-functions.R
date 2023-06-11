read_rds_tsr <- function(filename){
  #' Downloads file from Tidy Survey Analysis in R
  #' Reads in file
  #' Deletes file from computer
  
  if (!dir.exists("osf_dl")){
    osf_dl_del_later <- TRUE
    dir.create("osf_dl")
  } else{
    osf_dl_del_later <- FALSE
  }
  
  dat_det <- osf_retrieve_node("https://osf.io/gzbkn/?view_only=8ca80573293b4e12b7f934a0f742b957") %>%
    osf_ls_files() %>%
    filter(name==filename) %>%
    osf_download(conflicts="overwrite", path="osf_dl")
  
  out <- dat_det %>%
    pull(local_path) %>%
    read_rds()
  
  unlink(pull(dat_det, local_path))
  
  if (osf_dl_del_later){
    unlink("osf_dl", recursive = TRUE)
  }
  
  return(out)
}