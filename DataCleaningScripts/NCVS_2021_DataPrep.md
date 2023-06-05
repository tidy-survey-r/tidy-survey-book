National Crime Victimization Survey (NCVS) 2021 Data Prep
================

## Data information

Complete data is not stored on this repository but can be obtained on
[ICPSR](https://www.icpsr.umich.edu/web/ICPSR/studies/38429) by
downloading the R version of data files (United States. Bureau of
Justice Statistics (2022)). The files used here are from Version 1 and
were downloaded on March 11, 2023.

This script selects a subset of columns of several files and only
retains those on this repository.

``` r
library(here) #easy relative paths
```

``` r
library(tidyverse) #data manipulation
library(tidylog) #informative logging messages
library(osfr)
```

## Incident data file

``` r
inc_file_osf_det <- osf_retrieve_node("https://osf.io/z5c3m/") %>%
  osf_ls_files(path="NCVS_2021/DS0004") %>%
  osf_download(conflicts="overwrite", path=here("osf_dl"))

incfiles <- load(pull(inc_file_osf_det, local_path), verbose=TRUE)
```

    ## Loading objects:
    ##   da38429.0004

``` r
inc_in <- get(incfiles) %>%
  as_tibble()

unlink(pull(inc_file_osf_det, local_path))

make_num_fact <- function(x){
  xchar <- sub("^\\(0*([0-9]+)\\).+$", "\\1", x)
  xnum <- as.numeric(xchar)
  fct_reorder(xchar, xnum, .na_rm = TRUE)
}

inc_slim <- inc_in %>%
  select(
    YEARQ, IDHH, IDPER, V4012, WGTVICCY, # identifiers and weight
    num_range("V", 4016:4019), # series crime information
    V4021B, V4022, V4024, # time of incident, location of incident (macro and micro)
    num_range("V", 4049:4058), #weapon type
    V4234, V4235, num_range("V", 4241:4245), V4248, num_range("V", 4256:4278), starts_with("V4277"), # victim-offender relationship
    V4399, # report to police
    V4529 # type of crime
  ) %>%
  mutate(
    IDHH=as.character(IDHH),
    IDPER=as.character(IDPER),
    across(where(is.factor), make_num_fact)
  )
```

    ## select: dropped 1,201 variables (V4001, V4002, V4003, V4004, V4005, …)

    ## mutate: converted 'IDHH' from factor to character (0 new NA)

    ##         converted 'IDPER' from factor to character (0 new NA)

    ##         changed 8,982 values (100%) of 'V4017' (0 new NA)

    ##         changed 157 values (2%) of 'V4018' (0 new NA)

    ##         changed 153 values (2%) of 'V4019' (0 new NA)

    ##         changed 8,982 values (100%) of 'V4021B' (0 new NA)

    ##         changed 8,982 values (100%) of 'V4022' (0 new NA)

    ##         changed 8,982 values (100%) of 'V4024' (0 new NA)

    ##         changed 2,737 values (30%) of 'V4049' (0 new NA)

    ##         changed 409 values (5%) of 'V4050' (0 new NA)

    ##         changed 409 values (5%) of 'V4051' (0 new NA)

    ##         changed 409 values (5%) of 'V4052' (0 new NA)

    ##         changed 409 values (5%) of 'V4053' (0 new NA)

    ##         changed 409 values (5%) of 'V4054' (0 new NA)

    ##         changed 409 values (5%) of 'V4055' (0 new NA)

    ##         changed 409 values (5%) of 'V4056' (0 new NA)

    ##         changed 409 values (5%) of 'V4057' (0 new NA)

    ##         changed 409 values (5%) of 'V4058' (0 new NA)

    ##         changed 2,827 values (31%) of 'V4234' (0 new NA)

    ##         changed 398 values (4%) of 'V4235' (0 new NA)

    ##         changed 2,096 values (23%) of 'V4241' (0 new NA)

    ##         changed 920 values (10%) of 'V4242' (0 new NA)

    ##         changed 1,246 values (14%) of 'V4243' (0 new NA)

    ##         changed 831 values (9%) of 'V4244' (0 new NA)

    ##         changed 1,075 values (12%) of 'V4245' (0 new NA)

    ##         changed 353 values (4%) of 'V4256' (0 new NA)

    ##         changed 231 values (3%) of 'V4257' (0 new NA)

    ##         changed 139 values (2%) of 'V4258' (0 new NA)

    ##         changed 139 values (2%) of 'V4259' (0 new NA)

    ##         changed 139 values (2%) of 'V4260' (0 new NA)

    ##         changed 139 values (2%) of 'V4261' (0 new NA)

    ##         changed 139 values (2%) of 'V4262' (0 new NA)

    ##         changed 181 values (2%) of 'V4263' (0 new NA)

    ##         changed 104 values (1%) of 'V4264' (0 new NA)

    ##         changed 104 values (1%) of 'V4265' (0 new NA)

    ##         changed 104 values (1%) of 'V4266' (0 new NA)

    ##         changed 104 values (1%) of 'V4267' (0 new NA)

    ##         changed 104 values (1%) of 'V4268' (0 new NA)

    ##         changed 104 values (1%) of 'V4269' (0 new NA)

    ##         changed 104 values (1%) of 'V4270' (0 new NA)

    ##         changed 104 values (1%) of 'V4271' (0 new NA)

    ##         changed 104 values (1%) of 'V4272' (0 new NA)

    ##         changed 104 values (1%) of 'V4273' (0 new NA)

    ##         changed 104 values (1%) of 'V4274' (0 new NA)

    ##         changed 104 values (1%) of 'V4275' (0 new NA)

    ##         changed 104 values (1%) of 'V4276' (0 new NA)

    ##         changed 104 values (1%) of 'V4277' (0 new NA)

    ##         changed 104 values (1%) of 'V4278' (0 new NA)

    ##         changed 104 values (1%) of 'V4277A' (0 new NA)

    ##         changed 104 values (1%) of 'V4277B' (0 new NA)

    ##         changed 104 values (1%) of 'V4277C' (0 new NA)

    ##         changed 104 values (1%) of 'V4277D' (0 new NA)

    ##         changed 104 values (1%) of 'V4277E' (0 new NA)

    ##         changed 8,982 values (100%) of 'V4399' (0 new NA)

    ##         changed 8,982 values (100%) of 'V4529' (0 new NA)

``` r
summary(inc_slim)
```

    ##      YEARQ          IDHH              IDPER               V4012          WGTVICCY           V4016        
    ##  Min.   :2021   Length:8982        Length:8982        Min.   :1.000   Min.   :  221.6   Min.   :  1.000  
    ##  1st Qu.:2021   Class :character   Class :character   1st Qu.:1.000   1st Qu.:  867.3   1st Qu.:  1.000  
    ##  Median :2021   Mode  :character   Mode  :character   Median :1.000   Median : 1352.3   Median :  1.000  
    ##  Mean   :2021                                         Mean   :1.179   Mean   : 1674.9   Mean   :  4.324  
    ##  3rd Qu.:2021                                         3rd Qu.:1.000   3rd Qu.: 2217.4   3rd Qu.:  1.000  
    ##  Max.   :2021                                         Max.   :7.000   Max.   :10106.2   Max.   :998.000  
    ##                                                                                                          
    ##  V4017     V4018       V4019          V4021B     V4022        V4024       V4049       V4050       V4051     
    ##  1:8825   1   : 127   1   :  10   7      :1855   1:  34   5      :3210   1   : 409   1   : 380   0   : 278  
    ##  2: 131   2   :   4   2   : 117   9      :1217   2:  65   1      :1481   2   :1803   3   :  26   1   : 131  
    ##  8:  26   8   :  26   8   :  26   2      :1145   3:7697   7      : 727   3   : 525   7   :   3   NA's:8573  
    ##           NA's:8825   NA's:8829   3      : 940   4:1143   21     : 453   NA's:6245   NA's:8573              
    ##                                   8      : 856   5:  39   16     : 449                                      
    ##                                   4      : 833   8:   4   6      : 429                                      
    ##                                   (Other):2136            (Other):2233                                      
    ##   V4052       V4053       V4054       V4055       V4056       V4057       V4058       V4234       V4235     
    ##  0   : 390   0   : 334   0   : 394   0   : 302   0   : 360   0   : 406   0   : 380   1   :2076   1   :  20  
    ##  1   :  19   1   :  75   1   :  15   1   : 107   1   :  49   1   :   3   8   :  29   2   : 353   2   : 291  
    ##  NA's:8573   NA's:8573   NA's:8573   NA's:8573   NA's:8573   NA's:8573   NA's:8573   3   : 311   8   :  87  
    ##                                                                                      8   :  87   NA's:8584  
    ##                                                                                      NA's:6155              
    ##                                                                                                             
    ##                                                                                                             
    ##   V4241       V4242       V4243       V4244          V4245          V4248         V4256       V4257     
    ##  1   :1176   1   : 326   1   : 171   1   : 307   7      : 149   Min.   : 2.000   1   :  83   1   :  65  
    ##  2   : 793   2   : 240   2   : 292   2   : 424   8      : 139   1st Qu.: 2.000   2   :  37   2   :  63  
    ##  3   :  57   3   : 271   3   : 701   3   :   4   11     : 137   Median : 2.000   3   : 194   3   :  85  
    ##  8   :  70   8   :  83   6   :   1   8   :  96   13     : 114   Mean   : 7.992   4   :  20   8   :  18  
    ##  NA's:6886   NA's:8062   8   :  81   NA's:8151   98     :  85   3rd Qu.: 3.000   6   :   2   NA's:8751  
    ##                          NA's:7736               (Other): 451   Max.   :98.000   8   :  17              
    ##                                                  NA's   :7907   NA's   :8629     NA's:8629              
    ##   V4258       V4259       V4260       V4261       V4262       V4263       V4264       V4265       V4266     
    ##  1   : 122   0   :  85   0   :  76   0   :  77   0   : 122   1   :  65   1   :  87   0   :  87   0   :  83  
    ##  8   :  17   1   :  37   1   :  46   1   :  45   8   :  17   2   :  98   8   :  17   8   :  17   1   :   4  
    ##  NA's:8843   8   :  17   8   :  17   8   :  17   NA's:8843   8   :  18   NA's:8878   NA's:8878   8   :  17  
    ##              NA's:8843   NA's:8843   NA's:8843               NA's:8801                           NA's:8878  
    ##                                                                                                             
    ##                                                                                                             
    ##                                                                                                             
    ##   V4267       V4268       V4269       V4270       V4271       V4272       V4273       V4274       V4275     
    ##  0   :  82   0   :  84   0   :  84   0   :  83   0   :  84   0   :  66   0   :  81   0   :  84   0   :  64  
    ##  1   :   5   1   :   3   1   :   3   1   :   4   1   :   3   1   :  21   1   :   6   1   :   3   1   :  23  
    ##  8   :  17   8   :  17   8   :  17   8   :  17   8   :  17   8   :  17   8   :  17   8   :  17   8   :  17  
    ##  NA's:8878   NA's:8878   NA's:8878   NA's:8878   NA's:8878   NA's:8878   NA's:8878   NA's:8878   NA's:8878  
    ##                                                                                                             
    ##                                                                                                             
    ##                                                                                                             
    ##   V4276       V4277       V4278       V4277A      V4277B      V4277C      V4277D      V4277E     V4399   
    ##  0   :  85   0   :  62   0   :  85   0   :  87   0   :  87   0   :  87   0   :  84   0   :  87   1:3175  
    ##  1   :   2   1   :  25   8   :  19   8   :  17   8   :  17   8   :  17   1   :   3   8   :  17   2:5692  
    ##  8   :  17   8   :  17   NA's:8878   NA's:8878   NA's:8878   NA's:8878   8   :  17   NA's:8878   3: 103  
    ##  NA's:8878   NA's:8878                                                   NA's:8878               8:  12  
    ##                                                                                                          
    ##                                                                                                          
    ##                                                                                                          
    ##      V4529     
    ##  56     :1689  
    ##  57     :1431  
    ##  55     :1011  
    ##  58     : 799  
    ##  32     : 637  
    ##  20     : 609  
    ##  (Other):2806

``` r
inc_temp_loc <- here("osf_dl", "ncvs_2021_incident.rds")
write_rds(inc_slim, inc_temp_loc)
target_dir <- osf_retrieve_node("https://osf.io/gzbkn/?view_only=8ca80573293b4e12b7f934a0f742b957") 
osf_upload(target_dir, path=inc_temp_loc, conflicts="overwrite")
```

    ## # A tibble: 1 × 3
    ##   name                   id                       meta            
    ##   <chr>                  <chr>                    <list>          
    ## 1 ncvs_2021_incident.rds 647cfbcd85df4808fa7753f2 <named list [3]>

``` r
unlink(inc_temp_loc)
```

## Person data file

``` r
pers_file_osf_det <- osf_retrieve_node("https://osf.io/z5c3m/") %>%
  osf_ls_files(path="NCVS_2021/DS0003") %>%
  osf_download(conflicts="overwrite", path=here("osf_dl"))

persfiles <- load(pull(pers_file_osf_det, local_path), verbose=TRUE)
```

    ## Loading objects:
    ##   da38429.0003

``` r
pers_in <- get(persfiles) %>%
  as_tibble()

unlink(pull(pers_file_osf_det, local_path))

pers_slim <- pers_in %>%
  select(
    YEARQ, IDHH, IDPER, WGTPERCY, # identifiers and weight
    V3014, V3015, V3018, V3023A, V3024, V3084, V3086 
    # age, marital status, sex, race, hispanic origin, gender, sexual orientation
  ) %>%
  mutate(
    IDHH=as.character(IDHH),
    IDPER=as.character(IDPER),
    across(where(is.factor), make_num_fact)
  )
```

    ## select: dropped 418 variables (V3001, V3002, V3003, V3004, V3005, …)

    ## mutate: converted 'IDHH' from factor to character (0 new NA)

    ##         converted 'IDPER' from factor to character (0 new NA)

    ##         changed 291,878 values (100%) of 'V3015' (0 new NA)

    ##         changed 291,878 values (100%) of 'V3018' (0 new NA)

    ##         changed 291,878 values (100%) of 'V3023A' (0 new NA)

    ##         changed 291,878 values (100%) of 'V3024' (0 new NA)

    ##         changed 216,287 values (74%) of 'V3084' (0 new NA)

    ##         changed 216,287 values (74%) of 'V3086' (0 new NA)

``` r
summary(pers_slim)
```

    ##      YEARQ          IDHH              IDPER              WGTPERCY           V3014       V3015      V3018     
    ##  Min.   :2021   Length:291878      Length:291878      Min.   :    0.0   Min.   :12.00   1:148131   1:140922  
    ##  1st Qu.:2021   Class :character   Class :character   1st Qu.:  432.2   1st Qu.:31.00   2: 17668   2:150956  
    ##  Median :2021   Mode  :character   Mode  :character   Median :  791.5   Median :48.00   3: 28596             
    ##  Mean   :2021                                         Mean   :  956.5   Mean   :47.57   4:  4524             
    ##  3rd Qu.:2021                                         3rd Qu.: 1397.4   3rd Qu.:64.00   5: 90425             
    ##  Max.   :2021                                         Max.   :10691.5   Max.   :90.00   8:  2534             
    ##                                                                                                              
    ##      V3023A       V3024          V3084         V3086       
    ##  1      :236785   1: 41450   8      :151725   1   : 29733  
    ##  2      : 30972   2:249306   2      : 61108   2   : 34489  
    ##  4      : 16337   8:  1122   6      :  1477   3   :    56  
    ##  3      :  1776              1      :   924   4   :   115  
    ##  6      :  1590              3      :   611   8   :151894  
    ##  7      :  1465              (Other):   442   NA's: 75591  
    ##  (Other):  2953              NA's   : 75591

``` r
pers_temp_loc <- here("osf_dl", "ncvs_2021_person.rds")
write_rds(pers_slim, pers_temp_loc)
target_dir <- osf_retrieve_node("https://osf.io/gzbkn/?view_only=8ca80573293b4e12b7f934a0f742b957") 
osf_upload(target_dir, path=pers_temp_loc, conflicts="overwrite")
```

    ## # A tibble: 1 × 3
    ##   name                 id                       meta            
    ##   <chr>                <chr>                    <list>          
    ## 1 ncvs_2021_person.rds 647cfe9ba8dbe909bacb51bf <named list [3]>

``` r
unlink(pers_temp_loc)
```

## Household data file

``` r
hh_file_osf_det <- osf_retrieve_node("https://osf.io/z5c3m/") %>%
  osf_ls_files(path="NCVS_2021/DS0002") %>%
  osf_download(conflicts="overwrite", path=here("osf_dl"))

hhfiles <- load(pull(hh_file_osf_det, local_path), verbose=TRUE)
```

    ## Loading objects:
    ##   da38429.0002

``` r
hh_in <- get(hhfiles) %>%
  as_tibble()

unlink(pull(hh_file_osf_det, local_path))

hh_slim <- hh_in %>%
  select(
    YEARQ, IDHH, WGTHHCY, V2117, V2118, # identifiers, weight, design
    V2015, V2143, SC214A, V2122, V2126B, V2127B, V2129
    # tenure, urbanicity, income, family structure, place size, region, msa status
  ) %>%
  mutate(
    IDHH=as.character(IDHH),
    across(where(is.factor), make_num_fact)
  )
```

    ## select: dropped 440 variables (V2001, V2002, V2003, V2004, V2005, …)

    ## mutate: converted 'IDHH' from factor to character (0 new NA)

    ##         changed 150,138 values (59%) of 'V2015' (0 new NA)

    ##         changed 256,460 values (100%) of 'V2143' (0 new NA)

    ##         changed 253,779 values (99%) of 'SC214A' (0 new NA)

    ##         changed 256,460 values (100%) of 'V2122' (0 new NA)

    ##         changed 256,460 values (100%) of 'V2126B' (0 new NA)

    ##         changed 256,460 values (100%) of 'V2127B' (0 new NA)

    ##         changed 256,460 values (100%) of 'V2129' (0 new NA)

``` r
summary(hh_slim)
```

    ##      YEARQ          IDHH              WGTHHCY           V2117            V2118        V2015        V2143     
    ##  Min.   :2021   Length:256460      Min.   :   0.0   Min.   :  1.00   Min.   :1.000   1   :101944   1: 26878  
    ##  1st Qu.:2021   Class :character   1st Qu.:   0.0   1st Qu.: 24.00   1st Qu.:1.000   2   : 46269   2:173491  
    ##  Median :2021   Mode  :character   Median : 399.4   Median : 48.00   Median :2.000   3   :  1925   3: 56091  
    ##  Mean   :2021                      Mean   : 504.2   Mean   : 58.85   Mean   :1.526   NA's:106322             
    ##  3rd Qu.:2021                      3rd Qu.: 829.1   3rd Qu.: 88.00   3rd Qu.:2.000                           
    ##  Max.   :2021                      Max.   :4515.8   Max.   :160.00   Max.   :3.000                           
    ##                                                                                                              
    ##      SC214A           V2122            V2126B      V2127B    V2129     
    ##  13     : 44601   33     :106322   0      :69484   1:41585   1: 80895  
    ##  16     : 34287   32     : 28306   16     :53002   2:74666   2:135438  
    ##  15     : 33353   16     : 23617   13     :39873   3:87783   3: 40127  
    ##  12     : 23282   8      : 21383   17     :27205   4:52426             
    ##  18     : 16892   24     : 15629   18     :24461                       
    ##  (Other):101364   4      : 10477   20     :15194                       
    ##  NA's   :  2681   (Other): 50726   (Other):27241

``` r
hh_temp_loc <- here("osf_dl", "ncvs_2021_household.rds")
write_rds(hh_slim, hh_temp_loc)
target_dir <- osf_retrieve_node("https://osf.io/gzbkn/?view_only=8ca80573293b4e12b7f934a0f742b957") 
osf_upload(target_dir, path=hh_temp_loc, conflicts="overwrite")
```

    ## # A tibble: 1 × 3
    ##   name                    id                       meta            
    ##   <chr>                   <chr>                    <list>          
    ## 1 ncvs_2021_household.rds 647cfe323c3a380880a046d8 <named list [3]>

``` r
unlink(hh_temp_loc)
```

## Resources

- [USER’S GUIDE TO NATIONAL CRIME VICTIMIZATION SURVEY (NCVS) DIRECT
  VARIANCE
  ESTIMATION](https://bjs.ojp.gov/sites/g/files/xyckuh236/files/media/document/ncvs_variance_user_guide_11.06.14.pdf)
  -[Appendix C: Examples in
  SAS](https://bjs.ojp.gov/sites/g/files/xyckuh236/files/media/document/variance_guide_appendix_c_sas.pdf)

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-ncvs_data_2021" class="csl-entry">

United States. Bureau of Justice Statistics. 2022. “National Crime
Victimization Survey, \[United States\], 2021.” Inter-university
Consortium for Political; Social Research \[distributor\].
<https://doi.org/10.3886/ICPSR38429.v1>.

</div>

</div>
