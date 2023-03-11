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
```

    ## ── Attaching packages ────────────────────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0     ✔ purrr   0.3.5
    ## ✔ tibble  3.1.8     ✔ dplyr   1.1.0
    ## ✔ tidyr   1.2.1     ✔ stringr 1.5.0
    ## ✔ readr   2.1.3     ✔ forcats 0.5.2
    ## ── Conflicts ───────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tidylog) #informative logging messages
```

    ## 
    ## Attaching package: 'tidylog'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     add_count, add_tally, anti_join, count, distinct, distinct_all, distinct_at,
    ##     distinct_if, filter, filter_all, filter_at, filter_if, full_join, group_by,
    ##     group_by_all, group_by_at, group_by_if, inner_join, left_join, mutate,
    ##     mutate_all, mutate_at, mutate_if, relocate, rename, rename_all, rename_at,
    ##     rename_if, rename_with, right_join, sample_frac, sample_n, select, select_all,
    ##     select_at, select_if, semi_join, slice, slice_head, slice_max, slice_min,
    ##     slice_sample, slice_tail, summarise, summarise_all, summarise_at, summarise_if,
    ##     summarize, summarize_all, summarize_at, summarize_if, tally, top_frac, top_n,
    ##     transmute, transmute_all, transmute_at, transmute_if, ungroup
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     drop_na, fill, gather, pivot_longer, pivot_wider, replace_na, spread, uncount
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

## Incident data file

``` r
incfiles <- load(here("RawData", "NCVS_2021", "ICPSR_38429", "DS0004", "38429-0004-Data.rda"),
     verbose=TRUE)
```

    ## Loading objects:
    ##   da38429.0004

``` r
inc_in <- get(incfiles) %>%
  as_tibble()

inc_slim <- inc_in %>%
  select(
    YEARQ, IDHH, IDPER, V4012, WGTVICCY, # identifiers and weight
    num_range("V", 4016:4019), # series crime information
    V4021B, V4022, V4024, # time of incident, location of incident (macro and micro)
    num_range("V", 4049:4058), #weapon type
    V4234, V4235, num_range("V", 4241:4245), V4248, num_range("V", 4256:4278), starts_with("V4277"), # victim-offender relationship
    V4399, # report to police
    V4529 # type of crime
  )
```

    ## select: dropped 1,201 variables (V4001, V4002, V4003, V4004, V4005, …)

``` r
summary(inc_slim)
```

    ##      YEARQ                             IDHH                              IDPER     
    ##  Min.   :2021   1840783525391971564938155:  11   184078352539197156493815501:  11  
    ##  1st Qu.:2021   1998740225522643563236167:  10   199874022552264356323616702:  10  
    ##  Median :2021   2028125215026030564499142:  10   183682029235977456363634401:   9  
    ##  Mean   :2021   1785034192131022563459232:   9   189309199952428956594913501:   9  
    ##  3rd Qu.:2021   1836820292359774563636344:   9   171160359239283356443813202:   8  
    ##  Max.   :2021   1893091999524289565949135:   9   178565133359491856593411301:   8  
    ##                 (Other)                  :8924   (Other)                    :8927  
    ##      V4012          WGTVICCY           V4016                       V4017     
    ##  Min.   :1.000   Min.   :  221.6   Min.   :  1.000   (1) 1-5 incidents:8825  
    ##  1st Qu.:1.000   1st Qu.:  867.3   1st Qu.:  1.000   (2) 6 > incidents: 131  
    ##  Median :1.000   Median : 1352.3   Median :  1.000   (8) Residue      :  26  
    ##  Mean   :1.179   Mean   : 1674.9   Mean   :  4.324                           
    ##  3rd Qu.:1.000   3rd Qu.: 2217.4   3rd Qu.:  1.000                           
    ##  Max.   :7.000   Max.   :10106.2   Max.   :998.000                           
    ##                                                                              
    ##            V4018                       V4019                        V4021B    
    ##  (1) Similar  : 127   (1) Yes (not series):  10   (07) Aft 12pm-6am    :1855  
    ##  (2) Different:   4   (2) No (is series)  : 117   (09) DK day/night    :1217  
    ##  (8) Residue  :  26   (8) Residue         :  26   (02) Aft 12am-3pm    :1145  
    ##  NA's         :8825   NA's                :8829   (03) Aft 3pm-6pm     : 940  
    ##                                                   (08) DK time of night: 856  
    ##                                                   (04) DK time of day  : 833  
    ##                                                   (Other)              :2136  
    ##                   V4022                       V4024                 V4049     
    ##  (1) Outside U.S.    :  34   (05) N/hme-own yrd  :3210   (1) Yes       : 409  
    ##  (2) Not in city etc :  65   (01) R/hme-own dwell:1481   (2) No        :1803  
    ##  (3) Same city etc   :7697   (07) N/hme-on street: 727   (3) Don't know: 525  
    ##  (4) Diff city etc   :1143   (21) Open-on street : 453   (8) Residue   :   0  
    ##  (5) Don't know      :  39   (16) Park-noncomm   : 449   NA's          :6245  
    ##  (6) DK if 2, 4, or 5:   0   (06) N/hme apt hall : 429                        
    ##  (8) Residue         :   4   (Other)             :2233                        
    ##                   V4050              V4051              V4052              V4053     
    ##  (1) At least 1 entry: 380   (0) No     : 278   (0) No     : 390   (0) No     : 334  
    ##  (3) Yes weapon-NA   :  26   (1) Yes    : 131   (1) Yes    :  19   (1) Yes    :  75  
    ##  (7) Gun type unknown:   3   (8) Residue:   0   (8) Residue:   0   (8) Residue:   0  
    ##  (8) No good entry   :   0   NA's       :8573   NA's       :8573   NA's       :8573  
    ##  NA's                :8573                                                           
    ##                                                                                      
    ##                                                                                      
    ##          V4054              V4055              V4056              V4057     
    ##  (0) No     : 394   (0) No     : 302   (0) No     : 360   (0) No     : 406  
    ##  (1) Yes    :  15   (1) Yes    : 107   (1) Yes    :  49   (1) Yes    :   3  
    ##  (8) Residue:   0   (8) Residue:   0   (8) Residue:   0   (8) Residue:   0  
    ##  NA's       :8573   NA's       :8573   NA's       :8573   NA's       :8573  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                   V4058                    V4234              V4235     
    ##  (0) No out of range : 380   (1) Only one     :2076   (1) Yes    :  20  
    ##  (8) 1 > out of range:  29   (2) More than one: 353   (2) No     : 291  
    ##  NA's                :8573   (3) Don't know   : 311   (8) Residue:  87  
    ##                              (8) Residue      :  87   NA's       :8584  
    ##                              NA's             :6155                     
    ##                                                                         
    ##                                                                         
    ##                V4241                   V4242                      V4243     
    ##  (1) Knew/had seen:1176   (1) Yes         : 326   (1) Sight only     : 171  
    ##  (2) Stranger     : 793   (2) Not sure    : 240   (2) Casual acquaint: 292  
    ##  (3) Don't know   :  57   (3) No          : 271   (3) Well known     : 701  
    ##  (6) DK if 2 or 3 :   0   (6) DK if 1 or 2:   0   (6) DK know if 2, 3:   1  
    ##  (8) Residue      :  70   (8) Residue     :  83   (8) Residue        :  81  
    ##  NA's             :6886   NA's            :8062   NA's               :7736  
    ##                                                                             
    ##          V4244                         V4245          V4248                      V4256     
    ##  (1) Yes    : 307   (07) Boy/girlfrnd, ex : 149   Min.   : 2.000   (3) All strangers: 194  
    ##  (2) No     : 424   (08) Friend or ex     : 139   1st Qu.: 2.000   (1) All known    :  83  
    ##  (3) Other  :   4   (11) Neighbor         : 137   Median : 2.000   (2) Some known   :  37  
    ##  (8) Residue:  96   (13) Other nonrelative: 114   Mean   : 7.992   (4) Don't know   :  20  
    ##  NA's       :8151   (98) Residue          :  85   3rd Qu.: 3.000   (8) Residue      :  17  
    ##                     (Other)               : 451   Max.   :98.000   (Other)          :   2  
    ##                     NA's                  :7907   NA's   :8629     NA's             :8629  
    ##            V4257                       V4258              V4259              V4260     
    ##  (1) Yes      :  65   (1) At least 1 entry: 122   (0) No     :  85   (0) No     :  76  
    ##  (2) Not sure :  63   (8) No good entry   :  17   (1) Yes    :  37   (1) Yes    :  46  
    ##  (3) No       :  85   NA's                :8843   (8) Residue:  17   (8) Residue:  17  
    ##  (6) DK 1 or 2:   0                               NA's       :8843   NA's       :8843  
    ##  (8) Residue  :  18                                                                    
    ##  NA's         :8751                                                                    
    ##                                                                                        
    ##          V4261                       V4262              V4263                       V4264     
    ##  (0) No     :  77   (0) No out of range : 122   (1) Yes    :  65   (1) At least 1 entry:  87  
    ##  (1) Yes    :  45   (8) 1 > out of range:  17   (2) No     :  98   (8) No good entry   :  17  
    ##  (8) Residue:  17   NA's                :8843   (3) Other  :   0   NA's                :8878  
    ##  NA's       :8843                               (8) Residue:  18                              
    ##                                                 NA's       :8801                              
    ##                                                                                               
    ##                                                                                               
    ##          V4265              V4266              V4267              V4268     
    ##  (0) No     :  87   (0) No     :  83   (0) No     :  82   (0) No     :  84  
    ##  (1) Yes    :   0   (1) Yes    :   4   (1) Yes    :   5   (1) Yes    :   3  
    ##  (8) Residue:  17   (8) Residue:  17   (8) Residue:  17   (8) Residue:  17  
    ##  NA's       :8878   NA's       :8878   NA's       :8878   NA's       :8878  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##          V4269              V4270              V4271              V4272     
    ##  (0) No     :  84   (0) No     :  83   (0) No     :  84   (0) No     :  66  
    ##  (1) Yes    :   3   (1) Yes    :   4   (1) Yes    :   3   (1) Yes    :  21  
    ##  (8) Residue:  17   (8) Residue:  17   (8) Residue:  17   (8) Residue:  17  
    ##  NA's       :8878   NA's       :8878   NA's       :8878   NA's       :8878  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##          V4273              V4274              V4275              V4276     
    ##  (0) No     :  81   (0) No     :  84   (0) No     :  64   (0) No     :  85  
    ##  (1) Yes    :   6   (1) Yes    :   3   (1) Yes    :  23   (1) Yes    :   2  
    ##  (8) Residue:  17   (8) Residue:  17   (8) Residue:  17   (8) Residue:  17  
    ##  NA's       :8878   NA's       :8878   NA's       :8878   NA's       :8878  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##          V4277                       V4278              V4277A             V4277B    
    ##  (0) No     :  62   (0) No out of range :  85   (0) No     :  87   (0) No     :  87  
    ##  (1) Yes    :  25   (8) 1 > out of range:  19   (1) Yes    :   0   (1) Yes    :   0  
    ##  (8) Residue:  17   NA's                :8878   (8) Residue:  17   (8) Residue:  17  
    ##  NA's       :8878                               NA's       :8878   NA's       :8878  
    ##                                                                                      
    ##                                                                                      
    ##                                                                                      
    ##          V4277C             V4277D             V4277E                V4399     
    ##  (0) No     :  87   (0) No     :  84   (0) No     :  87   (1) Yes       :3175  
    ##  (1) Yes    :   0   (1) Yes    :   3   (1) Yes    :   0   (2) No        :5692  
    ##  (8) Residue:  17   (8) Residue:  17   (8) Residue:  17   (3) Don't know: 103  
    ##  NA's       :8878   NA's       :8878   NA's       :8878   (8) Residue   :  12  
    ##                                                                                
    ##                                                                                
    ##                                                                                
    ##                    V4529     
    ##  (56) Theft $50-$249  :1689  
    ##  (57) Theft $250+     :1431  
    ##  (55) Theft $10-$49   :1011  
    ##  (58) Theft value NA  : 799  
    ##  (32) Burg, ent wo for: 637  
    ##  (20) Verbal thr aslt : 609  
    ##  (Other)              :2806

``` r
write_rds(inc_slim, here("AnalysisData", "ncvs_2021_incident.rds"))
```

## Person data file

``` r
persfiles <- load(here("RawData", "NCVS_2021", "ICPSR_38429", "DS0003", "38429-0003-Data.rda"),
     verbose=TRUE)
```

    ## Loading objects:
    ##   da38429.0003

``` r
pers_in <- get(persfiles) %>%
  as_tibble()

pers_slim <- pers_in %>%
  select(
    YEARQ, IDHH, IDPER, WGTPERCY, # identifiers and weight
    V3014, V3015, V3018, V3023A, V3024, V3084, V3086 
    # age, marital status, sex, race, hispanic origin, gender, sexual orientation
  )
```

    ## select: dropped 418 variables (V3001, V3002, V3003, V3004, V3005, …)

``` r
summary(pers_slim)
```

    ##      YEARQ                             IDHH                                IDPER       
    ##  Min.   :2021   1836871692353234563244164:    20   171005136582712356493416301:     2  
    ##  1st Qu.:2021   1925751333331323563434126:    20   171005136582712356493416302:     2  
    ##  Median :2021   1955834133528905565959117:    20   171005136582712356493416303:     2  
    ##  Mean   :2021   2039603903163915564492131:    20   171005136582712356493416304:     2  
    ##  3rd Qu.:2021   2092801999265729563608161:    20   171005192538805556494916301:     2  
    ##  Max.   :2021   1711814192521784563459133:    18   171005392545800456494413301:     2  
    ##                 (Other)                  :291760   (Other)                    :291866  
    ##     WGTPERCY           V3014                     V3015                V3018       
    ##  Min.   :    0.0   Min.   :12.00   (1) Married      :148131   (1) Male   :140922  
    ##  1st Qu.:  432.2   1st Qu.:31.00   (2) Widowed      : 17668   (2) Female :150956  
    ##  Median :  791.5   Median :48.00   (3) Divorced     : 28596   (8) Residue:     0  
    ##  Mean   :  956.5   Mean   :47.57   (4) Separated    :  4524                       
    ##  3rd Qu.: 1397.4   3rd Qu.:64.00   (5) Never married: 90425                       
    ##  Max.   :10691.5   Max.   :90.00   (8) Residue      :  2534                       
    ##                                                                                   
    ##                         V3023A               V3024       
    ##  (01) White only           :236785   (1) Yes    : 41450  
    ##  (02) Black only           : 30972   (2) No     :249306  
    ##  (04) Asian only           : 16337   (8) Residue:  1122  
    ##  (03) Am Ind/AK native only:  1776                       
    ##  (06) White-Black          :  1590                       
    ##  (07) White-Amer Ind       :  1465                       
    ##  (Other)                   :  2953                       
    ##                                        V3084                      V3086       
    ##  (8) Residue                              :151725   (1) Male         : 29733  
    ##  (2) Straight, that is, not lesbian or gay: 61108   (2) Female       : 34489  
    ##  (6) Refused                              :  1477   (3) Transgender  :    56  
    ##  (1) Lesbian or gay                       :   924   (4) None of these:   115  
    ##  (3) Bisexual                             :   611   (8) Residue      :151894  
    ##  (Other)                                  :   442   NA's             : 75591  
    ##  NA's                                     : 75591

``` r
write_rds(pers_slim, here("AnalysisData", "ncvs_2021_person.rds"))
```

## Household data file

``` r
hhfiles <- load(here("RawData", "NCVS_2021", "ICPSR_38429", "DS0002", "38429-0002-Data.rda"),
     verbose=TRUE)
```

    ## Loading objects:
    ##   da38429.0002

``` r
hh_in <- get(hhfiles) %>%
  as_tibble()

hh_slim <- hh_in %>%
  select(
    YEARQ, IDHH, WGTHHCY, V2117, V2118, # identifiers, weight, design
    V2015, V2143, SC214A, V2122, V2126B, V2127B, V2129
    # tenure, urbanicity, income, family structure, place size, region, msa status
  )
```

    ## select: dropped 440 variables (V2001, V2002, V2003, V2004, V2005, …)

``` r
summary(hh_slim)
```

    ##      YEARQ                             IDHH           WGTHHCY           V2117       
    ##  Min.   :2021   1710051325461935564934133:     2   Min.   :   0.0   Min.   :  1.00  
    ##  1st Qu.:2021   1710051365827123564934163:     2   1st Qu.:   0.0   1st Qu.: 24.00  
    ##  Median :2021   1710051925388055564949163:     2   Median : 399.4   Median : 48.00  
    ##  Mean   :2021   1710051925567116564949143:     2   Mean   : 504.2   Mean   : 58.85  
    ##  3rd Qu.:2021   1710053925458004564944133:     2   3rd Qu.: 829.1   3rd Qu.: 88.00  
    ##  Max.   :2021   1710053965525445564944113:     2   Max.   :4515.8   Max.   :160.00  
    ##                 (Other)                  :256448                                    
    ##      V2118                        V2015                 V2143       
    ##  Min.   :1.000   (1) Owned/being bght:101944   (1) Urban   : 26878  
    ##  1st Qu.:1.000   (2) Rented for cash : 46269   (2) Suburban:173491  
    ##  Median :2.000   (3) No cash rent    :  1925   (3) Rural   : 56091  
    ##  Mean   :1.526   (8) Residue         :     0   (8) Residue :     0  
    ##  3rd Qu.:2.000   NA's                :106322                        
    ##  Max.   :3.000                                                      
    ##                                                                     
    ##                      SC214A      
    ##  (13) $50,000 to $74,999: 44601  
    ##  (16) $100,000-$149,999 : 34287  
    ##  (15) $75,000 to $99,999: 33353  
    ##  (12) $40,000 to $49,999: 23282  
    ##  (18) $200,000 or more  : 16892  
    ##  (Other)                :101364  
    ##  NA's                   :  2681  
    ##                                                                V2122       
    ##  (33) Other combinations                                          :106322  
    ##  (32) Lone female reference person only                           : 28306  
    ##  (16) Lone male reference person only                             : 23617  
    ##  (08) Male reference person, married female partner only          : 21383  
    ##  (24) Female reference person, married male partner only          : 15629  
    ##  (04) Male reference person, married female partner, children only: 10477  
    ##  (Other)                                                          : 50726  
    ##                   V2126B                V2127B                      V2129       
    ##  (00) Not in a place :69484   (1) Northeast:41585   (1) City of (S)MSA : 80895  
    ##  (16) 10,000-49,999  :53002   (2) Midwest  :74666   (2) (S)MSA not city:135438  
    ##  (13) Under 10,000   :39873   (3) South    :87783   (3) Not (S)MSA     : 40127  
    ##  (17) 50,000-99,999  :27205   (4) West     :52426   (8) Residue        :     0  
    ##  (18) 100,000-249,999:24461                                                     
    ##  (20) 500,000-999,999:15194                                                     
    ##  (Other)             :27241

``` r
write_rds(hh_slim, here("AnalysisData", "ncvs_2021_household.rds"))
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
