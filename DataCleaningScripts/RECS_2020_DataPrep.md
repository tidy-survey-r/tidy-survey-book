Residential Energy Consumption Survey (RECS) 2020 Data Prep
================

## Data information

All data and resources were downloaded from
<https://www.eia.gov/consumption/residential/data/2020/index.php?view=microdata>
on September 17, 2023.

``` r
library(tidyverse) #data manipulation
library(haven) #data import
library(tidylog) #informative logging messages
library(osfr)
```

## Import data and create derived variables

``` r
recs_file_osf_det <- osf_retrieve_node("https://osf.io/z5c3m/") %>%
  osf_ls_files(path="RECS_2020", pattern="csv") %>%
  filter(str_detect(name, "v5")) %>%
  osf_download(conflicts="overwrite", path=here::here("osf_dl"))
```

    ## filter: removed one row (50%), one row remaining

``` r
recs_in <- read_csv(pull(recs_file_osf_det, local_path))
```

    ## Rows: 18496 Columns: 789

    ## ── Column specification ──────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (8): REGIONC, DIVISION, STATE_FIPS, state_postal, state_name, BA_climate, IECC_climate_code,...
    ## dbl (781): DOEID, HDD65, CDD65, HDD30YR_PUB, CDD30YR_PUB, TYPEHUQ, CELLAR, CRAWL, CONCRETE, BASEOT...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unlink(pull(recs_file_osf_det, local_path))


# 2015 to 2020 differences
# Added states!
# Variables gone: METROMICRO, TOTUCSQFT (uncooled sq ft), TOTUSQFT (unheated sq ft), CDD80, HDD50, GNDHDD65, PELLETBTU
# HEATCNTL replaces EQUIPMUSE
# COOLCNTL replaces USECENAC
# CDD30YR_PUB replaces CDD30YR
# BA_climate replaces CLIMATE_REGION_PUB 
# IECC_climate_code replaces IECC_CLIMATE_PUB
# HDD30YR_PUB replaces HDD30YR
# BTUWD replaces WOODBTU
# BRR weights are NWEIGHT

recs <- recs_in %>%
   select(DOEID, REGIONC, DIVISION, STATE_FIPS, state_postal, state_name, UATYP10, TYPEHUQ, YEARMADERANGE, HEATHOME, HEATCNTL, TEMPHOME, TEMPGONE, TEMPNITE, AIRCOND, COOLCNTL, TEMPHOMEAC, TEMPGONEAC, TEMPNITEAC, TOTCSQFT, TOTHSQFT, TOTSQFT_EN, NWEIGHT, starts_with("NWEIGHT"), CDD30YR=CDD30YR_PUB, CDD65, BA_climate, IECC_climate_code, HDD30YR=HDD30YR_PUB, HDD65, BTUEL, DOLLAREL, BTUNG, DOLLARNG, BTULP, DOLLARLP, BTUFO, DOLLARFO, TOTALBTU, TOTALDOL, BTUWOOD=BTUWD) %>%
  mutate(
    Region=parse_factor(
      str_to_title(REGIONC),
      levels=c("Northeast", "Midwest", "South", "West")),
    Division=parse_factor(
      DIVISION, levels=c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain North", "Mountain South", "Pacific")),
    Urbanicity=parse_factor(
      case_when(
        UATYP10=="U"~"Urban Area",
        UATYP10=="C"~"Urban Cluster",
        UATYP10=="R"~"Rural"
      ),
      levels=c("Urban Area", "Urban Cluster", "Rural")
    ),
    HousingUnitType=parse_factor(
      case_when(
        TYPEHUQ==1~"Mobile home",
        TYPEHUQ==2~"Single-family detached",
        TYPEHUQ==3~"Single-family attached",
        TYPEHUQ==4~"Apartment: 2-4 Units",
        TYPEHUQ==5~"Apartment: 5 or more units",
      ), levels=c("Mobile home", "Single-family detached", "Single-family attached", "Apartment: 2-4 Units", "Apartment: 5 or more units")),
    YearMade=parse_factor(
      case_when(
        YEARMADERANGE==1~"Before 1950",
        YEARMADERANGE==2~"1950-1959",
        YEARMADERANGE==3~"1960-1969",
        YEARMADERANGE==4~"1970-1979",
        YEARMADERANGE==5~"1980-1989",
        YEARMADERANGE==6~"1990-1999",
        YEARMADERANGE==7~"2000-2009",
        YEARMADERANGE==8~"2010-2015",
        YEARMADERANGE==9~"2016-2020"
      ),
      levels=c("Before 1950", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2015", "2016-2020"),
      ordered = TRUE
    ),
    SpaceHeatingUsed=as.logical(HEATHOME),
    HeatingBehavior=parse_factor(
      case_when(
        HEATCNTL==1~"Set one temp and leave it",
        HEATCNTL==2~"Manually adjust at night/no one home",
        HEATCNTL==3~"Programmable or smart thermostat automatically adjusts the temperature",
        HEATCNTL==4~"Turn on or off as needed",
        HEATCNTL==5~"No control",
        HEATCNTL==99~"Other",
        HEATCNTL==-2~NA_character_),
      levels=c("Set one temp and leave it", "Manually adjust at night/no one home", "Programmable or smart thermostat automatically adjusts the temperature", "Turn on or off as needed", "No control", "Other")
    ),
    WinterTempDay=if_else(TEMPHOME>0, TEMPHOME, NA_real_),
    WinterTempAway=if_else(TEMPGONE>0, TEMPGONE, NA_real_),
    WinterTempNight=if_else(TEMPNITE>0, TEMPNITE, NA_real_),
    ACUsed=as.logical(AIRCOND),
    ACBehavior=parse_factor(
      case_when(
        COOLCNTL==1~"Set one temp and leave it",
        COOLCNTL==2~"Manually adjust at night/no one home",
        COOLCNTL==3~"Programmable or smart thermostat automatically adjusts the temperature",
        COOLCNTL==4~"Turn on or off as needed",
        COOLCNTL==5~"No control",
        COOLCNTL==99~"Other",
        COOLCNTL==-2~NA_character_),
      levels=c("Set one temp and leave it", "Manually adjust at night/no one home", "Programmable or smart thermostat automatically adjusts the temperature", "Turn on or off as needed", "No control", "Other")
    ),
    SummerTempDay=if_else(TEMPHOMEAC>0, TEMPHOMEAC, NA_real_),
    SummerTempAway=if_else(TEMPGONEAC>0, TEMPGONEAC, NA_real_),
    SummerTempNight=if_else(TEMPNITEAC>0, TEMPNITEAC, NA_real_),
    ClimateRegion_BA=parse_factor(BA_climate),
    state_name=factor(state_name),
    state_postal=fct_reorder(state_postal, as.numeric(state_name))
    )
```

    ## select: renamed 3 variables (CDD30YR, HDD30YR, BTUWOOD) and dropped 689 variables
    ## mutate: converted 'state_postal' from character to factor (0 new NA)
    ##         converted 'state_name' from character to factor (0 new NA)
    ##         new variable 'Region' (factor) with 4 unique values and 0% NA
    ##         new variable 'Division' (factor) with 10 unique values and 0% NA
    ##         new variable 'Urbanicity' (factor) with 3 unique values and 0% NA
    ##         new variable 'HousingUnitType' (factor) with 5 unique values and 0% NA
    ##         new variable 'YearMade' (ordered factor) with 9 unique values and 0% NA
    ##         new variable 'SpaceHeatingUsed' (logical) with 2 unique values and 0% NA
    ##         new variable 'HeatingBehavior' (factor) with 7 unique values and 0% NA
    ##         new variable 'WinterTempDay' (double) with 38 unique values and 4% NA
    ##         new variable 'WinterTempAway' (double) with 40 unique values and 4% NA
    ##         new variable 'WinterTempNight' (double) with 42 unique values and 4% NA
    ##         new variable 'ACUsed' (logical) with 2 unique values and 0% NA
    ##         new variable 'ACBehavior' (factor) with 7 unique values and 0% NA
    ##         new variable 'SummerTempDay' (double) with 42 unique values and 13% NA
    ##         new variable 'SummerTempAway' (double) with 41 unique values and 13% NA
    ##         new variable 'SummerTempNight' (double) with 40 unique values and 13% NA
    ##         new variable 'ClimateRegion_BA' (factor) with 8 unique values and 0% NA

## Check derived variables for correct coding

``` r
recs %>% count(Region, REGIONC)
```

    ## count: now 4 rows and 3 columns, ungrouped

    ## # A tibble: 4 × 3
    ##   Region    REGIONC       n
    ##   <fct>     <chr>     <int>
    ## 1 Northeast NORTHEAST  3657
    ## 2 Midwest   MIDWEST    3832
    ## 3 South     SOUTH      6426
    ## 4 West      WEST       4581

``` r
recs %>% count(Division, DIVISION)
```

    ## count: now 10 rows and 3 columns, ungrouped

    ## # A tibble: 10 × 3
    ##    Division           DIVISION               n
    ##    <fct>              <chr>              <int>
    ##  1 New England        New England         1680
    ##  2 Middle Atlantic    Middle Atlantic     1977
    ##  3 East North Central East North Central  2014
    ##  4 West North Central West North Central  1818
    ##  5 South Atlantic     South Atlantic      3256
    ##  6 East South Central East South Central  1343
    ##  7 West South Central West South Central  1827
    ##  8 Mountain North     Mountain North      1180
    ##  9 Mountain South     Mountain South       904
    ## 10 Pacific            Pacific             2497

``` r
recs %>% count(Urbanicity, UATYP10)
```

    ## count: now 3 rows and 3 columns, ungrouped

    ## # A tibble: 3 × 3
    ##   Urbanicity    UATYP10     n
    ##   <fct>         <chr>   <int>
    ## 1 Urban Area    U       12395
    ## 2 Urban Cluster C        2020
    ## 3 Rural         R        4081

``` r
recs %>% count(HousingUnitType, TYPEHUQ)
```

    ## count: now 5 rows and 3 columns, ungrouped

    ## # A tibble: 5 × 3
    ##   HousingUnitType            TYPEHUQ     n
    ##   <fct>                        <dbl> <int>
    ## 1 Mobile home                      1   974
    ## 2 Single-family detached           2 12319
    ## 3 Single-family attached           3  1751
    ## 4 Apartment: 2-4 Units             4  1013
    ## 5 Apartment: 5 or more units       5  2439

``` r
recs %>% count(YearMade, YEARMADERANGE)
```

    ## count: now 9 rows and 3 columns, ungrouped

    ## # A tibble: 9 × 3
    ##   YearMade    YEARMADERANGE     n
    ##   <ord>               <dbl> <int>
    ## 1 Before 1950             1  2721
    ## 2 1950-1959               2  1685
    ## 3 1960-1969               3  1867
    ## 4 1970-1979               4  2817
    ## 5 1980-1989               5  2435
    ## 6 1990-1999               6  2451
    ## 7 2000-2009               7  2748
    ## 8 2010-2015               8   989
    ## 9 2016-2020               9   783

``` r
recs %>% count(SpaceHeatingUsed, HEATHOME)
```

    ## count: now 2 rows and 3 columns, ungrouped

    ## # A tibble: 2 × 3
    ##   SpaceHeatingUsed HEATHOME     n
    ##   <lgl>               <dbl> <int>
    ## 1 FALSE                   0   751
    ## 2 TRUE                    1 17745

``` r
recs %>% count(HeatingBehavior, HEATCNTL)
```

    ## count: now 7 rows and 3 columns, ungrouped

    ## # A tibble: 7 × 3
    ##   HeatingBehavior                                                        HEATCNTL     n
    ##   <fct>                                                                     <dbl> <int>
    ## 1 Set one temp and leave it                                                     1  7806
    ## 2 Manually adjust at night/no one home                                          2  4654
    ## 3 Programmable or smart thermostat automatically adjusts the temperature        3  3310
    ## 4 Turn on or off as needed                                                      4  1491
    ## 5 No control                                                                    5   438
    ## 6 Other                                                                        99    46
    ## 7 <NA>                                                                         -2   751

``` r
recs %>% count(ACUsed, AIRCOND)
```

    ## count: now 2 rows and 3 columns, ungrouped

    ## # A tibble: 2 × 3
    ##   ACUsed AIRCOND     n
    ##   <lgl>    <dbl> <int>
    ## 1 FALSE        0  2325
    ## 2 TRUE         1 16171

``` r
recs %>% count(ACBehavior, COOLCNTL)
```

    ## count: now 7 rows and 3 columns, ungrouped

    ## # A tibble: 7 × 3
    ##   ACBehavior                                                             COOLCNTL     n
    ##   <fct>                                                                     <dbl> <int>
    ## 1 Set one temp and leave it                                                     1  6738
    ## 2 Manually adjust at night/no one home                                          2  3637
    ## 3 Programmable or smart thermostat automatically adjusts the temperature        3  2638
    ## 4 Turn on or off as needed                                                      4  2746
    ## 5 No control                                                                    5   409
    ## 6 Other                                                                        99     3
    ## 7 <NA>                                                                         -2  2325

``` r
recs %>% count(ClimateRegion_BA, BA_climate)
```

    ## count: now 8 rows and 3 columns, ungrouped

    ## # A tibble: 8 × 3
    ##   ClimateRegion_BA BA_climate      n
    ##   <fct>            <chr>       <int>
    ## 1 Mixed-Dry        Mixed-Dry     142
    ## 2 Mixed-Humid      Mixed-Humid  5579
    ## 3 Hot-Humid        Hot-Humid    2545
    ## 4 Hot-Dry          Hot-Dry      1577
    ## 5 Very-Cold        Very-Cold     572
    ## 6 Cold             Cold         7116
    ## 7 Marine           Marine        911
    ## 8 Subarctic        Subarctic      54

``` r
recs %>% count(state_postal, state_name, STATE_FIPS) %>% print(n=51)
```

    ## count: now 51 rows and 4 columns, ungrouped

    ## # A tibble: 51 × 4
    ##    state_postal state_name           STATE_FIPS     n
    ##    <fct>        <fct>                <chr>      <int>
    ##  1 AL           Alabama              01           242
    ##  2 AK           Alaska               02           311
    ##  3 AZ           Arizona              04           495
    ##  4 AR           Arkansas             05           268
    ##  5 CA           California           06          1152
    ##  6 CO           Colorado             08           360
    ##  7 CT           Connecticut          09           294
    ##  8 DE           Delaware             10           143
    ##  9 DC           District of Columbia 11           221
    ## 10 FL           Florida              12           655
    ## 11 GA           Georgia              13           417
    ## 12 HI           Hawaii               15           282
    ## 13 ID           Idaho                16           270
    ## 14 IL           Illinois             17           530
    ## 15 IN           Indiana              18           400
    ## 16 IA           Iowa                 19           286
    ## 17 KS           Kansas               20           208
    ## 18 KY           Kentucky             21           428
    ## 19 LA           Louisiana            22           311
    ## 20 ME           Maine                23           223
    ## 21 MD           Maryland             24           359
    ## 22 MA           Massachusetts        25           552
    ## 23 MI           Michigan             26           388
    ## 24 MN           Minnesota            27           325
    ## 25 MS           Mississippi          28           168
    ## 26 MO           Missouri             29           296
    ## 27 MT           Montana              30           172
    ## 28 NE           Nebraska             31           189
    ## 29 NV           Nevada               32           231
    ## 30 NH           New Hampshire        33           175
    ## 31 NJ           New Jersey           34           456
    ## 32 NM           New Mexico           35           178
    ## 33 NY           New York             36           904
    ## 34 NC           North Carolina       37           479
    ## 35 ND           North Dakota         38           331
    ## 36 OH           Ohio                 39           339
    ## 37 OK           Oklahoma             40           232
    ## 38 OR           Oregon               41           313
    ## 39 PA           Pennsylvania         42           617
    ## 40 RI           Rhode Island         44           191
    ## 41 SC           South Carolina       45           334
    ## 42 SD           South Dakota         46           183
    ## 43 TN           Tennessee            47           505
    ## 44 TX           Texas                48          1016
    ## 45 UT           Utah                 49           188
    ## 46 VT           Vermont              50           245
    ## 47 VA           Virginia             51           451
    ## 48 WA           Washington           53           439
    ## 49 WV           West Virginia        54           197
    ## 50 WI           Wisconsin            55           357
    ## 51 WY           Wyoming              56           190

## Save data

``` r
recs_out <- recs %>%
  select(DOEID, REGIONC, Region, Division, starts_with("state"), Urbanicity, 
         HousingUnitType, YearMade, SpaceHeatingUsed, HeatingBehavior, 
         WinterTempDay, WinterTempAway, WinterTempNight, ACUsed, 
         ACBehavior, SummerTempDay, SummerTempAway, SummerTempNight, 
         TOTCSQFT, TOTHSQFT, TOTSQFT_EN, NWEIGHT, 
         starts_with("NWEIGHT"), CDD30YR, CDD65, ClimateRegion_BA, 
         HDD30YR, HDD65, BTUEL, 
         DOLLAREL, BTUNG, DOLLARNG, BTULP, DOLLARLP, BTUFO, DOLLARFO, 
         TOTALBTU, TOTALDOL, BTUWOOD)
```

    ## select: dropped 16 variables (DIVISION, UATYP10, TYPEHUQ, YEARMADERANGE, HEATHOME, …)

``` r
source(here::here("helper-fun", "helper-function.R"))

recs_2015 <- read_osf("recs_2015.rds")

setdiff(names(recs_out), names(recs_2015)) #variables in 2020 and not 2015
```

    ##  [1] "STATE_FIPS"   "state_postal" "state_name"   "NWEIGHT1"     "NWEIGHT2"     "NWEIGHT3"    
    ##  [7] "NWEIGHT4"     "NWEIGHT5"     "NWEIGHT6"     "NWEIGHT7"     "NWEIGHT8"     "NWEIGHT9"    
    ## [13] "NWEIGHT10"    "NWEIGHT11"    "NWEIGHT12"    "NWEIGHT13"    "NWEIGHT14"    "NWEIGHT15"   
    ## [19] "NWEIGHT16"    "NWEIGHT17"    "NWEIGHT18"    "NWEIGHT19"    "NWEIGHT20"    "NWEIGHT21"   
    ## [25] "NWEIGHT22"    "NWEIGHT23"    "NWEIGHT24"    "NWEIGHT25"    "NWEIGHT26"    "NWEIGHT27"   
    ## [31] "NWEIGHT28"    "NWEIGHT29"    "NWEIGHT30"    "NWEIGHT31"    "NWEIGHT32"    "NWEIGHT33"   
    ## [37] "NWEIGHT34"    "NWEIGHT35"    "NWEIGHT36"    "NWEIGHT37"    "NWEIGHT38"    "NWEIGHT39"   
    ## [43] "NWEIGHT40"    "NWEIGHT41"    "NWEIGHT42"    "NWEIGHT43"    "NWEIGHT44"    "NWEIGHT45"   
    ## [49] "NWEIGHT46"    "NWEIGHT47"    "NWEIGHT48"    "NWEIGHT49"    "NWEIGHT50"    "NWEIGHT51"   
    ## [55] "NWEIGHT52"    "NWEIGHT53"    "NWEIGHT54"    "NWEIGHT55"    "NWEIGHT56"    "NWEIGHT57"   
    ## [61] "NWEIGHT58"    "NWEIGHT59"    "NWEIGHT60"

``` r
setdiff(names(recs_2015), names(recs_out)) #variables in 2015 and not 2020
```

    ##   [1] "MSAStatus"          "TOTUCSQFT"          "TOTUSQFT"           "BRRWT1"            
    ##   [5] "BRRWT2"             "BRRWT3"             "BRRWT4"             "BRRWT5"            
    ##   [9] "BRRWT6"             "BRRWT7"             "BRRWT8"             "BRRWT9"            
    ##  [13] "BRRWT10"            "BRRWT11"            "BRRWT12"            "BRRWT13"           
    ##  [17] "BRRWT14"            "BRRWT15"            "BRRWT16"            "BRRWT17"           
    ##  [21] "BRRWT18"            "BRRWT19"            "BRRWT20"            "BRRWT21"           
    ##  [25] "BRRWT22"            "BRRWT23"            "BRRWT24"            "BRRWT25"           
    ##  [29] "BRRWT26"            "BRRWT27"            "BRRWT28"            "BRRWT29"           
    ##  [33] "BRRWT30"            "BRRWT31"            "BRRWT32"            "BRRWT33"           
    ##  [37] "BRRWT34"            "BRRWT35"            "BRRWT36"            "BRRWT37"           
    ##  [41] "BRRWT38"            "BRRWT39"            "BRRWT40"            "BRRWT41"           
    ##  [45] "BRRWT42"            "BRRWT43"            "BRRWT44"            "BRRWT45"           
    ##  [49] "BRRWT46"            "BRRWT47"            "BRRWT48"            "BRRWT49"           
    ##  [53] "BRRWT50"            "BRRWT51"            "BRRWT52"            "BRRWT53"           
    ##  [57] "BRRWT54"            "BRRWT55"            "BRRWT56"            "BRRWT57"           
    ##  [61] "BRRWT58"            "BRRWT59"            "BRRWT60"            "BRRWT61"           
    ##  [65] "BRRWT62"            "BRRWT63"            "BRRWT64"            "BRRWT65"           
    ##  [69] "BRRWT66"            "BRRWT67"            "BRRWT68"            "BRRWT69"           
    ##  [73] "BRRWT70"            "BRRWT71"            "BRRWT72"            "BRRWT73"           
    ##  [77] "BRRWT74"            "BRRWT75"            "BRRWT76"            "BRRWT77"           
    ##  [81] "BRRWT78"            "BRRWT79"            "BRRWT80"            "BRRWT81"           
    ##  [85] "BRRWT82"            "BRRWT83"            "BRRWT84"            "BRRWT85"           
    ##  [89] "BRRWT86"            "BRRWT87"            "BRRWT88"            "BRRWT89"           
    ##  [93] "BRRWT90"            "BRRWT91"            "BRRWT92"            "BRRWT93"           
    ##  [97] "BRRWT94"            "BRRWT95"            "BRRWT96"            "CDD80"             
    ## [101] "ClimateRegion_IECC" "HDD50"              "GNDHDD65"           "BTUPELLET"

``` r
summary(recs_out)
```

    ##      DOEID          REGIONC                Region                   Division     STATE_FIPS       
    ##  Min.   :100001   Length:18496       Northeast:3657   South Atlantic    :3256   Length:18496      
    ##  1st Qu.:104625   Class :character   Midwest  :3832   Pacific           :2497   Class :character  
    ##  Median :109249   Mode  :character   South    :6426   East North Central:2014   Mode  :character  
    ##  Mean   :109249                      West     :4581   Middle Atlantic   :1977                     
    ##  3rd Qu.:113872                                       West South Central:1827                     
    ##  Max.   :118496                                       West North Central:1818                     
    ##                                                       (Other)           :5107                     
    ##   state_postal           state_name            Urbanicity                      HousingUnitType 
    ##  CA     : 1152   California   : 1152   Urban Area   :12395   Mobile home               :  974  
    ##  TX     : 1016   Texas        : 1016   Urban Cluster: 2020   Single-family detached    :12319  
    ##  NY     :  904   New York     :  904   Rural        : 4081   Single-family attached    : 1751  
    ##  FL     :  655   Florida      :  655                         Apartment: 2-4 Units      : 1013  
    ##  PA     :  617   Pennsylvania :  617                         Apartment: 5 or more units: 2439  
    ##  MA     :  552   Massachusetts:  552                                                           
    ##  (Other):13600   (Other)      :13600                                                           
    ##         YearMade    SpaceHeatingUsed
    ##  1970-1979  :2817   Mode :logical   
    ##  2000-2009  :2748   FALSE:751       
    ##  Before 1950:2721   TRUE :17745     
    ##  1990-1999  :2451                   
    ##  1980-1989  :2435                   
    ##  1960-1969  :1867                   
    ##  (Other)    :3457                   
    ##                                                                HeatingBehavior WinterTempDay  
    ##  Set one temp and leave it                                             :7806   Min.   :50.00  
    ##  Manually adjust at night/no one home                                  :4654   1st Qu.:68.00  
    ##  Programmable or smart thermostat automatically adjusts the temperature:3310   Median :70.00  
    ##  Turn on or off as needed                                              :1491   Mean   :69.77  
    ##  No control                                                            : 438   3rd Qu.:72.00  
    ##  Other                                                                 :  46   Max.   :90.00  
    ##  NA                                                                    : 751   NA's   :751    
    ##  WinterTempAway  WinterTempNight   ACUsed       
    ##  Min.   :50.00   Min.   :50.00   Mode :logical  
    ##  1st Qu.:65.00   1st Qu.:65.00   FALSE:2325     
    ##  Median :68.00   Median :68.00   TRUE :16171    
    ##  Mean   :67.45   Mean   :68.01                  
    ##  3rd Qu.:70.00   3rd Qu.:70.00                  
    ##  Max.   :90.00   Max.   :90.00                  
    ##  NA's   :751     NA's   :751                    
    ##                                                                   ACBehavior   SummerTempDay  
    ##  Set one temp and leave it                                             :6738   Min.   :50.00  
    ##  Manually adjust at night/no one home                                  :3637   1st Qu.:70.00  
    ##  Programmable or smart thermostat automatically adjusts the temperature:2638   Median :72.00  
    ##  Turn on or off as needed                                              :2746   Mean   :72.01  
    ##  No control                                                            : 409   3rd Qu.:75.00  
    ##  Other                                                                 :   3   Max.   :90.00  
    ##  NA                                                                    :2325   NA's   :2325   
    ##  SummerTempAway  SummerTempNight    TOTCSQFT        TOTHSQFT       TOTSQFT_EN       NWEIGHT       
    ##  Min.   :50.00   Min.   :50.00   Min.   :    0   Min.   :    0   Min.   :  200   Min.   :  437.9  
    ##  1st Qu.:70.00   1st Qu.:68.00   1st Qu.:  460   1st Qu.: 1000   1st Qu.: 1100   1st Qu.: 4018.7  
    ##  Median :74.00   Median :72.00   Median : 1200   Median : 1520   Median : 1700   Median : 6119.4  
    ##  Mean   :73.45   Mean   :71.22   Mean   : 1394   Mean   : 1744   Mean   : 1960   Mean   : 6678.7  
    ##  3rd Qu.:78.00   3rd Qu.:74.00   3rd Qu.: 2000   3rd Qu.: 2300   3rd Qu.: 2510   3rd Qu.: 8890.0  
    ##  Max.   :90.00   Max.   :90.00   Max.   :14600   Max.   :15000   Max.   :15000   Max.   :29279.1  
    ##  NA's   :2325    NA's   :2325                                                                     
    ##     NWEIGHT1        NWEIGHT2        NWEIGHT3        NWEIGHT4        NWEIGHT5        NWEIGHT6    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3950   1st Qu.: 3951   1st Qu.: 3954   1st Qu.: 3953   1st Qu.: 3957   1st Qu.: 3966  
    ##  Median : 6136   Median : 6151   Median : 6151   Median : 6153   Median : 6134   Median : 6147  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8976   3rd Qu.: 8979   3rd Qu.: 8994   3rd Qu.: 8998   3rd Qu.: 8987   3rd Qu.: 8984  
    ##  Max.   :30015   Max.   :29422   Max.   :29431   Max.   :29494   Max.   :30039   Max.   :29419  
    ##                                                                                                 
    ##     NWEIGHT7        NWEIGHT8        NWEIGHT9       NWEIGHT10       NWEIGHT11       NWEIGHT12    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3944   1st Qu.: 3956   1st Qu.: 3947   1st Qu.: 3961   1st Qu.: 3950   1st Qu.: 3947  
    ##  Median : 6135   Median : 6151   Median : 6139   Median : 6163   Median : 6140   Median : 6160  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8998   3rd Qu.: 8988   3rd Qu.: 8974   3rd Qu.: 8994   3rd Qu.: 8991   3rd Qu.: 8988  
    ##  Max.   :29586   Max.   :29499   Max.   :29845   Max.   :29635   Max.   :29681   Max.   :29849  
    ##                                                                                                 
    ##    NWEIGHT13       NWEIGHT14       NWEIGHT15       NWEIGHT16       NWEIGHT17       NWEIGHT18    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3967   1st Qu.: 3962   1st Qu.: 3958   1st Qu.: 3958   1st Qu.: 3958   1st Qu.: 3937  
    ##  Median : 6142   Median : 6154   Median : 6145   Median : 6133   Median : 6126   Median : 6155  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8977   3rd Qu.: 8981   3rd Qu.: 8997   3rd Qu.: 8979   3rd Qu.: 8977   3rd Qu.: 8993  
    ##  Max.   :29843   Max.   :30184   Max.   :29970   Max.   :29825   Max.   :30606   Max.   :29689  
    ##                                                                                                 
    ##    NWEIGHT19       NWEIGHT20       NWEIGHT21       NWEIGHT22       NWEIGHT23       NWEIGHT24    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3947   1st Qu.: 3943   1st Qu.: 3960   1st Qu.: 3964   1st Qu.: 3943   1st Qu.: 3946  
    ##  Median : 6153   Median : 6139   Median : 6135   Median : 6149   Median : 6148   Median : 6136  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8979   3rd Qu.: 8992   3rd Qu.: 8956   3rd Qu.: 8988   3rd Qu.: 8980   3rd Qu.: 8978  
    ##  Max.   :29336   Max.   :30274   Max.   :29766   Max.   :29791   Max.   :30126   Max.   :29946  
    ##                                                                                                 
    ##    NWEIGHT25       NWEIGHT26       NWEIGHT27       NWEIGHT28       NWEIGHT29       NWEIGHT30    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3952   1st Qu.: 3966   1st Qu.: 3942   1st Qu.: 3956   1st Qu.: 3970   1st Qu.: 3956  
    ##  Median : 6150   Median : 6136   Median : 6125   Median : 6149   Median : 6146   Median : 6149  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8972   3rd Qu.: 8980   3rd Qu.: 8996   3rd Qu.: 8989   3rd Qu.: 8979   3rd Qu.: 8991  
    ##  Max.   :30445   Max.   :29893   Max.   :30030   Max.   :29599   Max.   :30136   Max.   :29895  
    ##                                                                                                 
    ##    NWEIGHT31       NWEIGHT32       NWEIGHT33       NWEIGHT34       NWEIGHT35       NWEIGHT36    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3944   1st Qu.: 3954   1st Qu.: 3964   1st Qu.: 3950   1st Qu.: 3967   1st Qu.: 3948  
    ##  Median : 6144   Median : 6159   Median : 6148   Median : 6139   Median : 6141   Median : 6149  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8994   3rd Qu.: 8982   3rd Qu.: 8993   3rd Qu.: 8985   3rd Qu.: 8990   3rd Qu.: 8979  
    ##  Max.   :29604   Max.   :29310   Max.   :29408   Max.   :29564   Max.   :30437   Max.   :27896  
    ##                                                                                                 
    ##    NWEIGHT37       NWEIGHT38       NWEIGHT39       NWEIGHT40       NWEIGHT41       NWEIGHT42    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3955   1st Qu.: 3954   1st Qu.: 3940   1st Qu.: 3959   1st Qu.: 3975   1st Qu.: 3949  
    ##  Median : 6133   Median : 6139   Median : 6147   Median : 6144   Median : 6153   Median : 6137  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8975   3rd Qu.: 8974   3rd Qu.: 8991   3rd Qu.: 8980   3rd Qu.: 8982   3rd Qu.: 8988  
    ##  Max.   :30596   Max.   :30130   Max.   :29262   Max.   :30344   Max.   :29594   Max.   :29938  
    ##                                                                                                 
    ##    NWEIGHT43       NWEIGHT44       NWEIGHT45       NWEIGHT46       NWEIGHT47       NWEIGHT48    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3947   1st Qu.: 3956   1st Qu.: 3952   1st Qu.: 3966   1st Qu.: 3938   1st Qu.: 3953  
    ##  Median : 6157   Median : 6148   Median : 6149   Median : 6152   Median : 6150   Median : 6139  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 9005   3rd Qu.: 8986   3rd Qu.: 8992   3rd Qu.: 8959   3rd Qu.: 8991   3rd Qu.: 8991  
    ##  Max.   :29878   Max.   :29896   Max.   :29729   Max.   :29103   Max.   :30070   Max.   :29343  
    ##                                                                                                 
    ##    NWEIGHT49       NWEIGHT50       NWEIGHT51       NWEIGHT52       NWEIGHT53       NWEIGHT54    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3947   1st Qu.: 3948   1st Qu.: 3958   1st Qu.: 3938   1st Qu.: 3959   1st Qu.: 3954  
    ##  Median : 6146   Median : 6159   Median : 6150   Median : 6154   Median : 6156   Median : 6151  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8990   3rd Qu.: 8995   3rd Qu.: 8992   3rd Qu.: 9012   3rd Qu.: 8979   3rd Qu.: 8973  
    ##  Max.   :29590   Max.   :30027   Max.   :29247   Max.   :29445   Max.   :30131   Max.   :29439  
    ##                                                                                                 
    ##    NWEIGHT55       NWEIGHT56       NWEIGHT57       NWEIGHT58       NWEIGHT59       NWEIGHT60    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3945   1st Qu.: 3957   1st Qu.: 3942   1st Qu.: 3962   1st Qu.: 3965   1st Qu.: 3953  
    ##  Median : 6143   Median : 6153   Median : 6138   Median : 6137   Median : 6144   Median : 6140  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8977   3rd Qu.: 8995   3rd Qu.: 9004   3rd Qu.: 8986   3rd Qu.: 8977   3rd Qu.: 8983  
    ##  Max.   :29216   Max.   :29203   Max.   :29819   Max.   :29818   Max.   :29606   Max.   :29818  
    ##                                                                                                 
    ##     CDD30YR         CDD65         ClimateRegion_BA    HDD30YR          HDD65           BTUEL         
    ##  Min.   :   0   Min.   :   0   Cold       :7116    Min.   :    0   Min.   :    0   Min.   :   143.3  
    ##  1st Qu.: 601   1st Qu.: 814   Mixed-Humid:5579    1st Qu.: 2898   1st Qu.: 2434   1st Qu.: 20205.8  
    ##  Median :1020   Median :1179   Hot-Humid  :2545    Median : 4825   Median : 4396   Median : 31890.0  
    ##  Mean   :1310   Mean   :1526   Hot-Dry    :1577    Mean   : 4679   Mean   : 4272   Mean   : 37016.2  
    ##  3rd Qu.:1703   3rd Qu.:1805   Marine     : 911    3rd Qu.: 6290   3rd Qu.: 5810   3rd Qu.: 48298.0  
    ##  Max.   :4905   Max.   :5534   Very-Cold  : 572    Max.   :16071   Max.   :17383   Max.   :628155.5  
    ##                                (Other)    : 196                                                      
    ##     DOLLAREL           BTUNG            DOLLARNG          BTULP           DOLLARLP      
    ##  Min.   : -889.5   Min.   :      0   Min.   :   0.0   Min.   :     0   Min.   :   0.00  
    ##  1st Qu.:  836.5   1st Qu.:      0   1st Qu.:   0.0   1st Qu.:     0   1st Qu.:   0.00  
    ##  Median : 1257.9   Median :  22012   Median : 313.9   Median :     0   Median :   0.00  
    ##  Mean   : 1424.8   Mean   :  36961   Mean   : 396.0   Mean   :  3917   Mean   :  80.89  
    ##  3rd Qu.: 1819.0   3rd Qu.:  62714   3rd Qu.: 644.9   3rd Qu.:     0   3rd Qu.:   0.00  
    ##  Max.   :15680.2   Max.   :1134709   Max.   :8155.0   Max.   :364215   Max.   :6621.44  
    ##                                                                                         
    ##      BTUFO           DOLLARFO          TOTALBTU          TOTALDOL          BTUWOOD      
    ##  Min.   :     0   Min.   :   0.00   Min.   :   1182   Min.   : -150.5   Min.   :     0  
    ##  1st Qu.:     0   1st Qu.:   0.00   1st Qu.:  45565   1st Qu.: 1258.3   1st Qu.:     0  
    ##  Median :     0   Median :   0.00   Median :  74180   Median : 1793.2   Median :     0  
    ##  Mean   :  5109   Mean   :  88.43   Mean   :  83002   Mean   : 1990.2   Mean   :  3596  
    ##  3rd Qu.:     0   3rd Qu.:   0.00   3rd Qu.: 108535   3rd Qu.: 2472.0   3rd Qu.:     0  
    ##  Max.   :426269   Max.   :7003.69   Max.   :1367548   Max.   :20043.4   Max.   :500000  
    ## 

``` r
nrow(recs_out)
```

    ## [1] 18496

``` r
recs_der_tmp_loc <- here::here("osf_dl", "recs_2020.rds")
write_rds(recs_out, recs_der_tmp_loc)
target_dir <- osf_retrieve_node("https://osf.io/gzbkn/?view_only=8ca80573293b4e12b7f934a0f742b957") 
osf_upload(target_dir, path=recs_der_tmp_loc, conflicts="overwrite")
```

    ## # A tibble: 1 × 3
    ##   name          id                       meta            
    ##   <chr>         <chr>                    <list>          
    ## 1 recs_2020.rds 647d2d6bbf3d0f09b6d87a32 <named list [3]>

``` r
unlink(recs_der_tmp_loc)
```
