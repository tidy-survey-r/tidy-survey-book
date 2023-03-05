Residential Energy Consumption Survey (RECS) 2020 Data Prep
================

## Data information

All data and resources were downloaded from
<https://www.eia.gov/consumption/residential/data/2020/index.php?view=microdata>
on March 5, 2023.

``` r
library(here) #easy relative paths
```

``` r
library(tidyverse) #data manipulation
library(haven) #data import
library(tidylog) #informative logging messages
```

## Import data and create derived variables

``` r
recs_in <- read_csv(here("RawData", "RECS_2020", "recs2020_public_v1.csv"))
```

    ## Rows: 18496 Columns: 601
    ## ── Column specification ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (8): REGIONC, DIVISION, STATE_FIPS, state_postal, state_name, BA_climate, IECC_climate_code, UATYP10
    ## dbl (593): DOEID, HDD65, CDD65, HDD30YR_pub, CDD30YR_pub, TYPEHUQ, CELLAR, CRAWL, CONCRETE, BASEOTH, BASEFIN, ATTIC, ATTICFIN, STORIES, PRKGPL...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#TOTCSQFT, TOTHSQFT, TOTSQFT_EN, TOTUCSQFT, TOTUSQFT, CDD50, HDD50, GNDHDD65, BTUEL, DOLLAREL, , BTUNG, DOLLARNG, BTULP, DOLLARLP, BTUFO, DOLLARFO, TOTALBTU, TOTALDOL, BTUWOOD=WOODBTU, BTUPELLET=PELLETBTU 

recs <- recs_in %>%
  select(DOEID, REGIONC, DIVISION, STATE_FIPS, state_postal, state_name, UATYP10, TYPEHUQ, YEARMADERANGE, HEATHOME, HEATCNTL, TEMPHOME, TEMPGONE, TEMPNITE, AIRCOND, COOLCNTL, TEMPHOMEAC, TEMPGONEAC, TEMPNITEAC, NWEIGHT, starts_with("BRRWT"), CDD30YR_pub, CDD65, BA_climate, IECC_climate_code, HDD30YR_pub, HDD65) %>%
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
        HEATCNTL==3~"Program thermostat to change at certain times",
        HEATCNTL==4~"Turn on or off as needed",
        HEATCNTL==5~"No control",
        HEATCNTL==99~"Other",
        HEATCNTL==-2~NA_character_),
      levels=c("Set one temp and leave it", "Manually adjust at night/no one home", "Program thermostat to change at certain times", "Turn on or off as needed", "No control", "Other")
    ),
    WinterTempDay=if_else(TEMPHOME>0, TEMPHOME, NA_real_),
    WinterTempAway=if_else(TEMPGONE>0, TEMPGONE, NA_real_),
    WinterTempNight=if_else(TEMPNITE>0, TEMPNITE, NA_real_),
    ACUsed=as.logical(AIRCOND),
    ACBehavior=parse_factor(
      case_when(
        COOLCNTL==1~"Set one temp and leave it",
        COOLCNTL==2~"Manually adjust at night/no one home",
        COOLCNTL==3~"Program thermostat to change at certain times",
        COOLCNTL==4~"Turn on or off as needed",
        COOLCNTL==5~"No control",
        COOLCNTL==99~"Other",
        COOLCNTL==-2~NA_character_),
      levels=c("Set one temp and leave it", "Manually adjust at night/no one home", "Program thermostat to change at certain times", "Turn on or off as needed", "No control", "Other")
    ),
    SummerTempDay=if_else(TEMPHOMEAC>0, TEMPHOMEAC, NA_real_),
    SummerTempAway=if_else(TEMPGONEAC>0, TEMPGONEAC, NA_real_),
    SummerTempNight=if_else(TEMPNITEAC>0, TEMPNITEAC, NA_real_),
    ClimateRegion_BA=parse_factor(BA_climate),
    IECC_climate_code_num=as.numeric(str_sub(IECC_climate_code, 1, 1)),
    IECC_climate_code_ch=match(str_sub(IECC_climate_code, 2, 2), LETTERS),
    ClimateRegion_IECC=factor(case_when(
      IECC_climate_code=="1A"~ "1A: Very Hot Humid",
      IECC_climate_code=="2A"~ "2A: Hot Humid",
      IECC_climate_code=="2B"~ "2B: Hot Dry",
      IECC_climate_code=="3A"~ "3A: Warm Humid",
      IECC_climate_code=="3B"~ "3B: Warm Dry",
      IECC_climate_code=="3C"~ "3C: Warm Marine",
      IECC_climate_code=="4A"~ "4A: Mixed Humid",
      IECC_climate_code=="4B"~ "4B: Mixed Dry",
      IECC_climate_code=="4C"~ "4C: Mixed Marine",
      IECC_climate_code=="5A"~ "5A: Cool Humid",
      IECC_climate_code=="5B"~ "5B: Cool Marine",
      IECC_climate_code=="5C"~ "5C: Cool Marine",
      IECC_climate_code=="6A"~ "6A: Cold Humid",
      IECC_climate_code=="6B"~ "6B: Cold Dry",
      IECC_climate_code %in% c("7A", "7AK", "7B")~ "7: Very Cold",
      IECC_climate_code=="8AK"~"8: Subarctic/Arctic"
    )),
    ClimateRegion_IECC=fct_reorder(ClimateRegion_IECC, IECC_climate_code_num+IECC_climate_code_ch/10)
  )
```

    ## select: dropped 575 variables (CELLAR, CRAWL, CONCRETE, BASEOTH, BASEFIN, …)
    ## mutate: new variable 'Region' (factor) with 4 unique values and 0% NA
    ##         new variable 'Division' (factor) with 10 unique values and 0% NA
    ##         new variable 'Urbanicity' (factor) with 3 unique values and 0% NA
    ##         new variable 'HousingUnitType' (factor) with 5 unique values and 0% NA
    ##         new variable 'YearMade' (ordered factor) with 9 unique values and 0% NA
    ##         new variable 'SpaceHeatingUsed' (logical) with 2 unique values and 0% NA
    ##         new variable 'HeatingBehavior' (factor) with 7 unique values and 0% NA
    ##         new variable 'WinterTempDay' (double) with 37 unique values and 4% NA
    ##         new variable 'WinterTempAway' (double) with 40 unique values and 4% NA
    ##         new variable 'WinterTempNight' (double) with 42 unique values and 4% NA
    ##         new variable 'ACUsed' (logical) with 2 unique values and 0% NA
    ##         new variable 'ACBehavior' (factor) with 7 unique values and 0% NA
    ##         new variable 'SummerTempDay' (double) with 42 unique values and 13% NA
    ##         new variable 'SummerTempAway' (double) with 41 unique values and 13% NA
    ##         new variable 'SummerTempNight' (double) with 40 unique values and 13% NA
    ##         new variable 'ClimateRegion_BA' (factor) with 8 unique values and 0% NA
    ##         new variable 'IECC_climate_code_num' (double) with 8 unique values and 0% NA
    ##         new variable 'IECC_climate_code_ch' (integer) with 3 unique values and 0% NA
    ##         new variable 'ClimateRegion_IECC' (factor) with 16 unique values and 0% NA

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
    ## 1 FALSE                   0   791
    ## 2 TRUE                    1 17705

``` r
recs %>% count(HeatingBehavior, HEATCNTL)
```

    ## count: now 7 rows and 3 columns, ungrouped

    ## # A tibble: 7 × 3
    ##   HeatingBehavior                               HEATCNTL     n
    ##   <fct>                                            <dbl> <int>
    ## 1 Set one temp and leave it                            1  7776
    ## 2 Manually adjust at night/no one home                 2  4644
    ## 3 Program thermostat to change at certain times        3  3310
    ## 4 Turn on or off as needed                             4  1491
    ## 5 No control                                           5   438
    ## 6 Other                                               99    46
    ## 7 <NA>                                                -2   791

``` r
recs %>% count(ACUsed, AIRCOND)
```

    ## count: now 2 rows and 3 columns, ungrouped

    ## # A tibble: 2 × 3
    ##   ACUsed AIRCOND     n
    ##   <lgl>    <dbl> <int>
    ## 1 FALSE        0  2396
    ## 2 TRUE         1 16100

``` r
recs %>% count(ACBehavior, COOLCNTL)
```

    ## count: now 7 rows and 3 columns, ungrouped

    ## # A tibble: 7 × 3
    ##   ACBehavior                                    COOLCNTL     n
    ##   <fct>                                            <dbl> <int>
    ## 1 Set one temp and leave it                            1  6687
    ## 2 Manually adjust at night/no one home                 2  3637
    ## 3 Program thermostat to change at certain times        3  2637
    ## 4 Turn on or off as needed                             4  2727
    ## 5 No control                                           5   409
    ## 6 Other                                               99     3
    ## 7 <NA>                                                -2  2396

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
recs %>% count(ClimateRegion_IECC, IECC_climate_code)
```

    ## count: now 18 rows and 3 columns, ungrouped

    ## # A tibble: 18 × 3
    ##    ClimateRegion_IECC  IECC_climate_code     n
    ##    <fct>               <chr>             <int>
    ##  1 1A: Very Hot Humid  1A                  400
    ##  2 2A: Hot Humid       2A                 1459
    ##  3 2B: Hot Dry         2B                  443
    ##  4 3A: Warm Humid      3A                 2160
    ##  5 3B: Warm Dry        3B                 1144
    ##  6 3C: Warm Marine     3C                  289
    ##  7 4A: Mixed Humid     4A                 4095
    ##  8 4B: Mixed Dry       4B                  143
    ##  9 4C: Mixed Marine    4C                  633
    ## 10 5A: Cool Humid      5A                 4014
    ## 11 5B: Cool Marine     5B                  975
    ## 12 5C: Cool Marine     5C                   12
    ## 13 6A: Cold Humid      6A                 1654
    ## 14 6B: Cold Dry        6B                  449
    ## 15 7: Very Cold        7A                  308
    ## 16 7: Very Cold        7AK                 257
    ## 17 7: Very Cold        7B                    7
    ## 18 8: Subarctic/Arctic 8AK                  54

## Save data

``` r
recs_out <- recs %>%
   select(DOEID, starts_with("state"), Region, Division, Urbanicity, HousingUnitType, YearMade, SpaceHeatingUsed, HeatingBehavior, WinterTempDay, WinterTempAway, WinterTempNight, ACUsed, ACBehavior, SummerTempDay, SummerTempAway, SummerTempNight, NWEIGHT, starts_with("BRRWT"), CDD30YR=CDD30YR_pub, CDD65, ClimateRegion_BA, ClimateRegion_IECC, HDD30YR=HDD30YR_pub, HDD65)
```

    ## select: renamed 2 variables (CDD30YR, HDD30YR) and dropped 19 variables

``` r
summary(recs_out)
```

    ##      DOEID         STATE_FIPS        state_postal        state_name              Region                   Division            Urbanicity   
    ##  Min.   :100001   Length:18496       Length:18496       Length:18496       Northeast:3657   South Atlantic    :3256   Urban Area   :12395  
    ##  1st Qu.:104625   Class :character   Class :character   Class :character   Midwest  :3832   Pacific           :2497   Urban Cluster: 2020  
    ##  Median :109249   Mode  :character   Mode  :character   Mode  :character   South    :6426   East North Central:2014   Rural        : 4081  
    ##  Mean   :109249                                                            West     :4581   Middle Atlantic   :1977                        
    ##  3rd Qu.:113872                                                                             West South Central:1827                        
    ##  Max.   :118496                                                                             West North Central:1818                        
    ##                                                                                             (Other)           :5107                        
    ##                    HousingUnitType         YearMade    SpaceHeatingUsed                                      HeatingBehavior WinterTempDay  
    ##  Mobile home               :  974   1970-1979  :2817   Mode :logical    Set one temp and leave it                    :7776   Min.   :50.00  
    ##  Single-family detached    :12319   2000-2009  :2748   FALSE:791        Manually adjust at night/no one home         :4644   1st Qu.:68.00  
    ##  Single-family attached    : 1751   Before 1950:2721   TRUE :17705      Program thermostat to change at certain times:3310   Median :70.00  
    ##  Apartment: 2-4 Units      : 1013   1990-1999  :2451                    Turn on or off as needed                     :1491   Mean   :69.77  
    ##  Apartment: 5 or more units: 2439   1980-1989  :2435                    No control                                   : 438   3rd Qu.:72.00  
    ##                                     1960-1969  :1867                    Other                                        :  46   Max.   :90.00  
    ##                                     (Other)    :3457                    NA                                           : 791   NA's   :791    
    ##  WinterTempAway  WinterTempNight   ACUsed                                                ACBehavior   SummerTempDay   SummerTempAway 
    ##  Min.   :50.00   Min.   :50.00   Mode :logical   Set one temp and leave it                    :6687   Min.   :50.00   Min.   :50.00  
    ##  1st Qu.:65.00   1st Qu.:65.00   FALSE:2396      Manually adjust at night/no one home         :3637   1st Qu.:70.00   1st Qu.:70.00  
    ##  Median :68.00   Median :68.00   TRUE :16100     Program thermostat to change at certain times:2637   Median :72.00   Median :74.00  
    ##  Mean   :67.46   Mean   :68.02                   Turn on or off as needed                     :2727   Mean   :72.01   Mean   :73.45  
    ##  3rd Qu.:70.00   3rd Qu.:70.00                   No control                                   : 409   3rd Qu.:75.00   3rd Qu.:78.00  
    ##  Max.   :90.00   Max.   :90.00                   Other                                        :   3   Max.   :90.00   Max.   :90.00  
    ##  NA's   :791     NA's   :791                     NA                                           :2396   NA's   :2396    NA's   :2396   
    ##  SummerTempNight    NWEIGHT           CDD30YR         CDD65         ClimateRegion_BA       ClimateRegion_IECC    HDD30YR          HDD65      
    ##  Min.   :50.00   Min.   :  437.9   Min.   :   0   Min.   :   0   Cold       :7116    4A: Mixed Humid:4095     Min.   :    0   Min.   :    0  
    ##  1st Qu.:68.00   1st Qu.: 4018.7   1st Qu.: 601   1st Qu.: 814   Mixed-Humid:5579    5A: Cool Humid :4014     1st Qu.: 2898   1st Qu.: 2434  
    ##  Median :72.00   Median : 6119.4   Median :1020   Median :1179   Hot-Humid  :2545    3A: Warm Humid :2160     Median : 4825   Median : 4396  
    ##  Mean   :71.22   Mean   : 6678.7   Mean   :1310   Mean   :1526   Hot-Dry    :1577    6A: Cold Humid :1654     Mean   : 4679   Mean   : 4272  
    ##  3rd Qu.:74.00   3rd Qu.: 8890.0   3rd Qu.:1703   3rd Qu.:1805   Marine     : 911    2A: Hot Humid  :1459     3rd Qu.: 6290   3rd Qu.: 5810  
    ##  Max.   :90.00   Max.   :29279.1   Max.   :4905   Max.   :5534   Very-Cold  : 572    3B: Warm Dry   :1144     Max.   :16071   Max.   :17383  
    ##  NA's   :2396                                                    (Other)    : 196    (Other)        :3970

``` r
write_rds(recs_out, here("AnalysisData", "recs_2020.rds"))
```
