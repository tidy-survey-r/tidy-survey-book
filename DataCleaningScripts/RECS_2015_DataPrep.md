Residential Energy Consumption Survey (RECS) 2015 Data Prep
================

## Data information

All data and resources were downloaded from
<https://www.eia.gov/consumption/residential/data/2015/index.php?view=microdata>
on March 3, 2021.

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
recs_in <- read_csv(here("RawData", "RECS_2015", "recs2015_public_v4.csv"))
```

    ## Rows: 5686 Columns: 759
    ## ── Column specification ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (4): METROMICRO, UATYP10, CLIMATE_REGION_PUB, IECC_CLIMATE_PUB
    ## dbl (755): DOEID, REGIONC, DIVISION, TYPEHUQ, ZTYPEHUQ, CELLAR, ZCELLAR, BASEFIN, ZBASEFIN, ATTIC, ZATTIC, ATTICFIN, ZATTICFIN, STORIES, ZSTOR...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
recs <- recs_in %>%
   select(DOEID, REGIONC, DIVISION, METROMICRO, UATYP10, TYPEHUQ, YEARMADERANGE, HEATHOME, EQUIPMUSE, TEMPHOME, TEMPGONE, TEMPNITE, AIRCOND, USECENAC, TEMPHOMEAC, TEMPGONEAC, TEMPNITEAC, TOTCSQFT, TOTHSQFT, TOTSQFT_EN, TOTUCSQFT, TOTUSQFT, NWEIGHT, starts_with("BRRWT"), CDD30YR, CDD65, CDD80, CLIMATE_REGION_PUB, IECC_CLIMATE_PUB, HDD30YR, HDD65, HDD50, GNDHDD65, BTUEL, DOLLAREL, BTUNG, DOLLARNG, BTULP, DOLLARLP, BTUFO, DOLLARFO, TOTALBTU, TOTALDOL, BTUWOOD=WOODBTU, BTUPELLET=PELLETBTU ) %>%
   mutate(
      Region=parse_factor(
         case_when(
            REGIONC==1~"Northeast",
            REGIONC==2~"Midwest",
            REGIONC==3~"South",
            REGIONC==4~"West",
      ), levels=c("Northeast", "Midwest", "South", "West")),
      Division=parse_factor(
         case_when(
            DIVISION==1~"New England",
            DIVISION==2~"Middle Atlantic",
            DIVISION==3~"East North Central",
            DIVISION==4~"West North Central",
            DIVISION==5~"South Atlantic",
            DIVISION==6~"East South Central",
            DIVISION==7~"West South Central",
            DIVISION==8~"Mountain North",
            DIVISION==9~"Mountain South",
            DIVISION==10~"Pacific",
      ), levels=c("New England", "Middle Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain North", "Mountain South", "Pacific")),
      MSAStatus=fct_recode(METROMICRO, "Metropolitan Statistical Area"="METRO", "Micropolitan Statistical Area"="MICRO", "None"="NONE"),
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
         ),
         levels=c("Before 1950", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2015"),
         ordered = TRUE
      ),
      SpaceHeatingUsed=as.logical(HEATHOME),
      HeatingBehavior=parse_factor(
         case_when(
            EQUIPMUSE==1~"Set one temp and leave it",
            EQUIPMUSE==2~"Manually adjust at night/no one home",
            EQUIPMUSE==3~"Program thermostat to change at certain times",
            EQUIPMUSE==4~"Turn on or off as needed",
            EQUIPMUSE==5~"No control",
            EQUIPMUSE==9~"Other",
            EQUIPMUSE==-9~NA_character_),
         levels=c("Set one temp and leave it", "Manually adjust at night/no one home", "Program thermostat to change at certain times", "Turn on or off as needed", "No control", "Other")
      ),
      WinterTempDay=if_else(TEMPHOME>0, TEMPHOME, NA_real_),
      WinterTempAway=if_else(TEMPGONE>0, TEMPGONE, NA_real_),
      WinterTempNight=if_else(TEMPNITE>0, TEMPNITE, NA_real_),
      ACUsed=as.logical(AIRCOND),
      ACBehavior=parse_factor(
         case_when(
            USECENAC==1~"Set one temp and leave it",
            USECENAC==2~"Manually adjust at night/no one home",
            USECENAC==3~"Program thermostat to change at certain times",
            USECENAC==4~"Turn on or off as needed",
            USECENAC==5~"No control",
            USECENAC==-9~NA_character_),
         levels=c("Set one temp and leave it", "Manually adjust at night/no one home", "Program thermostat to change at certain times", "Turn on or off as needed", "No control")
      ),
      SummerTempDay=if_else(TEMPHOMEAC>0, TEMPHOMEAC, NA_real_),
      SummerTempAway=if_else(TEMPGONEAC>0, TEMPGONEAC, NA_real_),
      SummerTempNight=if_else(TEMPNITEAC>0, TEMPNITEAC, NA_real_),
      ClimateRegion_BA=parse_factor(CLIMATE_REGION_PUB),
      ClimateRegion_IECC=factor(IECC_CLIMATE_PUB)
      
   )
```

    ## select: renamed 2 variables (BTUWOOD, BTUPELLET) and dropped 619 variables
    ## mutate: new variable 'Region' (factor) with 4 unique values and 0% NA
    ##         new variable 'Division' (factor) with 10 unique values and 0% NA
    ##         new variable 'MSAStatus' (factor) with 3 unique values and 0% NA
    ##         new variable 'Urbanicity' (factor) with 3 unique values and 0% NA
    ##         new variable 'HousingUnitType' (factor) with 5 unique values and 0% NA
    ##         new variable 'YearMade' (ordered factor) with 8 unique values and 0% NA
    ##         new variable 'SpaceHeatingUsed' (logical) with 2 unique values and 0% NA
    ##         new variable 'HeatingBehavior' (factor) with 7 unique values and 0% NA
    ##         new variable 'WinterTempDay' (double) with 35 unique values and 5% NA
    ##         new variable 'WinterTempAway' (double) with 37 unique values and 5% NA
    ##         new variable 'WinterTempNight' (double) with 38 unique values and 5% NA
    ##         new variable 'ACUsed' (logical) with 2 unique values and 0% NA
    ##         new variable 'ACBehavior' (factor) with 6 unique values and 0% NA
    ##         new variable 'SummerTempDay' (double) with 38 unique values and 13% NA
    ##         new variable 'SummerTempAway' (double) with 35 unique values and 13% NA
    ##         new variable 'SummerTempNight' (double) with 36 unique values and 13% NA
    ##         new variable 'ClimateRegion_BA' (factor) with 5 unique values and 0% NA
    ##         new variable 'ClimateRegion_IECC' (factor) with 11 unique values and 0% NA

## Check derived variables for correct coding

``` r
recs %>% count(Region, REGIONC)
```

    ## count: now 4 rows and 3 columns, ungrouped

    ## # A tibble: 4 × 3
    ##   Region    REGIONC     n
    ##   <fct>       <dbl> <int>
    ## 1 Northeast       1   794
    ## 2 Midwest         2  1327
    ## 3 South           3  2010
    ## 4 West            4  1555

``` r
recs %>% count(Division, DIVISION)
```

    ## count: now 10 rows and 3 columns, ungrouped

    ## # A tibble: 10 × 3
    ##    Division           DIVISION     n
    ##    <fct>                 <dbl> <int>
    ##  1 New England               1   253
    ##  2 Middle Atlantic           2   541
    ##  3 East North Central        3   836
    ##  4 West North Central        4   491
    ##  5 South Atlantic            5  1058
    ##  6 East South Central        6   372
    ##  7 West South Central        7   580
    ##  8 Mountain North            8   228
    ##  9 Mountain South            9   242
    ## 10 Pacific                  10  1085

``` r
recs %>% count(MSAStatus, METROMICRO)
```

    ## count: now 3 rows and 3 columns, ungrouped

    ## # A tibble: 3 × 3
    ##   MSAStatus                     METROMICRO     n
    ##   <fct>                         <chr>      <int>
    ## 1 Metropolitan Statistical Area METRO       4745
    ## 2 Micropolitan Statistical Area MICRO        584
    ## 3 None                          NONE         357

``` r
recs %>% count(Urbanicity, UATYP10)
```

    ## count: now 3 rows and 3 columns, ungrouped

    ## # A tibble: 3 × 3
    ##   Urbanicity    UATYP10     n
    ##   <fct>         <chr>   <int>
    ## 1 Urban Area    U        3928
    ## 2 Urban Cluster C         598
    ## 3 Rural         R        1160

``` r
recs %>% count(HousingUnitType, TYPEHUQ)
```

    ## count: now 5 rows and 3 columns, ungrouped

    ## # A tibble: 5 × 3
    ##   HousingUnitType            TYPEHUQ     n
    ##   <fct>                        <dbl> <int>
    ## 1 Mobile home                      1   286
    ## 2 Single-family detached           2  3752
    ## 3 Single-family attached           3   479
    ## 4 Apartment: 2-4 Units             4   311
    ## 5 Apartment: 5 or more units       5   858

``` r
recs %>% count(YearMade, YEARMADERANGE)
```

    ## count: now 8 rows and 3 columns, ungrouped

    ## # A tibble: 8 × 3
    ##   YearMade    YEARMADERANGE     n
    ##   <ord>               <dbl> <int>
    ## 1 Before 1950             1   858
    ## 2 1950-1959               2   544
    ## 3 1960-1969               3   565
    ## 4 1970-1979               4   928
    ## 5 1980-1989               5   874
    ## 6 1990-1999               6   786
    ## 7 2000-2009               7   901
    ## 8 2010-2015               8   230

``` r
recs %>% count(SpaceHeatingUsed, HEATHOME)
```

    ## count: now 2 rows and 3 columns, ungrouped

    ## # A tibble: 2 × 3
    ##   SpaceHeatingUsed HEATHOME     n
    ##   <lgl>               <dbl> <int>
    ## 1 FALSE                   0   258
    ## 2 TRUE                    1  5428

``` r
recs %>% count(HeatingBehavior, EQUIPMUSE)
```

    ## count: now 7 rows and 3 columns, ungrouped

    ## # A tibble: 7 × 3
    ##   HeatingBehavior                               EQUIPMUSE     n
    ##   <fct>                                             <dbl> <int>
    ## 1 Set one temp and leave it                             1  2156
    ## 2 Manually adjust at night/no one home                  2  1414
    ## 3 Program thermostat to change at certain times         3   972
    ## 4 Turn on or off as needed                              4   761
    ## 5 No control                                            5   114
    ## 6 Other                                                 9    11
    ## 7 <NA>                                                 -2   258

``` r
recs %>% count(ACUsed, AIRCOND)
```

    ## count: now 2 rows and 3 columns, ungrouped

    ## # A tibble: 2 × 3
    ##   ACUsed AIRCOND     n
    ##   <lgl>    <dbl> <int>
    ## 1 FALSE        0   737
    ## 2 TRUE         1  4949

``` r
recs %>% count(ACBehavior, USECENAC)
```

    ## count: now 6 rows and 3 columns, ungrouped

    ## # A tibble: 6 × 3
    ##   ACBehavior                                    USECENAC     n
    ##   <fct>                                            <dbl> <int>
    ## 1 Set one temp and leave it                            1  1661
    ## 2 Manually adjust at night/no one home                 2   984
    ## 3 Program thermostat to change at certain times        3   727
    ## 4 Turn on or off as needed                             4   438
    ## 5 No control                                           5     2
    ## 6 <NA>                                                -2  1874

``` r
recs %>% count(ClimateRegion_BA, CLIMATE_REGION_PUB)
```

    ## count: now 5 rows and 3 columns, ungrouped

    ## # A tibble: 5 × 3
    ##   ClimateRegion_BA  CLIMATE_REGION_PUB     n
    ##   <fct>             <chr>              <int>
    ## 1 Hot-Dry/Mixed-Dry Hot-Dry/Mixed-Dry    750
    ## 2 Hot-Humid         Hot-Humid           1036
    ## 3 Mixed-Humid       Mixed-Humid         1468
    ## 4 Cold/Very Cold    Cold/Very Cold      2008
    ## 5 Marine            Marine               424

``` r
recs %>% count(ClimateRegion_IECC, IECC_CLIMATE_PUB)
```

    ## count: now 11 rows and 3 columns, ungrouped

    ## # A tibble: 11 × 3
    ##    ClimateRegion_IECC IECC_CLIMATE_PUB     n
    ##    <fct>              <chr>            <int>
    ##  1 1A-2A              1A-2A              846
    ##  2 2B                 2B                 106
    ##  3 3A                 3A                 637
    ##  4 3B-4B              3B-4B              644
    ##  5 3C                 3C                 209
    ##  6 4A                 4A                1021
    ##  7 4C                 4C                 215
    ##  8 5A                 5A                1240
    ##  9 5B-5C              5B-5C              332
    ## 10 6A-6B              6A-6B              376
    ## 11 7A-7B-7AK-8AK      7A-7B-7AK-8AK       60

## Save data

``` r
recs_out <- recs %>%
   select(DOEID, Region, Division, MSAStatus, Urbanicity, HousingUnitType, YearMade, SpaceHeatingUsed, HeatingBehavior, WinterTempDay, WinterTempAway, WinterTempNight, ACUsed, ACBehavior, SummerTempDay, SummerTempAway, SummerTempNight, TOTCSQFT, TOTHSQFT, TOTSQFT_EN, TOTUCSQFT, TOTUSQFT, NWEIGHT, starts_with("BRRWT"), CDD30YR, CDD65, CDD80, ClimateRegion_BA, ClimateRegion_IECC, HDD30YR, HDD65, HDD50, GNDHDD65, BTUEL, DOLLAREL, BTUNG, DOLLARNG, BTULP, DOLLARLP, BTUFO, DOLLARFO, TOTALBTU, TOTALDOL, BTUWOOD, BTUPELLET)
```

    ## select: dropped 18 variables (REGIONC, DIVISION, METROMICRO, UATYP10, TYPEHUQ, …)

``` r
summary(recs_out)
```

    ##      DOEID             Region                   Division                            MSAStatus            Urbanicity  
    ##  Min.   :10001   Northeast: 794   Pacific           :1085   Metropolitan Statistical Area:4745   Urban Area   :3928  
    ##  1st Qu.:11422   Midwest  :1327   South Atlantic    :1058   Micropolitan Statistical Area: 584   Urban Cluster: 598  
    ##  Median :12844   South    :2010   East North Central: 836   None                         : 357   Rural        :1160  
    ##  Mean   :12844   West     :1555   West South Central: 580                                                            
    ##  3rd Qu.:14265                    Middle Atlantic   : 541                                                            
    ##  Max.   :15686                    West North Central: 491                                                            
    ##                                   (Other)           :1095                                                            
    ##                    HousingUnitType        YearMade   SpaceHeatingUsed                                      HeatingBehavior WinterTempDay  
    ##  Mobile home               : 286   1970-1979  :928   Mode :logical    Set one temp and leave it                    :2156   Min.   :50.00  
    ##  Single-family detached    :3752   2000-2009  :901   FALSE:258        Manually adjust at night/no one home         :1414   1st Qu.:68.00  
    ##  Single-family attached    : 479   1980-1989  :874   TRUE :5428       Program thermostat to change at certain times: 972   Median :70.00  
    ##  Apartment: 2-4 Units      : 311   Before 1950:858                    Turn on or off as needed                     : 761   Mean   :70.06  
    ##  Apartment: 5 or more units: 858   1990-1999  :786                    No control                                   : 114   3rd Qu.:72.00  
    ##                                    1960-1969  :565                    Other                                        :  11   Max.   :90.00  
    ##                                    (Other)    :774                    NA                                           : 258   NA's   :258    
    ##  WinterTempAway  WinterTempNight   ACUsed                                                ACBehavior   SummerTempDay   SummerTempAway 
    ##  Min.   :50.00   Min.   :50.00   Mode :logical   Set one temp and leave it                    :1661   Min.   :50.00   Min.   :50.00  
    ##  1st Qu.:65.00   1st Qu.:65.00   FALSE:737       Manually adjust at night/no one home         : 984   1st Qu.:70.00   1st Qu.:71.00  
    ##  Median :68.00   Median :68.00   TRUE :4949      Program thermostat to change at certain times: 727   Median :72.00   Median :75.00  
    ##  Mean   :67.12   Mean   :68.06                   Turn on or off as needed                     : 438   Mean   :72.66   Mean   :74.63  
    ##  3rd Qu.:70.00   3rd Qu.:70.00                   No control                                   :   2   3rd Qu.:76.00   3rd Qu.:78.00  
    ##  Max.   :90.00   Max.   :90.00                   NA                                           :1874   Max.   :90.00   Max.   :90.00  
    ##  NA's   :258     NA's   :258                                                                          NA's   :737     NA's   :737    
    ##  SummerTempNight    TOTCSQFT         TOTHSQFT      TOTSQFT_EN     TOTUCSQFT         TOTUSQFT         NWEIGHT           BRRWT1      
    ##  Min.   :50.00   Min.   :   0.0   Min.   :   0   Min.   : 221   Min.   :   0.0   Min.   :   0.0   Min.   :  1236   Min.   :  1836  
    ##  1st Qu.:70.00   1st Qu.: 466.2   1st Qu.:1008   1st Qu.:1100   1st Qu.:   0.0   1st Qu.:   0.0   1st Qu.: 13874   1st Qu.:  9859  
    ##  Median :72.00   Median :1218.5   Median :1559   Median :1774   Median : 400.0   Median : 250.0   Median : 18510   Median : 16942  
    ##  Mean   :71.82   Mean   :1454.5   Mean   :1816   Mean   :2081   Mean   : 793.9   Mean   : 432.6   Mean   : 20789   Mean   : 20789  
    ##  3rd Qu.:75.00   3rd Qu.:2094.0   3rd Qu.:2400   3rd Qu.:2766   3rd Qu.:1150.0   3rd Qu.: 569.8   3rd Qu.: 24840   3rd Qu.: 27219  
    ##  Max.   :90.00   Max.   :8066.0   Max.   :8066   Max.   :8501   Max.   :7986.0   Max.   :6660.0   Max.   :139307   Max.   :203902  
    ##  NA's   :737                                                                                                                       
    ##      BRRWT2             BRRWT3             BRRWT4             BRRWT5             BRRWT6             BRRWT7             BRRWT8      
    ##  Min.   :   685.9   Min.   :   543.9   Min.   :   699.7   Min.   :   649.3   Min.   :   638.7   Min.   :   564.1   Min.   :   591  
    ##  1st Qu.:  9733.0   1st Qu.:  9575.3   1st Qu.:  9518.5   1st Qu.:  9598.5   1st Qu.:  9501.7   1st Qu.:  9534.4   1st Qu.:  9653  
    ##  Median : 16993.7   Median : 16698.7   Median : 17034.2   Median : 16487.5   Median : 16150.6   Median : 16332.5   Median : 16802  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789  
    ##  3rd Qu.: 27825.1   3rd Qu.: 27941.8   3rd Qu.: 27931.5   3rd Qu.: 27856.7   3rd Qu.: 28092.8   3rd Qu.: 27992.5   3rd Qu.: 27926  
    ##  Max.   :189788.1   Max.   :180155.3   Max.   :159902.6   Max.   :141796.4   Max.   :189031.8   Max.   :192311.7   Max.   :195071  
    ##                                                                                                                                    
    ##      BRRWT9            BRRWT10            BRRWT11            BRRWT12            BRRWT13          BRRWT14            BRRWT15        
    ##  Min.   :   545.2   Min.   :   732.5   Min.   :   586.1   Min.   :   549.8   Min.   :   668   Min.   :   544.5   Min.   :   671.4  
    ##  1st Qu.:  9595.0   1st Qu.:  9077.6   1st Qu.:  9448.5   1st Qu.:  9388.2   1st Qu.:  9757   1st Qu.:  9491.8   1st Qu.:  9341.8  
    ##  Median : 17352.7   Median : 16601.9   Median : 16172.3   Median : 16167.4   Median : 16584   Median : 17028.9   Median : 15996.8  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27753.7   3rd Qu.: 28089.9   3rd Qu.: 28022.1   3rd Qu.: 28075.4   3rd Qu.: 27455   3rd Qu.: 27975.3   3rd Qu.: 28117.5  
    ##  Max.   :117167.3   Max.   :183073.4   Max.   :195408.4   Max.   :197373.3   Max.   :182228   Max.   :173341.2   Max.   :179152.7  
    ##                                                                                                                                    
    ##     BRRWT16            BRRWT17            BRRWT18            BRRWT19          BRRWT20            BRRWT21            BRRWT22        
    ##  Min.   :   603.4   Min.   :   563.3   Min.   :   517.2   Min.   :   657   Min.   :   682.2   Min.   :   689.4   Min.   :   581.3  
    ##  1st Qu.:  9804.6   1st Qu.:  9593.2   1st Qu.:  9839.6   1st Qu.:  9776   1st Qu.:  9569.2   1st Qu.:  9663.9   1st Qu.:  9805.3  
    ##  Median : 16562.6   Median : 16750.8   Median : 16560.5   Median : 16779   Median : 16881.2   Median : 16503.8   Median : 16711.4  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27322.1   3rd Qu.: 27458.0   3rd Qu.: 27636.2   3rd Qu.: 27986   3rd Qu.: 27467.7   3rd Qu.: 27863.0   3rd Qu.: 27503.4  
    ##  Max.   :210507.2   Max.   :195346.9   Max.   :158094.9   Max.   :197236   Max.   :146347.4   Max.   :181583.8   Max.   :173557.2  
    ##                                                                                                                                    
    ##     BRRWT23            BRRWT24            BRRWT25            BRRWT26            BRRWT27          BRRWT28            BRRWT29      
    ##  Min.   :   658.4   Min.   :   698.7   Min.   :   541.3   Min.   :   832.9   Min.   :  1372   Min.   :   764.7   Min.   :   854  
    ##  1st Qu.:  9597.1   1st Qu.:  9387.9   1st Qu.:  9502.9   1st Qu.:  9593.2   1st Qu.:  9333   1st Qu.:  9358.0   1st Qu.:  9596  
    ##  Median : 16205.0   Median : 16398.2   Median : 17120.6   Median : 16642.2   Median : 16671   Median : 16663.4   Median : 16336  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789  
    ##  3rd Qu.: 27855.2   3rd Qu.: 27791.0   3rd Qu.: 28108.8   3rd Qu.: 28018.5   3rd Qu.: 27832   3rd Qu.: 28065.9   3rd Qu.: 27506  
    ##  Max.   :182366.0   Max.   :170970.0   Max.   :128220.6   Max.   :176770.0   Max.   :176453   Max.   :210413.6   Max.   :194434  
    ##                                                                                                                                  
    ##     BRRWT30            BRRWT31            BRRWT32            BRRWT33            BRRWT34          BRRWT35            BRRWT36        
    ##  Min.   :   680.6   Min.   :   868.4   Min.   :   645.1   Min.   :   714.2   Min.   :  1880   Min.   :   629.3   Min.   :   980.2  
    ##  1st Qu.:  9689.3   1st Qu.:  9493.1   1st Qu.:  9370.6   1st Qu.:  9530.8   1st Qu.:  9703   1st Qu.:  9842.0   1st Qu.:  9439.6  
    ##  Median : 16683.8   Median : 16876.0   Median : 16594.5   Median : 16839.7   Median : 16380   Median : 17204.4   Median : 16440.6  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27613.1   3rd Qu.: 27807.8   3rd Qu.: 28250.9   3rd Qu.: 27610.2   3rd Qu.: 27846   3rd Qu.: 27533.4   3rd Qu.: 28354.2  
    ##  Max.   :118557.6   Max.   :197960.8   Max.   :182658.3   Max.   :183414.8   Max.   :130246   Max.   :125674.9   Max.   :171375.9  
    ##                                                                                                                                    
    ##     BRRWT37            BRRWT38            BRRWT39            BRRWT40          BRRWT41          BRRWT42            BRRWT43        
    ##  Min.   :   634.6   Min.   :   738.1   Min.   :   684.5   Min.   :  1531   Min.   :  1406   Min.   :   943.8   Min.   :   683.3  
    ##  1st Qu.:  9276.7   1st Qu.:  9737.9   1st Qu.:  9389.5   1st Qu.:  9624   1st Qu.:  9776   1st Qu.:  9446.7   1st Qu.:  9563.6  
    ##  Median : 16620.9   Median : 16862.8   Median : 16797.7   Median : 16644   Median : 16910   Median : 16177.2   Median : 16999.1  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27754.3   3rd Qu.: 27710.0   3rd Qu.: 27850.3   3rd Qu.: 27858   3rd Qu.: 27616   3rd Qu.: 28089.3   3rd Qu.: 27724.1  
    ##  Max.   :209103.9   Max.   :187208.7   Max.   :136106.4   Max.   :165612   Max.   :145467   Max.   :189726.6   Max.   :192302.9  
    ##                                                                                                                                  
    ##     BRRWT44            BRRWT45          BRRWT46            BRRWT47          BRRWT48            BRRWT49            BRRWT50      
    ##  Min.   :   866.4   Min.   :  1105   Min.   :   750.7   Min.   :  1230   Min.   :   684.4   Min.   :   627.1   Min.   :  1638  
    ##  1st Qu.:  9595.5   1st Qu.:  9563   1st Qu.:  9616.2   1st Qu.:  9362   1st Qu.:  9383.9   1st Qu.:  9489.0   1st Qu.:  9601  
    ##  Median : 17034.6   Median : 16629   Median : 16821.6   Median : 16243   Median : 16720.3   Median : 17068.6   Median : 16788  
    ##  Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789  
    ##  3rd Qu.: 27593.8   3rd Qu.: 27773   3rd Qu.: 27563.3   3rd Qu.: 27547   3rd Qu.: 27965.8   3rd Qu.: 27829.1   3rd Qu.: 27667  
    ##  Max.   :190671.5   Max.   :160108   Max.   :183963.8   Max.   :196001   Max.   :199079.7   Max.   :203407.7   Max.   :223546  
    ##                                                                                                                                
    ##     BRRWT51            BRRWT52            BRRWT53            BRRWT54            BRRWT55          BRRWT56            BRRWT57        
    ##  Min.   :   922.9   Min.   :   749.9   Min.   :   871.8   Min.   :   687.9   Min.   :  2056   Min.   :   623.7   Min.   :   713.4  
    ##  1st Qu.:  9704.7   1st Qu.:  9496.9   1st Qu.:  9489.1   1st Qu.:  9623.3   1st Qu.:  9595   1st Qu.:  9798.4   1st Qu.:  9393.8  
    ##  Median : 16706.2   Median : 16442.9   Median : 16494.9   Median : 16662.9   Median : 16589   Median : 16624.8   Median : 17198.4  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27755.8   3rd Qu.: 27621.2   3rd Qu.: 28075.0   3rd Qu.: 27612.8   3rd Qu.: 27857   3rd Qu.: 27650.0   3rd Qu.: 27964.1  
    ##  Max.   :161561.8   Max.   :146056.0   Max.   :143796.6   Max.   :174657.5   Max.   :206797   Max.   :226169.8   Max.   :162193.6  
    ##                                                                                                                                    
    ##     BRRWT58            BRRWT59            BRRWT60          BRRWT61            BRRWT62            BRRWT63            BRRWT64        
    ##  Min.   :   905.5   Min.   :   630.7   Min.   :  1275   Min.   :   546.4   Min.   :   739.7   Min.   :   671.5   Min.   :   926.4  
    ##  1st Qu.:  9559.2   1st Qu.:  9623.7   1st Qu.:  9577   1st Qu.:  9387.4   1st Qu.:  9643.5   1st Qu.:  9455.3   1st Qu.:  9400.5  
    ##  Median : 16540.0   Median : 16656.6   Median : 16197   Median : 16376.3   Median : 17067.2   Median : 16632.1   Median : 16508.1  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27780.9   3rd Qu.: 27577.8   3rd Qu.: 27781   3rd Qu.: 28016.5   3rd Qu.: 27540.6   3rd Qu.: 28020.8   3rd Qu.: 27693.9  
    ##  Max.   :211170.6   Max.   :206702.7   Max.   :169387   Max.   :122260.9   Max.   :158200.9   Max.   :196933.9   Max.   :217490.7  
    ##                                                                                                                                    
    ##     BRRWT65          BRRWT66          BRRWT67            BRRWT68          BRRWT69          BRRWT70            BRRWT71            BRRWT72        
    ##  Min.   :  1144   Min.   :  1264   Min.   :   684.8   Min.   :  1053   Min.   :  1676   Min.   :   758.4   Min.   :   892.2   Min.   :   695.5  
    ##  1st Qu.:  9597   1st Qu.:  9758   1st Qu.:  9588.0   1st Qu.:  9245   1st Qu.:  9371   1st Qu.:  9622.5   1st Qu.:  9451.9   1st Qu.:  9516.0  
    ##  Median : 16442   Median : 16565   Median : 16560.8   Median : 16464   Median : 16682   Median : 16676.4   Median : 16482.8   Median : 16717.8  
    ##  Mean   : 20789   Mean   : 20789   Mean   : 20789.3   Mean   : 20789   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27348   3rd Qu.: 27884   3rd Qu.: 27838.7   3rd Qu.: 28108   3rd Qu.: 27957   3rd Qu.: 27897.7   3rd Qu.: 27882.7   3rd Qu.: 27611.7  
    ##  Max.   :239712   Max.   :157193   Max.   :179204.9   Max.   :183266   Max.   :193274   Max.   :146583.8   Max.   :126528.3   Max.   :196704.6  
    ##                                                                                                                                                 
    ##     BRRWT73          BRRWT74            BRRWT75            BRRWT76          BRRWT77            BRRWT78            BRRWT79        
    ##  Min.   :   875   Min.   :   541.6   Min.   :   669.7   Min.   :   617   Min.   :   560.5   Min.   :   526.7   Min.   :   651.1  
    ##  1st Qu.:  9734   1st Qu.:  9503.9   1st Qu.:  9835.9   1st Qu.:  9385   1st Qu.:  9673.8   1st Qu.:  9744.1   1st Qu.:  9549.7  
    ##  Median : 16930   Median : 16128.6   Median : 16921.5   Median : 17000   Median : 16713.6   Median : 17098.9   Median : 16676.0  
    ##  Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27756   3rd Qu.: 27849.9   3rd Qu.: 27352.3   3rd Qu.: 27558   3rd Qu.: 27712.8   3rd Qu.: 27459.8   3rd Qu.: 27857.9  
    ##  Max.   :184412   Max.   :125833.8   Max.   :194829.8   Max.   :212262   Max.   :234971.4   Max.   :152055.4   Max.   :180157.0  
    ##                                                                                                                                  
    ##     BRRWT80            BRRWT81            BRRWT82            BRRWT83            BRRWT84            BRRWT85            BRRWT86        
    ##  Min.   :   675.7   Min.   :   681.2   Min.   :   563.6   Min.   :   656.9   Min.   :   652.7   Min.   :   675.4   Min.   :   680.3  
    ##  1st Qu.:  9554.4   1st Qu.:  9489.0   1st Qu.:  9216.4   1st Qu.:  9634.4   1st Qu.:  9432.5   1st Qu.:  9551.2   1st Qu.:  9619.8  
    ##  Median : 16707.8   Median : 16769.3   Median : 16121.6   Median : 16516.9   Median : 16454.8   Median : 16902.2   Median : 16772.0  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 27688.3   3rd Qu.: 27901.5   3rd Qu.: 28253.1   3rd Qu.: 27725.8   3rd Qu.: 28006.4   3rd Qu.: 27325.4   3rd Qu.: 27638.1  
    ##  Max.   :165661.6   Max.   :191740.1   Max.   :171004.8   Max.   :184719.0   Max.   :191550.3   Max.   :198238.4   Max.   :232065.5  
    ##                                                                                                                                      
    ##     BRRWT87            BRRWT88            BRRWT89            BRRWT90            BRRWT91            BRRWT92            BRRWT93        
    ##  Min.   :   551.7   Min.   :   704.2   Min.   :   644.9   Min.   :   649.2   Min.   :   568.2   Min.   :   591.9   Min.   :   545.3  
    ##  1st Qu.:  9436.6   1st Qu.:  9393.1   1st Qu.:  9643.2   1st Qu.:  9467.7   1st Qu.:  9506.3   1st Qu.:  9610.6   1st Qu.:  9688.4  
    ##  Median : 16799.0   Median : 16778.6   Median : 16586.1   Median : 16212.0   Median : 16781.5   Median : 16524.1   Median : 16258.4  
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3  
    ##  3rd Qu.: 28046.3   3rd Qu.: 27789.9   3rd Qu.: 28075.4   3rd Qu.: 28020.8   3rd Qu.: 27876.1   3rd Qu.: 27915.1   3rd Qu.: 27728.8  
    ##  Max.   :179835.0   Max.   :166866.1   Max.   :144299.3   Max.   :175279.5   Max.   :205917.4   Max.   :225638.4   Max.   :117260.5  
    ##                                                                                                                                      
    ##     BRRWT94            BRRWT95            BRRWT96            CDD30YR         CDD65          CDD80                 ClimateRegion_BA
    ##  Min.   :   716.2   Min.   :   566.4   Min.   :   551.1   Min.   :   0   Min.   :   0   Min.   :   0.0   Hot-Dry/Mixed-Dry: 750   
    ##  1st Qu.:  9561.6   1st Qu.:  9530.2   1st Qu.:  9533.2   1st Qu.: 712   1st Qu.: 793   1st Qu.:  10.0   Hot-Humid        :1036   
    ##  Median : 17099.7   Median : 16577.2   Median : 16358.9   Median :1150   Median :1378   Median :  60.0   Mixed-Humid      :1468   
    ##  Mean   : 20789.3   Mean   : 20789.3   Mean   : 20789.3   Mean   :1451   Mean   :1719   Mean   : 174.7   Cold/Very Cold   :2008   
    ##  3rd Qu.: 27853.9   3rd Qu.: 27441.4   3rd Qu.: 27823.1   3rd Qu.:1880   3rd Qu.:2231   3rd Qu.: 208.0   Marine           : 424   
    ##  Max.   :207264.3   Max.   :205015.8   Max.   :171550.8   Max.   :5792   Max.   :6607   Max.   :2297.0                            
    ##                                                                                                                                   
    ##  ClimateRegion_IECC    HDD30YR          HDD65          HDD50         GNDHDD65         BTUEL             DOLLAREL           BTUNG       
    ##  5A     :1240       Min.   :    0   Min.   :   0   Min.   :   0   Min.   :    0   Min.   :   201.6   Min.   :  18.72   Min.   :     0  
    ##  4A     :1021       1st Qu.: 2102   1st Qu.:1881   1st Qu.: 260   1st Qu.: 1337   1st Qu.: 20221.3   1st Qu.: 815.12   1st Qu.:     0  
    ##  1A-2A  : 846       Median : 4353   Median :3878   Median :1260   Median : 3704   Median : 32582.4   Median :1253.02   Median : 17961  
    ##  3B-4B  : 644       Mean   : 4087   Mean   :3708   Mean   :1486   Mean   : 3578   Mean   : 37630.7   Mean   :1403.78   Mean   : 33331  
    ##  3A     : 637       3rd Qu.: 5967   3rd Qu.:5467   3rd Qu.:2499   3rd Qu.: 5630   3rd Qu.: 49670.6   3rd Qu.:1830.83   3rd Qu.: 57126  
    ##  6A-6B  : 376       Max.   :12184   Max.   :9843   Max.   :4956   Max.   :11851   Max.   :215695.7   Max.   :8121.56   Max.   :306594  
    ##  (Other): 922                                                                                                                          
    ##     DOLLARNG          BTULP           DOLLARLP           BTUFO           DOLLARFO          TOTALBTU           TOTALDOL           BTUWOOD      
    ##  Min.   :   0.0   Min.   :     0   Min.   :   0.00   Min.   :     0   Min.   :   0.00   Min.   :   201.6   Min.   :   60.46   Min.   :     0  
    ##  1st Qu.:   0.0   1st Qu.:     0   1st Qu.:   0.00   1st Qu.:     0   1st Qu.:   0.00   1st Qu.: 42655.8   1st Qu.: 1175.49   1st Qu.:     0  
    ##  Median : 231.8   Median :     0   Median :   0.00   Median :     0   Median :   0.00   Median : 68663.3   Median : 1724.60   Median :     0  
    ##  Mean   : 346.8   Mean   :  3192   Mean   :  67.72   Mean   :  3569   Mean   :  64.08   Mean   : 77722.9   Mean   : 1882.34   Mean   :  4140  
    ##  3rd Qu.: 605.1   3rd Qu.:     0   3rd Qu.:   0.00   3rd Qu.:     0   3rd Qu.:   0.00   3rd Qu.:103832.9   3rd Qu.: 2385.84   3rd Qu.:     0  
    ##  Max.   :2789.8   Max.   :220435   Max.   :5121.27   Max.   :273608   Max.   :4700.03   Max.   :490187.4   Max.   :10135.99   Max.   :295476  
    ##                                                                                                                                               
    ##    BTUPELLET       
    ##  Min.   :     0.0  
    ##  1st Qu.:     0.0  
    ##  Median :     0.0  
    ##  Mean   :   197.4  
    ##  3rd Qu.:     0.0  
    ##  Max.   :115500.0  
    ## 

``` r
write_rds(recs_out, here("AnalysisData", "recs_2015.rds"))
```
