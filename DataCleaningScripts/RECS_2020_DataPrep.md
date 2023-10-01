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
  osf_ls_files(path="RECS_2020", pattern="sas7bdat") %>%
  filter(str_detect(name, "v5")) %>%
  osf_download(conflicts="overwrite", path=here::here("osf_dl"))
```

    ## filter: no rows removed

``` r
recs_in <- haven::read_sas(pull(recs_file_osf_det, local_path))

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
  select(DOEID, starts_with("NWEIGHT"),
         REGIONC, Region, Division, starts_with("state"), Urbanicity, 
         HousingUnitType, YearMade, SpaceHeatingUsed, HeatingBehavior, 
         WinterTempDay, WinterTempAway, WinterTempNight, ACUsed, 
         ACBehavior, SummerTempDay, SummerTempAway, SummerTempNight, 
         TOTCSQFT, TOTHSQFT, TOTSQFT_EN, 
         CDD30YR, CDD65, ClimateRegion_BA, 
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

    ##  [1] "NWEIGHT1"     "NWEIGHT2"     "NWEIGHT3"     "NWEIGHT4"     "NWEIGHT5"     "NWEIGHT6"     "NWEIGHT7"     "NWEIGHT8"     "NWEIGHT9"     "NWEIGHT10"   
    ## [11] "NWEIGHT11"    "NWEIGHT12"    "NWEIGHT13"    "NWEIGHT14"    "NWEIGHT15"    "NWEIGHT16"    "NWEIGHT17"    "NWEIGHT18"    "NWEIGHT19"    "NWEIGHT20"   
    ## [21] "NWEIGHT21"    "NWEIGHT22"    "NWEIGHT23"    "NWEIGHT24"    "NWEIGHT25"    "NWEIGHT26"    "NWEIGHT27"    "NWEIGHT28"    "NWEIGHT29"    "NWEIGHT30"   
    ## [31] "NWEIGHT31"    "NWEIGHT32"    "NWEIGHT33"    "NWEIGHT34"    "NWEIGHT35"    "NWEIGHT36"    "NWEIGHT37"    "NWEIGHT38"    "NWEIGHT39"    "NWEIGHT40"   
    ## [41] "NWEIGHT41"    "NWEIGHT42"    "NWEIGHT43"    "NWEIGHT44"    "NWEIGHT45"    "NWEIGHT46"    "NWEIGHT47"    "NWEIGHT48"    "NWEIGHT49"    "NWEIGHT50"   
    ## [51] "NWEIGHT51"    "NWEIGHT52"    "NWEIGHT53"    "NWEIGHT54"    "NWEIGHT55"    "NWEIGHT56"    "NWEIGHT57"    "NWEIGHT58"    "NWEIGHT59"    "NWEIGHT60"   
    ## [61] "STATE_FIPS"   "state_postal" "state_name"

``` r
setdiff(names(recs_2015), names(recs_out)) #variables in 2015 and not 2020
```

    ##   [1] "MSAStatus"          "TOTUCSQFT"          "TOTUSQFT"           "BRRWT1"             "BRRWT2"             "BRRWT3"             "BRRWT4"            
    ##   [8] "BRRWT5"             "BRRWT6"             "BRRWT7"             "BRRWT8"             "BRRWT9"             "BRRWT10"            "BRRWT11"           
    ##  [15] "BRRWT12"            "BRRWT13"            "BRRWT14"            "BRRWT15"            "BRRWT16"            "BRRWT17"            "BRRWT18"           
    ##  [22] "BRRWT19"            "BRRWT20"            "BRRWT21"            "BRRWT22"            "BRRWT23"            "BRRWT24"            "BRRWT25"           
    ##  [29] "BRRWT26"            "BRRWT27"            "BRRWT28"            "BRRWT29"            "BRRWT30"            "BRRWT31"            "BRRWT32"           
    ##  [36] "BRRWT33"            "BRRWT34"            "BRRWT35"            "BRRWT36"            "BRRWT37"            "BRRWT38"            "BRRWT39"           
    ##  [43] "BRRWT40"            "BRRWT41"            "BRRWT42"            "BRRWT43"            "BRRWT44"            "BRRWT45"            "BRRWT46"           
    ##  [50] "BRRWT47"            "BRRWT48"            "BRRWT49"            "BRRWT50"            "BRRWT51"            "BRRWT52"            "BRRWT53"           
    ##  [57] "BRRWT54"            "BRRWT55"            "BRRWT56"            "BRRWT57"            "BRRWT58"            "BRRWT59"            "BRRWT60"           
    ##  [64] "BRRWT61"            "BRRWT62"            "BRRWT63"            "BRRWT64"            "BRRWT65"            "BRRWT66"            "BRRWT67"           
    ##  [71] "BRRWT68"            "BRRWT69"            "BRRWT70"            "BRRWT71"            "BRRWT72"            "BRRWT73"            "BRRWT74"           
    ##  [78] "BRRWT75"            "BRRWT76"            "BRRWT77"            "BRRWT78"            "BRRWT79"            "BRRWT80"            "BRRWT81"           
    ##  [85] "BRRWT82"            "BRRWT83"            "BRRWT84"            "BRRWT85"            "BRRWT86"            "BRRWT87"            "BRRWT88"           
    ##  [92] "BRRWT89"            "BRRWT90"            "BRRWT91"            "BRRWT92"            "BRRWT93"            "BRRWT94"            "BRRWT95"           
    ##  [99] "BRRWT96"            "CDD80"              "ClimateRegion_IECC" "HDD50"              "GNDHDD65"           "BTUPELLET"

``` r
for (var in colnames(recs_out)) {
  attr(recs_out[[deparse(as.name(var))]], "format.sas") <- NULL
}

cb_in <- readxl::read_xlsx(here::here("DataCleaningScripts", "RECS 2020 Codebook Questions.xlsx"), skip=1)

cb_ord <- cb_in %>% mutate(Order=row_number())
```

    ## mutate: new variable 'Order' (integer) with 789 unique values and 0% NA

``` r
cb_slim <- cb_ord %>% select(Variable=BookDerived, `Description and Labels`, Question, Section, Order) %>%
  filter(!is.na(Variable)) %>%
  bind_rows(select(cb_ord, Variable, `Description and Labels`, Question, Section, Order)) %>%
  arrange(Order)
```

    ## select: dropped 3 variables (Type, Response Codes, BookDerived)

    ## filter: removed 770 rows (98%), 19 rows remaining

    ## select: dropped 3 variables (Type, Response Codes, BookDerived)

``` r
names(recs_out)[!(names(recs_out) %in% pull(cb_slim, Variable))]
```

    ## character(0)

``` r
cb_vars <- cb_slim %>%
  filter(Variable %in% c(names(recs_out)))
```

    ## filter: removed 708 rows (88%), 100 rows remaining

``` r
nrow(cb_vars)
```

    ## [1] 100

``` r
ncol(recs_out)
```

    ## [1] 100

``` r
recs_ord <- recs_out %>% select(all_of(pull(cb_vars, Variable)))
```

    ## select: columns reordered (DOEID, Region, REGIONC, Division, STATE_FIPS, …)

``` r
for (var in pull(cb_vars, Variable)) {
  vi <- cb_vars %>% filter(Variable==var)
  attr(recs_ord[[deparse(as.name(var))]], "label") <- pull(vi, `Description and Labels`)
  if (!is.na(pull(vi, Question))) attr(recs_ord[[deparse(as.name(var))]], "Question") <- pull(vi, Question)
}
```

    ## filter: removed 99 rows (99%), one row remaining

    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining
    ## filter: removed 99 rows (99%), one row remaining

``` r
summary(recs_ord)
```

    ##      DOEID              Region       REGIONC                        Division     STATE_FIPS         state_postal           state_name   
    ##  Min.   :100001   Northeast:3657   Length:18496       South Atlantic    :3256   Length:18496       CA     : 1152   California   : 1152  
    ##  1st Qu.:104625   Midwest  :3832   Class :character   Pacific           :2497   Class :character   TX     : 1016   Texas        : 1016  
    ##  Median :109249   South    :6426   Mode  :character   East North Central:2014   Mode  :character   NY     :  904   New York     :  904  
    ##  Mean   :109249   West     :4581                      Middle Atlantic   :1977                      FL     :  655   Florida      :  655  
    ##  3rd Qu.:113872                                       West South Central:1827                      PA     :  617   Pennsylvania :  617  
    ##  Max.   :118496                                       West North Central:1818                      MA     :  552   Massachusetts:  552  
    ##                                                       (Other)           :5107                      (Other):13600   (Other)      :13600  
    ##     ClimateRegion_BA         Urbanicity        HDD65           CDD65         HDD30YR         CDD30YR                       HousingUnitType 
    ##  Cold       :7116    Urban Area   :12395   Min.   :    0   Min.   :   0   Min.   :    0   Min.   :   0   Mobile home               :  974  
    ##  Mixed-Humid:5579    Urban Cluster: 2020   1st Qu.: 2434   1st Qu.: 814   1st Qu.: 2898   1st Qu.: 601   Single-family detached    :12319  
    ##  Hot-Humid  :2545    Rural        : 4081   Median : 4396   Median :1179   Median : 4825   Median :1020   Single-family attached    : 1751  
    ##  Hot-Dry    :1577                          Mean   : 4272   Mean   :1526   Mean   : 4679   Mean   :1310   Apartment: 2-4 Units      : 1013  
    ##  Marine     : 911                          3rd Qu.: 5810   3rd Qu.:1805   3rd Qu.: 6290   3rd Qu.:1703   Apartment: 5 or more units: 2439  
    ##  Very-Cold  : 572                          Max.   :17383   Max.   :5534   Max.   :16071   Max.   :4905                                     
    ##  (Other)    : 196                                                                                                                          
    ##         YearMade    SpaceHeatingUsed   ACUsed                                                                      HeatingBehavior WinterTempDay  
    ##  1970-1979  :2817   Mode :logical    Mode :logical   Set one temp and leave it                                             :7806   Min.   :50.00  
    ##  2000-2009  :2748   FALSE:751        FALSE:2325      Manually adjust at night/no one home                                  :4654   1st Qu.:68.00  
    ##  Before 1950:2721   TRUE :17745      TRUE :16171     Programmable or smart thermostat automatically adjusts the temperature:3310   Median :70.00  
    ##  1990-1999  :2451                                    Turn on or off as needed                                              :1491   Mean   :69.77  
    ##  1980-1989  :2435                                    No control                                                            : 438   3rd Qu.:72.00  
    ##  1960-1969  :1867                                    Other                                                                 :  46   Max.   :90.00  
    ##  (Other)    :3457                                    NA                                                                    : 751   NA's   :751    
    ##  WinterTempAway  WinterTempNight                                                                  ACBehavior   SummerTempDay   SummerTempAway 
    ##  Min.   :50.00   Min.   :50.00   Set one temp and leave it                                             :6738   Min.   :50.00   Min.   :50.00  
    ##  1st Qu.:65.00   1st Qu.:65.00   Manually adjust at night/no one home                                  :3637   1st Qu.:70.00   1st Qu.:70.00  
    ##  Median :68.00   Median :68.00   Programmable or smart thermostat automatically adjusts the temperature:2638   Median :72.00   Median :74.00  
    ##  Mean   :67.45   Mean   :68.01   Turn on or off as needed                                              :2746   Mean   :72.01   Mean   :73.45  
    ##  3rd Qu.:70.00   3rd Qu.:70.00   No control                                                            : 409   3rd Qu.:75.00   3rd Qu.:78.00  
    ##  Max.   :90.00   Max.   :90.00   Other                                                                 :   3   Max.   :90.00   Max.   :90.00  
    ##  NA's   :751     NA's   :751     NA                                                                    :2325   NA's   :2325    NA's   :2325   
    ##  SummerTempNight   TOTSQFT_EN       TOTHSQFT        TOTCSQFT        NWEIGHT           NWEIGHT1        NWEIGHT2        NWEIGHT3        NWEIGHT4    
    ##  Min.   :50.00   Min.   :  200   Min.   :    0   Min.   :    0   Min.   :  437.9   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.:68.00   1st Qu.: 1100   1st Qu.: 1000   1st Qu.:  460   1st Qu.: 4018.7   1st Qu.: 3950   1st Qu.: 3951   1st Qu.: 3954   1st Qu.: 3953  
    ##  Median :72.00   Median : 1700   Median : 1520   Median : 1200   Median : 6119.4   Median : 6136   Median : 6151   Median : 6151   Median : 6153  
    ##  Mean   :71.22   Mean   : 1960   Mean   : 1744   Mean   : 1394   Mean   : 6678.7   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.:74.00   3rd Qu.: 2510   3rd Qu.: 2300   3rd Qu.: 2000   3rd Qu.: 8890.0   3rd Qu.: 8976   3rd Qu.: 8979   3rd Qu.: 8994   3rd Qu.: 8998  
    ##  Max.   :90.00   Max.   :15000   Max.   :15000   Max.   :14600   Max.   :29279.1   Max.   :30015   Max.   :29422   Max.   :29431   Max.   :29494  
    ##  NA's   :2325                                                                                                                                     
    ##     NWEIGHT5        NWEIGHT6        NWEIGHT7        NWEIGHT8        NWEIGHT9       NWEIGHT10       NWEIGHT11       NWEIGHT12       NWEIGHT13    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3957   1st Qu.: 3966   1st Qu.: 3944   1st Qu.: 3956   1st Qu.: 3947   1st Qu.: 3961   1st Qu.: 3950   1st Qu.: 3947   1st Qu.: 3967  
    ##  Median : 6134   Median : 6147   Median : 6135   Median : 6151   Median : 6139   Median : 6163   Median : 6140   Median : 6160   Median : 6142  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8987   3rd Qu.: 8984   3rd Qu.: 8998   3rd Qu.: 8988   3rd Qu.: 8974   3rd Qu.: 8994   3rd Qu.: 8991   3rd Qu.: 8988   3rd Qu.: 8977  
    ##  Max.   :30039   Max.   :29419   Max.   :29586   Max.   :29499   Max.   :29845   Max.   :29635   Max.   :29681   Max.   :29849   Max.   :29843  
    ##                                                                                                                                                 
    ##    NWEIGHT14       NWEIGHT15       NWEIGHT16       NWEIGHT17       NWEIGHT18       NWEIGHT19       NWEIGHT20       NWEIGHT21       NWEIGHT22    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3962   1st Qu.: 3958   1st Qu.: 3958   1st Qu.: 3958   1st Qu.: 3937   1st Qu.: 3947   1st Qu.: 3943   1st Qu.: 3960   1st Qu.: 3964  
    ##  Median : 6154   Median : 6145   Median : 6133   Median : 6126   Median : 6155   Median : 6153   Median : 6139   Median : 6135   Median : 6149  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8981   3rd Qu.: 8997   3rd Qu.: 8979   3rd Qu.: 8977   3rd Qu.: 8993   3rd Qu.: 8979   3rd Qu.: 8992   3rd Qu.: 8956   3rd Qu.: 8988  
    ##  Max.   :30184   Max.   :29970   Max.   :29825   Max.   :30606   Max.   :29689   Max.   :29336   Max.   :30274   Max.   :29766   Max.   :29791  
    ##                                                                                                                                                 
    ##    NWEIGHT23       NWEIGHT24       NWEIGHT25       NWEIGHT26       NWEIGHT27       NWEIGHT28       NWEIGHT29       NWEIGHT30       NWEIGHT31    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3943   1st Qu.: 3946   1st Qu.: 3952   1st Qu.: 3966   1st Qu.: 3942   1st Qu.: 3956   1st Qu.: 3970   1st Qu.: 3956   1st Qu.: 3944  
    ##  Median : 6148   Median : 6136   Median : 6150   Median : 6136   Median : 6125   Median : 6149   Median : 6146   Median : 6149   Median : 6144  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8980   3rd Qu.: 8978   3rd Qu.: 8972   3rd Qu.: 8980   3rd Qu.: 8996   3rd Qu.: 8989   3rd Qu.: 8979   3rd Qu.: 8991   3rd Qu.: 8994  
    ##  Max.   :30126   Max.   :29946   Max.   :30445   Max.   :29893   Max.   :30030   Max.   :29599   Max.   :30136   Max.   :29895   Max.   :29604  
    ##                                                                                                                                                 
    ##    NWEIGHT32       NWEIGHT33       NWEIGHT34       NWEIGHT35       NWEIGHT36       NWEIGHT37       NWEIGHT38       NWEIGHT39       NWEIGHT40    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3954   1st Qu.: 3964   1st Qu.: 3950   1st Qu.: 3967   1st Qu.: 3948   1st Qu.: 3955   1st Qu.: 3954   1st Qu.: 3940   1st Qu.: 3959  
    ##  Median : 6159   Median : 6148   Median : 6139   Median : 6141   Median : 6149   Median : 6133   Median : 6139   Median : 6147   Median : 6144  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8982   3rd Qu.: 8993   3rd Qu.: 8985   3rd Qu.: 8990   3rd Qu.: 8979   3rd Qu.: 8975   3rd Qu.: 8974   3rd Qu.: 8991   3rd Qu.: 8980  
    ##  Max.   :29310   Max.   :29408   Max.   :29564   Max.   :30437   Max.   :27896   Max.   :30596   Max.   :30130   Max.   :29262   Max.   :30344  
    ##                                                                                                                                                 
    ##    NWEIGHT41       NWEIGHT42       NWEIGHT43       NWEIGHT44       NWEIGHT45       NWEIGHT46       NWEIGHT47       NWEIGHT48       NWEIGHT49    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3975   1st Qu.: 3949   1st Qu.: 3947   1st Qu.: 3956   1st Qu.: 3952   1st Qu.: 3966   1st Qu.: 3938   1st Qu.: 3953   1st Qu.: 3947  
    ##  Median : 6153   Median : 6137   Median : 6157   Median : 6148   Median : 6149   Median : 6152   Median : 6150   Median : 6139   Median : 6146  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8982   3rd Qu.: 8988   3rd Qu.: 9005   3rd Qu.: 8986   3rd Qu.: 8992   3rd Qu.: 8959   3rd Qu.: 8991   3rd Qu.: 8991   3rd Qu.: 8990  
    ##  Max.   :29594   Max.   :29938   Max.   :29878   Max.   :29896   Max.   :29729   Max.   :29103   Max.   :30070   Max.   :29343   Max.   :29590  
    ##                                                                                                                                                 
    ##    NWEIGHT50       NWEIGHT51       NWEIGHT52       NWEIGHT53       NWEIGHT54       NWEIGHT55       NWEIGHT56       NWEIGHT57       NWEIGHT58    
    ##  Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0   Min.   :    0  
    ##  1st Qu.: 3948   1st Qu.: 3958   1st Qu.: 3938   1st Qu.: 3959   1st Qu.: 3954   1st Qu.: 3945   1st Qu.: 3957   1st Qu.: 3942   1st Qu.: 3962  
    ##  Median : 6159   Median : 6150   Median : 6154   Median : 6156   Median : 6151   Median : 6143   Median : 6153   Median : 6138   Median : 6137  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679   Mean   : 6679  
    ##  3rd Qu.: 8995   3rd Qu.: 8992   3rd Qu.: 9012   3rd Qu.: 8979   3rd Qu.: 8973   3rd Qu.: 8977   3rd Qu.: 8995   3rd Qu.: 9004   3rd Qu.: 8986  
    ##  Max.   :30027   Max.   :29247   Max.   :29445   Max.   :30131   Max.   :29439   Max.   :29216   Max.   :29203   Max.   :29819   Max.   :29818  
    ##                                                                                                                                                 
    ##    NWEIGHT59       NWEIGHT60         BTUEL             DOLLAREL           BTUNG            DOLLARNG          BTULP           DOLLARLP      
    ##  Min.   :    0   Min.   :    0   Min.   :   143.3   Min.   : -889.5   Min.   :      0   Min.   :   0.0   Min.   :     0   Min.   :   0.00  
    ##  1st Qu.: 3965   1st Qu.: 3953   1st Qu.: 20205.8   1st Qu.:  836.5   1st Qu.:      0   1st Qu.:   0.0   1st Qu.:     0   1st Qu.:   0.00  
    ##  Median : 6144   Median : 6140   Median : 31890.0   Median : 1257.9   Median :  22012   Median : 313.9   Median :     0   Median :   0.00  
    ##  Mean   : 6679   Mean   : 6679   Mean   : 37016.2   Mean   : 1424.8   Mean   :  36961   Mean   : 396.0   Mean   :  3917   Mean   :  80.89  
    ##  3rd Qu.: 8977   3rd Qu.: 8983   3rd Qu.: 48298.0   3rd Qu.: 1819.0   3rd Qu.:  62714   3rd Qu.: 644.9   3rd Qu.:     0   3rd Qu.:   0.00  
    ##  Max.   :29606   Max.   :29818   Max.   :628155.5   Max.   :15680.2   Max.   :1134709   Max.   :8155.0   Max.   :364215   Max.   :6621.44  
    ##                                                                                                                                            
    ##      BTUFO           DOLLARFO          BTUWOOD          TOTALBTU          TOTALDOL      
    ##  Min.   :     0   Min.   :   0.00   Min.   :     0   Min.   :   1182   Min.   : -150.5  
    ##  1st Qu.:     0   1st Qu.:   0.00   1st Qu.:     0   1st Qu.:  45565   1st Qu.: 1258.3  
    ##  Median :     0   Median :   0.00   Median :     0   Median :  74180   Median : 1793.2  
    ##  Mean   :  5109   Mean   :  88.43   Mean   :  3596   Mean   :  83002   Mean   : 1990.2  
    ##  3rd Qu.:     0   3rd Qu.:   0.00   3rd Qu.:     0   3rd Qu.: 108535   3rd Qu.: 2472.0  
    ##  Max.   :426269   Max.   :7003.69   Max.   :500000   Max.   :1367548   Max.   :20043.4  
    ## 

``` r
str(recs_ord)
```

    ## tibble [18,496 × 100] (S3: tbl_df/tbl/data.frame)
    ##  $ DOEID           : num [1:18496] 1e+05 1e+05 1e+05 1e+05 1e+05 ...
    ##   ..- attr(*, "label")= chr "Unique identifier for each respondent"
    ##  $ Region          : Factor w/ 4 levels "Northeast","Midwest",..: 4 3 4 3 1 3 3 3 3 4 ...
    ##   ..- attr(*, "label")= chr "Census Region"
    ##  $ REGIONC         : chr [1:18496] "WEST" "SOUTH" "WEST" "SOUTH" ...
    ##   ..- attr(*, "label")= chr "Census Region"
    ##  $ Division        : Factor w/ 10 levels "New England",..: 9 7 9 5 2 7 7 6 5 9 ...
    ##   ..- attr(*, "label")= chr "Census Division, Mountain Division is divided into North and South for RECS purposes"
    ##  $ STATE_FIPS      : chr [1:18496] "35" "05" "35" "45" ...
    ##   ..- attr(*, "label")= chr "State Federal Information Processing System Code"
    ##  $ state_postal    : Factor w/ 51 levels "AL","AK","AZ",..: 32 4 32 41 31 44 37 25 9 3 ...
    ##   ..- attr(*, "label")= chr "State Postal Code"
    ##  $ state_name      : Factor w/ 51 levels "Alabama","Alaska",..: 32 4 32 41 31 44 37 25 9 3 ...
    ##   ..- attr(*, "label")= chr "State Name"
    ##  $ ClimateRegion_BA: Factor w/ 8 levels "Mixed-Dry","Mixed-Humid",..: 1 2 1 2 2 3 2 2 2 4 ...
    ##   ..- attr(*, "label")= chr "Building America Climate Zone"
    ##  $ Urbanicity      : Factor w/ 3 levels "Urban Area","Urban Cluster",..: 1 1 1 1 1 1 1 2 1 1 ...
    ##   ..- attr(*, "label")= chr "2010 Census Urban Type Code"
    ##  $ HDD65           : num [1:18496] 3844 3766 3819 2614 4219 ...
    ##   ..- attr(*, "label")= chr "Heating degree days in 2020, base temperature 65F; Derived from the weighted temperatures of nearby weather stations"
    ##  $ CDD65           : num [1:18496] 1679 1458 1696 1718 1363 ...
    ##   ..- attr(*, "label")= chr "Cooling degree days in 2020, base temperature 65F; Derived from the weighted temperatures of nearby weather stations"
    ##  $ HDD30YR         : num [1:18496] 4451 4429 4500 3229 4896 ...
    ##   ..- attr(*, "label")= chr "Heating degree days, 30-year average 1981-2010, base temperature 65F; Taken from nearest weather station, inocu"| __truncated__
    ##  $ CDD30YR         : num [1:18496] 1027 1305 1010 1653 1059 ...
    ##   ..- attr(*, "label")= chr "Cooling degree days, 30-year average 1981-2010, base temperature 65F; Taken from nearest weather station, inocu"| __truncated__
    ##  $ HousingUnitType : Factor w/ 5 levels "Mobile home",..: 2 5 5 2 5 2 2 5 5 5 ...
    ##   ..- attr(*, "label")= chr "Type of housing unit"
    ##   ..- attr(*, "Question")= chr "Which best describes your home?"
    ##  $ YearMade        : Ord.factor w/ 9 levels "Before 1950"<..: 4 5 3 5 3 6 2 7 7 5 ...
    ##   ..- attr(*, "label")= chr "Range when housing unit was built"
    ##   ..- attr(*, "Question")= chr "Derived from: In what year was your home built? AND Although you do not know the exact year your home was built"| __truncated__
    ##  $ SpaceHeatingUsed: logi [1:18496] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..- attr(*, "label")= chr "Space heating equipment used"
    ##   ..- attr(*, "Question")= chr "Is your home heated during the winter?"
    ##  $ ACUsed          : logi [1:18496] TRUE TRUE TRUE TRUE TRUE TRUE ...
    ##   ..- attr(*, "label")= chr "Air conditioning equipment used"
    ##  $ HeatingBehavior : Factor w/ 7 levels "Set one temp and leave it",..: 1 4 1 1 1 3 2 2 1 2 ...
    ##   ..- attr(*, "label")= chr "Winter temperature control method"
    ##   ..- attr(*, "Question")= chr "Which of the following best describes how your household controls the indoor temperature during the winter?"
    ##  $ WinterTempDay   : num [1:18496] 70 70 69 68 68 76 74 70 68 70 ...
    ##   ..- attr(*, "label")= chr "Winter thermostat setting or temperature in home when someone is home during the day"
    ##   ..- attr(*, "Question")= chr "During the winter, what is your home’s typical indoor temperature when someone is home during the day?"
    ##  $ WinterTempAway  : num [1:18496] 70 65 68 68 68 76 65 70 60 70 ...
    ##   ..- attr(*, "label")= chr "Winter thermostat setting or temperature in home when no one is home during the day"
    ##   ..- attr(*, "Question")= chr "During the winter, what is your home’s typical indoor temperature when no one is inside your home during the day?"
    ##  $ WinterTempNight : num [1:18496] 68 65 67 68 68 68 74 68 62 68 ...
    ##   ..- attr(*, "label")= chr "Winter thermostat setting or temperature in home at night"
    ##   ..- attr(*, "Question")= chr "During the winter, what is your home’s typical indoor temperature inside your home at night?"
    ##  $ ACBehavior      : Factor w/ 7 levels "Set one temp and leave it",..: 1 4 1 1 2 3 2 7 2 2 ...
    ##   ..- attr(*, "label")= chr "Summer temperature control method"
    ##   ..- attr(*, "Question")= chr "Which of the following best describes how your household controls the indoor temperature during the summer?"
    ##  $ SummerTempDay   : num [1:18496] 71 68 70 72 72 69 68 NA 72 74 ...
    ##   ..- attr(*, "label")= chr "Summer thermostat setting or temperature in home when someone is home during the day"
    ##   ..- attr(*, "Question")= chr "During the summer, what is your home’s typical indoor temperature when someone is home during the day?"
    ##  $ SummerTempAway  : num [1:18496] 71 68 68 72 72 74 70 NA 76 74 ...
    ##   ..- attr(*, "label")= chr "Summer thermostat setting or temperature in home when no one is home during the day"
    ##   ..- attr(*, "Question")= chr "During the summer, what is your home’s typical indoor temperature when no one is inside your home during the day?"
    ##  $ SummerTempNight : num [1:18496] 71 68 68 72 72 68 70 NA 68 72 ...
    ##   ..- attr(*, "label")= chr "Summer thermostat setting or temperature in home at night"
    ##   ..- attr(*, "Question")= chr "During the summer, what is your home’s typical indoor temperature inside your home at night?"
    ##  $ TOTSQFT_EN      : num [1:18496] 2100 590 900 2100 800 4520 2100 900 750 760 ...
    ##   ..- attr(*, "label")= chr "Total energy-consuming area (square footage) of the housing unit. Includes all main living areas; all basements"| __truncated__
    ##  $ TOTHSQFT        : num [1:18496] 2100 590 900 2100 800 3010 1200 900 750 760 ...
    ##   ..- attr(*, "label")= chr "Square footage of the housing unit that is heated by space heating equipment. A derived variable rounded to the nearest 10"
    ##  $ TOTCSQFT        : num [1:18496] 2100 590 900 2100 800 3010 1200 0 500 760 ...
    ##   ..- attr(*, "label")= chr "Square footage of the housing unit that is cooled by air-conditioning equipment or evaporative cooler, a derive"| __truncated__
    ##  $ NWEIGHT         : num [1:18496] 3284 9007 5669 5294 9935 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight"
    ##  $ NWEIGHT1        : num [1:18496] 3273 9020 5793 5361 10048 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 1"
    ##  $ NWEIGHT2        : num [1:18496] 3349 9081 5914 5362 10262 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 2"
    ##  $ NWEIGHT3        : num [1:18496] 3345 9020 5763 5371 10037 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 3"
    ##  $ NWEIGHT4        : num [1:18496] 3437 9213 5870 5393 9961 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 4"
    ##  $ NWEIGHT5        : num [1:18496] 3416 9117 5721 5328 10108 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 5"
    ##  $ NWEIGHT6        : num [1:18496] 3355 9179 5663 5354 10298 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 6"
    ##  $ NWEIGHT7        : num [1:18496] 3372 9096 5700 5325 10065 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 7"
    ##  $ NWEIGHT8        : num [1:18496] 3364 8920 5704 5376 10097 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 8"
    ##  $ NWEIGHT9        : num [1:18496] 3362 9189 5668 5391 10321 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 9"
    ##  $ NWEIGHT10       : num [1:18496] 3302 9060 5793 5501 9944 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 10"
    ##  $ NWEIGHT11       : num [1:18496] 3211 9127 5806 5427 10267 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 11"
    ##  $ NWEIGHT12       : num [1:18496] 3500 9264 5650 5384 10127 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 12"
    ##  $ NWEIGHT13       : num [1:18496] 3314 9222 5648 5302 10241 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 13"
    ##  $ NWEIGHT14       : num [1:18496] 3359 9199 5829 5362 9872 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 14"
    ##  $ NWEIGHT15       : num [1:18496] 3424 9143 5642 5383 10275 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 15"
    ##  $ NWEIGHT16       : num [1:18496] 3384 9042 5718 5381 9921 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 16"
    ##  $ NWEIGHT17       : num [1:18496] 3312 9417 5969 5418 10312 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 17"
    ##  $ NWEIGHT18       : num [1:18496] 3324 9163 5828 5356 10004 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 18"
    ##  $ NWEIGHT19       : num [1:18496] 3367 9192 5814 5343 10437 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 19"
    ##  $ NWEIGHT20       : num [1:18496] 3327 9092 5697 5360 10101 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 20"
    ##  $ NWEIGHT21       : num [1:18496] 3340 0 5687 5336 9982 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 21"
    ##  $ NWEIGHT22       : num [1:18496] 3292 9098 5739 5390 10000 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 22"
    ##  $ NWEIGHT23       : num [1:18496] 3278 9320 5945 5397 10180 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 23"
    ##  $ NWEIGHT24       : num [1:18496] 3340 9081 5820 5448 9826 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 24"
    ##  $ NWEIGHT25       : num [1:18496] 3386 9406 5823 5382 10149 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 25"
    ##  $ NWEIGHT26       : num [1:18496] 3301 9256 5650 5387 0 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 26"
    ##  $ NWEIGHT27       : num [1:18496] 3312 9318 5862 5351 10141 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 27"
    ##  $ NWEIGHT28       : num [1:18496] 3348 9154 5707 5371 9948 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 28"
    ##  $ NWEIGHT29       : num [1:18496] 3356 9372 5619 5362 10065 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 29"
    ##  $ NWEIGHT30       : num [1:18496] 3322 9137 5796 5381 10083 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 30"
    ##  $ NWEIGHT31       : num [1:18496] 3256 9233 5995 5320 10133 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 31"
    ##  $ NWEIGHT32       : num [1:18496] 3318 9115 0 5339 9978 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 32"
    ##  $ NWEIGHT33       : num [1:18496] 3402 9177 5638 0 10213 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 33"
    ##  $ NWEIGHT34       : num [1:18496] 3364 9191 5619 5380 9964 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 34"
    ##  $ NWEIGHT35       : num [1:18496] 3304 9100 5652 5363 10071 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 35"
    ##  $ NWEIGHT36       : num [1:18496] 3333 9072 5834 5477 9988 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 36"
    ##  $ NWEIGHT37       : num [1:18496] 3390 9263 5712 5386 10120 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 37"
    ##  $ NWEIGHT38       : num [1:18496] 3382 9078 5765 5326 10024 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 38"
    ##  $ NWEIGHT39       : num [1:18496] 3329 9011 5887 5421 10024 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 39"
    ##  $ NWEIGHT40       : num [1:18496] 3293 9166 5650 5370 10185 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 40"
    ##  $ NWEIGHT41       : num [1:18496] 3295 9091 5958 5339 10069 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 41"
    ##  $ NWEIGHT42       : num [1:18496] 3414 9194 5593 5329 9959 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 42"
    ##  $ NWEIGHT43       : num [1:18496] 3264 9215 6035 5409 10352 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 43"
    ##  $ NWEIGHT44       : num [1:18496] 3342 9048 5732 5416 10092 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 44"
    ##  $ NWEIGHT45       : num [1:18496] 3275 9259 5877 5453 10228 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 45"
    ##  $ NWEIGHT46       : num [1:18496] 3364 9171 5654 5449 10069 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 46"
    ##  $ NWEIGHT47       : num [1:18496] 3336 9260 5763 5376 9996 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 47"
    ##  $ NWEIGHT48       : num [1:18496] 3329 9105 5929 5408 10198 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 48"
    ##  $ NWEIGHT49       : num [1:18496] 3348 9117 5772 5400 10094 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 49"
    ##  $ NWEIGHT50       : num [1:18496] 3357 9261 5785 5359 10196 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 50"
    ##  $ NWEIGHT51       : num [1:18496] 3335 8955 5636 5448 10017 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 51"
    ##  $ NWEIGHT52       : num [1:18496] 3240 9000 5944 5344 9954 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 52"
    ##  $ NWEIGHT53       : num [1:18496] 3430 9290 5684 5438 10051 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 53"
    ##  $ NWEIGHT54       : num [1:18496] 3294 9199 5736 5378 10019 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 54"
    ##  $ NWEIGHT55       : num [1:18496] 3398 8959 5675 5357 10310 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 55"
    ##  $ NWEIGHT56       : num [1:18496] 3293 9233 5661 5421 10143 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 56"
    ##  $ NWEIGHT57       : num [1:18496] 0 9140 5917 5365 10177 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 57"
    ##  $ NWEIGHT58       : num [1:18496] 3370 9307 5571 5402 10043 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 58"
    ##  $ NWEIGHT59       : num [1:18496] 3358 9062 5887 5403 10248 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 59"
    ##  $ NWEIGHT60       : num [1:18496] 3404 8958 5838 5351 10110 ...
    ##   ..- attr(*, "label")= chr "Final Analysis Weight for replicate 60"
    ##  $ BTUEL           : num [1:18496] 42723 17889 8147 31647 20027 ...
    ##   ..- attr(*, "label")= chr "Total electricity use, in thousand Btu, 2020, including self-generation of solar power"
    ##  $ DOLLAREL        : num [1:18496] 1955 713 335 1425 1087 ...
    ##   ..- attr(*, "label")= chr "Total electricity cost, in dollars, 2020"
    ##  $ BTUNG           : num [1:18496] 101924 10145 22603 55119 39100 ...
    ##   ..- attr(*, "label")= chr "Total natural gas use, in thousand Btu, 2020"
    ##  $ DOLLARNG        : num [1:18496] 702 262 188 637 376 ...
    ##   ..- attr(*, "label")= chr "Total natural gas cost, in dollars, 2020"
    ##  $ BTULP           : num [1:18496] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..- attr(*, "label")= chr "Total propane use, in thousand Btu, 2020"
    ##  $ DOLLARLP        : num [1:18496] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..- attr(*, "label")= chr "Total propane cost, in dollars, 2020"
    ##  $ BTUFO           : num [1:18496] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..- attr(*, "label")= chr "Total fuel oil/kerosene use, in thousand Btu, 2020"
    ##  $ DOLLARFO        : num [1:18496] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..- attr(*, "label")= chr "Total fuel oil/kerosene cost, in dollars, 2020"
    ##  $ BTUWOOD         : num [1:18496] 0 0 0 0 0 3000 0 0 0 0 ...
    ##   ..- attr(*, "label")= chr "Total wood use, in thousand Btu, 2020"
    ##  $ TOTALBTU        : num [1:18496] 144648 28035 30750 86765 59127 ...
    ##   ..- attr(*, "label")= chr "Total usage including electricity, natural gas, propane, and fuel oil, in thousand Btu, 2020"
    ##   [list output truncated]

``` r
recs_der_tmp_loc <- here::here("osf_dl", "recs_2020.rds")
write_rds(recs_ord, recs_der_tmp_loc)
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
