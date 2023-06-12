American National Election Studies (ANES) 2020 Time Series Study Data
Prep
================

## Data information

All data and resources were downloaded from
<https://electionstudies.org/data-center/2020-time-series-study/> on
February 28, 2022.

American National Election Studies. 2021. ANES 2020 Time Series Study
Full Release \[dataset and documentation\]. www.electionstudies.org

``` r
library(here) # easy relative paths
```

``` r
library(tidyverse) # data manipulation
library(haven) # data import
library(tidylog) # informative logging messages
library(osfr)
```

## Import data and create derived variables

``` r
anes_file_osf_det <- osf_retrieve_node("https://osf.io/z5c3m/") %>%
  osf_ls_files(path="ANES_2020", pattern="sav") %>%
  osf_download(conflicts="overwrite", path=here("osf_dl"))

anes_in_2020 <- read_sav(pull(anes_file_osf_det, local_path))

unlink(pull(anes_file_osf_det, local_path))

# weight validity for post-election survey
anes_in_2020 %>%
   select(V200004, V200010a, V200010b) %>%
   group_by(V200004) %>% #type of respondent
   summarise(
      n=n(),
      nvalidwt_pre=sum(!is.na(V200010a) & V200010a>0),
      nvalidwt_post=sum(!is.na(V200010b) & V200010b>0)
   )
```

    ## select: dropped 1,768 variables (version, V200001, V160001_orig, V200002, V200003, …)

    ## group_by: one grouping variable (V200004)

    ## summarise: now 2 rows and 4 columns, ungrouped

    ## # A tibble: 2 × 4
    ##   V200004                                                     n nvalidwt_pre nvalidwt_post
    ##   <dbl+lbl>                                               <int>        <int>         <int>
    ## 1 1 [1. pre-election interview (only) complete]             827          827             0
    ## 2 3 [3. pre and post-election interviews (both) complete]  7453         7453          7453

``` r
# Are all PSU/Stratum represented in post-weight? If so, we can drop pre-only cases later

anes_in_2020 %>%
   count(V200010d, V200010c, V200004) %>%
   group_by(V200010d, V200010c) %>%
   mutate(
      Pct=n/sum(n)
   ) %>%
   filter(V200004==3) %>%
   arrange(Pct)
```

    ## count: now 202 rows and 4 columns, ungrouped

    ## group_by: 2 grouping variables (V200010d, V200010c)

    ## mutate (grouped): new variable 'Pct' (double) with 168 unique values and 0% NA

    ## filter (grouped): removed 101 rows (50%), 101 rows remaining

    ## # A tibble: 101 × 5
    ## # Groups:   V200010d, V200010c [101]
    ##    V200010d V200010c V200004                                                     n   Pct
    ##       <dbl>    <dbl> <dbl+lbl>                                               <int> <dbl>
    ##  1       32        1 3 [3. pre and post-election interviews (both) complete]    63 0.797
    ##  2       33        2 3 [3. pre and post-election interviews (both) complete]    67 0.798
    ##  3       45        1 3 [3. pre and post-election interviews (both) complete]    60 0.8  
    ##  4        8        1 3 [3. pre and post-election interviews (both) complete]    72 0.828
    ##  5       38        2 3 [3. pre and post-election interviews (both) complete]    71 0.835
    ##  6       49        2 3 [3. pre and post-election interviews (both) complete]    67 0.838
    ##  7       36        1 3 [3. pre and post-election interviews (both) complete]    68 0.85 
    ##  8       47        2 3 [3. pre and post-election interviews (both) complete]    69 0.852
    ##  9       50        2 3 [3. pre and post-election interviews (both) complete]    69 0.852
    ## 10        4        1 3 [3. pre and post-election interviews (both) complete]    76 0.854
    ## # ℹ 91 more rows

``` r
anes_2020 <- anes_in_2020 %>%
   filter(V200004==3) %>%
   select(
      "V200010b", # FULL SAMPLE POST-ELECTION WEIGHT
      "V200010d", # FULL SAMPLE VARIANCE STRATUM
      "V200010c", # FULL SAMPLE VARIANCE UNIT
      "V200002", # MODE OF INTERVIEW: PRE-ELECTION INTERVIEW
      "V201006", # PRE: HOW INTERESTED IN FOLLOWING CAMPAIGNS
      "V201102", # PRE: DID R VOTE FOR PRESIDENT IN 2016
      "V201101", # PRE: DID R VOTE FOR PRESIDENT IN 2016 [REVISED]
      "V201103", # PRE: RECALL OF LAST (2016) PRESIDENTIAL VOTE CHOICE)
      "V201025x", # PRE: SUMMARY: REGISTRATION AND EARLY VOTE STATUS
      "V201231x", # PRE: SUMMARY: PARTY ID
      "V201233", # PRE: HOW OFTEN TRUST GOVERNMENT IN WASHINGTON TO DO WHAT IS RIGHT [REVISED]
      "V201237", # PRE: HOW OFTEN CAN PEOPLE BE TRUSTED
      "V201507x", # PRE: SUMMARY: RESPONDENT AGE
      "V201510", # PRE: HIGHEST LEVEL OF EDUCATION
      "V201549x", # PRE: SUMMARY: R SELF-IDENTIFIED RACE/ETHNICITY
      "V201600", # PRE: WHAT IS YOUR (R) SEX? [REVISED]
      "V201617x", # PRE: SUMMARY: TOTAL (FAMILY) INCOME
      "V202066", # POST: DID R VOTE IN NOVEMBER 2020 ELECTION
      "V202109x", # PRE-POST: SUMMARY: VOTER TURNOUT IN 2020
      "V202072", # POST: DID R VOTE FOR PRESIDENT
      "V202073", # POST: FOR WHOM DID R VOTE FOR PRESIDENT
      "V202110x" # PRE-POST: SUMMARY: 2020 PRESIDENTIAL VOTE
   ) %>%
   mutate(
      InterviewMode = fct_recode(as.character(V200002), Video = "1", Telephone = "2", Web = "3"),
      Weight = V200010b,
      Stratum = as.factor(V200010d),
      VarUnit = as.factor(V200010c),
      Age = if_else(V201507x > 0, as.numeric(V201507x), NA_real_),
      AgeGroup = cut(Age, c(17, 29, 39, 49, 59, 69, 200),
                     labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70 or older")
      ),
      Gender = factor(
         case_when(
            V201600 == 1 ~ "Male",
            V201600 == 2 ~ "Female",
            TRUE ~ NA_character_
         ),
         levels = c("Male", "Female")
      ),
      RaceEth = factor(
         case_when(
            V201549x == 1 ~ "White",
            V201549x == 2 ~ "Black",
            V201549x == 3 ~ "Hispanic",
            V201549x == 4 ~ "Asian, NH/PI",
            V201549x == 5 ~ "AI/AN",
            V201549x == 6 ~ "Other/multiple race",
            TRUE ~ NA_character_
         ),
         levels = c("White", "Black", "Hispanic", "Asian, NH/PI", "AI/AN", "Other/multiple race", NA_character_)
      ),
      PartyID = factor(
         case_when(
            V201231x == 1 ~ "Strong democrat",
            V201231x == 2 ~ "Not very strong democrat",
            V201231x == 3 ~ "Independent-democrat",
            V201231x == 4 ~ "Independent",
            V201231x == 5 ~ "Independent-republican",
            V201231x == 6 ~ "Not very strong republican",
            V201231x == 7 ~ "Strong republican",
            TRUE ~ NA_character_
         ),
         levels = c("Strong democrat", "Not very strong democrat", "Independent-democrat", "Independent", "Independent-republican", "Not very strong republican", "Strong republican")
      ),
      Education = factor(
         case_when(
            V201510 <= 0 ~ NA_character_,
            V201510 == 1 ~ "Less than HS",
            V201510 == 2 ~ "High school",
            V201510 <= 5 ~ "Post HS",
            V201510 == 6 ~ "Bachelor's",
            V201510 <= 8 ~ "Graduate",
            TRUE ~ NA_character_
         ),
         levels = c("Less than HS", "High school", "Post HS", "Bachelor's", "Graduate")
      ),
      Income = cut(V201617x, c(-5, 1:22),
                   labels = c(
                      "Under $9,999",
                      "$10,000-14,999",
                      "$15,000-19,999",
                      "$20,000-24,999",
                      "$25,000-29,999",
                      "$30,000-34,999",
                      "$35,000-39,999",
                      "$40,000-44,999",
                      "$45,000-49,999",
                      "$50,000-59,999",
                      "$60,000-64,999",
                      "$65,000-69,999",
                      "$70,000-74,999",
                      "$75,000-79,999",
                      "$80,000-89,999",
                      "$90,000-99,999",
                      "$100,000-109,999",
                      "$110,000-124,999",
                      "$125,000-149,999",
                      "$150,000-174,999",
                      "$175,000-249,999",
                      "$250,000 or more"
                   )
      ),
      Income7 = fct_collapse(
         Income,
         "Under $20k" = c("Under $9,999", "$10,000-14,999", "$15,000-19,999"),
         "$20-40k" = c("$20,000-24,999", "$25,000-29,999", "$30,000-34,999", "$35,000-39,999"),
         "$40-60k" = c("$40,000-44,999", "$45,000-49,999", "$50,000-59,999"),
         "$60-80k" = c("$60,000-64,999", "$65,000-69,999", "$70,000-74,999", "$75,000-79,999"),
         "$80-100k" = c("$80,000-89,999", "$90,000-99,999"),
         "$100-125k" = c("$100,000-109,999", "$110,000-124,999"),
         "$125k or more" = c("$125,000-149,999", "$150,000-174,999", "$175,000-249,999", "$250,000 or more")
      ),
      CampaignInterest = factor(
         case_when(
            V201006 == 1 ~ "Very much interested",
            V201006 == 2 ~ "Somewhat interested",
            V201006 == 3 ~ "Not much interested",
            TRUE ~ NA_character_
         ),
         levels = c("Very much interested", "Somewhat interested", "Not much interested")
      ),
      TrustGovernment = factor(
         case_when(
            V201233 == 1 ~ "Always",
            V201233 == 2 ~ "Most of the time",
            V201233 == 3 ~ "About half the time",
            V201233 == 4 ~ "Some of the time",
            V201233 == 5 ~ "Never",
            TRUE ~ NA_character_
         ),
         levels = c("Always", "Most of the time", "About half the time", "Some of the time", "Never")
      ),
      TrustPeople = factor(
         case_when(
            V201237 == 1 ~ "Always",
            V201237 == 2 ~ "Most of the time",
            V201237 == 3 ~ "About half the time",
            V201237 == 4 ~ "Some of the time",
            V201237 == 5 ~ "Never",
            TRUE ~ NA_character_
         ),
         levels = c("Always", "Most of the time", "About half the time", "Some of the time", "Never")
      ),
      VotedPres2016 = factor(
         case_when(
            V201101 == 1 | V201102 == 1 ~ "Yes",
            V201101 == 2 | V201102 == 2 ~ "No",
            TRUE ~ NA_character_
         ),
         levels = c("Yes", "No")
      ),
      VotedPres2016_selection = factor(
         case_when(
            V201103 == 1 ~ "Clinton",
            V201103 == 2 ~ "Trump",
            V201103 == 5 ~ "Other",
            TRUE ~ NA_character_
         ),
         levels = c("Clinton", "Trump", "Other")
      ),
      VotedPres2020 = factor(
         case_when(
            V202109x == 1 ~ "Yes",
            V202109x == 0 ~ "No",
            TRUE ~ NA_character_
         ),
         levels = c("Yes", "No")
      ),
      VotedPres2020_selection = factor(
         case_when(
            V202073 == 1 ~ "Biden",
            V202073 == 2 ~ "Trump",
            V202073 >= 3 & V202073 <= 8~ "Other",
            V202073 == 11 ~ NA_character_,
            V202073 == 12 ~ NA_character_,
            TRUE ~ NA_character_
         ),
         levels = c("Biden", "Trump", "Other")
      ),
      EarlyVote2020 = factor(
         case_when(
            V201025x < 0 ~ NA_character_,
            V201025x == 4 ~ "Yes",
            VotedPres2020 == "Yes" ~ "No",
            TRUE ~ NA_character_), 
         levels = c("Yes", "No")
      )
   )
```

    ## filter: removed 827 rows (10%), 7,453 rows remaining

    ## select: dropped 1,749 variables (version, V200001, V160001_orig, V200003, V200004, …)

    ## mutate: new variable 'InterviewMode' (factor) with 3 unique values and 0% NA

    ##         new variable 'Weight' (double) with 7,195 unique values and 0% NA

    ##         new variable 'Stratum' (factor) with 50 unique values and 0% NA

    ##         new variable 'VarUnit' (factor) with 3 unique values and 0% NA

    ##         new variable 'Age' (double) with 64 unique values and 4% NA

    ##         new variable 'AgeGroup' (factor) with 7 unique values and 4% NA

    ##         new variable 'Gender' (factor) with 3 unique values and 1% NA

    ##         new variable 'RaceEth' (factor) with 7 unique values and 1% NA

    ##         new variable 'PartyID' (factor) with 8 unique values and <1% NA

    ##         new variable 'Education' (factor) with 6 unique values and 2% NA

    ##         new variable 'Income' (factor) with 23 unique values and 7% NA

    ##         new variable 'Income7' (factor) with 8 unique values and 7% NA

    ##         new variable 'CampaignInterest' (factor) with 4 unique values and <1% NA

    ##         new variable 'TrustGovernment' (factor) with 6 unique values and <1% NA

    ##         new variable 'TrustPeople' (factor) with 6 unique values and <1% NA

    ##         new variable 'VotedPres2016' (factor) with 3 unique values and <1% NA

    ##         new variable 'VotedPres2016_selection' (factor) with 4 unique values and 23% NA

    ##         new variable 'VotedPres2020' (factor) with 3 unique values and <1% NA

    ##         new variable 'VotedPres2020_selection' (factor) with 4 unique values and 21% NA

    ##         new variable 'EarlyVote2020' (factor) with 3 unique values and 14% NA

``` r
summary(anes_2020)
```

    ##     V200010b           V200010d        V200010c        V200002         V201006          V201102       
    ##  Min.   :0.008262   Min.   : 1.00   Min.   :1.000   Min.   :1.000   Min.   :-9.000   Min.   :-9.0000  
    ##  1st Qu.:0.386263   1st Qu.:12.00   1st Qu.:1.000   1st Qu.:3.000   1st Qu.: 1.000   1st Qu.:-1.0000  
    ##  Median :0.686301   Median :24.00   Median :2.000   Median :3.000   Median : 1.000   Median : 1.0000  
    ##  Mean   :1.000000   Mean   :24.63   Mean   :1.507   Mean   :2.911   Mean   : 1.596   Mean   : 0.1048  
    ##  3rd Qu.:1.211032   3rd Qu.:37.00   3rd Qu.:2.000   3rd Qu.:3.000   3rd Qu.: 2.000   3rd Qu.: 1.0000  
    ##  Max.   :6.650665   Max.   :50.00   Max.   :3.000   Max.   :3.000   Max.   : 3.000   Max.   : 2.0000  
    ##                                                                                                       
    ##     V201101            V201103          V201025x         V201231x         V201233          V201237     
    ##  Min.   :-9.00000   Min.   :-9.000   Min.   :-4.000   Min.   :-9.000   Min.   :-9.000   Min.   :-9.00  
    ##  1st Qu.:-1.00000   1st Qu.: 1.000   1st Qu.: 3.000   1st Qu.: 2.000   1st Qu.: 3.000   1st Qu.: 2.00  
    ##  Median :-1.00000   Median : 1.000   Median : 3.000   Median : 4.000   Median : 4.000   Median : 3.00  
    ##  Mean   : 0.08493   Mean   : 1.042   Mean   : 2.919   Mean   : 3.834   Mean   : 3.429   Mean   : 2.78  
    ##  3rd Qu.: 1.00000   3rd Qu.: 2.000   3rd Qu.: 3.000   3rd Qu.: 6.000   3rd Qu.: 4.000   3rd Qu.: 3.00  
    ##  Max.   : 2.00000   Max.   : 5.000   Max.   : 4.000   Max.   : 7.000   Max.   : 5.000   Max.   : 5.00  
    ##                                                                                                        
    ##     V201507x        V201510          V201549x         V201600          V201617x        V202066      
    ##  Min.   :-9.00   Min.   :-9.000   Min.   :-9.000   Min.   :-9.000   Min.   :-9.00   Min.   :-9.000  
    ##  1st Qu.:35.00   1st Qu.: 3.000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 4.00   1st Qu.: 4.000  
    ##  Median :51.00   Median : 5.000   Median : 1.000   Median : 2.000   Median :11.00   Median : 4.000  
    ##  Mean   :49.43   Mean   : 5.621   Mean   : 1.499   Mean   : 1.472   Mean   :10.36   Mean   : 3.402  
    ##  3rd Qu.:66.00   3rd Qu.: 6.000   3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.:17.00   3rd Qu.: 4.000  
    ##  Max.   :80.00   Max.   :95.000   Max.   : 6.000   Max.   : 2.000   Max.   :22.00   Max.   : 4.000  
    ##                                                                                                     
    ##     V202109x          V202072           V202073           V202110x         InterviewMode      Weight        
    ##  Min.   :-2.0000   Min.   :-9.0000   Min.   :-9.0000   Min.   :-9.0000   Video    : 274   Min.   :0.008262  
    ##  1st Qu.: 1.0000   1st Qu.: 1.0000   1st Qu.: 1.0000   1st Qu.: 1.0000   Telephone: 115   1st Qu.:0.386263  
    ##  Median : 1.0000   Median : 1.0000   Median : 1.0000   Median : 1.0000   Web      :7064   Median :0.686301  
    ##  Mean   : 0.8578   Mean   : 0.6234   Mean   : 0.9415   Mean   : 0.9902                    Mean   :1.000000  
    ##  3rd Qu.: 1.0000   3rd Qu.: 1.0000   3rd Qu.: 2.0000   3rd Qu.: 2.0000                    3rd Qu.:1.211032  
    ##  Max.   : 1.0000   Max.   : 2.0000   Max.   :12.0000   Max.   : 5.0000                    Max.   :6.650665  
    ##                                                                                                             
    ##     Stratum     VarUnit       Age               AgeGroup       Gender                    RaceEth    
    ##  12     : 179   1:3689   Min.   :18.00   18-29      : 871   Male  :3375   White              :5420  
    ##  6      : 172   2:3750   1st Qu.:37.00   30-39      :1241   Female:4027   Black              : 650  
    ##  27     : 172   3:  14   Median :53.00   40-49      :1081   NA's  :  51   Hispanic           : 662  
    ##  21     : 170            Mean   :51.83   50-59      :1200                 Asian, NH/PI       : 248  
    ##  25     : 169            3rd Qu.:66.00   60-69      :1436                 AI/AN              : 155  
    ##  1      : 167            Max.   :80.00   70 or older:1330                 Other/multiple race: 237  
    ##  (Other):6424            NA's   :294     NA's       : 294                 NA's               :  81  
    ##                      PartyID            Education                 Income              Income7    
    ##  Strong democrat         :1796   Less than HS: 312   Under $9,999    : 647   $125k or more:1468  
    ##  Strong republican       :1545   High school :1160   $50,000-59,999  : 485   Under $20k   :1076  
    ##  Independent-democrat    : 881   Post HS     :2514   $100,000-109,999: 451   $20-40k      :1051  
    ##  Independent             : 876   Bachelor's  :1877   $250,000 or more: 405   $40-60k      : 984  
    ##  Not very strong democrat: 790   Graduate    :1474   $80,000-89,999  : 383   $60-80k      : 920  
    ##  (Other)                 :1540   NA's        : 116   (Other)         :4565   (Other)      :1437  
    ##  NA's                    :  25                       NA's            : 517   NA's         : 517  
    ##              CampaignInterest            TrustGovernment              TrustPeople   VotedPres2016
    ##  Very much interested:3940    Always             :  80   Always             :  48   Yes :5810    
    ##  Somewhat interested :2569    Most of the time   :1016   Most of the time   :3511   No  :1622    
    ##  Not much interested : 943    About half the time:2313   About half the time:2020   NA's:  21    
    ##  NA's                :   1    Some of the time   :3313   Some of the time   :1597                
    ##                               Never              : 702   Never              : 264                
    ##                               NA's               :  29   NA's               :  13                
    ##                                                                                                  
    ##  VotedPres2016_selection VotedPres2020 VotedPres2020_selection EarlyVote2020
    ##  Clinton:2911            Yes :6407     Biden:3267              Yes : 371    
    ##  Trump  :2466            No  :1039     Trump:2462              No  :6035    
    ##  Other  : 390            NA's:   7     Other: 152              NA's:1047    
    ##  NA's   :1686                          NA's :1572                           
    ##                                                                             
    ##                                                                             
    ## 

## Check derived variables for correct coding

``` r
anes_2020 %>% count(InterviewMode, V200002)
```

    ## count: now 3 rows and 3 columns, ungrouped

    ## # A tibble: 3 × 3
    ##   InterviewMode V200002              n
    ##   <fct>         <dbl+lbl>        <int>
    ## 1 Video         1 [1. Video]       274
    ## 2 Telephone     2 [2. Telephone]   115
    ## 3 Web           3 [3. Web]        7064

``` r
anes_2020 %>%
   group_by(AgeGroup) %>%
   summarise(
      minAge = min(Age),
      maxAge = max(Age),
      minV = min(V201507x),
      maxV = max(V201507x)
   )
```

    ## group_by: one grouping variable (AgeGroup)

    ## summarise: now 7 rows and 5 columns, ungrouped

    ## # A tibble: 7 × 5
    ##   AgeGroup    minAge maxAge minV             maxV                    
    ##   <fct>        <dbl>  <dbl> <dbl+lbl>        <dbl+lbl>               
    ## 1 18-29           18     29 18               29                      
    ## 2 30-39           30     39 30               39                      
    ## 3 40-49           40     49 40               49                      
    ## 4 50-59           50     59 50               59                      
    ## 5 60-69           60     69 60               69                      
    ## 6 70 or older     70     80 70               80 [80. Age 80 or older]
    ## 7 <NA>            NA     NA -9 [-9. Refused] -9 [-9. Refused]

``` r
anes_2020 %>% count(Gender, V201600)
```

    ## count: now 3 rows and 3 columns, ungrouped

    ## # A tibble: 3 × 3
    ##   Gender V201600              n
    ##   <fct>  <dbl+lbl>        <int>
    ## 1 Male    1 [1. Male]      3375
    ## 2 Female  2 [2. Female]    4027
    ## 3 <NA>   -9 [-9. Refused]    51

``` r
anes_2020 %>% count(RaceEth, V201549x)
```

    ## count: now 8 rows and 3 columns, ungrouped

    ## # A tibble: 8 × 3
    ##   RaceEth             V201549x                                                                        n
    ##   <fct>               <dbl+lbl>                                                                   <int>
    ## 1 White                1 [1. White, non-Hispanic]                                                  5420
    ## 2 Black                2 [2. Black, non-Hispanic]                                                   650
    ## 3 Hispanic             3 [3. Hispanic]                                                              662
    ## 4 Asian, NH/PI         4 [4. Asian or Native Hawaiian/other Pacific Islander, non-Hispanic alone]   248
    ## 5 AI/AN                5 [5. Native American/Alaska Native or other race, non-Hispanic alone]       155
    ## 6 Other/multiple race  6 [6. Multiple races, non-Hispanic]                                          237
    ## 7 <NA>                -9 [-9. Refused]                                                               75
    ## 8 <NA>                -8 [-8. Don't know]                                                             6

``` r
anes_2020 %>% count(PartyID, V201231x)
```

    ## count: now 9 rows and 3 columns, ungrouped

    ## # A tibble: 9 × 3
    ##   PartyID                    V201231x                               n
    ##   <fct>                      <dbl+lbl>                          <int>
    ## 1 Strong democrat             1 [1. Strong Democrat]             1796
    ## 2 Not very strong democrat    2 [2. Not very strong Democrat]     790
    ## 3 Independent-democrat        3 [3. Independent-Democrat]         881
    ## 4 Independent                 4 [4. Independent]                  876
    ## 5 Independent-republican      5 [5. Independent-Republican]       782
    ## 6 Not very strong republican  6 [6. Not very strong Republican]   758
    ## 7 Strong republican           7 [7. Strong Republican]           1545
    ## 8 <NA>                       -9 [-9. Refused]                      23
    ## 9 <NA>                       -8 [-8. Don't know]                    2

``` r
anes_2020 %>% count(Education, V201510)
```

    ## count: now 11 rows and 3 columns, ungrouped

    ## # A tibble: 11 × 3
    ##    Education    V201510                                                                                       n
    ##    <fct>        <dbl+lbl>                                                                                 <int>
    ##  1 Less than HS  1 [1. Less than high school credential]                                                    312
    ##  2 High school   2 [2.  High school graduate - High school diploma or equivalent (e.g. GED)]               1160
    ##  3 Post HS       3 [3. Some college but no degree]                                                         1519
    ##  4 Post HS       4 [4. Associate degree in college - occupational/vocational]                               550
    ##  5 Post HS       5 [5. Associate degree in college - academic]                                              445
    ##  6 Bachelor's    6 [6. Bachelor's degree (e.g. BA, AB, BS)]                                                1877
    ##  7 Graduate      7 [7. Master's degree (e.g. MA, MS, MEng, MEd, MSW, MBA)]                                 1092
    ##  8 Graduate      8 [8. Professional school degree (e.g. MD, DDS, DVM, LLB, JD)/Doctoral degree (e.g. PHD…   382
    ##  9 <NA>         -9 [-9. Refused]                                                                             25
    ## 10 <NA>         -8 [-8. Don't know]                                                                           1
    ## 11 <NA>         95 [95. Other {SPECIFY}]                                                                     90

``` r
anes_2020 %>%
   count(Income, Income7, V201617x) %>%
   print(n = 30)
```

    ## count: now 24 rows and 4 columns, ungrouped

    ## # A tibble: 24 × 4
    ##    Income           Income7       V201617x                                                n
    ##    <fct>            <fct>         <dbl+lbl>                                           <int>
    ##  1 Under $9,999     Under $20k     1 [1. Under $9,999]                                  647
    ##  2 $10,000-14,999   Under $20k     2 [2. $10,000-14,999]                                244
    ##  3 $15,000-19,999   Under $20k     3 [3. $15,000-19,999]                                185
    ##  4 $20,000-24,999   $20-40k        4 [4. $20,000-24,999]                                301
    ##  5 $25,000-29,999   $20-40k        5 [5. $25,000-29,999]                                228
    ##  6 $30,000-34,999   $20-40k        6 [6. $30,000-34,999]                                296
    ##  7 $35,000-39,999   $20-40k        7 [7. $35,000-39,999]                                226
    ##  8 $40,000-44,999   $40-60k        8 [8. $40,000-44,999]                                286
    ##  9 $45,000-49,999   $40-60k        9 [9. $45,000-49,999]                                213
    ## 10 $50,000-59,999   $40-60k       10 [10. $50,000-59,999]                               485
    ## 11 $60,000-64,999   $60-80k       11 [11. $60,000-64,999]                               294
    ## 12 $65,000-69,999   $60-80k       12 [12. $65,000-69,999]                               168
    ## 13 $70,000-74,999   $60-80k       13 [13. $70,000-74,999]                               243
    ## 14 $75,000-79,999   $60-80k       14 [14. $75,000-79,999]                               215
    ## 15 $80,000-89,999   $80-100k      15 [15. $80,000-89,999]                               383
    ## 16 $90,000-99,999   $80-100k      16 [16. $90,000-99,999]                               291
    ## 17 $100,000-109,999 $100-125k     17 [17. $100,000-109,999]                             451
    ## 18 $110,000-124,999 $100-125k     18 [18. $110,000-124,999]                             312
    ## 19 $125,000-149,999 $125k or more 19 [19. $125,000-149,999]                             323
    ## 20 $150,000-174,999 $125k or more 20 [20. $150,000-174,999]                             366
    ## 21 $175,000-249,999 $125k or more 21 [21. $175,000-249,999]                             374
    ## 22 $250,000 or more $125k or more 22 [22. $250,000 or more]                             405
    ## 23 <NA>             <NA>          -9 [-9. Refused]                                      502
    ## 24 <NA>             <NA>          -5 [-5. Interview breakoff (sufficient partial IW)]    15

``` r
anes_2020 %>% count(CampaignInterest, V201006)
```

    ## count: now 4 rows and 3 columns, ungrouped

    ## # A tibble: 4 × 3
    ##   CampaignInterest     V201006                          n
    ##   <fct>                <dbl+lbl>                    <int>
    ## 1 Very much interested  1 [1. Very much interested]  3940
    ## 2 Somewhat interested   2 [2. Somewhat interested]   2569
    ## 3 Not much interested   3 [3. Not much interested]    943
    ## 4 <NA>                 -9 [-9. Refused]                 1

``` r
anes_2020 %>% count(TrustGovernment, V201233)
```

    ## count: now 7 rows and 3 columns, ungrouped

    ## # A tibble: 7 × 3
    ##   TrustGovernment     V201233                         n
    ##   <fct>               <dbl+lbl>                   <int>
    ## 1 Always               1 [1. Always]                 80
    ## 2 Most of the time     2 [2. Most of the time]     1016
    ## 3 About half the time  3 [3. About half the time]  2313
    ## 4 Some of the time     4 [4. Some of the time]     3313
    ## 5 Never                5 [5. Never]                 702
    ## 6 <NA>                -9 [-9. Refused]               26
    ## 7 <NA>                -8 [-8. Don't know]             3

``` r
anes_2020 %>% count(TrustPeople, V201237)
```

    ## count: now 7 rows and 3 columns, ungrouped

    ## # A tibble: 7 × 3
    ##   TrustPeople         V201237                         n
    ##   <fct>               <dbl+lbl>                   <int>
    ## 1 Always               1 [1. Always]                 48
    ## 2 Most of the time     2 [2. Most of the time]     3511
    ## 3 About half the time  3 [3. About half the time]  2020
    ## 4 Some of the time     4 [4. Some of the time]     1597
    ## 5 Never                5 [5. Never]                 264
    ## 6 <NA>                -9 [-9. Refused]               12
    ## 7 <NA>                -8 [-8. Don't know]             1

``` r
anes_2020 %>% count(VotedPres2016, V201101, V201102)
```

    ## count: now 8 rows and 4 columns, ungrouped

    ## # A tibble: 8 × 4
    ##   VotedPres2016 V201101                 V201102                     n
    ##   <fct>         <dbl+lbl>               <dbl+lbl>               <int>
    ## 1 Yes           -1 [-1. Inapplicable]    1 [1. Yes, voted]       3030
    ## 2 Yes            1 [1. Yes, voted]      -1 [-1. Inapplicable]    2780
    ## 3 No            -1 [-1. Inapplicable]    2 [2. No, didn't vote]   743
    ## 4 No             2 [2. No, didn't vote] -1 [-1. Inapplicable]     879
    ## 5 <NA>          -9 [-9. Refused]        -1 [-1. Inapplicable]      13
    ## 6 <NA>          -8 [-8. Don't know]     -1 [-1. Inapplicable]       1
    ## 7 <NA>          -1 [-1. Inapplicable]   -9 [-9. Refused]            6
    ## 8 <NA>          -1 [-1. Inapplicable]   -8 [-8. Don't know]         1

``` r
anes_2020 %>% count(VotedPres2016_selection, V201103)
```

    ## count: now 6 rows and 3 columns, ungrouped

    ## # A tibble: 6 × 3
    ##   VotedPres2016_selection V201103                     n
    ##   <fct>                   <dbl+lbl>               <int>
    ## 1 Clinton                  1 [1. Hillary Clinton]  2911
    ## 2 Trump                    2 [2. Donald Trump]     2466
    ## 3 Other                    5 [5. Other {SPECIFY}]   390
    ## 4 <NA>                    -9 [-9. Refused]           41
    ## 5 <NA>                    -8 [-8. Don't know]         2
    ## 6 <NA>                    -1 [-1. Inapplicable]    1643

``` r
anes_2020 %>% count(VotedPres2020, V202109x)
```

    ## count: now 3 rows and 3 columns, ungrouped

    ## # A tibble: 3 × 3
    ##   VotedPres2020 V202109x                  n
    ##   <fct>         <dbl+lbl>             <int>
    ## 1 Yes            1 [1. Voted]          6407
    ## 2 No             0 [0. Did not vote]   1039
    ## 3 <NA>          -2 [-2. Not reported]     7

``` r
anes_2020 %>% count(VotedPres2020_selection, V202073)
```

    ## count: now 12 rows and 3 columns, ungrouped

    ## # A tibble: 12 × 3
    ##    VotedPres2020_selection V202073                                        n
    ##    <fct>                   <dbl+lbl>                                  <int>
    ##  1 Biden                    1 [1. Joe Biden]                           3267
    ##  2 Trump                    2 [2. Donald Trump]                        2462
    ##  3 Other                    3 [3. Jo Jorgensen]                          69
    ##  4 Other                    4 [4. Howie Hawkins]                         23
    ##  5 Other                    5 [5. Other candidate {SPECIFY}]             56
    ##  6 Other                    7 [7. Specified as Republican candidate]      1
    ##  7 Other                    8 [8. Specified as Libertarian candidate]     3
    ##  8 <NA>                    -9 [-9. Refused]                              53
    ##  9 <NA>                    -6 [-6. No post-election interview]            4
    ## 10 <NA>                    -1 [-1. Inapplicable]                       1497
    ## 11 <NA>                    11 [11. Specified as don't know]               2
    ## 12 <NA>                    12 [12. Specified as refused]                 16

``` r
anes_2020 %>% count(EarlyVote2020, V201025x, VotedPres2020)
```

    ## count: now 11 rows and 4 columns, ungrouped

    ## # A tibble: 11 × 4
    ##    EarlyVote2020 V201025x                                                                   VotedPres2020     n
    ##    <fct>         <dbl+lbl>                                                                  <fct>         <int>
    ##  1 Yes            4 [4. Registered and voted early]                                         Yes             371
    ##  2 No             1 [1. Not registered (or DK/RF), does not intend to register (or DK/RF i… Yes              35
    ##  3 No             2 [2. Not registered (or DK/RF), intends to register]                     Yes             109
    ##  4 No             3 [3. Registered but did not vote early (or DK/RF)]                       Yes            5891
    ##  5 <NA>          -4 [-4. Technical error]                                                   Yes               1
    ##  6 <NA>           1 [1. Not registered (or DK/RF), does not intend to register (or DK/RF i… No              301
    ##  7 <NA>           1 [1. Not registered (or DK/RF), does not intend to register (or DK/RF i… <NA>              3
    ##  8 <NA>           2 [2. Not registered (or DK/RF), intends to register]                     No              180
    ##  9 <NA>           2 [2. Not registered (or DK/RF), intends to register]                     <NA>              1
    ## 10 <NA>           3 [3. Registered but did not vote early (or DK/RF)]                       No              558
    ## 11 <NA>           3 [3. Registered but did not vote early (or DK/RF)]                       <NA>              3

``` r
anes_2020 %>%
   summarise(WtSum = sum(Weight, na.rm = TRUE)) %>%
   pull(WtSum)
```

    ## summarise: now one row and one column, ungrouped

    ## [1] 7453

## Save data

``` r
summary(anes_2020)
```

    ##     V200010b           V200010d        V200010c        V200002         V201006          V201102       
    ##  Min.   :0.008262   Min.   : 1.00   Min.   :1.000   Min.   :1.000   Min.   :-9.000   Min.   :-9.0000  
    ##  1st Qu.:0.386263   1st Qu.:12.00   1st Qu.:1.000   1st Qu.:3.000   1st Qu.: 1.000   1st Qu.:-1.0000  
    ##  Median :0.686301   Median :24.00   Median :2.000   Median :3.000   Median : 1.000   Median : 1.0000  
    ##  Mean   :1.000000   Mean   :24.63   Mean   :1.507   Mean   :2.911   Mean   : 1.596   Mean   : 0.1048  
    ##  3rd Qu.:1.211032   3rd Qu.:37.00   3rd Qu.:2.000   3rd Qu.:3.000   3rd Qu.: 2.000   3rd Qu.: 1.0000  
    ##  Max.   :6.650665   Max.   :50.00   Max.   :3.000   Max.   :3.000   Max.   : 3.000   Max.   : 2.0000  
    ##                                                                                                       
    ##     V201101            V201103          V201025x         V201231x         V201233          V201237     
    ##  Min.   :-9.00000   Min.   :-9.000   Min.   :-4.000   Min.   :-9.000   Min.   :-9.000   Min.   :-9.00  
    ##  1st Qu.:-1.00000   1st Qu.: 1.000   1st Qu.: 3.000   1st Qu.: 2.000   1st Qu.: 3.000   1st Qu.: 2.00  
    ##  Median :-1.00000   Median : 1.000   Median : 3.000   Median : 4.000   Median : 4.000   Median : 3.00  
    ##  Mean   : 0.08493   Mean   : 1.042   Mean   : 2.919   Mean   : 3.834   Mean   : 3.429   Mean   : 2.78  
    ##  3rd Qu.: 1.00000   3rd Qu.: 2.000   3rd Qu.: 3.000   3rd Qu.: 6.000   3rd Qu.: 4.000   3rd Qu.: 3.00  
    ##  Max.   : 2.00000   Max.   : 5.000   Max.   : 4.000   Max.   : 7.000   Max.   : 5.000   Max.   : 5.00  
    ##                                                                                                        
    ##     V201507x        V201510          V201549x         V201600          V201617x        V202066      
    ##  Min.   :-9.00   Min.   :-9.000   Min.   :-9.000   Min.   :-9.000   Min.   :-9.00   Min.   :-9.000  
    ##  1st Qu.:35.00   1st Qu.: 3.000   1st Qu.: 1.000   1st Qu.: 1.000   1st Qu.: 4.00   1st Qu.: 4.000  
    ##  Median :51.00   Median : 5.000   Median : 1.000   Median : 2.000   Median :11.00   Median : 4.000  
    ##  Mean   :49.43   Mean   : 5.621   Mean   : 1.499   Mean   : 1.472   Mean   :10.36   Mean   : 3.402  
    ##  3rd Qu.:66.00   3rd Qu.: 6.000   3rd Qu.: 2.000   3rd Qu.: 2.000   3rd Qu.:17.00   3rd Qu.: 4.000  
    ##  Max.   :80.00   Max.   :95.000   Max.   : 6.000   Max.   : 2.000   Max.   :22.00   Max.   : 4.000  
    ##                                                                                                     
    ##     V202109x          V202072           V202073           V202110x         InterviewMode      Weight        
    ##  Min.   :-2.0000   Min.   :-9.0000   Min.   :-9.0000   Min.   :-9.0000   Video    : 274   Min.   :0.008262  
    ##  1st Qu.: 1.0000   1st Qu.: 1.0000   1st Qu.: 1.0000   1st Qu.: 1.0000   Telephone: 115   1st Qu.:0.386263  
    ##  Median : 1.0000   Median : 1.0000   Median : 1.0000   Median : 1.0000   Web      :7064   Median :0.686301  
    ##  Mean   : 0.8578   Mean   : 0.6234   Mean   : 0.9415   Mean   : 0.9902                    Mean   :1.000000  
    ##  3rd Qu.: 1.0000   3rd Qu.: 1.0000   3rd Qu.: 2.0000   3rd Qu.: 2.0000                    3rd Qu.:1.211032  
    ##  Max.   : 1.0000   Max.   : 2.0000   Max.   :12.0000   Max.   : 5.0000                    Max.   :6.650665  
    ##                                                                                                             
    ##     Stratum     VarUnit       Age               AgeGroup       Gender                    RaceEth    
    ##  12     : 179   1:3689   Min.   :18.00   18-29      : 871   Male  :3375   White              :5420  
    ##  6      : 172   2:3750   1st Qu.:37.00   30-39      :1241   Female:4027   Black              : 650  
    ##  27     : 172   3:  14   Median :53.00   40-49      :1081   NA's  :  51   Hispanic           : 662  
    ##  21     : 170            Mean   :51.83   50-59      :1200                 Asian, NH/PI       : 248  
    ##  25     : 169            3rd Qu.:66.00   60-69      :1436                 AI/AN              : 155  
    ##  1      : 167            Max.   :80.00   70 or older:1330                 Other/multiple race: 237  
    ##  (Other):6424            NA's   :294     NA's       : 294                 NA's               :  81  
    ##                      PartyID            Education                 Income              Income7    
    ##  Strong democrat         :1796   Less than HS: 312   Under $9,999    : 647   $125k or more:1468  
    ##  Strong republican       :1545   High school :1160   $50,000-59,999  : 485   Under $20k   :1076  
    ##  Independent-democrat    : 881   Post HS     :2514   $100,000-109,999: 451   $20-40k      :1051  
    ##  Independent             : 876   Bachelor's  :1877   $250,000 or more: 405   $40-60k      : 984  
    ##  Not very strong democrat: 790   Graduate    :1474   $80,000-89,999  : 383   $60-80k      : 920  
    ##  (Other)                 :1540   NA's        : 116   (Other)         :4565   (Other)      :1437  
    ##  NA's                    :  25                       NA's            : 517   NA's         : 517  
    ##              CampaignInterest            TrustGovernment              TrustPeople   VotedPres2016
    ##  Very much interested:3940    Always             :  80   Always             :  48   Yes :5810    
    ##  Somewhat interested :2569    Most of the time   :1016   Most of the time   :3511   No  :1622    
    ##  Not much interested : 943    About half the time:2313   About half the time:2020   NA's:  21    
    ##  NA's                :   1    Some of the time   :3313   Some of the time   :1597                
    ##                               Never              : 702   Never              : 264                
    ##                               NA's               :  29   NA's               :  13                
    ##                                                                                                  
    ##  VotedPres2016_selection VotedPres2020 VotedPres2020_selection EarlyVote2020
    ##  Clinton:2911            Yes :6407     Biden:3267              Yes : 371    
    ##  Trump  :2466            No  :1039     Trump:2462              No  :6035    
    ##  Other  : 390            NA's:   7     Other: 152              NA's:1047    
    ##  NA's   :1686                          NA's :1572                           
    ##                                                                             
    ##                                                                             
    ## 

``` r
anes_der_tmp_loc <- here("osf_dl", "anes_2020.rds")
write_rds(anes_2020, anes_der_tmp_loc)
target_dir <- osf_retrieve_node("https://osf.io/gzbkn/?view_only=8ca80573293b4e12b7f934a0f742b957") 
osf_upload(target_dir, path=anes_der_tmp_loc, conflicts="overwrite")
```

    ## # A tibble: 1 × 3
    ##   name          id                       meta            
    ##   <chr>         <chr>                    <list>          
    ## 1 anes_2020.rds 647d2affa8dbe909c6cb5482 <named list [3]>

``` r
unlink(anes_der_tmp_loc)
```
