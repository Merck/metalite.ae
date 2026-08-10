# Format AE specific subgroup analysis

Format AE specific subgroup analysis

## Usage

``` r
format_ae_specific_subgroup(
  outdata,
  display = c("n", "prop"),
  digits_prop = 1,
  digits_ci = 1,
  digits_p = 3,
  digits_dur = c(1, 1),
  digits_events = c(1, 1),
  mock = FALSE
)
```

## Arguments

- outdata:

  An `outdata` object created by
  [`prepare_ae_specific()`](https://merck.github.io/metalite.ae/reference/prepare_ae_specific.md).

- display:

  A character vector of measurement to be displayed.

  - `n`: Number of subjects with adverse event.

  - `prop`: Proportion of subjects with adverse event.

  - `total`: Total columns.

  - `dur`: Average of adverse event duration.

  - `events`: Average number of adverse event per subject.

- digits_prop:

  A numeric value of number of digits for proportion value.

- digits_ci:

  A numeric value of number of digits for confidence interval.

- digits_p:

  A numeric value of number of digits for p-value.

- digits_dur:

  A numeric value of number of digits for average duration of adverse
  event.

- digits_events:

  A numeric value of number of digits for average of number of adverse
  event per subjects.

- mock:

  Logical. Display mock table or not.

## Value

A list of analysis raw datasets for subgroup analysis.

## Examples

``` r
# Define metadata
adsl <- forestly::forestly_adsl
adae <- forestly::forestly_adae

adsl$TRTA <- factor(
  adsl$TRT01A,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)
adae$TRTA <- factor(
  adae$TRTA,
  levels = c("Xanomeline Low Dose", "Placebo"),
  labels = c("Low Dose", "Placebo")
)

analysis_plan <- metalite::plan(
  analysis = "ae_specific",
  population = "apat",
  observation = "wk12",
  parameter = "rel"
)

meta <- metalite::meta_adam(observation = adae, population = adsl) |>
  metalite::define_plan(analysis_plan) |>
  metalite::define_population(
    name = "apat",
    var = c("USUBJID", "SAFFL", "TRTA", "SITEID", "SEX", "RACE", "AGE"),
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "All Participants as Treated"
  ) |>
  metalite::define_observation(
    name = "wk12",
    var = c(
      "USUBJID", "SAFFL", "TRTA", "SEX", "AEDECOD", "AEBODSYS",
      "AEREL", "AESER", "AEOUT", "AEACN", "AESDTH", "ASTDT", "AENDT"
    ),
    group = "TRTA",
    subset = SAFFL == "Y",
    label = "Weeks 0 to 12"
  ) |>
  metalite::define_parameter(
    name = "rel",
    term1 = "Drug-Related",
    term2 = "",
    subset = AEREL %in% c("POSSIBLE", "PROBABLE"),
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Drug-related AEs"
  ) |>
  metalite::define_analysis(
    name = "ae_specific",
    title = "Participants With Drug-Related Adverse Events"
  ) |>
  metalite::meta_build()

# Prepare and format subgroup analysis
prepare_ae_specific_subgroup(meta,
  population = "apat",
  observation = "wk12",
  parameter = "rel",
  subgroup_var = "SEX",
  display_subgroup_total = TRUE
) |>
  format_ae_specific_subgroup()
#> $components
#> [1] "soc" "par"
#> 
#> $group
#> [1] "Low Dose" "Placebo" 
#> 
#> $subgroup
#> [1] "f" "m"
#> 
#> $display_subgroup_total
#> [1] TRUE
#> 
#> $meta
#> ADaM metadata: 
#>    .$data_population     Population data with 170 subjects 
#>    .$data_observation    Observation data with 736 records 
#>    .$plan    Analysis plan with 1 plans 
#> 
#> 
#>   Analysis population type:
#>     name        id  group                                          var
#> 1 'apat' 'USUBJID' 'TRTA' USUBJID, SAFFL, TRTA, SITEID, SEX, RACE, AGE
#>         subset                         label
#> 1 SAFFL == 'Y' 'All Participants as Treated'
#> 
#> 
#>   Analysis observation type:
#>     name        id  group
#> 1 'wk12' 'USUBJID' 'TRTA'
#>                                                                                              var
#> 1 USUBJID, SAFFL, TRTA, SEX, AEDECOD, AEBODSYS, AEREL, AESER, AEOUT, AEACN, AESDTH, ASTDT, AENDT
#>         subset           label
#> 1 SAFFL == 'Y' 'Weeks 0 to 12'
#> 
#> 
#>   Analysis parameter type:
#>    name              label                               subset
#> 1 'rel' 'Drug-related AEs' AEREL %in% c('POSSIBLE', 'PROBABLE')
#> 
#> 
#>   Analysis function:
#>            name                           label
#> 1 'ae_specific' 'Table: specific adverse event'
#> 
#> 
#> $population
#> [1] "apat"
#> 
#> $observation
#> [1] "wk12"
#> 
#> $parameter
#> [1] "rel"
#> 
#> $out_all
#> $out_all$F
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 114 obs. of  3 variables:
#>  $ order          : num [1:114] 1 100 200 900 1000 ...
#>  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group: num 2
#>  $ prop           :'data.frame': 114 obs. of  3 variables:
#>  $ diff           :'data.frame': 114 obs. of  1 variable:
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:114] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:114] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
#> 
#> $out_all$M
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 114 obs. of  3 variables:
#>  $ order          : num [1:114] 1 100 200 900 1000 ...
#>  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group: num 2
#>  $ prop           :'data.frame': 114 obs. of  3 variables:
#>  $ diff           :'data.frame': 114 obs. of  1 variable:
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:114] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:114] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
#> 
#> $out_all$Total
#> List of 15
#>  $ meta           :List of 7
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 114 obs. of  3 variables:
#>  $ order          : num [1:114] 1 100 200 900 1000 ...
#>  $ group          : chr [1:3] "Low Dose" "Placebo" "Total"
#>  $ reference_group: num 2
#>  $ prop           :'data.frame': 114 obs. of  3 variables:
#>  $ diff           :'data.frame': 114 obs. of  1 variable:
#>  $ n_pop          :'data.frame': 1 obs. of  3 variables:
#>  $ name           : chr [1:114] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:114] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language prepare_ae_specific(meta = meta, population = population, observation = observation,      parameter = parameter, | __truncated__
#> 
#> 
#> $tbl
#>                                                     name Fn_1 Fprop_1 Fn_2
#> 78                            Participants in population   50    <NA>   53
#> 114         with one or more drug-related adverse events   41  (82.0)   28
#> 113                  with no drug-related adverse events    9  (18.0)   25
#> 1                                                          NA    <NA>   NA
#> 29                                     Cardiac disorders    4   (8.0)    4
#> 19                                   Atrial fibrillation    0   (0.0)    1
#> 20                                        Atrial flutter    0   (0.0)    0
#> 21                   Atrioventricular block first degree    0   (0.0)    1
#> 22                  Atrioventricular block second degree    0   (0.0)    0
#> 27                                           Bradycardia    0   (0.0)    1
#> 28                             Bundle branch block right    0   (0.0)    0
#> 30                            Cardiac failure congestive    0   (0.0)    1
#> 70                                 Myocardial infarction    1   (2.0)    2
#> 76                                          Palpitations    0   (0.0)    0
#> 91                                      Sinus arrhythmia    0   (0.0)    0
#> 92                                     Sinus bradycardia    1   (2.0)    2
#> 100                       Supraventricular extrasystoles    1   (2.0)    0
#> 106                            Ventricular extrasystoles    1   (2.0)    0
#> 111                       Wolff-parkinson-white syndrome    1   (2.0)    0
#> 35            Congenital, familial and genetic disorders    0   (0.0)    0
#> 107                            Ventricular septal defect    0   (0.0)    0
#> 44                           Ear and labyrinth disorders    2   (4.0)    0
#> 102                                             Tinnitus    1   (2.0)    0
#> 108                                              Vertigo    1   (2.0)    0
#> 49                                         Eye disorders    0   (0.0)    0
#> 109                                       Vision blurred    0   (0.0)    0
#> 53                            Gastrointestinal disorders    6  (12.0)    1
#> 2                                         Abdominal pain    1   (2.0)    0
#> 40                                             Diarrhoea    3   (6.0)    0
#> 42                                             Dyspepsia    0   (0.0)    1
#> 54                      Gastrooesophageal reflux disease    0   (0.0)    1
#> 71                                                Nausea    2   (4.0)    0
#> 110                                             Vomiting    2   (4.0)    0
#> 55  General disorders and administration site conditions   23  (46.0)   11
#> 5                              Application site bleeding    1   (2.0)    0
#> 6                            Application site dermatitis    5  (10.0)    2
#> 7                          Application site desquamation    0   (0.0)    0
#> 8                        Application site discolouration    0   (0.0)    0
#> 9                              Application site erythema    5  (10.0)    2
#> 10                           Application site induration    0   (0.0)    0
#> 11                           Application site irritation    6  (12.0)    3
#> 12                             Application site pruritus   12  (24.0)    4
#> 13                             Application site reaction    0   (0.0)    0
#> 14                             Application site swelling    0   (0.0)    0
#> 15                            Application site urticaria    0   (0.0)    0
#> 16                             Application site vesicles    1   (2.0)    0
#> 17                               Application site warmth    1   (2.0)    0
#> 18                                              Asthenia    0   (0.0)    1
#> 31                                                Chills    0   (0.0)    0
#> 51                                               Fatigue    1   (2.0)    1
#> 67                                               Malaise    0   (0.0)    0
#> 73                                                Oedema    1   (2.0)    0
#> 75                                                  Pain    1   (2.0)    0
#> 63        Injury, poisoning and procedural complications    2   (4.0)    0
#> 50                                                  Fall    1   (2.0)    0
#> 96                                       Skin laceration    1   (2.0)    0
#> 112                                                Wound    1   (2.0)    0
#> 64                                        Investigations    2   (4.0)    2
#> 25                Blood creatine phosphokinase increased    0   (0.0)    0
#> 26                            Body temperature increased    1   (2.0)    0
#> 45               Electrocardiogram st segment depression    1   (2.0)    1
#> 57                                  Heart rate increased    0   (0.0)    0
#> 58                                  Heart rate irregular    0   (0.0)    1
#> 68                    Metabolism and nutrition disorders    0   (0.0)    3
#> 37                                    Decreased appetite    0   (0.0)    1
#> 52                                          Food craving    0   (0.0)    1
#> 62                                    Increased appetite    0   (0.0)    1
#> 69       Musculoskeletal and connective tissue disorders    0   (0.0)    0
#> 90                                         Shoulder pain    0   (0.0)    0
#> 72                              Nervous system disorders    8  (16.0)    3
#> 23                                      Balance disorder    1   (2.0)    0
#> 33                              Complex partial seizures    1   (2.0)    0
#> 36                                 Coordination abnormal    1   (2.0)    0
#> 41                                             Dizziness    3   (6.0)    1
#> 56                                              Headache    0   (0.0)    2
#> 66                                              Lethargy    0   (0.0)    0
#> 77                                     Paraesthesia oral    1   (2.0)    0
#> 98                                            Somnolence    0   (0.0)    0
#> 99                                                Stupor    0   (0.0)    0
#> 101                                              Syncope    4   (8.0)    0
#> 103                           Transient ischaemic attack    0   (0.0)    0
#> 82                                 Psychiatric disorders    6  (12.0)    1
#> 3                                              Agitation    2   (4.0)    0
#> 4                                                Anxiety    3   (6.0)    0
#> 34                                     Confusional state    1   (2.0)    0
#> 38                                        Depressed mood    0   (0.0)    0
#> 65                                          Irritability    1   (2.0)    1
#> 89                                          Restlessness    0   (0.0)    0
#> 86                           Renal and urinary disorders    1   (2.0)    0
#> 47                                              Enuresis    1   (2.0)    0
#> 87              Reproductive system and breast disorders    0   (0.0)    1
#> 79                                           Pelvic pain    0   (0.0)    1
#> 88       Respiratory, thoracic and mediastinal disorders    0   (0.0)    2
#> 43                                              Dyspnoea    0   (0.0)    1
#> 46                                             Emphysema    0   (0.0)    1
#> 93                Skin and subcutaneous tissue disorders   21  (42.0)   12
#> 24                                               Blister    2   (4.0)    0
#> 32                                            Cold sweat    0   (0.0)    0
#> 39                                    Dermatitis contact    0   (0.0)    0
#> 48                                              Erythema    7  (14.0)    6
#> 59                                         Hyperhidrosis    1   (2.0)    1
#> 80                                              Pruritus   12  (24.0)    6
#> 81                                  Pruritus generalised    0   (0.0)    0
#> 83                                                  Rash    6  (12.0)    2
#> 84                                     Rash erythematous    1   (2.0)    0
#> 85                                         Rash pruritic    1   (2.0)    0
#> 94                                      Skin exfoliation    1   (2.0)    0
#> 95                                       Skin irritation    5  (10.0)    2
#> 97                                            Skin ulcer    0   (0.0)    0
#> 104                                            Urticaria    0   (0.0)    0
#> 105                                   Vascular disorders    2   (4.0)    0
#> 60                                          Hypertension    1   (2.0)    0
#> 61                                           Hypotension    1   (2.0)    0
#> 74                               Orthostatic hypotension    0   (0.0)    0
#>     Fprop_2 Mn_1 Mprop_1 Mn_2 Mprop_2 Totaln_1 Totalprop_1 Totaln_2 Totalprop_2
#> 78     <NA>   34    <NA>   33    <NA>       84        <NA>       86        <NA>
#> 114  (52.8)   32  (94.1)   16  (48.5)       73      (86.9)       44      (51.2)
#> 113  (47.2)    2   (5.9)   17  (51.5)       11      (13.1)       42      (48.8)
#> 1      <NA>   NA    <NA>   NA    <NA>       NA        <NA>       NA        <NA>
#> 29    (7.5)    3   (8.8)    2   (6.1)        7       (8.3)        6       (7.0)
#> 19    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 20    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 21    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 22    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 27    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 28    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 30    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 70    (3.8)    0   (0.0)    0   (0.0)        1       (1.2)        2       (2.3)
#> 76    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 91    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 92    (3.8)    1   (2.9)    0   (0.0)        2       (2.4)        2       (2.3)
#> 100   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 106   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 111   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 35    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 107   (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 44    (0.0)    0   (0.0)    0   (0.0)        2       (2.4)        0       (0.0)
#> 102   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 108   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 49    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 109   (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 53    (1.9)    2   (5.9)    3   (9.1)        8       (9.5)        4       (4.7)
#> 2     (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 40    (0.0)    0   (0.0)    3   (9.1)        3       (3.6)        3       (3.5)
#> 42    (1.9)    1   (2.9)    0   (0.0)        1       (1.2)        1       (1.2)
#> 54    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 71    (0.0)    1   (2.9)    0   (0.0)        3       (3.6)        0       (0.0)
#> 110   (0.0)    0   (0.0)    0   (0.0)        2       (2.4)        0       (0.0)
#> 55   (20.8)   20  (58.8)    7  (21.2)       43      (51.2)       18      (20.9)
#> 5     (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 6     (3.8)    4  (11.8)    3   (9.1)        9      (10.7)        5       (5.8)
#> 7     (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 8     (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 9     (3.8)    7  (20.6)    1   (3.0)       12      (14.3)        3       (3.5)
#> 10    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 11    (5.7)    3   (8.8)    0   (0.0)        9      (10.7)        3       (3.5)
#> 12    (7.5)   10  (29.4)    2   (6.1)       22      (26.2)        6       (7.0)
#> 13    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 14    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 15    (0.0)    2   (5.9)    0   (0.0)        2       (2.4)        0       (0.0)
#> 16    (0.0)    3   (8.8)    1   (3.0)        4       (4.8)        1       (1.2)
#> 17    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 18    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 31    (0.0)    1   (2.9)    1   (3.0)        1       (1.2)        1       (1.2)
#> 51    (1.9)    1   (2.9)    0   (0.0)        2       (2.4)        1       (1.2)
#> 67    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 73    (0.0)    1   (2.9)    0   (0.0)        2       (2.4)        0       (0.0)
#> 75    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 63    (0.0)    0   (0.0)    0   (0.0)        2       (2.4)        0       (0.0)
#> 50    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 96    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 112   (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 64    (3.8)    0   (0.0)    2   (6.1)        2       (2.4)        4       (4.7)
#> 25    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 26    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 45    (1.9)    0   (0.0)    0   (0.0)        1       (1.2)        1       (1.2)
#> 57    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 58    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 68    (5.7)    0   (0.0)    0   (0.0)        0       (0.0)        3       (3.5)
#> 37    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 52    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 62    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 69    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 90    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 72    (5.7)    4  (11.8)    2   (6.1)       12      (14.3)        5       (5.8)
#> 23    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 33    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 36    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 41    (1.9)    3   (8.8)    1   (3.0)        6       (7.1)        2       (2.3)
#> 56    (3.8)    1   (2.9)    0   (0.0)        1       (1.2)        2       (2.3)
#> 66    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 77    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 98    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 99    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 101   (0.0)    0   (0.0)    0   (0.0)        4       (4.8)        0       (0.0)
#> 103   (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 82    (1.9)    3   (8.8)    1   (3.0)        9      (10.7)        2       (2.3)
#> 3     (0.0)    0   (0.0)    0   (0.0)        2       (2.4)        0       (0.0)
#> 4     (0.0)    0   (0.0)    0   (0.0)        3       (3.6)        0       (0.0)
#> 34    (0.0)    1   (2.9)    1   (3.0)        2       (2.4)        1       (1.2)
#> 38    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 65    (1.9)    0   (0.0)    0   (0.0)        1       (1.2)        1       (1.2)
#> 89    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 86    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 47    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 87    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 79    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 88    (3.8)    0   (0.0)    0   (0.0)        0       (0.0)        2       (2.3)
#> 43    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 46    (1.9)    0   (0.0)    0   (0.0)        0       (0.0)        1       (1.2)
#> 93   (22.6)   18  (52.9)    5  (15.2)       39      (46.4)       17      (19.8)
#> 24    (0.0)    3   (8.8)    0   (0.0)        5       (6.0)        0       (0.0)
#> 32    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 39    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 48   (11.3)    6  (17.6)    3   (9.1)       13      (15.5)        9      (10.5)
#> 59    (1.9)    3   (8.8)    0   (0.0)        4       (4.8)        1       (1.2)
#> 80   (11.3)    9  (26.5)    1   (3.0)       21      (25.0)        7       (8.1)
#> 81    (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 83    (3.8)    5  (14.7)    1   (3.0)       11      (13.1)        3       (3.5)
#> 84    (0.0)    1   (2.9)    0   (0.0)        2       (2.4)        0       (0.0)
#> 85    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 94    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 95    (3.8)    1   (2.9)    0   (0.0)        6       (7.1)        2       (2.3)
#> 97    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#> 104   (0.0)    1   (2.9)    0   (0.0)        1       (1.2)        0       (0.0)
#> 105   (0.0)    0   (0.0)    1   (3.0)        2       (2.4)        1       (1.2)
#> 60    (0.0)    0   (0.0)    0   (0.0)        1       (1.2)        0       (0.0)
#> 61    (0.0)    0   (0.0)    1   (3.0)        1       (1.2)        1       (1.2)
#> 74    (0.0)    0   (0.0)    1   (3.0)        0       (0.0)        1       (1.2)
#>     order
#> 78      1
#> 114   100
#> 113   200
#> 1     900
#> 29   1000
#> 19   1018
#> 20   1019
#> 21   1020
#> 22   1021
#> 27   1026
#> 28   1027
#> 30   1028
#> 70   1059
#> 76   1064
#> 91   1074
#> 92   1075
#> 100  1082
#> 106  1087
#> 111  1092
#> 35   2000
#> 107  2088
#> 44   3000
#> 102  3084
#> 108  3089
#> 49   4000
#> 109  4090
#> 53   5000
#> 2    5001
#> 40   5037
#> 42   5039
#> 54   5048
#> 71   5060
#> 110  5091
#> 55   6000
#> 5    6004
#> 6    6005
#> 7    6006
#> 8    6007
#> 9    6008
#> 10   6009
#> 11   6010
#> 12   6011
#> 13   6012
#> 14   6013
#> 15   6014
#> 16   6015
#> 17   6016
#> 18   6017
#> 31   6029
#> 51   6046
#> 67   6058
#> 73   6061
#> 75   6063
#> 63   7000
#> 50   7045
#> 96   7078
#> 112  7093
#> 64   8000
#> 25   8024
#> 26   8025
#> 45   8041
#> 57   8050
#> 58   8051
#> 68   9000
#> 37   9034
#> 52   9047
#> 62   9055
#> 69  10000
#> 90  10073
#> 72  11000
#> 23  11022
#> 33  11031
#> 36  11033
#> 41  11038
#> 56  11049
#> 66  11057
#> 77  11065
#> 98  11080
#> 99  11081
#> 101 11083
#> 103 11085
#> 82  12000
#> 3   12002
#> 4   12003
#> 34  12032
#> 38  12035
#> 65  12056
#> 89  12072
#> 86  13000
#> 47  13043
#> 87  14000
#> 79  14066
#> 88  15000
#> 43  15040
#> 46  15042
#> 93  16000
#> 24  16023
#> 32  16030
#> 39  16036
#> 48  16044
#> 59  16052
#> 80  16067
#> 81  16068
#> 83  16069
#> 84  16070
#> 85  16071
#> 94  16076
#> 95  16077
#> 97  16079
#> 104 16086
#> 105 17000
#> 60  17053
#> 61  17054
#> 74  17062
#> 
#> $display
#> [1] "n"    "prop"
#> 
```
