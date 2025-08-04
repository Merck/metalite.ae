# rtf output: n, and prop w/o total

    Code
      tbl
    Output
      $components
      [1] "soc" "par"
      
      $group
      [1] "Placebo"   "Low Dose"  "High Dose"
      
      $subgroup
      [1] "f" "m"
      
      $display_subgroup_total
      [1] TRUE
      
      $meta
      ADaM metadata: 
         .$data_population 	Population data with 254 subjects 
         .$data_observation 	Observation data with 1191 records 
         .$plan 	Analysis plan with 18 plans 
      
      
        Analysis population type:
          name        id  group var       subset                         label
      1 'apat' 'USUBJID' 'TRTA'     SAFFL == 'Y' 'All Participants as Treated'
      
      
        Analysis observation type:
          name        id  group var          subset           label
      1 'wk12' 'USUBJID' 'TRTA'        SAFFL == 'Y' 'Weeks 0 to 12'
      2 'wk24' 'USUBJID' 'TRTA'     AOCC01FL == 'Y' 'Weeks 0 to 24'
      
      
        Analysis parameter type:
           name                                label
      1   'rel'        'drug-related adverse events'
      2 'aeosi' 'adverse events of special interest'
      3   'any'                 'any adverse events'
      4   'ser'             'serious adverse events'
                                      subset
      1 AEREL %in% c('POSSIBLE', 'PROBABLE')
      2                         AEOSI == 'Y'
      3                                     
      4                         AESER == 'Y'
      
      
        Analysis function:
                 name                             label
      1  'ae_summary'    'Table: adverse event summary'
      2  'ae_listing'          'Listing: adverse event'
      3  'ae_exp_adj' 'Exposure Adjusted Incident Rate'
      4 'ae_specific'   'Table: specific adverse event'
      
      
      $population
      [1] "apat"
      
      $observation
      [1] "wk12"
      
      $parameter
      [1] "rel"
      
      $out_all
      $out_all$F
      List of 15
       $ meta           :List of 7
       $ population     : chr "apat"
       $ observation    : chr "wk12"
       $ parameter      : chr "rel"
       $ n              :'data.frame':	138 obs. of  4 variables:
       $ order          : num [1:138] 1 100 200 900 1000 ...
       $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
       $ reference_group: num 1
       $ prop           :'data.frame':	138 obs. of  4 variables:
       $ diff           :'data.frame':	138 obs. of  2 variables:
       $ n_pop          :'data.frame':	1 obs. of  4 variables:
       $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
       $ soc_name       : chr [1:138] NA NA NA NA ...
       $ components     : chr [1:2] "soc" "par"
       $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
      
      $out_all$M
      List of 15
       $ meta           :List of 7
       $ population     : chr "apat"
       $ observation    : chr "wk12"
       $ parameter      : chr "rel"
       $ n              :'data.frame':	138 obs. of  4 variables:
       $ order          : num [1:138] 1 100 200 900 1000 ...
       $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
       $ reference_group: num 1
       $ prop           :'data.frame':	138 obs. of  4 variables:
       $ diff           :'data.frame':	138 obs. of  2 variables:
       $ n_pop          :'data.frame':	1 obs. of  4 variables:
       $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
       $ soc_name       : chr [1:138] NA NA NA NA ...
       $ components     : chr [1:2] "soc" "par"
       $ prepare_call   : language FUN(meta = X[[i]], population = ..1, observation = ..2, parameter = ..3,      components = ..4)
      
      $out_all$Total
      List of 15
       $ meta           :List of 7
       $ population     : chr "apat"
       $ observation    : chr "wk12"
       $ parameter      : chr "rel"
       $ n              :'data.frame':	138 obs. of  4 variables:
       $ order          : num [1:138] 1 100 200 900 10000 ...
       $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
       $ reference_group: num 1
       $ prop           :'data.frame':	138 obs. of  4 variables:
       $ diff           :'data.frame':	138 obs. of  2 variables:
       $ n_pop          :'data.frame':	1 obs. of  4 variables:
       $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
       $ soc_name       : chr [1:138] NA NA NA NA ...
       $ components     : chr [1:2] "soc" "par"
       $ prepare_call   : language prepare_ae_specific(meta = meta, population = population, observation = observation,      parameter = parameter, | __truncated__
      
      
      $tbl
                                                          name Fn_1 Fprop_1 Fn_2
      96                            Participants in population   53    <NA>   50
      138         with one or more drug-related adverse events   28  (52.8)   41
      137                  with no drug-related adverse events   25  (47.2)    9
      1                                                          NA    <NA>   NA
      33                                     Cardiac disorders    4   (7.5)    4
      22                                   Atrial fibrillation    1   (1.9)    0
      23                                        Atrial flutter    0   (0.0)    0
      24                   Atrioventricular block first degree    1   (1.9)    0
      25                  Atrioventricular block second degree    0   (0.0)    0
      30                                           Bradycardia    1   (1.9)    0
      31                             Bundle branch block right    0   (0.0)    0
      34                            Cardiac failure congestive    1   (1.9)    0
      86                                 Myocardial infarction    2   (3.8)    1
      93                                          Palpitations    0   (0.0)    0
      112                                     Sinus arrhythmia    0   (0.0)    0
      113                                    Sinus bradycardia    2   (3.8)    1
      122                       Supraventricular extrasystoles    0   (0.0)    1
      129                            Ventricular extrasystoles    0   (0.0)    1
      134                       Wolff-parkinson-white syndrome    0   (0.0)    1
      40            Congenital, familial and genetic disorders    0   (0.0)    0
      130                            Ventricular septal defect    0   (0.0)    0
      50                           Ear and labyrinth disorders    0   (0.0)    2
      125                                             Tinnitus    0   (0.0)    1
      131                                              Vertigo    0   (0.0)    1
      57                                         Eye disorders    0   (0.0)    0
      132                                       Vision blurred    0   (0.0)    0
      62                            Gastrointestinal disorders    1   (1.9)    6
      2                                         Abdominal pain    0   (0.0)    1
      46                                             Diarrhoea    0   (0.0)    3
      48                                             Dyspepsia    1   (1.9)    0
      63                      Gastrooesophageal reflux disease    1   (1.9)    0
      87                                                Nausea    0   (0.0)    2
      110                              Salivary hypersecretion    0   (0.0)    0
      120                                   Stomach discomfort    0   (0.0)    0
      133                                             Vomiting    0   (0.0)    2
      64  General disorders and administration site conditions   11  (20.8)   23
      5                              Application site bleeding    0   (0.0)    1
      6                            Application site dermatitis    2   (3.8)    5
      7                          Application site desquamation    0   (0.0)    0
      8                             Application site discharge    0   (0.0)    0
      9                        Application site discolouration    0   (0.0)    0
      10                             Application site erythema    2   (3.8)    5
      11                           Application site induration    0   (0.0)    0
      12                           Application site irritation    3   (5.7)    6
      13                                 Application site pain    0   (0.0)    0
      14                         Application site perspiration    0   (0.0)    0
      15                             Application site pruritus    4   (7.5)   12
      16                             Application site reaction    0   (0.0)    0
      17                             Application site swelling    0   (0.0)    0
      18                            Application site urticaria    0   (0.0)    0
      19                             Application site vesicles    0   (0.0)    1
      20                               Application site warmth    0   (0.0)    1
      21                                              Asthenia    1   (1.9)    0
      35                                      Chest discomfort    0   (0.0)    0
      36                                                Chills    0   (0.0)    0
      59                                               Fatigue    1   (1.9)    1
      60                                      Feeling abnormal    0   (0.0)    0
      81                                               Malaise    0   (0.0)    0
      89                                                Oedema    0   (0.0)    1
      90                                     Oedema peripheral    0   (0.0)    0
      92                                                  Pain    0   (0.0)    1
      74        Injury, poisoning and procedural complications    0   (0.0)    2
      56                                           Excoriation    0   (0.0)    0
      58                                                  Fall    0   (0.0)    1
      117                                      Skin laceration    0   (0.0)    1
      135                                                Wound    0   (0.0)    1
      76                                        Investigations    2   (3.8)    2
      28                Blood creatine phosphokinase increased    0   (0.0)    0
      29                            Body temperature increased    0   (0.0)    1
      51               Electrocardiogram st segment depression    1   (1.9)    1
      52                    Electrocardiogram t wave inversion    0   (0.0)    0
      67                                  Heart rate increased    0   (0.0)    0
      68                                  Heart rate irregular    1   (1.9)    0
      82                    Metabolism and nutrition disorders    3   (5.7)    0
      42                                    Decreased appetite    1   (1.9)    0
      61                                          Food craving    1   (1.9)    0
      73                                    Increased appetite    1   (1.9)    0
      84       Musculoskeletal and connective tissue disorders    0   (0.0)    0
      85                                               Myalgia    0   (0.0)    0
      111                                        Shoulder pain    0   (0.0)    0
      88                              Nervous system disorders    3   (5.7)    8
      26                                      Balance disorder    0   (0.0)    1
      32                                     Burning sensation    0   (0.0)    0
      38                              Complex partial seizures    0   (0.0)    1
      41                                 Coordination abnormal    0   (0.0)    1
      47                                             Dizziness    1   (1.9)    3
      66                                              Headache    2   (3.8)    0
      70                                           Hypersomnia    0   (0.0)    0
      78                                              Lethargy    0   (0.0)    0
      94                                     Paraesthesia oral    0   (0.0)    1
      95                                              Parosmia    0   (0.0)    0
      119                                           Somnolence    0   (0.0)    0
      121                                               Stupor    0   (0.0)    0
      123                                              Syncope    0   (0.0)    4
      124                                    Syncope vasovagal    0   (0.0)    0
      126                           Transient ischaemic attack    0   (0.0)    0
      100                                Psychiatric disorders    1   (1.9)    6
      3                                              Agitation    0   (0.0)    2
      4                                                Anxiety    0   (0.0)    3
      39                                     Confusional state    0   (0.0)    1
      43                                              Delirium    0   (0.0)    0
      44                                        Depressed mood    0   (0.0)    0
      65                                 Hallucination, visual    0   (0.0)    0
      75                                              Insomnia    0   (0.0)    0
      77                                          Irritability    1   (1.9)    1
      79                                      Libido decreased    0   (0.0)    0
      80                                              Listless    0   (0.0)    0
      109                                         Restlessness    0   (0.0)    0
      106                          Renal and urinary disorders    0   (0.0)    1
      54                                              Enuresis    0   (0.0)    1
      83                                   Micturition urgency    0   (0.0)    0
      107             Reproductive system and breast disorders    1   (1.9)    0
      97                                           Pelvic pain    1   (1.9)    0
      108      Respiratory, thoracic and mediastinal disorders    2   (3.8)    0
      49                                              Dyspnoea    1   (1.9)    0
      53                                             Emphysema    1   (1.9)    0
      114               Skin and subcutaneous tissue disorders   12  (22.6)   21
      27                                               Blister    0   (0.0)    2
      37                                            Cold sweat    0   (0.0)    0
      45                                    Dermatitis contact    0   (0.0)    0
      55                                              Erythema    6  (11.3)    7
      69                                         Hyperhidrosis    1   (1.9)    1
      98                                              Pruritus    6  (11.3)   12
      99                                  Pruritus generalised    0   (0.0)    0
      101                                                 Rash    2   (3.8)    6
      102                                    Rash erythematous    0   (0.0)    1
      103                                  Rash maculo-papular    0   (0.0)    0
      104                                         Rash papular    0   (0.0)    0
      105                                        Rash pruritic    0   (0.0)    1
      115                                     Skin exfoliation    0   (0.0)    1
      116                                      Skin irritation    2   (3.8)    5
      118                                           Skin ulcer    0   (0.0)    0
      127                                            Urticaria    0   (0.0)    0
      128                                   Vascular disorders    0   (0.0)    2
      71                                          Hypertension    0   (0.0)    1
      72                                           Hypotension    0   (0.0)    1
      91                               Orthostatic hypotension    0   (0.0)    0
      136                                    Wound haemorrhage    0   (0.0)    0
          Fprop_2 Fn_3 Fprop_3 Mn_1 Mprop_1 Mn_2 Mprop_2 Mn_3 Mprop_3 Totaln_1
      96     <NA>   40    <NA>   33    <NA>   34    <NA>   44    <NA>       86
      138  (82.0)   32  (80.0)   16  (48.5)   32  (94.1)   38  (86.4)       44
      137  (18.0)    8  (20.0)   17  (51.5)    2   (5.9)    6  (13.6)       42
      1      <NA>   NA    <NA>   NA    <NA>   NA    <NA>   NA    <NA>       NA
      33    (8.0)    4  (10.0)    2   (6.1)    3   (8.8)    0   (0.0)        6
      22    (0.0)    2   (5.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      23    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      24    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      25    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      30    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      31    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      34    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      86    (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        2
      93    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      112   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      113   (2.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        2
      122   (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      129   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      134   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      40    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      130   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      50    (4.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      125   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      131   (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      57    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      132   (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      62   (12.0)    3   (7.5)    3   (9.1)    2   (5.9)    7  (15.9)        4
      2     (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      46    (6.0)    0   (0.0)    3   (9.1)    0   (0.0)    2   (4.5)        3
      48    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        1
      63    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      87    (4.0)    2   (5.0)    0   (0.0)    1   (2.9)    1   (2.3)        0
      110   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    3   (6.8)        0
      120   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      133   (4.0)    0   (0.0)    0   (0.0)    0   (0.0)    3   (6.8)        0
      64   (46.0)   17  (42.5)    7  (21.2)   20  (58.8)   18  (40.9)       18
      5     (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      6    (10.0)    4  (10.0)    3   (9.1)    4  (11.8)    3   (6.8)        5
      7     (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      8     (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      9     (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      10   (10.0)    5  (12.5)    1   (3.0)    7  (20.6)   10  (22.7)        3
      11    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      12   (12.0)    5  (12.5)    0   (0.0)    3   (8.8)    4   (9.1)        3
      13    (0.0)    2   (5.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      14    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    2   (4.5)        0
      15   (24.0)   10  (25.0)    2   (6.1)   10  (29.4)   12  (27.3)        6
      16    (0.0)    1   (2.5)    1   (3.0)    0   (0.0)    0   (0.0)        1
      17    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    1   (2.3)        0
      18    (0.0)    1   (2.5)    0   (0.0)    2   (5.9)    0   (0.0)        0
      19    (2.0)    3   (7.5)    1   (3.0)    3   (8.8)    3   (6.8)        1
      20    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      21    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      35    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      36    (0.0)    0   (0.0)    1   (3.0)    1   (2.9)    0   (0.0)        1
      59    (2.0)    4  (10.0)    0   (0.0)    1   (2.9)    0   (0.0)        1
      60    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      81    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    1   (2.3)        0
      89    (2.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      90    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      92    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      74    (4.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      56    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      58    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      117   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      135   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      76    (4.0)    1   (2.5)    2   (6.1)    0   (0.0)    0   (0.0)        4
      28    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      29    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      51    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      52    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      67    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      68    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      82    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        3
      42    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        1
      61    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      73    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      84    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    1   (2.3)        1
      85    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      111   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      88   (16.0)    7  (17.5)    2   (6.1)    4  (11.8)    8  (18.2)        5
      26    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      32    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    2   (4.5)        0
      38    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      41    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      47    (6.0)    2   (5.0)    1   (3.0)    3   (8.8)    4   (9.1)        2
      66    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        2
      70    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      78    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      94    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      95    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      119   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      121   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      123   (8.0)    2   (5.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      124   (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      126   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      100  (12.0)    1   (2.5)    1   (3.0)    3   (8.8)    4   (9.1)        2
      3     (4.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      4     (6.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      39    (2.0)    0   (0.0)    1   (3.0)    1   (2.9)    0   (0.0)        1
      43    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      44    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      65    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      75    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    2   (4.5)        0
      77    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      79    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      80    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      109   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      106   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      54    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      83    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      107   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      97    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      108   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        2
      49    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      53    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      114  (42.0)   13  (32.5)    5  (15.2)   18  (52.9)   26  (59.1)       17
      27    (4.0)    0   (0.0)    0   (0.0)    3   (8.8)    1   (2.3)        0
      37    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      45    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      55   (14.0)    7  (17.5)    3   (9.1)    6  (17.6)    7  (15.9)        9
      69    (2.0)    2   (5.0)    0   (0.0)    3   (8.8)    6  (13.6)        1
      98   (24.0)   11  (27.5)    1   (3.0)    9  (26.5)   15  (34.1)        7
      99    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    1   (2.3)        0
      101  (12.0)    2   (5.0)    1   (3.0)    5  (14.7)    5  (11.4)        3
      102   (2.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      103   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      104   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      105   (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    1   (2.3)        0
      115   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      116  (10.0)    1   (2.5)    0   (0.0)    1   (2.9)    4   (9.1)        2
      118   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      127   (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      128   (4.0)    1   (2.5)    1   (3.0)    0   (0.0)    0   (0.0)        1
      71    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      72    (2.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      91    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      136   (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
          Totalprop_1 Totaln_2 Totalprop_2 Totaln_3 Totalprop_3  order
      96         <NA>       84        <NA>       84        <NA>      1
      138      (51.2)       73      (86.9)       70      (83.3)    100
      137      (48.8)       11      (13.1)       14      (16.7)    200
      1          <NA>       NA        <NA>       NA        <NA>    900
      33        (7.0)        7       (8.3)        4       (4.8)  10000
      22        (1.2)        0       (0.0)        2       (2.4)  10021
      23        (0.0)        1       (1.2)        0       (0.0)  10022
      24        (1.2)        0       (0.0)        0       (0.0)  10023
      25        (1.2)        0       (0.0)        0       (0.0)  10024
      30        (1.2)        0       (0.0)        0       (0.0)  10029
      31        (0.0)        1       (1.2)        0       (0.0)  10030
      34        (1.2)        0       (0.0)        0       (0.0)  10032
      86        (2.3)        1       (1.2)        1       (1.2)  10075
      93        (0.0)        1       (1.2)        0       (0.0)  10081
      112       (1.2)        0       (0.0)        0       (0.0)  10095
      113       (2.3)        2       (2.4)        0       (0.0)  10096
      122       (0.0)        1       (1.2)        1       (1.2)  10104
      129       (0.0)        1       (1.2)        0       (0.0)  10110
      134       (0.0)        1       (1.2)        0       (0.0)  10115
      40        (0.0)        1       (1.2)        0       (0.0)  20000
      130       (0.0)        1       (1.2)        0       (0.0)  20111
      50        (0.0)        2       (2.4)        1       (1.2)  30000
      125       (0.0)        1       (1.2)        0       (0.0)  30107
      131       (0.0)        1       (1.2)        1       (1.2)  30112
      57        (0.0)        1       (1.2)        1       (1.2)  40000
      132       (0.0)        1       (1.2)        1       (1.2)  40113
      62        (4.7)        8       (9.5)       10      (11.9)  50000
      2         (0.0)        1       (1.2)        1       (1.2)  50001
      46        (3.5)        3       (3.6)        2       (2.4)  50043
      48        (1.2)        1       (1.2)        0       (0.0)  50045
      63        (1.2)        0       (0.0)        0       (0.0)  50057
      87        (0.0)        3       (3.6)        3       (3.6)  50076
      110       (0.0)        0       (0.0)        3       (3.6)  50093
      120       (0.0)        0       (0.0)        1       (1.2)  50102
      133       (0.0)        2       (2.4)        3       (3.6)  50114
      64       (20.9)       43      (51.2)       35      (41.7)  60000
      5         (0.0)        1       (1.2)        0       (0.0)  60004
      6         (5.8)        9      (10.7)        7       (8.3)  60005
      7         (0.0)        1       (1.2)        0       (0.0)  60006
      8         (0.0)        0       (0.0)        1       (1.2)  60007
      9         (0.0)        1       (1.2)        0       (0.0)  60008
      10        (3.5)       12      (14.3)       15      (17.9)  60009
      11        (1.2)        0       (0.0)        0       (0.0)  60010
      12        (3.5)        9      (10.7)        9      (10.7)  60011
      13        (0.0)        0       (0.0)        2       (2.4)  60012
      14        (0.0)        0       (0.0)        2       (2.4)  60013
      15        (7.0)       22      (26.2)       22      (26.2)  60014
      16        (1.2)        0       (0.0)        1       (1.2)  60015
      17        (0.0)        1       (1.2)        2       (2.4)  60016
      18        (0.0)        2       (2.4)        1       (1.2)  60017
      19        (1.2)        4       (4.8)        6       (7.1)  60018
      20        (0.0)        1       (1.2)        0       (0.0)  60019
      21        (1.2)        0       (0.0)        0       (0.0)  60020
      35        (0.0)        0       (0.0)        1       (1.2)  60033
      36        (1.2)        1       (1.2)        0       (0.0)  60034
      59        (1.2)        2       (2.4)        4       (4.8)  60054
      60        (0.0)        0       (0.0)        1       (1.2)  60055
      81        (0.0)        1       (1.2)        1       (1.2)  60072
      89        (0.0)        2       (2.4)        0       (0.0)  60077
      90        (0.0)        0       (0.0)        1       (1.2)  60078
      92        (0.0)        1       (1.2)        1       (1.2)  60080
      74        (0.0)        2       (2.4)        1       (1.2)  70000
      56        (0.0)        0       (0.0)        1       (1.2)  70052
      58        (0.0)        1       (1.2)        0       (0.0)  70053
      117       (0.0)        1       (1.2)        0       (0.0)  70099
      135       (0.0)        1       (1.2)        0       (0.0)  70116
      76        (4.7)        2       (2.4)        1       (1.2)  80000
      28        (1.2)        0       (0.0)        0       (0.0)  80027
      29        (0.0)        1       (1.2)        0       (0.0)  80028
      51        (1.2)        1       (1.2)        0       (0.0)  80047
      52        (0.0)        0       (0.0)        1       (1.2)  80048
      67        (1.2)        0       (0.0)        0       (0.0)  80060
      68        (1.2)        0       (0.0)        0       (0.0)  80061
      82        (3.5)        0       (0.0)        1       (1.2)  90000
      42        (1.2)        0       (0.0)        1       (1.2)  90039
      61        (1.2)        0       (0.0)        0       (0.0)  90056
      73        (1.2)        0       (0.0)        0       (0.0)  90066
      84        (1.2)        0       (0.0)        1       (1.2) 100000
      85        (0.0)        0       (0.0)        1       (1.2) 100074
      111       (1.2)        0       (0.0)        0       (0.0) 100094
      88        (5.8)       12      (14.3)       15      (17.9) 110000
      26        (0.0)        1       (1.2)        0       (0.0) 110025
      32        (0.0)        0       (0.0)        2       (2.4) 110031
      38        (0.0)        1       (1.2)        0       (0.0) 110036
      41        (0.0)        1       (1.2)        0       (0.0) 110038
      47        (2.3)        6       (7.1)        6       (7.1) 110044
      66        (2.3)        1       (1.2)        1       (1.2) 110059
      70        (0.0)        0       (0.0)        1       (1.2) 110063
      78        (0.0)        1       (1.2)        1       (1.2) 110069
      94        (0.0)        1       (1.2)        0       (0.0) 110082
      95        (0.0)        0       (0.0)        1       (1.2) 110083
      119       (1.2)        0       (0.0)        0       (0.0) 110101
      121       (0.0)        1       (1.2)        0       (0.0) 110103
      123       (0.0)        4       (4.8)        3       (3.6) 110105
      124       (0.0)        0       (0.0)        1       (1.2) 110106
      126       (0.0)        1       (1.2)        0       (0.0) 110108
      100       (2.3)        9      (10.7)        5       (6.0) 120000
      3         (0.0)        2       (2.4)        0       (0.0) 120002
      4         (0.0)        3       (3.6)        0       (0.0) 120003
      39        (1.2)        2       (2.4)        0       (0.0) 120037
      43        (0.0)        0       (0.0)        1       (1.2) 120040
      44        (0.0)        1       (1.2)        0       (0.0) 120041
      65        (0.0)        0       (0.0)        1       (1.2) 120058
      75        (0.0)        0       (0.0)        2       (2.4) 120067
      77        (1.2)        1       (1.2)        0       (0.0) 120068
      79        (0.0)        0       (0.0)        1       (1.2) 120070
      80        (0.0)        0       (0.0)        1       (1.2) 120071
      109       (0.0)        1       (1.2)        0       (0.0) 120092
      106       (0.0)        1       (1.2)        1       (1.2) 130000
      54        (0.0)        1       (1.2)        0       (0.0) 130050
      83        (0.0)        0       (0.0)        1       (1.2) 130073
      107       (1.2)        0       (0.0)        0       (0.0) 140000
      97        (1.2)        0       (0.0)        0       (0.0) 140084
      108       (2.3)        0       (0.0)        0       (0.0) 150000
      49        (1.2)        0       (0.0)        0       (0.0) 150046
      53        (1.2)        0       (0.0)        0       (0.0) 150049
      114      (19.8)       39      (46.4)       39      (46.4) 160000
      27        (0.0)        5       (6.0)        1       (1.2) 160026
      37        (1.2)        0       (0.0)        0       (0.0) 160035
      45        (0.0)        1       (1.2)        0       (0.0) 160042
      55       (10.5)       13      (15.5)       14      (16.7) 160051
      69        (1.2)        4       (4.8)        8       (9.5) 160062
      98        (8.1)       21      (25.0)       26      (31.0) 160085
      99        (0.0)        1       (1.2)        1       (1.2) 160086
      101       (3.5)       11      (13.1)        7       (8.3) 160087
      102       (0.0)        2       (2.4)        0       (0.0) 160088
      103       (0.0)        0       (0.0)        1       (1.2) 160089
      104       (0.0)        0       (0.0)        1       (1.2) 160090
      105       (0.0)        1       (1.2)        2       (2.4) 160091
      115       (0.0)        1       (1.2)        0       (0.0) 160097
      116       (2.3)        6       (7.1)        5       (6.0) 160098
      118       (1.2)        0       (0.0)        0       (0.0) 160100
      127       (0.0)        1       (1.2)        1       (1.2) 160109
      128       (1.2)        2       (2.4)        1       (1.2) 170000
      71        (0.0)        1       (1.2)        0       (0.0) 170064
      72        (1.2)        1       (1.2)        0       (0.0) 170065
      91        (1.2)        0       (0.0)        0       (0.0) 170079
      136       (0.0)        0       (0.0)        1       (1.2) 170117
      
      $display
      [1] "n"    "prop"
      
      $rtf
                                                          name Fn_1 Fprop_1 Fn_2
      96                            Participants in population   53    <NA>   50
      138         with one or more drug-related adverse events   28  (52.8)   41
      137                  with no drug-related adverse events   25  (47.2)    9
      1                                                        <NA>    <NA> <NA>
      33                                     Cardiac disorders    4   (7.5)    4
      22                                   Atrial fibrillation    1   (1.9)    0
      23                                        Atrial flutter    0   (0.0)    0
      24                   Atrioventricular block first degree    1   (1.9)    0
      25                  Atrioventricular block second degree    0   (0.0)    0
      30                                           Bradycardia    1   (1.9)    0
      31                             Bundle branch block right    0   (0.0)    0
      34                            Cardiac failure congestive    1   (1.9)    0
      86                                 Myocardial infarction    2   (3.8)    1
      93                                          Palpitations    0   (0.0)    0
      112                                     Sinus arrhythmia    0   (0.0)    0
      113                                    Sinus bradycardia    2   (3.8)    1
      122                       Supraventricular extrasystoles    0   (0.0)    1
      129                            Ventricular extrasystoles    0   (0.0)    1
      134                       Wolff-parkinson-white syndrome    0   (0.0)    1
      40            Congenital, familial and genetic disorders    0   (0.0)    0
      130                            Ventricular septal defect    0   (0.0)    0
      50                           Ear and labyrinth disorders    0   (0.0)    2
      125                                             Tinnitus    0   (0.0)    1
      131                                              Vertigo    0   (0.0)    1
      57                                         Eye disorders    0   (0.0)    0
      132                                       Vision blurred    0   (0.0)    0
      62                            Gastrointestinal disorders    1   (1.9)    6
      2                                         Abdominal pain    0   (0.0)    1
      46                                             Diarrhoea    0   (0.0)    3
      48                                             Dyspepsia    1   (1.9)    0
      63                      Gastrooesophageal reflux disease    1   (1.9)    0
      87                                                Nausea    0   (0.0)    2
      110                              Salivary hypersecretion    0   (0.0)    0
      120                                   Stomach discomfort    0   (0.0)    0
      133                                             Vomiting    0   (0.0)    2
      64  General disorders and administration site conditions   11  (20.8)   23
      5                              Application site bleeding    0   (0.0)    1
      6                            Application site dermatitis    2   (3.8)    5
      7                          Application site desquamation    0   (0.0)    0
      8                             Application site discharge    0   (0.0)    0
      9                        Application site discolouration    0   (0.0)    0
      10                             Application site erythema    2   (3.8)    5
      11                           Application site induration    0   (0.0)    0
      12                           Application site irritation    3   (5.7)    6
      13                                 Application site pain    0   (0.0)    0
      14                         Application site perspiration    0   (0.0)    0
      15                             Application site pruritus    4   (7.5)   12
      16                             Application site reaction    0   (0.0)    0
      17                             Application site swelling    0   (0.0)    0
      18                            Application site urticaria    0   (0.0)    0
      19                             Application site vesicles    0   (0.0)    1
      20                               Application site warmth    0   (0.0)    1
      21                                              Asthenia    1   (1.9)    0
      35                                      Chest discomfort    0   (0.0)    0
      36                                                Chills    0   (0.0)    0
      59                                               Fatigue    1   (1.9)    1
      60                                      Feeling abnormal    0   (0.0)    0
      81                                               Malaise    0   (0.0)    0
      89                                                Oedema    0   (0.0)    1
      90                                     Oedema peripheral    0   (0.0)    0
      92                                                  Pain    0   (0.0)    1
      74        Injury, poisoning and procedural complications    0   (0.0)    2
      56                                           Excoriation    0   (0.0)    0
      58                                                  Fall    0   (0.0)    1
      117                                      Skin laceration    0   (0.0)    1
      135                                                Wound    0   (0.0)    1
      76                                        Investigations    2   (3.8)    2
      28                Blood creatine phosphokinase increased    0   (0.0)    0
      29                            Body temperature increased    0   (0.0)    1
      51               Electrocardiogram st segment depression    1   (1.9)    1
      52                    Electrocardiogram t wave inversion    0   (0.0)    0
      67                                  Heart rate increased    0   (0.0)    0
      68                                  Heart rate irregular    1   (1.9)    0
      82                    Metabolism and nutrition disorders    3   (5.7)    0
      42                                    Decreased appetite    1   (1.9)    0
      61                                          Food craving    1   (1.9)    0
      73                                    Increased appetite    1   (1.9)    0
      84       Musculoskeletal and connective tissue disorders    0   (0.0)    0
      85                                               Myalgia    0   (0.0)    0
      111                                        Shoulder pain    0   (0.0)    0
      88                              Nervous system disorders    3   (5.7)    8
      26                                      Balance disorder    0   (0.0)    1
      32                                     Burning sensation    0   (0.0)    0
      38                              Complex partial seizures    0   (0.0)    1
      41                                 Coordination abnormal    0   (0.0)    1
      47                                             Dizziness    1   (1.9)    3
      66                                              Headache    2   (3.8)    0
      70                                           Hypersomnia    0   (0.0)    0
      78                                              Lethargy    0   (0.0)    0
      94                                     Paraesthesia oral    0   (0.0)    1
      95                                              Parosmia    0   (0.0)    0
      119                                           Somnolence    0   (0.0)    0
      121                                               Stupor    0   (0.0)    0
      123                                              Syncope    0   (0.0)    4
      124                                    Syncope vasovagal    0   (0.0)    0
      126                           Transient ischaemic attack    0   (0.0)    0
      100                                Psychiatric disorders    1   (1.9)    6
      3                                              Agitation    0   (0.0)    2
      4                                                Anxiety    0   (0.0)    3
      39                                     Confusional state    0   (0.0)    1
      43                                              Delirium    0   (0.0)    0
      44                                        Depressed mood    0   (0.0)    0
      65                                 Hallucination, visual    0   (0.0)    0
      75                                              Insomnia    0   (0.0)    0
      77                                          Irritability    1   (1.9)    1
      79                                      Libido decreased    0   (0.0)    0
      80                                              Listless    0   (0.0)    0
      109                                         Restlessness    0   (0.0)    0
      106                          Renal and urinary disorders    0   (0.0)    1
      54                                              Enuresis    0   (0.0)    1
      83                                   Micturition urgency    0   (0.0)    0
      107             Reproductive system and breast disorders    1   (1.9)    0
      97                                           Pelvic pain    1   (1.9)    0
      108      Respiratory, thoracic and mediastinal disorders    2   (3.8)    0
      49                                              Dyspnoea    1   (1.9)    0
      53                                             Emphysema    1   (1.9)    0
      114               Skin and subcutaneous tissue disorders   12  (22.6)   21
      27                                               Blister    0   (0.0)    2
      37                                            Cold sweat    0   (0.0)    0
      45                                    Dermatitis contact    0   (0.0)    0
      55                                              Erythema    6  (11.3)    7
      69                                         Hyperhidrosis    1   (1.9)    1
      98                                              Pruritus    6  (11.3)   12
      99                                  Pruritus generalised    0   (0.0)    0
      101                                                 Rash    2   (3.8)    6
      102                                    Rash erythematous    0   (0.0)    1
      103                                  Rash maculo-papular    0   (0.0)    0
      104                                         Rash papular    0   (0.0)    0
      105                                        Rash pruritic    0   (0.0)    1
      115                                     Skin exfoliation    0   (0.0)    1
      116                                      Skin irritation    2   (3.8)    5
      118                                           Skin ulcer    0   (0.0)    0
      127                                            Urticaria    0   (0.0)    0
      128                                   Vascular disorders    0   (0.0)    2
      71                                          Hypertension    0   (0.0)    1
      72                                           Hypotension    0   (0.0)    1
      91                               Orthostatic hypotension    0   (0.0)    0
      136                                    Wound haemorrhage    0   (0.0)    0
          Fprop_2 Fn_3 Fprop_3 Mn_1 Mprop_1 Mn_2 Mprop_2 Mn_3 Mprop_3 Totaln_1
      96     <NA>   40    <NA>   33    <NA>   34    <NA>   44    <NA>       86
      138  (82.0)   32  (80.0)   16  (48.5)   32  (94.1)   38  (86.4)       44
      137  (18.0)    8  (20.0)   17  (51.5)    2   (5.9)    6  (13.6)       42
      1      <NA> <NA>    <NA> <NA>    <NA> <NA>    <NA> <NA>    <NA>     <NA>
      33    (8.0)    4  (10.0)    2   (6.1)    3   (8.8)    0   (0.0)        6
      22    (0.0)    2   (5.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      23    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      24    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      25    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      30    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      31    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      34    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      86    (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        2
      93    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      112   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      113   (2.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        2
      122   (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      129   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      134   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      40    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      130   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      50    (4.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      125   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      131   (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      57    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      132   (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      62   (12.0)    3   (7.5)    3   (9.1)    2   (5.9)    7  (15.9)        4
      2     (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      46    (6.0)    0   (0.0)    3   (9.1)    0   (0.0)    2   (4.5)        3
      48    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        1
      63    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      87    (4.0)    2   (5.0)    0   (0.0)    1   (2.9)    1   (2.3)        0
      110   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    3   (6.8)        0
      120   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      133   (4.0)    0   (0.0)    0   (0.0)    0   (0.0)    3   (6.8)        0
      64   (46.0)   17  (42.5)    7  (21.2)   20  (58.8)   18  (40.9)       18
      5     (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      6    (10.0)    4  (10.0)    3   (9.1)    4  (11.8)    3   (6.8)        5
      7     (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      8     (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      9     (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      10   (10.0)    5  (12.5)    1   (3.0)    7  (20.6)   10  (22.7)        3
      11    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      12   (12.0)    5  (12.5)    0   (0.0)    3   (8.8)    4   (9.1)        3
      13    (0.0)    2   (5.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      14    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    2   (4.5)        0
      15   (24.0)   10  (25.0)    2   (6.1)   10  (29.4)   12  (27.3)        6
      16    (0.0)    1   (2.5)    1   (3.0)    0   (0.0)    0   (0.0)        1
      17    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    1   (2.3)        0
      18    (0.0)    1   (2.5)    0   (0.0)    2   (5.9)    0   (0.0)        0
      19    (2.0)    3   (7.5)    1   (3.0)    3   (8.8)    3   (6.8)        1
      20    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      21    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      35    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      36    (0.0)    0   (0.0)    1   (3.0)    1   (2.9)    0   (0.0)        1
      59    (2.0)    4  (10.0)    0   (0.0)    1   (2.9)    0   (0.0)        1
      60    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      81    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    1   (2.3)        0
      89    (2.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      90    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      92    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      74    (4.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      56    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      58    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      117   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      135   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      76    (4.0)    1   (2.5)    2   (6.1)    0   (0.0)    0   (0.0)        4
      28    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      29    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      51    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      52    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      67    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      68    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      82    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        3
      42    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        1
      61    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      73    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      84    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    1   (2.3)        1
      85    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      111   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      88   (16.0)    7  (17.5)    2   (6.1)    4  (11.8)    8  (18.2)        5
      26    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      32    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    2   (4.5)        0
      38    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      41    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      47    (6.0)    2   (5.0)    1   (3.0)    3   (8.8)    4   (9.1)        2
      66    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        2
      70    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      78    (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      94    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      95    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      119   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      121   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      123   (8.0)    2   (5.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      124   (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      126   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      100  (12.0)    1   (2.5)    1   (3.0)    3   (8.8)    4   (9.1)        2
      3     (4.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      4     (6.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      39    (2.0)    0   (0.0)    1   (3.0)    1   (2.9)    0   (0.0)        1
      43    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      44    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      65    (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
      75    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    2   (4.5)        0
      77    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      79    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      80    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      109   (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      106   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      54    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      83    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      107   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      97    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      108   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        2
      49    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      53    (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        1
      114  (42.0)   13  (32.5)    5  (15.2)   18  (52.9)   26  (59.1)       17
      27    (4.0)    0   (0.0)    0   (0.0)    3   (8.8)    1   (2.3)        0
      37    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      45    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      55   (14.0)    7  (17.5)    3   (9.1)    6  (17.6)    7  (15.9)        9
      69    (2.0)    2   (5.0)    0   (0.0)    3   (8.8)    6  (13.6)        1
      98   (24.0)   11  (27.5)    1   (3.0)    9  (26.5)   15  (34.1)        7
      99    (0.0)    0   (0.0)    0   (0.0)    1   (2.9)    1   (2.3)        0
      101  (12.0)    2   (5.0)    1   (3.0)    5  (14.7)    5  (11.4)        3
      102   (2.0)    0   (0.0)    0   (0.0)    1   (2.9)    0   (0.0)        0
      103   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      104   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)    1   (2.3)        0
      105   (2.0)    1   (2.5)    0   (0.0)    0   (0.0)    1   (2.3)        0
      115   (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      116  (10.0)    1   (2.5)    0   (0.0)    1   (2.9)    4   (9.1)        2
      118   (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      127   (0.0)    1   (2.5)    0   (0.0)    1   (2.9)    0   (0.0)        0
      128   (4.0)    1   (2.5)    1   (3.0)    0   (0.0)    0   (0.0)        1
      71    (2.0)    0   (0.0)    0   (0.0)    0   (0.0)    0   (0.0)        0
      72    (2.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      91    (0.0)    0   (0.0)    1   (3.0)    0   (0.0)    0   (0.0)        1
      136   (0.0)    1   (2.5)    0   (0.0)    0   (0.0)    0   (0.0)        0
          Totalprop_1 Totaln_2 Totalprop_2 Totaln_3 Totalprop_3
      96         <NA>       84        <NA>       84        <NA>
      138      (51.2)       73      (86.9)       70      (83.3)
      137      (48.8)       11      (13.1)       14      (16.7)
      1          <NA>     <NA>        <NA>     <NA>        <NA>
      33        (7.0)        7       (8.3)        4       (4.8)
      22        (1.2)        0       (0.0)        2       (2.4)
      23        (0.0)        1       (1.2)        0       (0.0)
      24        (1.2)        0       (0.0)        0       (0.0)
      25        (1.2)        0       (0.0)        0       (0.0)
      30        (1.2)        0       (0.0)        0       (0.0)
      31        (0.0)        1       (1.2)        0       (0.0)
      34        (1.2)        0       (0.0)        0       (0.0)
      86        (2.3)        1       (1.2)        1       (1.2)
      93        (0.0)        1       (1.2)        0       (0.0)
      112       (1.2)        0       (0.0)        0       (0.0)
      113       (2.3)        2       (2.4)        0       (0.0)
      122       (0.0)        1       (1.2)        1       (1.2)
      129       (0.0)        1       (1.2)        0       (0.0)
      134       (0.0)        1       (1.2)        0       (0.0)
      40        (0.0)        1       (1.2)        0       (0.0)
      130       (0.0)        1       (1.2)        0       (0.0)
      50        (0.0)        2       (2.4)        1       (1.2)
      125       (0.0)        1       (1.2)        0       (0.0)
      131       (0.0)        1       (1.2)        1       (1.2)
      57        (0.0)        1       (1.2)        1       (1.2)
      132       (0.0)        1       (1.2)        1       (1.2)
      62        (4.7)        8       (9.5)       10      (11.9)
      2         (0.0)        1       (1.2)        1       (1.2)
      46        (3.5)        3       (3.6)        2       (2.4)
      48        (1.2)        1       (1.2)        0       (0.0)
      63        (1.2)        0       (0.0)        0       (0.0)
      87        (0.0)        3       (3.6)        3       (3.6)
      110       (0.0)        0       (0.0)        3       (3.6)
      120       (0.0)        0       (0.0)        1       (1.2)
      133       (0.0)        2       (2.4)        3       (3.6)
      64       (20.9)       43      (51.2)       35      (41.7)
      5         (0.0)        1       (1.2)        0       (0.0)
      6         (5.8)        9      (10.7)        7       (8.3)
      7         (0.0)        1       (1.2)        0       (0.0)
      8         (0.0)        0       (0.0)        1       (1.2)
      9         (0.0)        1       (1.2)        0       (0.0)
      10        (3.5)       12      (14.3)       15      (17.9)
      11        (1.2)        0       (0.0)        0       (0.0)
      12        (3.5)        9      (10.7)        9      (10.7)
      13        (0.0)        0       (0.0)        2       (2.4)
      14        (0.0)        0       (0.0)        2       (2.4)
      15        (7.0)       22      (26.2)       22      (26.2)
      16        (1.2)        0       (0.0)        1       (1.2)
      17        (0.0)        1       (1.2)        2       (2.4)
      18        (0.0)        2       (2.4)        1       (1.2)
      19        (1.2)        4       (4.8)        6       (7.1)
      20        (0.0)        1       (1.2)        0       (0.0)
      21        (1.2)        0       (0.0)        0       (0.0)
      35        (0.0)        0       (0.0)        1       (1.2)
      36        (1.2)        1       (1.2)        0       (0.0)
      59        (1.2)        2       (2.4)        4       (4.8)
      60        (0.0)        0       (0.0)        1       (1.2)
      81        (0.0)        1       (1.2)        1       (1.2)
      89        (0.0)        2       (2.4)        0       (0.0)
      90        (0.0)        0       (0.0)        1       (1.2)
      92        (0.0)        1       (1.2)        1       (1.2)
      74        (0.0)        2       (2.4)        1       (1.2)
      56        (0.0)        0       (0.0)        1       (1.2)
      58        (0.0)        1       (1.2)        0       (0.0)
      117       (0.0)        1       (1.2)        0       (0.0)
      135       (0.0)        1       (1.2)        0       (0.0)
      76        (4.7)        2       (2.4)        1       (1.2)
      28        (1.2)        0       (0.0)        0       (0.0)
      29        (0.0)        1       (1.2)        0       (0.0)
      51        (1.2)        1       (1.2)        0       (0.0)
      52        (0.0)        0       (0.0)        1       (1.2)
      67        (1.2)        0       (0.0)        0       (0.0)
      68        (1.2)        0       (0.0)        0       (0.0)
      82        (3.5)        0       (0.0)        1       (1.2)
      42        (1.2)        0       (0.0)        1       (1.2)
      61        (1.2)        0       (0.0)        0       (0.0)
      73        (1.2)        0       (0.0)        0       (0.0)
      84        (1.2)        0       (0.0)        1       (1.2)
      85        (0.0)        0       (0.0)        1       (1.2)
      111       (1.2)        0       (0.0)        0       (0.0)
      88        (5.8)       12      (14.3)       15      (17.9)
      26        (0.0)        1       (1.2)        0       (0.0)
      32        (0.0)        0       (0.0)        2       (2.4)
      38        (0.0)        1       (1.2)        0       (0.0)
      41        (0.0)        1       (1.2)        0       (0.0)
      47        (2.3)        6       (7.1)        6       (7.1)
      66        (2.3)        1       (1.2)        1       (1.2)
      70        (0.0)        0       (0.0)        1       (1.2)
      78        (0.0)        1       (1.2)        1       (1.2)
      94        (0.0)        1       (1.2)        0       (0.0)
      95        (0.0)        0       (0.0)        1       (1.2)
      119       (1.2)        0       (0.0)        0       (0.0)
      121       (0.0)        1       (1.2)        0       (0.0)
      123       (0.0)        4       (4.8)        3       (3.6)
      124       (0.0)        0       (0.0)        1       (1.2)
      126       (0.0)        1       (1.2)        0       (0.0)
      100       (2.3)        9      (10.7)        5       (6.0)
      3         (0.0)        2       (2.4)        0       (0.0)
      4         (0.0)        3       (3.6)        0       (0.0)
      39        (1.2)        2       (2.4)        0       (0.0)
      43        (0.0)        0       (0.0)        1       (1.2)
      44        (0.0)        1       (1.2)        0       (0.0)
      65        (0.0)        0       (0.0)        1       (1.2)
      75        (0.0)        0       (0.0)        2       (2.4)
      77        (1.2)        1       (1.2)        0       (0.0)
      79        (0.0)        0       (0.0)        1       (1.2)
      80        (0.0)        0       (0.0)        1       (1.2)
      109       (0.0)        1       (1.2)        0       (0.0)
      106       (0.0)        1       (1.2)        1       (1.2)
      54        (0.0)        1       (1.2)        0       (0.0)
      83        (0.0)        0       (0.0)        1       (1.2)
      107       (1.2)        0       (0.0)        0       (0.0)
      97        (1.2)        0       (0.0)        0       (0.0)
      108       (2.3)        0       (0.0)        0       (0.0)
      49        (1.2)        0       (0.0)        0       (0.0)
      53        (1.2)        0       (0.0)        0       (0.0)
      114      (19.8)       39      (46.4)       39      (46.4)
      27        (0.0)        5       (6.0)        1       (1.2)
      37        (1.2)        0       (0.0)        0       (0.0)
      45        (0.0)        1       (1.2)        0       (0.0)
      55       (10.5)       13      (15.5)       14      (16.7)
      69        (1.2)        4       (4.8)        8       (9.5)
      98        (8.1)       21      (25.0)       26      (31.0)
      99        (0.0)        1       (1.2)        1       (1.2)
      101       (3.5)       11      (13.1)        7       (8.3)
      102       (0.0)        2       (2.4)        0       (0.0)
      103       (0.0)        0       (0.0)        1       (1.2)
      104       (0.0)        0       (0.0)        1       (1.2)
      105       (0.0)        1       (1.2)        2       (2.4)
      115       (0.0)        1       (1.2)        0       (0.0)
      116       (2.3)        6       (7.1)        5       (6.0)
      118       (1.2)        0       (0.0)        0       (0.0)
      127       (0.0)        1       (1.2)        1       (1.2)
      128       (1.2)        2       (2.4)        1       (1.2)
      71        (0.0)        1       (1.2)        0       (0.0)
      72        (1.2)        1       (1.2)        0       (0.0)
      91        (1.2)        0       (0.0)        0       (0.0)
      136       (0.0)        0       (0.0)        1       (1.2)
      

