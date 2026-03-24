# Prepare datasets for AE specific analysis

Prepare datasets for AE specific analysis

## Usage

``` r
prepare_ae_specific(
  meta,
  population,
  observation,
  parameter,
  components = c("soc", "par"),
  reference_group = NULL
)
```

## Arguments

- meta:

  A metadata object created by metalite.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

- components:

  A character vector of components name.

- reference_group:

  An integer to indicate reference group. Default is 2 if there are 2
  groups, otherwise, the default is 1.

## Value

A list of analysis datasets needed for AE specific analysis.

## Examples

``` r
meta <- meta_ae_example()
str(prepare_ae_specific(meta, "apat", "wk12", "rel"))
#> List of 15
#>  $ meta           :List of 7
#>   ..$ data_population :'data.frame': 254 obs. of  49 variables:
#>   .. ..$ STUDYID : chr [1:254] "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" ...
#>   .. .. ..- attr(*, "label")= chr "Study Identifier"
#>   .. ..$ USUBJID : chr [1:254] "01-701-1015" "01-701-1023" "01-701-1028" "01-701-1033" ...
#>   .. .. ..- attr(*, "label")= chr "Unique Subject Identifier"
#>   .. ..$ SUBJID  : chr [1:254] "1015" "1023" "1028" "1033" ...
#>   .. .. ..- attr(*, "label")= chr "Subject Identifier for the Study"
#>   .. ..$ SITEID  : chr [1:254] "701" "701" "701" "701" ...
#>   .. .. ..- attr(*, "label")= chr "Study Site Identifier"
#>   .. ..$ SITEGR1 : chr [1:254] "701" "701" "701" "701" ...
#>   .. .. ..- attr(*, "label")= chr "Pooled Site Group 1"
#>   .. ..$ ARM     : chr [1:254] "Placebo" "Placebo" "Xanomeline High Dose" "Xanomeline Low Dose" ...
#>   .. .. ..- attr(*, "label")= chr "Description of Planned Arm"
#>   .. ..$ TRT01P  : chr [1:254] "Placebo" "Placebo" "Xanomeline High Dose" "Xanomeline Low Dose" ...
#>   .. .. ..- attr(*, "label")= chr "Planned Treatment for Period 01"
#>   .. ..$ TRT01PN : num [1:254] 0 0 81 54 81 0 54 54 54 0 ...
#>   .. .. ..- attr(*, "label")= chr "Planned Treatment for Period 01 (N)"
#>   .. ..$ TRT01A  : chr [1:254] "Placebo" "Placebo" "Xanomeline High Dose" "Xanomeline Low Dose" ...
#>   .. .. ..- attr(*, "label")= chr "Actual Treatment for Period 01"
#>   .. ..$ TRT01AN : num [1:254] 0 0 81 54 81 0 54 54 54 0 ...
#>   .. .. ..- attr(*, "label")= chr "Actual Treatment for Period 01 (N)"
#>   .. ..$ TRTSDT  : Date[1:254], format: "2014-01-02" "2012-08-05" ...
#>   .. ..$ TRTEDT  : Date[1:254], format: "2014-07-02" "2012-09-01" ...
#>   .. ..$ TRTDUR  : num [1:254] 182 28 180 14 183 26 190 10 55 182 ...
#>   .. .. ..- attr(*, "label")= chr "Duration of Treatment (days)"
#>   .. ..$ AVGDD   : num [1:254] 0 0 77.7 54 76.9 0 54 54 54 0 ...
#>   .. .. ..- attr(*, "label")= chr "Avg Daily Dose (as planned)"
#>   .. ..$ CUMDOSE : num [1:254] 0 0 13986 756 14067 ...
#>   .. .. ..- attr(*, "label")= chr "Cumulative Dose (as planned)"
#>   .. ..$ AGE     : num [1:254] 63 64 71 74 77 85 68 81 84 52 ...
#>   .. .. ..- attr(*, "label")= chr "Age"
#>   .. ..$ AGEGR1  : chr [1:254] "<65" "<65" "65-80" "65-80" ...
#>   .. .. ..- attr(*, "label")= chr "Pooled Age Group 1"
#>   .. ..$ AGEGR1N : num [1:254] 1 1 2 2 2 3 2 3 3 1 ...
#>   .. .. ..- attr(*, "label")= chr "Pooled Age Group 1 (N)"
#>   .. ..$ AGEU    : chr [1:254] "YEARS" "YEARS" "YEARS" "YEARS" ...
#>   .. .. ..- attr(*, "label")= chr "Age Units"
#>   .. ..$ RACE    : chr [1:254] "WHITE" "WHITE" "WHITE" "WHITE" ...
#>   .. ..$ RACEN   : num [1:254] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. .. ..- attr(*, "label")= chr "Race (N)"
#>   .. ..$ SEX     : chr [1:254] "F" "M" "M" "M" ...
#>   .. .. ..- attr(*, "label")= chr "Sex"
#>   .. ..$ ETHNIC  : chr [1:254] "HISPANIC OR LATINO" "HISPANIC OR LATINO" "NOT HISPANIC OR LATINO" "NOT HISPANIC OR LATINO" ...
#>   .. .. ..- attr(*, "label")= chr "Ethnicity"
#>   .. ..$ SAFFL   : chr [1:254] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "Safety Population Flag"
#>   .. ..$ ITTFL   : chr [1:254] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "Intent-To-Treat Population Flag"
#>   .. ..$ EFFFL   : chr [1:254] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "Efficacy Population Flag"
#>   .. ..$ COMP8FL : chr [1:254] "Y" "N" "Y" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Completers of Week 8 Population Flag"
#>   .. ..$ COMP16FL: chr [1:254] "Y" "N" "Y" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Completers of Week 16 Population Flag"
#>   .. ..$ COMP24FL: chr [1:254] "Y" "N" "Y" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Completers of Week 24 Population Flag"
#>   .. ..$ DISCONFL: chr [1:254] "" "Y" "" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "Did the Subject Discontinue the Study?"
#>   .. ..$ DSRAEFL : chr [1:254] "" "Y" "" "" ...
#>   .. .. ..- attr(*, "label")= chr "Discontinued due to AE?"
#>   .. ..$ DTHFL   : chr [1:254] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= chr "Subject Died?"
#>   .. ..$ BMIBL   : num [1:254] 25.1 30.4 31.4 28.8 26.1 30.4 27.3 23.9 23.9 21.9 ...
#>   .. .. ..- attr(*, "label")= chr "Baseline BMI (kg/m^2)"
#>   .. ..$ BMIBLGR1: chr [1:254] "25-<30" ">=30" ">=30" "25-<30" ...
#>   .. .. ..- attr(*, "label")= chr "Pooled Baseline BMI Group 1"
#>   .. ..$ HEIGHTBL: num [1:254] 147 163 178 175 155 ...
#>   .. .. ..- attr(*, "label")= chr "Baseline Height (cm)"
#>   .. ..$ WEIGHTBL: num [1:254] 54.4 80.3 99.3 88.5 62.6 67.1 78 59.9 78.9 71.2 ...
#>   .. .. ..- attr(*, "label")= chr "Baseline Weight (kg)"
#>   .. ..$ EDUCLVL : num [1:254] 16 14 16 12 9 8 18 22 12 14 ...
#>   .. .. ..- attr(*, "label")= chr "Years of Education"
#>   .. ..$ DISONSDT: Date[1:254], format: "2010-04-30" "2006-03-11" ...
#>   .. ..$ DURDIS  : num [1:254] 43.9 76.4 42.8 55.3 32.9 ...
#>   .. .. ..- attr(*, "label")= chr "Duration of Disease (Months)"
#>   .. ..$ DURDSGR1: chr [1:254] ">=12" ">=12" ">=12" ">=12" ...
#>   .. .. ..- attr(*, "label")= chr "Pooled Disease Duration Group 1"
#>   .. ..$ VISIT1DT: Date[1:254], format: "2013-12-26" "2012-07-22" ...
#>   .. ..$ RFSTDTC : chr [1:254] "2014-01-02" "2012-08-05" "2013-07-19" "2014-03-18" ...
#>   .. .. ..- attr(*, "label")= chr "Subject Reference Start Date/Time"
#>   .. ..$ RFENDTC : chr [1:254] "2014-07-02" "2012-09-02" "2014-01-14" "2014-04-14" ...
#>   .. .. ..- attr(*, "label")= chr "Subject Reference End Date/Time"
#>   .. ..$ VISNUMEN: num [1:254] 12 5 12 5 12 6 12 4 8 12 ...
#>   .. .. ..- attr(*, "label")= chr "End of Trt Visit (Vis 12 or Early Term.)"
#>   .. ..$ RFENDT  : Date[1:254], format: "2014-07-02" "2012-09-02" ...
#>   .. ..$ DCDECOD : chr [1:254] "COMPLETED" "ADVERSE EVENT" "COMPLETED" "STUDY TERMINATED BY SPONSOR" ...
#>   .. .. ..- attr(*, "label")= chr "Standardized Disposition Term"
#>   .. ..$ DCREASCD: chr [1:254] "Completed" "Adverse Event" "Completed" "Sponsor Decision" ...
#>   .. .. ..- attr(*, "label")= chr "Reason for Discontinuation"
#>   .. ..$ MMSETOT : num [1:254] 23 23 23 23 21 23 10 23 20 20 ...
#>   .. .. ..- attr(*, "label")= chr "MMSE Total"
#>   .. ..$ TRTA    : Factor w/ 3 levels "Placebo","Low Dose",..: 1 1 3 2 3 1 2 2 2 1 ...
#>   .. ..- attr(*, "data_name")= chr "adsl"
#>   ..$ data_observation:'data.frame': 1191 obs. of  60 variables:
#>   .. ..$ STUDYID     : chr [1:1191] "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" ...
#>   .. .. ..- attr(*, "label")= chr "Study Identifier"
#>   .. ..$ SITEID      : chr [1:1191] "701" "701" "701" "701" ...
#>   .. .. ..- attr(*, "label")= chr "Study Site Identifier"
#>   .. ..$ USUBJID     : chr [1:1191] "01-701-1015" "01-701-1015" "01-701-1015" "01-701-1023" ...
#>   .. .. ..- attr(*, "label")= chr "Unique Subject Identifier"
#>   .. ..$ TRTA        : Factor w/ 3 levels "Placebo","Low Dose",..: 1 1 1 1 1 1 1 3 3 3 ...
#>   .. ..$ TRTAN       : num [1:1191] 0 0 0 0 0 0 0 81 81 81 ...
#>   .. .. ..- attr(*, "label")= chr "Actual Treatment (N)"
#>   .. ..$ AGE         : num [1:1191] 63 63 63 64 64 64 64 71 71 77 ...
#>   .. .. ..- attr(*, "label")= chr "Age"
#>   .. ..$ AGEGR1      : chr [1:1191] "<65" "<65" "<65" "<65" ...
#>   .. .. ..- attr(*, "label")= chr "Pooled Age Group 1"
#>   .. ..$ AGEGR1N     : num [1:1191] 1 1 1 1 1 1 1 2 2 2 ...
#>   .. .. ..- attr(*, "label")= chr "Pooled Age Group 1 (N)"
#>   .. ..$ RACE        : chr [1:1191] "WHITE" "WHITE" "WHITE" "WHITE" ...
#>   .. ..$ RACEN       : num [1:1191] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. .. ..- attr(*, "label")= chr "Race (N)"
#>   .. ..$ SEX         : chr [1:1191] "F" "F" "F" "M" ...
#>   .. .. ..- attr(*, "label")= chr "Sex"
#>   .. ..$ SAFFL       : chr [1:1191] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "Safety Population Flag"
#>   .. ..$ TRTSDT      : Date[1:1191], format: "2014-01-02" "2014-01-02" ...
#>   .. ..$ TRTEDT      : Date[1:1191], format: "2014-07-02" "2014-07-02" ...
#>   .. ..$ ASTDT       : Date[1:1191], format: "2014-01-03" "2014-01-03" ...
#>   .. ..$ ASTDTF      : chr [1:1191] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= chr "Analysis Start Date Imputation Flag"
#>   .. ..$ ASTDY       : num [1:1191] 2 2 8 3 3 22 3 3 21 58 ...
#>   .. .. ..- attr(*, "label")= chr "Analysis Start Relative Day"
#>   .. ..$ AENDT       : Date[1:1191], format: NA NA ...
#>   .. ..$ AENDY       : num [1:1191] NA NA 10 26 NA NA 26 NA NA NA ...
#>   .. .. ..- attr(*, "label")= chr "Analysis End Relative Day"
#>   .. ..$ ADURN       : num [1:1191] NA NA 3 24 NA NA 24 NA NA NA ...
#>   .. .. ..- attr(*, "label")= chr "AE Duration (N)"
#>   .. ..$ ADURU       : chr [1:1191] "" "" "DAY" "DAY" ...
#>   .. .. ..- attr(*, "label")= chr "AE Duration Units"
#>   .. ..$ AETERM      : chr [1:1191] "APPLICATION SITE ERYTHEMA" "APPLICATION SITE PRURITUS" "DIARRHOEA" "ERYTHEMA" ...
#>   .. .. ..- attr(*, "label")= chr "Reported Term for the Adverse Event"
#>   .. ..$ AELLT       : chr [1:1191] "APPLICATION SITE REDNESS" "APPLICATION SITE ITCHING" "DIARRHEA" "ERYTHEMA" ...
#>   .. .. ..- attr(*, "label")= chr "Lowest Level Term"
#>   .. ..$ AELLTCD     : num [1:1191] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= chr "Lowest Level Term Code"
#>   .. ..$ AEDECOD     : chr [1:1191] "APPLICATION SITE ERYTHEMA" "APPLICATION SITE PRURITUS" "DIARRHOEA" "ERYTHEMA" ...
#>   .. .. ..- attr(*, "label")= chr "Adverse Event"
#>   .. ..$ AEPTCD      : num [1:1191] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= chr "Preferred Term Code"
#>   .. ..$ AEHLT       : chr [1:1191] "HLT_0617" "HLT_0317" "HLT_0148" "HLT_0284" ...
#>   .. .. ..- attr(*, "label")= chr "High Level Term"
#>   .. ..$ AEHLTCD     : num [1:1191] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= chr "High Level Term Code"
#>   .. ..$ AEHLGT      : chr [1:1191] "HLGT_0152" "HLGT_0338" "HLGT_0588" "HLGT_0192" ...
#>   .. .. ..- attr(*, "label")= chr "High Level Group Term"
#>   .. ..$ AEHLGTCD    : num [1:1191] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= chr "High Level Group Term Code"
#>   .. ..$ AEBODSYS    : chr [1:1191] "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GASTROINTESTINAL DISORDERS" "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" ...
#>   .. .. ..- attr(*, "label")= chr "Body System or Organ Class"
#>   .. ..$ AESOC       : chr [1:1191] "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GASTROINTESTINAL DISORDERS" "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" ...
#>   .. .. ..- attr(*, "label")= chr "Primary System Organ Class"
#>   .. ..$ AESOCCD     : num [1:1191] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= chr "Primary System Organ Class Code"
#>   .. ..$ AESEV       : chr [1:1191] "MILD" "MILD" "MILD" "MILD" ...
#>   .. .. ..- attr(*, "label")= chr "Intensity"
#>   .. ..$ AESER       : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Serious"
#>   .. ..$ AESCAN      : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Involves Cancer"
#>   .. ..$ AESCONG     : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Congenital Anomaly or Birth Defect"
#>   .. ..$ AESDISAB    : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Persist or Signif Disability/Incapacity"
#>   .. ..$ AESDTH      : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Results in Death"
#>   .. ..$ AESHOSP     : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Requires or Prolongs Hospitalization"
#>   .. ..$ AESLIFE     : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Is Life Threatening"
#>   .. ..$ AESOD       : chr [1:1191] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= chr "Occurred with Overdose"
#>   .. ..$ AEREL       : chr [1:1191] "PROBABLE" "PROBABLE" "REMOTE" "POSSIBLE" ...
#>   .. .. ..- attr(*, "label")= chr "Causality"
#>   .. ..$ AEACN       : chr [1:1191] "DOSE NOT CHANGED" "DOSE NOT CHANGED" "NOT APPLICABLE" "DOSE NOT CHANGED" ...
#>   .. ..$ AEOUT       : chr [1:1191] "NOT RECOVERED/NOT RESOLVED" "NOT RECOVERED/NOT RESOLVED" "RECOVERED/RESOLVED" "NOT RECOVERED/NOT RESOLVED" ...
#>   .. .. ..- attr(*, "label")= chr "Outcome of Adverse Event"
#>   .. ..$ AESEQ       : num [1:1191] 1 2 3 1 2 3 4 1 2 1 ...
#>   .. .. ..- attr(*, "label")= chr "Sequence Number"
#>   .. ..$ TRTEMFL     : chr [1:1191] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "Treatment Emergent Analysis Flag"
#>   .. ..$ AOCCFL      : chr [1:1191] "Y" "" "" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "1st Occurrence of Any AE Flag"
#>   .. ..$ AOCCSFL     : chr [1:1191] "Y" "" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "1st Occurrence of SOC Flag"
#>   .. ..$ AOCCPFL     : chr [1:1191] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "1st Occurrence of Preferred Term Flag"
#>   .. ..$ AOCC02FL    : chr [1:1191] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= chr "1st Occurrence 02 Flag for Serious"
#>   .. ..$ AOCC03FL    : chr [1:1191] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= chr "1st Occurrence 03 Flag for Serious SOC"
#>   .. ..$ AOCC04FL    : chr [1:1191] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= chr "1st Occurrence 04 Flag for Serious PT"
#>   .. ..$ CQ01NAM     : chr [1:1191] "DERMATOLOGIC EVENTS" "DERMATOLOGIC EVENTS" "" "DERMATOLOGIC EVENTS" ...
#>   .. .. ..- attr(*, "label")= chr "Customized Query 01 Name"
#>   .. ..$ AOCC01FL    : chr [1:1191] "Y" "" "" "Y" ...
#>   .. .. ..- attr(*, "label")= chr "1st Occurrence 01 Flag for CQ01"
#>   .. ..$ related     : chr [1:1191] "Probable" "Probable" "Remote" "Possible" ...
#>   .. .. ..- attr(*, "label")= chr "Related"
#>   .. ..$ outcome     : chr [1:1191] "Resolved" "Resolved" "Resolved" "Resolved" ...
#>   .. .. ..- attr(*, "label")= chr "Outcome"
#>   .. ..$ action_taken: chr [1:1191] "None" "None" "N/A" "None" ...
#>   .. .. ..- attr(*, "label")= chr "Action Taken"
#>   .. ..$ duration    : chr [1:1191] "Continuing" "Continuing" "3 Day" "24 Day" ...
#>   .. .. ..- attr(*, "label")= chr "Duration"
#>   .. ..$ subline     : chr [1:1191] "Subject ID = 01-701-1015, Gender = F, Race = WHITE, AGE = 63 Years, TRT = Placebo" "Subject ID = 01-701-1015, Gender = F, Race = WHITE, AGE = 63 Years, TRT = Placebo" "Subject ID = 01-701-1015, Gender = F, Race = WHITE, AGE = 63 Years, TRT = Placebo" "Subject ID = 01-701-1023, Gender = M, Race = WHITE, AGE = 64 Years, TRT = Placebo" ...
#>   .. ..- attr(*, "data_name")= chr "adae"
#>   ..$ plan            :Classes ‘meta_plan’ and 'data.frame': 20 obs. of  5 variables:
#>   .. ..$ mock       : num [1:20] 1 1 2 2 2 2 2 2 2 2 ...
#>   .. ..$ analysis   : chr [1:20] "ae_summary" "ae_summary" "ae_specific" "ae_specific" ...
#>   .. ..$ population : chr [1:20] "apat" "apat" "apat" "apat" ...
#>   .. ..$ observation: chr [1:20] "wk12" "wk24" "wk12" "wk24" ...
#>   .. ..$ parameter  : chr [1:20] "any;rel;ser" "any;rel;ser" "any" "any" ...
#>   .. ..- attr(*, "out.attrs")=List of 2
#>   .. .. ..$ dim     : Named int [1:5] 1 1 1 2 1
#>   .. .. .. ..- attr(*, "names")= chr [1:5] "mock" "analysis" "population" "observation" ...
#>   .. .. ..$ dimnames:List of 5
#>   .. .. .. ..$ mock       : chr "mock=1"
#>   .. .. .. ..$ analysis   : chr "analysis=ae_summary"
#>   .. .. .. ..$ population : chr "population=apat"
#>   .. .. .. ..$ observation: chr [1:2] "observation=wk12" "observation=wk24"
#>   .. .. .. ..$ parameter  : chr "parameter=any;rel;ser"
#>   ..$ observation     :List of 2
#>   .. ..$ wk12:List of 6
#>   .. .. ..$ name  : chr "wk12"
#>   .. .. ..$ id    : chr "USUBJID"
#>   .. .. ..$ group : chr "TRTA"
#>   .. .. ..$ var   : NULL
#>   .. .. ..$ subset: language SAFFL == "Y"
#>   .. .. ..$ label : chr "Weeks 0 to 12"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ wk24:List of 6
#>   .. .. ..$ name  : chr "wk24"
#>   .. .. ..$ id    : chr "USUBJID"
#>   .. .. ..$ group : chr "TRTA"
#>   .. .. ..$ var   : NULL
#>   .. .. ..$ subset: language AOCC01FL == "Y"
#>   .. .. ..$ label : chr "Weeks 0 to 24"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..$ population      :List of 1
#>   .. ..$ apat:List of 6
#>   .. .. ..$ name  : chr "apat"
#>   .. .. ..$ id    : chr "USUBJID"
#>   .. .. ..$ group : chr "TRTA"
#>   .. .. ..$ var   : NULL
#>   .. .. ..$ subset: language SAFFL == "Y"
#>   .. .. ..$ label : chr "All Participants as Treated"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..$ parameter       :List of 5
#>   .. ..$ rel    :List of 12
#>   .. .. ..$ name     : chr "rel"
#>   .. .. ..$ id       : NULL
#>   .. .. ..$ group    : NULL
#>   .. .. ..$ var      : chr "AEDECOD"
#>   .. .. ..$ subset   : language AEREL %in% c("POSSIBLE", "PROBABLE")
#>   .. .. ..$ label    : chr "drug-related adverse events"
#>   .. .. ..$ soc      : chr "AEBODSYS"
#>   .. .. ..$ seq      : num 200
#>   .. .. ..$ term1    : chr "Drug-Related"
#>   .. .. ..$ term2    : chr ""
#>   .. .. ..$ summ_row : chr "with drug-related{^a} adverse events"
#>   .. .. ..$ summ_foot: chr "{^a}Determined by the investigator to be related to the drug."
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ aeosi  :List of 9
#>   .. .. ..$ name  : chr "aeosi"
#>   .. .. ..$ id    : NULL
#>   .. .. ..$ group : NULL
#>   .. .. ..$ var   : chr "AEDECOD"
#>   .. .. ..$ subset: language AEOSI == "Y"
#>   .. .. ..$ label : chr "adverse events of special interest"
#>   .. .. ..$ soc   : chr "AEBODSYS"
#>   .. .. ..$ term1 : chr ""
#>   .. .. ..$ term2 : chr "of special interest"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ dtc0rel:List of 11
#>   .. .. ..$ name    : chr "dtc0rel"
#>   .. .. ..$ id      : NULL
#>   .. .. ..$ group   : NULL
#>   .. .. ..$ var     : chr "AEDECOD"
#>   .. .. ..$ subset  : language AESDTH == "Y" & AEREL == "Y"
#>   .. .. ..$ label   : chr "drug-related adverse events result in death"
#>   .. .. ..$ soc     : chr "AEBODSYS"
#>   .. .. ..$ term1   : chr "Drug-Related"
#>   .. .. ..$ term2   : chr "Resulting in Death"
#>   .. .. ..$ seq     : num 601
#>   .. .. ..$ summ_row: chr "who died due to a drug-related adverse event"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ any    :List of 11
#>   .. .. ..$ name    : chr "any"
#>   .. .. ..$ id      : NULL
#>   .. .. ..$ group   : NULL
#>   .. .. ..$ var     : chr "AEDECOD"
#>   .. .. ..$ subset  : NULL
#>   .. .. ..$ label   : chr "any adverse events"
#>   .. .. ..$ soc     : chr "AEBODSYS"
#>   .. .. ..$ seq     : num 100
#>   .. .. ..$ term1   : chr ""
#>   .. .. ..$ term2   : chr ""
#>   .. .. ..$ summ_row: chr "with one or more adverse events"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ ser    :List of 11
#>   .. .. ..$ name    : chr "ser"
#>   .. .. ..$ id      : NULL
#>   .. .. ..$ group   : NULL
#>   .. .. ..$ var     : chr "AEDECOD"
#>   .. .. ..$ subset  : language AESER == "Y"
#>   .. .. ..$ label   : chr "serious adverse events"
#>   .. .. ..$ soc     : chr "AEBODSYS"
#>   .. .. ..$ seq     : num 401
#>   .. .. ..$ term1   : chr "Serious"
#>   .. .. ..$ term2   : chr ""
#>   .. .. ..$ summ_row: chr "with serious adverse events"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..$ analysis        :List of 4
#>   .. ..$ ae_summary :List of 7
#>   .. .. ..$ name  : chr "ae_summary"
#>   .. .. ..$ id    : NULL
#>   .. .. ..$ group : NULL
#>   .. .. ..$ var   : NULL
#>   .. .. ..$ subset: NULL
#>   .. .. ..$ label : chr "Table: adverse event summary"
#>   .. .. ..$ title : chr "Summary of Adverse Events"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ ae_listing :List of 11
#>   .. .. ..$ name      : chr "ae_listing"
#>   .. .. ..$ id        : NULL
#>   .. .. ..$ group     : NULL
#>   .. .. ..$ var       : NULL
#>   .. .. ..$ subset    : NULL
#>   .. .. ..$ label     : chr "Listing: adverse event"
#>   .. .. ..$ var_name  : chr [1:9] "USUBJID" "ASTDY" "AEDECOD" "duration" ...
#>   .. .. ..$ subline_by: NULL
#>   .. .. ..$ group_by  : chr [1:2] "USUBJID" "ASTDY"
#>   .. .. ..$ page_by   : chr [1:2] "TRTA" "subline"
#>   .. .. ..$ title     : chr "Listing of Participants With {term1} Adverse Events {term2}"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ ae_exp_adj :List of 7
#>   .. .. ..$ name  : chr "ae_exp_adj"
#>   .. .. ..$ id    : NULL
#>   .. .. ..$ group : NULL
#>   .. .. ..$ var   : NULL
#>   .. .. ..$ subset: NULL
#>   .. .. ..$ label : chr "Exposure Adjusted Incident Rate"
#>   .. .. ..$ title : chr "Exposure-Adjusted Adverse Event Summary"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   .. ..$ ae_specific:List of 7
#>   .. .. ..$ name  : chr "ae_specific"
#>   .. .. ..$ id    : NULL
#>   .. .. ..$ group : NULL
#>   .. .. ..$ var   : NULL
#>   .. .. ..$ subset: NULL
#>   .. .. ..$ label : chr "Table: specific adverse event"
#>   .. .. ..$ title : chr "Participants With {term1} Adverse Events {term2}"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..- attr(*, "class")= chr "meta_adam"
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "rel"
#>  $ n              :'data.frame': 138 obs. of  4 variables:
#>   ..$ n_1: int [1:138] 86 44 42 NA 6 1 0 1 1 1 ...
#>   ..$ n_2: int [1:138] 84 73 11 NA 7 0 1 0 0 0 ...
#>   ..$ n_3: int [1:138] 84 70 14 NA 4 2 0 0 0 0 ...
#>   ..$ n_4: int [1:138] 254 187 67 NA 17 3 1 1 1 1 ...
#>  $ order          : num [1:138] 1 100 200 900 10000 ...
#>  $ group          : chr [1:4] "Placebo" "Low Dose" "High Dose" "Total"
#>  $ reference_group: num 1
#>  $ prop           :'data.frame': 138 obs. of  4 variables:
#>   ..$ prop_1: num [1:138] NA 51.16 48.84 NA 6.98 ...
#>   ..$ prop_2: num [1:138] NA 86.9 13.1 NA 8.33 ...
#>   ..$ prop_3: num [1:138] NA 83.33 16.67 NA 4.76 ...
#>   ..$ prop_4: num [1:138] NA 73.62 26.38 NA 6.69 ...
#>  $ diff           :'data.frame': 138 obs. of  2 variables:
#>   ..$ diff_2: num [1:138] NA 35.74 -35.74 NA 1.36 ...
#>   ..$ diff_3: num [1:138] NA 32.17 -32.17 NA -2.21 ...
#>  $ n_pop          :'data.frame': 1 obs. of  4 variables:
#>   ..$ n_1: int 86
#>   ..$ n_2: int 84
#>   ..$ n_3: int 84
#>   ..$ n_4: int 254
#>  $ name           : chr [1:138] "Participants in population" "with one or more drug-related adverse events" "with no drug-related adverse events" "" ...
#>  $ soc_name       : chr [1:138] NA NA NA NA ...
#>  $ components     : chr [1:2] "soc" "par"
#>  $ prepare_call   : language prepare_ae_specific(meta = meta, population = "apat", observation = "wk12",      parameter = "rel")
#>  - attr(*, "class")= chr "outdata"

# Allow to extract each components
prepare_ae_specific(meta, "apat", "wk12", "rel", components = NULL)$data
#> NULL
prepare_ae_specific(meta, "apat", "wk12", "rel", components = "soc")$data
#> NULL
prepare_ae_specific(meta, "apat", "wk12", "rel", components = "par")$data
#> NULL
```
