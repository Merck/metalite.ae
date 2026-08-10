# Prepare datasets for AE listing

Prepare datasets for AE listing

## Usage

``` r
prepare_ae_listing(meta, analysis, population, observation, parameter)
```

## Arguments

- meta:

  A metadata object created by metalite.

- analysis:

  Analysis name from `meta`.

- population:

  A character value of population term name. The term name is used as
  key to link information.

- observation:

  A character value of observation term name. The term name is used as
  key to link information.

- parameter:

  A character value of parameter term name. The term name is used as key
  to link information.

## Value

A list of analysis datasets needed for AE listing.

## Examples

``` r
# Define metadata
adsl <- forestly::forestly_adsl
adae <- forestly::forestly_adae

adsl$TRT01A <- factor(
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
  analysis = "ae_listing", population = "apat",
  observation = "wk12", parameter = "ser"
)

meta <- metalite::meta_adam(observation = adae, population = adsl) |>
  metalite::define_plan(analysis_plan) |>
  metalite::define_population(
    name = "apat",
    var = c("USUBJID", "SAFFL", "TRT01A", "SITEID", "SEX", "RACE", "AGE"),
    group = "TRT01A",
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
    name = "ser",
    term1 = "Serious",
    term2 = "",
    subset = AESER == "Y",
    var = "AEDECOD",
    soc = "AEBODSYS",
    label = "Serious AEs"
  ) |>
  metalite::define_analysis(
    name = "ae_listing",
    var_name = c("USUBJID", "ASTDY", "AEDECOD", "ADURN", "AESEV", "AESER", "AEREL", "AEOUT"),
    group_by = c("USUBJID", "ASTDY"),
    page_by = "TRTA"
  ) |>
  metalite::meta_build()

str(prepare_ae_listing(meta, "ae_listing", "apat", "wk12", "ser"))
#> List of 11
#>  $ meta           :List of 7
#>   ..$ data_population :'data.frame': 170 obs. of  49 variables:
#>   .. ..$ STUDYID : chr [1:170] "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" ...
#>   .. .. ..- attr(*, "label")= Named chr "Study Identifier"
#>   .. .. .. ..- attr(*, "names")= chr "STUDYID"
#>   .. ..$ USUBJID : chr [1:170] "01-701-1015" "01-701-1023" "01-701-1033" "01-701-1047" ...
#>   .. .. ..- attr(*, "label")= Named chr "Unique Subject Identifier"
#>   .. .. .. ..- attr(*, "names")= chr "USUBJID"
#>   .. ..$ SUBJID  : chr [1:170] "1015" "1023" "1033" "1047" ...
#>   .. .. ..- attr(*, "label")= Named chr "Subject Identifier for the Study"
#>   .. .. .. ..- attr(*, "names")= chr "SUBJID"
#>   .. ..$ SITEID  : chr [1:170] "701" "701" "701" "701" ...
#>   .. .. ..- attr(*, "label")= Named chr "Study Site Identifier"
#>   .. .. .. ..- attr(*, "names")= chr "SITEID"
#>   .. ..$ SITEGR1 : chr [1:170] "701" "701" "701" "701" ...
#>   .. .. ..- attr(*, "label")= Named chr "Pooled Site Group 1"
#>   .. .. .. ..- attr(*, "names")= chr "SITEGR1"
#>   .. ..$ ARM     : chr [1:170] "Placebo" "Placebo" "Xanomeline Low Dose" "Placebo" ...
#>   .. .. ..- attr(*, "label")= Named chr "Description of Planned Arm"
#>   .. .. .. ..- attr(*, "names")= chr "ARM"
#>   .. ..$ TRT01P  : chr [1:170] "Placebo" "Placebo" "Xanomeline Low Dose" "Placebo" ...
#>   .. .. ..- attr(*, "label")= Named chr "Planned Treatment for Period 01"
#>   .. .. .. ..- attr(*, "names")= chr "TRT01P"
#>   .. ..$ TRT01PN : num [1:170] 0 0 54 0 54 54 54 0 0 0 ...
#>   .. .. ..- attr(*, "label")= Named chr "Planned Treatment for Period 01 (N)"
#>   .. .. .. ..- attr(*, "names")= chr "TRT01PN"
#>   .. ..$ TRT01A  : Factor w/ 2 levels "Low Dose","Placebo": 2 2 1 2 1 1 1 2 2 2 ...
#>   .. ..$ TRT01AN : num [1:170] 0 0 54 0 54 54 54 0 0 0 ...
#>   .. .. ..- attr(*, "label")= Named chr "Actual Treatment for Period 01 (N)"
#>   .. .. .. ..- attr(*, "names")= chr "TRT01AN"
#>   .. ..$ TRTSDT  : Date[1:170], format: "2014-01-02" "2012-08-05" ...
#>   .. ..$ TRTEDT  : Date[1:170], format: "2014-07-02" "2012-09-01" ...
#>   .. ..$ TRTDUR  : num [1:170] 182 28 14 26 190 10 55 182 183 175 ...
#>   .. .. ..- attr(*, "label")= Named chr "Duration of Treatment (days)"
#>   .. .. .. ..- attr(*, "names")= chr "TRTDUR"
#>   .. ..$ AVGDD   : num [1:170] 0 0 54 0 54 54 54 0 0 0 ...
#>   .. .. ..- attr(*, "label")= Named chr "Avg Daily Dose (as planned)"
#>   .. .. .. ..- attr(*, "names")= chr "AVGDD"
#>   .. ..$ CUMDOSE : num [1:170] 0 0 756 0 10260 ...
#>   .. .. ..- attr(*, "label")= Named chr "Cumulative Dose (as planned)"
#>   .. .. .. ..- attr(*, "names")= chr "CUMDOSE"
#>   .. ..$ AGE     : num [1:170] 63 64 74 85 68 81 84 52 84 79 ...
#>   .. .. ..- attr(*, "label")= Named chr "Age"
#>   .. .. .. ..- attr(*, "names")= chr "AGE"
#>   .. ..$ AGEGR1  : chr [1:170] "<65" "<65" "65-80" ">80" ...
#>   .. .. ..- attr(*, "label")= Named chr "Pooled Age Group 1"
#>   .. .. .. ..- attr(*, "names")= chr "AGEGR1"
#>   .. ..$ AGEGR1N : num [1:170] 1 1 2 3 2 3 3 1 3 2 ...
#>   .. .. ..- attr(*, "label")= Named chr "Pooled Age Group 1 (N)"
#>   .. .. .. ..- attr(*, "names")= chr "AGEGR1N"
#>   .. ..$ AGEU    : chr [1:170] "YEARS" "YEARS" "YEARS" "YEARS" ...
#>   .. .. ..- attr(*, "label")= Named chr "Age Units"
#>   .. .. .. ..- attr(*, "names")= chr "AGEU"
#>   .. ..$ RACE    : chr [1:170] "WHITE" "WHITE" "WHITE" "WHITE" ...
#>   .. .. ..- attr(*, "label")= Named chr "Race"
#>   .. .. .. ..- attr(*, "names")= chr "RACE"
#>   .. ..$ RACEN   : num [1:170] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. .. ..- attr(*, "label")= Named chr "Race (N)"
#>   .. .. .. ..- attr(*, "names")= chr "RACEN"
#>   .. ..$ SEX     : chr [1:170] "F" "M" "M" "F" ...
#>   .. .. ..- attr(*, "label")= Named chr "Sex"
#>   .. .. .. ..- attr(*, "names")= chr "SEX"
#>   .. ..$ ETHNIC  : chr [1:170] "HISPANIC OR LATINO" "HISPANIC OR LATINO" "NOT HISPANIC OR LATINO" "NOT HISPANIC OR LATINO" ...
#>   .. .. ..- attr(*, "label")= Named chr "Ethnicity"
#>   .. .. .. ..- attr(*, "names")= chr "ETHNIC"
#>   .. ..$ SAFFL   : chr [1:170] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "Safety Population Flag"
#>   .. .. .. ..- attr(*, "names")= chr "SAFFL"
#>   .. ..$ ITTFL   : chr [1:170] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "Intent-To-Treat Population Flag"
#>   .. .. .. ..- attr(*, "names")= chr "ITTFL"
#>   .. ..$ EFFFL   : chr [1:170] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "Efficacy Population Flag"
#>   .. .. .. ..- attr(*, "names")= chr "EFFFL"
#>   .. ..$ COMP8FL : chr [1:170] "Y" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Completers of Week 8 Population Flag"
#>   .. .. .. ..- attr(*, "names")= chr "COMP8FL"
#>   .. ..$ COMP16FL: chr [1:170] "Y" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Completers of Week 16 Population Flag"
#>   .. .. .. ..- attr(*, "names")= chr "COMP16FL"
#>   .. ..$ COMP24FL: chr [1:170] "Y" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Completers of Week 24 Population Flag"
#>   .. .. .. ..- attr(*, "names")= chr "COMP24FL"
#>   .. ..$ DISCONFL: chr [1:170] "" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "Did the Subject Discontinue the Study?"
#>   .. .. .. ..- attr(*, "names")= chr "DISCONFL"
#>   .. ..$ DSRAEFL : chr [1:170] "" "Y" "" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "Discontinued due to AE?"
#>   .. .. .. ..- attr(*, "names")= chr "DSRAEFL"
#>   .. ..$ DTHFL   : chr [1:170] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= Named chr "Subject Died?"
#>   .. .. .. ..- attr(*, "names")= chr "DTHFL"
#>   .. ..$ BMIBL   : num [1:170] 25.1 30.4 28.8 30.4 27.3 23.9 23.9 21.9 27.6 23.8 ...
#>   .. .. ..- attr(*, "label")= Named chr "Baseline BMI (kg/m^2)"
#>   .. .. .. ..- attr(*, "names")= chr "BMIBL"
#>   .. ..$ BMIBLGR1: chr [1:170] "25-<30" ">=30" "25-<30" ">=30" ...
#>   .. .. ..- attr(*, "label")= Named chr "Pooled Baseline BMI Group 1"
#>   .. .. .. ..- attr(*, "names")= chr "BMIBLGR1"
#>   .. ..$ HEIGHTBL: num [1:170] 147 163 175 149 169 ...
#>   .. .. ..- attr(*, "label")= Named chr "Baseline Height (cm)"
#>   .. .. .. ..- attr(*, "names")= chr "HEIGHTBL"
#>   .. ..$ WEIGHTBL: num [1:170] 54.4 80.3 88.5 67.1 78 59.9 78.9 71.2 79.4 58.1 ...
#>   .. .. ..- attr(*, "label")= Named chr "Baseline Weight (kg)"
#>   .. .. .. ..- attr(*, "names")= chr "WEIGHTBL"
#>   .. ..$ EDUCLVL : num [1:170] 16 14 12 8 18 22 12 14 12 6 ...
#>   .. .. ..- attr(*, "label")= Named chr "Years of Education"
#>   .. .. .. ..- attr(*, "names")= chr "EDUCLVL"
#>   .. ..$ DISONSDT: Date[1:170], format: "2010-04-30" "2006-03-11" ...
#>   .. ..$ DURDIS  : num [1:170] 43.9 76.4 55.3 42 99.1 ...
#>   .. .. ..- attr(*, "label")= Named chr "Duration of Disease (Months)"
#>   .. .. .. ..- attr(*, "names")= chr "DURDIS"
#>   .. ..$ DURDSGR1: chr [1:170] ">=12" ">=12" ">=12" ">=12" ...
#>   .. .. ..- attr(*, "label")= Named chr "Pooled Disease Duration Group 1"
#>   .. .. .. ..- attr(*, "names")= chr "DURDSGR1"
#>   .. ..$ VISIT1DT: Date[1:170], format: "2013-12-26" "2012-07-22" ...
#>   .. ..$ RFSTDTC : chr [1:170] "2014-01-02" "2012-08-05" "2014-03-18" "2013-02-12" ...
#>   .. .. ..- attr(*, "label")= Named chr "Subject Reference Start Date/Time"
#>   .. .. .. ..- attr(*, "names")= chr "RFSTDTC"
#>   .. ..$ RFENDTC : chr [1:170] "2014-07-02" "2012-09-02" "2014-04-14" "2013-03-29" ...
#>   .. .. ..- attr(*, "label")= Named chr "Subject Reference End Date/Time"
#>   .. .. .. ..- attr(*, "names")= chr "RFENDTC"
#>   .. ..$ VISNUMEN: num [1:170] 12 5 5 6 12 4 8 12 12 12 ...
#>   .. .. ..- attr(*, "label")= Named chr "End of Trt Visit (Vis 12 or Early Term.)"
#>   .. .. .. ..- attr(*, "names")= chr "VISNUMEN"
#>   .. ..$ RFENDT  : Date[1:170], format: "2014-07-02" "2012-09-02" ...
#>   .. ..$ DCDECOD : chr [1:170] "COMPLETED" "ADVERSE EVENT" "STUDY TERMINATED BY SPONSOR" "ADVERSE EVENT" ...
#>   .. .. ..- attr(*, "label")= Named chr "Standardized Disposition Term"
#>   .. .. .. ..- attr(*, "names")= chr "DCDECOD"
#>   .. ..$ DCREASCD: chr [1:170] "Completed" "Adverse Event" "Sponsor Decision" "Adverse Event" ...
#>   .. .. ..- attr(*, "label")= Named chr "Reason for Discontinuation"
#>   .. .. .. ..- attr(*, "names")= chr "DCREASCD"
#>   .. ..$ MMSETOT : num [1:170] 23 23 23 23 10 23 20 20 19 10 ...
#>   .. .. ..- attr(*, "label")= Named chr "MMSE Total"
#>   .. .. .. ..- attr(*, "names")= chr "MMSETOT"
#>   .. ..$ TRTA    : Factor w/ 2 levels "Placebo","Xanomeline Low Dose": 1 1 2 1 2 2 2 1 1 1 ...
#>   .. .. ..- attr(*, "label")= chr "Actual Treatment"
#>   .. ..- attr(*, "data_name")= chr "adsl"
#>   ..$ data_observation:'data.frame': 736 obs. of  57 variables:
#>   .. ..$ STUDYID : chr [1:736] "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" "CDISCPILOT01" ...
#>   .. .. ..- attr(*, "label")= Named chr "Study Identifier"
#>   .. .. .. ..- attr(*, "names")= chr "STUDYID"
#>   .. ..$ SITEID  : chr [1:736] "701" "701" "701" "701" ...
#>   .. .. ..- attr(*, "label")= Named chr "Study Site Identifier"
#>   .. .. .. ..- attr(*, "names")= chr "SITEID"
#>   .. ..$ USUBJID : chr [1:736] "01-701-1015" "01-701-1015" "01-701-1015" "01-701-1023" ...
#>   .. .. ..- attr(*, "label")= Named chr "Unique Subject Identifier"
#>   .. .. .. ..- attr(*, "names")= chr "USUBJID"
#>   .. ..$ TRTA    : Factor w/ 2 levels "Low Dose","Placebo": 2 2 2 2 2 2 2 2 2 2 ...
#>   .. ..$ TRTAN   : num [1:736] 0 0 0 0 0 0 0 0 0 0 ...
#>   .. .. ..- attr(*, "label")= Named chr "Actual Treatment (N)"
#>   .. .. .. ..- attr(*, "names")= chr "TRTAN"
#>   .. ..$ AGE     : num [1:736] 63 63 63 64 64 64 64 85 85 85 ...
#>   .. .. ..- attr(*, "label")= Named chr "Age"
#>   .. .. .. ..- attr(*, "names")= chr "AGE"
#>   .. ..$ AGEGR1  : chr [1:736] "<65" "<65" "<65" "<65" ...
#>   .. .. ..- attr(*, "label")= Named chr "Pooled Age Group 1"
#>   .. .. .. ..- attr(*, "names")= chr "AGEGR1"
#>   .. ..$ AGEGR1N : num [1:736] 1 1 1 1 1 1 1 3 3 3 ...
#>   .. .. ..- attr(*, "label")= Named chr "Pooled Age Group 1 (N)"
#>   .. .. .. ..- attr(*, "names")= chr "AGEGR1N"
#>   .. ..$ RACE    : chr [1:736] "WHITE" "WHITE" "WHITE" "WHITE" ...
#>   .. .. ..- attr(*, "label")= Named chr "Race"
#>   .. .. .. ..- attr(*, "names")= chr "RACE"
#>   .. ..$ RACEN   : num [1:736] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. .. ..- attr(*, "label")= Named chr "Race (N)"
#>   .. .. .. ..- attr(*, "names")= chr "RACEN"
#>   .. ..$ SEX     : chr [1:736] "F" "F" "F" "M" ...
#>   .. .. ..- attr(*, "label")= Named chr "Sex"
#>   .. .. .. ..- attr(*, "names")= chr "SEX"
#>   .. ..$ SAFFL   : chr [1:736] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "Safety Population Flag"
#>   .. .. .. ..- attr(*, "names")= chr "SAFFL"
#>   .. ..$ TRTSDT  : Date[1:736], format: "2014-01-02" "2014-01-02" ...
#>   .. ..$ TRTEDT  : Date[1:736], format: "2014-07-02" "2014-07-02" ...
#>   .. ..$ ASTDT   : Date[1:736], format: "2014-01-03" "2014-01-03" ...
#>   .. ..$ ASTDTF  : chr [1:736] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= Named chr "Analysis Start Date Imputation Flag"
#>   .. .. .. ..- attr(*, "names")= chr "ASTDTF"
#>   .. ..$ ASTDY   : num [1:736] 2 2 8 3 3 22 3 1 1 23 ...
#>   .. .. ..- attr(*, "label")= Named chr "Analysis Start Relative Day"
#>   .. .. .. ..- attr(*, "names")= chr "ASTDY"
#>   .. ..$ AENDT   : Date[1:736], format: NA NA ...
#>   .. ..$ AENDY   : num [1:736] NA NA 10 26 NA NA 26 1 1 NA ...
#>   .. .. ..- attr(*, "label")= Named chr "Analysis End Relative Day"
#>   .. .. .. ..- attr(*, "names")= chr "AENDY"
#>   .. ..$ ADURN   : num [1:736] NA NA 3 24 NA NA 24 1 1 NA ...
#>   .. .. ..- attr(*, "label")= Named chr "AE Duration (N)"
#>   .. .. .. ..- attr(*, "names")= chr "ADURN"
#>   .. ..$ ADURU   : chr [1:736] "" "" "DAY" "DAY" ...
#>   .. .. ..- attr(*, "label")= Named chr "AE Duration Units"
#>   .. .. .. ..- attr(*, "names")= chr "ADURU"
#>   .. ..$ AETERM  : chr [1:736] "APPLICATION SITE ERYTHEMA" "APPLICATION SITE PRURITUS" "DIARRHOEA" "ERYTHEMA" ...
#>   .. .. ..- attr(*, "label")= Named chr "Reported Term for the Adverse Event"
#>   .. .. .. ..- attr(*, "names")= chr "AETERM"
#>   .. ..$ AELLT   : chr [1:736] "APPLICATION SITE REDNESS" "APPLICATION SITE ITCHING" "DIARRHEA" "ERYTHEMA" ...
#>   .. .. ..- attr(*, "label")= Named chr "Lowest Level Term"
#>   .. .. .. ..- attr(*, "names")= chr "AELLT"
#>   .. ..$ AELLTCD : num [1:736] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= Named chr "Lowest Level Term Code"
#>   .. .. .. ..- attr(*, "names")= chr "AELLTCD"
#>   .. ..$ AEDECOD : chr [1:736] "APPLICATION SITE ERYTHEMA" "APPLICATION SITE PRURITUS" "DIARRHOEA" "ERYTHEMA" ...
#>   .. .. ..- attr(*, "label")= Named chr "Dictionary-Derived Term"
#>   .. .. .. ..- attr(*, "names")= chr "AEDECOD"
#>   .. ..$ AEPTCD  : num [1:736] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= Named chr "Preferred Term Code"
#>   .. .. .. ..- attr(*, "names")= chr "AEPTCD"
#>   .. ..$ AEHLT   : chr [1:736] "HLT_0617" "HLT_0317" "HLT_0148" "HLT_0284" ...
#>   .. .. ..- attr(*, "label")= Named chr "High Level Term"
#>   .. .. .. ..- attr(*, "names")= chr "AEHLT"
#>   .. ..$ AEHLTCD : num [1:736] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= Named chr "High Level Term Code"
#>   .. .. .. ..- attr(*, "names")= chr "AEHLTCD"
#>   .. ..$ AEHLGT  : chr [1:736] "HLGT_0152" "HLGT_0338" "HLGT_0588" "HLGT_0192" ...
#>   .. .. ..- attr(*, "label")= Named chr "High Level Group Term"
#>   .. .. .. ..- attr(*, "names")= chr "AEHLGT"
#>   .. ..$ AEHLGTCD: num [1:736] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= Named chr "High Level Group Term Code"
#>   .. .. .. ..- attr(*, "names")= chr "AEHLGTCD"
#>   .. ..$ AEBODSYS: chr [1:736] "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GASTROINTESTINAL DISORDERS" "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" ...
#>   .. .. ..- attr(*, "label")= Named chr "Body System or Organ Class"
#>   .. .. .. ..- attr(*, "names")= chr "AEBODSYS"
#>   .. ..$ AESOC   : chr [1:736] "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS" "GASTROINTESTINAL DISORDERS" "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" ...
#>   .. .. ..- attr(*, "label")= Named chr "Primary System Organ Class"
#>   .. .. .. ..- attr(*, "names")= chr "AESOC"
#>   .. ..$ AESOCCD : num [1:736] NA NA NA NA NA NA NA NA NA NA ...
#>   .. .. ..- attr(*, "label")= Named chr "Primary System Organ Class Code"
#>   .. .. .. ..- attr(*, "names")= chr "AESOCCD"
#>   .. ..$ AESEV   : chr [1:736] "MILD" "MILD" "MILD" "MILD" ...
#>   .. .. ..- attr(*, "label")= Named chr "Severity/Intensity"
#>   .. .. .. ..- attr(*, "names")= chr "AESEV"
#>   .. ..$ AESER   : chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Serious Event"
#>   .. .. .. ..- attr(*, "names")= chr "AESER"
#>   .. ..$ AESCAN  : chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Involves Cancer"
#>   .. .. .. ..- attr(*, "names")= chr "AESCAN"
#>   .. ..$ AESCONG : chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Congenital Anomaly or Birth Defect"
#>   .. .. .. ..- attr(*, "names")= chr "AESCONG"
#>   .. ..$ AESDISAB: chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Persist or Signif Disability/Incapacity"
#>   .. .. .. ..- attr(*, "names")= chr "AESDISAB"
#>   .. ..$ AESDTH  : chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Results in Death"
#>   .. .. .. ..- attr(*, "names")= chr "AESDTH"
#>   .. ..$ AESHOSP : chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Requires or Prolongs Hospitalization"
#>   .. .. .. ..- attr(*, "names")= chr "AESHOSP"
#>   .. ..$ AESLIFE : chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Is Life Threatening"
#>   .. .. .. ..- attr(*, "names")= chr "AESLIFE"
#>   .. ..$ AESOD   : chr [1:736] "N" "N" "N" "N" ...
#>   .. .. ..- attr(*, "label")= Named chr "Occurred with Overdose"
#>   .. .. .. ..- attr(*, "names")= chr "AESOD"
#>   .. ..$ AEREL   : chr [1:736] "PROBABLE" "PROBABLE" "REMOTE" "POSSIBLE" ...
#>   .. .. ..- attr(*, "label")= Named chr "Causality"
#>   .. .. .. ..- attr(*, "names")= chr "AEREL"
#>   .. ..$ AEACN   : chr [1:736] "DRUG INTERRUPTED" "DRUG INTERRUPTED" "DOSE REDUCED" "DRUG WITHDRAWN" ...
#>   .. ..$ AEOUT   : chr [1:736] "NOT RECOVERED/NOT RESOLVED" "NOT RECOVERED/NOT RESOLVED" "RECOVERED/RESOLVED" "NOT RECOVERED/NOT RESOLVED" ...
#>   .. .. ..- attr(*, "label")= Named chr "Outcome of Adverse Event"
#>   .. .. .. ..- attr(*, "names")= chr "AEOUT"
#>   .. ..$ AESEQ   : num [1:736] 1 2 3 1 2 3 4 1 2 3 ...
#>   .. .. ..- attr(*, "label")= Named chr "Sequence Number"
#>   .. .. .. ..- attr(*, "names")= chr "AESEQ"
#>   .. ..$ TRTEMFL : chr [1:736] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "Treatment Emergent Analysis Flag"
#>   .. .. .. ..- attr(*, "names")= chr "TRTEMFL"
#>   .. ..$ AOCCFL  : chr [1:736] "Y" "" "" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "1st Occurrence of Any AE Flag"
#>   .. .. .. ..- attr(*, "names")= chr "AOCCFL"
#>   .. ..$ AOCCSFL : chr [1:736] "Y" "" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "1st Occurrence of SOC Flag"
#>   .. .. .. ..- attr(*, "names")= chr "AOCCSFL"
#>   .. ..$ AOCCPFL : chr [1:736] "Y" "Y" "Y" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "1st Occurrence of Preferred Term Flag"
#>   .. .. .. ..- attr(*, "names")= chr "AOCCPFL"
#>   .. ..$ AOCC02FL: chr [1:736] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= Named chr "1st Occurrence 02 Flag for Serious"
#>   .. .. .. ..- attr(*, "names")= chr "AOCC02FL"
#>   .. ..$ AOCC03FL: chr [1:736] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= Named chr "1st Occurrence 03 Flag for Serious SOC"
#>   .. .. .. ..- attr(*, "names")= chr "AOCC03FL"
#>   .. ..$ AOCC04FL: chr [1:736] "" "" "" "" ...
#>   .. .. ..- attr(*, "label")= Named chr "1st Occurrence 04 Flag for Serious PT"
#>   .. .. .. ..- attr(*, "names")= chr "AOCC04FL"
#>   .. ..$ CQ01NAM : chr [1:736] "DERMATOLOGIC EVENTS" "DERMATOLOGIC EVENTS" "" "DERMATOLOGIC EVENTS" ...
#>   .. .. ..- attr(*, "label")= Named chr "Customized Query 01 Name"
#>   .. .. .. ..- attr(*, "names")= chr "CQ01NAM"
#>   .. ..$ AOCC01FL: chr [1:736] "Y" "" "" "Y" ...
#>   .. .. ..- attr(*, "label")= Named chr "1st Occurrence 01 Flag for CQ01"
#>   .. .. .. ..- attr(*, "names")= chr "AOCC01FL"
#>   .. ..$ AREL    : chr [1:736] "RELATED" "RELATED" "NOT RELATED" "RELATED" ...
#>   .. .. ..- attr(*, "label")= Named chr NA
#>   .. .. .. ..- attr(*, "names")= chr NA
#>   .. ..$ ATOXGRN : int [1:736] 2 1 4 3 1 3 3 5 3 4 ...
#>   .. ..- attr(*, "data_name")= chr "adae"
#>   ..$ plan            :Classes ‘meta_plan’ and 'data.frame': 1 obs. of  5 variables:
#>   .. ..$ mock       : num 1
#>   .. ..$ analysis   : chr "ae_listing"
#>   .. ..$ population : chr "apat"
#>   .. ..$ observation: chr "wk12"
#>   .. ..$ parameter  : chr "ser"
#>   .. ..- attr(*, "out.attrs")=List of 2
#>   .. .. ..$ dim     : Named int [1:5] 1 1 1 1 1
#>   .. .. .. ..- attr(*, "names")= chr [1:5] "mock" "analysis" "population" "observation" ...
#>   .. .. ..$ dimnames:List of 5
#>   .. .. .. ..$ mock       : chr "mock=1"
#>   .. .. .. ..$ analysis   : chr "analysis=ae_listing"
#>   .. .. .. ..$ population : chr "population=apat"
#>   .. .. .. ..$ observation: chr "observation=wk12"
#>   .. .. .. ..$ parameter  : chr "parameter=ser"
#>   ..$ observation     :List of 1
#>   .. ..$ wk12:List of 6
#>   .. .. ..$ name  : chr "wk12"
#>   .. .. ..$ id    : chr "USUBJID"
#>   .. .. ..$ group : chr "TRTA"
#>   .. .. ..$ var   : chr [1:13] "USUBJID" "SAFFL" "TRTA" "SEX" ...
#>   .. .. ..$ subset: language SAFFL == "Y"
#>   .. .. ..$ label : chr "Weeks 0 to 12"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..$ population      :List of 1
#>   .. ..$ apat:List of 6
#>   .. .. ..$ name  : chr "apat"
#>   .. .. ..$ id    : chr "USUBJID"
#>   .. .. ..$ group : chr "TRT01A"
#>   .. .. ..$ var   : chr [1:7] "USUBJID" "SAFFL" "TRT01A" "SITEID" ...
#>   .. .. ..$ subset: language SAFFL == "Y"
#>   .. .. ..$ label : chr "All Participants as Treated"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..$ parameter       :List of 1
#>   .. ..$ ser:List of 11
#>   .. .. ..$ name    : chr "ser"
#>   .. .. ..$ id      : NULL
#>   .. .. ..$ group   : NULL
#>   .. .. ..$ var     : chr "AEDECOD"
#>   .. .. ..$ subset  : language AESER == "Y"
#>   .. .. ..$ label   : chr "Serious AEs"
#>   .. .. ..$ term1   : chr "Serious"
#>   .. .. ..$ term2   : chr ""
#>   .. .. ..$ soc     : chr "AEBODSYS"
#>   .. .. ..$ seq     : num 401
#>   .. .. ..$ summ_row: chr "with serious adverse events"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..$ analysis        :List of 1
#>   .. ..$ ae_listing:List of 10
#>   .. .. ..$ name    : chr "ae_listing"
#>   .. .. ..$ id      : NULL
#>   .. .. ..$ group   : NULL
#>   .. .. ..$ var     : NULL
#>   .. .. ..$ subset  : NULL
#>   .. .. ..$ label   : chr "Listing: adverse event"
#>   .. .. ..$ var_name: chr [1:8] "USUBJID" "ASTDY" "AEDECOD" "ADURN" ...
#>   .. .. ..$ group_by: chr [1:2] "USUBJID" "ASTDY"
#>   .. .. ..$ page_by : chr "TRTA"
#>   .. .. ..$ title   : chr "Listing of Participants With {term1} Adverse Events {term2}"
#>   .. .. ..- attr(*, "class")= chr "adam_mapping"
#>   ..- attr(*, "class")= chr "meta_adam"
#>  $ population     : chr "apat"
#>  $ observation    : chr "wk12"
#>  $ parameter      : chr "ser"
#>  $ n              : NULL
#>  $ order          : NULL
#>  $ group          : NULL
#>  $ reference_group: NULL
#>  $ col_name       : Named chr [1:9] "Unique Subject Identifier" "Analysis Start Relative Day" "Dictionary-Derived Term" "AE Duration (N)" ...
#>   ..- attr(*, "names")= chr [1:9] "USUBJID" "ASTDY" "AEDECOD" "ADURN" ...
#>  $ tbl            :'data.frame': 1 obs. of  9 variables:
#>   ..$ USUBJID: chr "01-718-1170"
#>   .. ..- attr(*, "label")= Named chr "Unique Subject Identifier"
#>   .. .. ..- attr(*, "names")= chr "USUBJID"
#>   ..$ ASTDY  : num 27
#>   .. ..- attr(*, "label")= Named chr "Analysis Start Relative Day"
#>   .. .. ..- attr(*, "names")= chr "ASTDY"
#>   ..$ AEDECOD: chr "SYNCOPE"
#>   .. ..- attr(*, "label")= Named chr "Dictionary-Derived Term"
#>   .. .. ..- attr(*, "names")= chr "AEDECOD"
#>   ..$ ADURN  : num 2
#>   .. ..- attr(*, "label")= Named chr "AE Duration (N)"
#>   .. .. ..- attr(*, "names")= chr "ADURN"
#>   ..$ AESEV  : chr "SEVERE"
#>   .. ..- attr(*, "label")= Named chr "Severity/Intensity"
#>   .. .. ..- attr(*, "names")= chr "AESEV"
#>   ..$ AESER  : chr "Y"
#>   .. ..- attr(*, "label")= Named chr "Serious Event"
#>   .. .. ..- attr(*, "names")= chr "AESER"
#>   ..$ AEREL  : chr "PROBABLE"
#>   .. ..- attr(*, "label")= Named chr "Causality"
#>   .. .. ..- attr(*, "names")= chr "AEREL"
#>   ..$ AEOUT  : chr "RECOVERED/RESOLVED"
#>   .. ..- attr(*, "label")= Named chr "Outcome of Adverse Event"
#>   .. .. ..- attr(*, "names")= chr "AEOUT"
#>   ..$ TRTA   : Factor w/ 2 levels "Low Dose","Placebo": 1
#>   .. ..- attr(*, "label")= Named chr "TRTA"
#>   .. .. ..- attr(*, "names")= chr "TRTA"
#>  $ prepare_call   : language prepare_ae_listing(meta = meta, analysis = "ae_listing", population = "apat",      observation = "wk12", parameter = "ser")
#>  - attr(*, "class")= chr "outdata"
```
