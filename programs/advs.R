# Name: ADVS
#
# Label: Vital Signs Analysis Dataset
# Modified from admiral::use_ad_template("advs") template code
# Quick mini tour of come of RStudio/Posit Features (Outline, Environment, Data, Theme, Shortcuts)
#
# Input: adsl, vs
library(admiral)
library(admiral.test) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
library(xportr)

# Load source datasets ----

# Use e.g. `haven::read_sas()` to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data("admiral_vs")
data("admiral_adsl")

adsl <- admiral_adsl
vs <- admiral_vs

# Read in prepared spec file for ADVS ----
#advs_spec <- readxl::read_xlsx("/cloud/project/advs_admiral_spec.xlsx", sheet = "Variables")
advs_spec <- readxl::read_xlsx("/cloud/project/metadata/specs.xlsx", sheet = "Variables")%>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>%
  mutate(format = str_to_lower(format)) %>%
  filter(dataset == "ADVS")

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values

vs <- convert_blanks_to_na(vs)

# See metatools::combine_supp for combing supplementary SDTM datasets (not covered)

# Lookup tables ----

# Assign PARAMCD, PARAM, and PARAMN
# NOTE: Mistake in VSTESTCD that we will fix when at derive_vars_merged_lookup
param_lookup <- tibble::tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "SYSBP", "SYSBP", "Systolic Blood Pressure (mmHg)", 1,
  "DIABP", "DIABP", "Diastolic Blood Pressure (mmHg)", 2,
  "PULSE", "PULSE", "Pulse Rate (beats/min)", 3,
  "WEIGHT", "WEIGHT", "Weight (kg)", 4,
  "HEIGHT", "HEIGHT", "Height (cm)", 5,
  "TEMP", "TEMP", "Temperature (C)", 6,
  "MAP", "MAP", "Mean Arterial Pressure (mmHg)", 7,
  "BMI", "BMI", "Body Mass Index(kg/m^2)", 8,
  "BSA", "BSA", "Body Surface Area(m^2)", 9
)


# Assign ANRLO/HI, A1LO/HI
range_lookup <- tibble::tribble(
  ~PARAMCD, ~ANRLO, ~ANRHI, ~A1LO, ~A1HI,
  "SYSBP", 90, 130, 70, 140,
  "DIABP", 60, 80, 40, 90,
  "PULSE", 60, 100, 40, 110,
  "TEMP", 36.5, 37.5, 35, 38
)

# Assign AVALCAT1
avalcat_lookup <- tibble::tribble(
  ~PARAMCD, ~AVALCA1N, ~AVALCAT1,
  "HEIGHT", 1, ">100 cm",
  "HEIGHT", 2, "<= 100 cm"
)

# User defined functions ----

# Here are some examples of how you can create your own functions that
#  operates on vectors, which can be used in `mutate()`.

# NOTE: Functions appears in a different section of the Environment Window
format_avalcat1n <- function(param, aval) {
  case_when(
    param == "HEIGHT" & aval > 140 ~ 1,
    param == "HEIGHT" & aval <= 140 ~ 2
  )
}

# Derivations ----

# Gets a list of ADSL vars required for derivations
# NOTE: Look at the difference between the objects
adsl_vars <- vars(TRTSDT, TRTEDT, TRT01A, TRT01P)
adsl_vars_v <- c("TRTSDT", "TRTEDT", "TRT01A", "TRT01P")

## Dates and Times ----
# NOTE: Functions with vars add variables and functions with param/summary add records
# NOTE: A Few exception to the rule.
advs_dt_dy <- vs %>%
  # Join ADSL with VS (need TRTSDT for ADY derivation)
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = vars(STUDYID, USUBJID)  # Why not just do a dplyr::left_join()
  ) %>%
  ## Calculate ADT, ADY ----
derive_vars_dt(
  new_vars_prefix = "A",
  dtc = VSDTC
) %>%
  ## Calculate ADTM with imputation (no time, but just for fun!)
  derive_vars_dtm(
    new_vars_prefix = "A",
    dtc = VSDTC,
    highest_imputation = "h",
    date_imputation = "first",
    time_imputation = "last") %>%
  derive_vars_dtm_to_tm(vars(ADTM)) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT))

## Add PARAMCD only - add PARAM etc later ----
advs_paramcd <- advs_dt_dy %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars(PARAMCD),
    by_vars = vars(VSTESTCD),
    print_not_mapped = TRUE,
    check_type = "warning"
  ) %>%
  ## Calculate AVAL and AVALC ----
mutate(
  AVAL = VSSTRESN,
  AVALC = VSSTRESC
)

# Derive new parameters based on existing records ----
# Note that, for the following three `derive_param_*()` functions, only the
# variables specified in `by_vars` will be populated in the newly created
# records.

# NOTE: These are just wrappers around derive_param_computed()
# NOTE: Expose function code with no parentheses: derive_param_bsa
# NOTE: ?"!!!" what is this devilry?
advs_computed <- advs_paramcd %>%

  ## Derive Mean Arterial Pressure ----
derive_param_map(
  by_vars = vars(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM), # what is carried over in records
  set_values_to = vars(PARAMCD = "MAP"),
  get_unit_expr = VSSTRESU,
  filter = VSSTAT != "NOT DONE" | is.na(VSSTAT)
) %>%
  ## Derive Body Surface Area ----
derive_param_bsa(
  by_vars = vars(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM),
  method = "Mosteller",
  set_values_to = vars(PARAMCD = "BSA"),
  get_unit_expr = VSSTRESU,
  filter = VSSTAT != "NOT DONE" | is.na(VSSTAT)
) %>%
  ## Derive Body Mass Index ----
derive_param_bmi(
  by_vars = vars(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM),
  set_values_to = vars(PARAMCD = "BMI"), # This can take PARAMs as well - ?derive_param_bmi
  get_unit_expr = VSSTRESU,
  filter = VSSTAT != "NOT DONE" | is.na(VSSTAT)
)


## Get visit info ----
# See also the "Visit and Period Variables" vignette
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html#visits)
advs_visit <- advs_computed %>%
  # Derive Timing
  mutate(
    ATPTN = VSTPTNUM,
    ATPT = VSTPT,
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_, # Did it work?
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_ # Special NA for character variables - see ?NA
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")),
      TRUE ~ NA_character_
    ))
  )

## Derive a new record as a summary record (e.g. mean of the triplicates at each time point) ----
# NOTE: You can supply a user defined summary function
advs_avg <- advs_visit %>%
  derive_summary_records(
    by_vars = vars(STUDYID, USUBJID, !!!adsl_vars, PARAMCD, AVISITN, AVISIT, ADT, ADY),
    filter = !is.na(AVAL),
    analysis_var = AVAL,
    summary_fun = mean,
    set_values_to = vars(DTYPE = "AVERAGE")
  )

## Calculate ONTRTFL ----
advs_ontrtfl <- advs_avg %>%
  derive_var_ontrtfl(
    start_date = ADT,
    ref_start_date = TRTSDT,
    ref_end_date = TRTEDT,
    filter_pre_timepoint = AVISIT == "Baseline" # Excluding Baseline records
  )

## Calculate ANRIND : requires the reference ranges ANRLO, ANRHI ----
# Also accommodates the ranges A1LO, A1HI
advs_rind <- advs_ontrtfl %>%
  derive_vars_merged(dataset_add = range_lookup, by_vars = vars(PARAMCD)) %>%
  # Calculate ANRIND
  derive_var_anrind()

# Derive baseline flags ----
advs_btype <- advs_rind %>%
  ## Calculate BASETYPE ----
derive_var_basetype(
  basetypes = rlang::exprs(
    "LAST: AFTER LYING DOWN FOR 5 MINUTES" = ATPTN == 815,
    "LAST: AFTER STANDING FOR 1 MINUTE" = ATPTN == 816,
    "LAST: AFTER STANDING FOR 3 MINUTES" = ATPTN == 817,
    "LAST" = is.na(ATPTN)
  )
)

advs_ablfl <- advs_btype %>%
  ## Calculate ABLFL ----
## NOTE: Perform a derivation on a subset of data
# https://pharmaverse.github.io/admiral/articles/higher_order.html
restrict_derivation(
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(STUDYID, USUBJID, BASETYPE, PARAMCD),
    order = vars(ADT, VISITNUM, VSSEQ),
    new_var = ABLFL,
    mode = "last"
  ),
  filter = (!is.na(AVAL) &
              ADT <= TRTSDT & !is.na(BASETYPE) & is.na(DTYPE))
)

## Derive baseline information ----
advs_base_chg <- advs_ablfl %>%
  # Calculate BASE
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = AVAL,
    new_var = BASE
  ) %>%
  # Calculate BASEC
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = AVALC,
    new_var = BASEC
  ) %>%
  # Calculate BNRIND
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, BASETYPE),
    source_var = ANRIND,
    new_var = BNRIND
  ) %>%
  # Calculate CHG
  derive_var_chg() %>%
  # Calculate PCHG
  derive_var_pchg()


## ANL01FL: Flag last result within an AVISIT and ATPT for post-baseline records ----
# NOTE: the use of restrict to apply only to a subset of the data for flag
advs_anl01fl <- advs_base_chg %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ANL01FL,
      by_vars = vars(USUBJID, PARAMCD, AVISIT, ATPT, DTYPE),
      order = vars(ADT, AVAL),
      mode = "last"
    ),
    filter = !is.na(AVISITN) & ONTRTFL == "Y"
  )

## Get treatment information ----
# See also the "Visit and Period Variables" vignette
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html#treatment_bds)
advs_eotfl <- advs_anl01fl %>%
  # Assign TRTA, TRTP
  # Create End of Treatment Record
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, PARAMCD, ATPTN),
      order = vars(ADT),
      new_var = EOTFL,
      mode = "last"
    ),
    filter = (4 < VISITNUM &
                VISITNUM <= 13 & ANL01FL == "Y" & is.na(DTYPE))
  ) %>%
  filter(EOTFL == "Y") %>%
  mutate(
    AVISIT = "End of Treatment",
    AVISITN = 99
  ) %>%
  union_all(advs_anl01fl) %>%
  select(-EOTFL) %>%
  mutate(
    TRTP = TRT01P,
    TRTA = TRT01A
  )

## Get ASEQ and AVALCATx and add PARAM/PARAMN ----
advs_seq <- advs_eotfl %>%
  # Calculate ASEQ
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = vars(STUDYID, USUBJID),
    order = vars(PARAMCD, ADT, AVISITN, VISITNUM, ATPTN, DTYPE),
    check_type = "error"
  ) %>%
  # Derive AVALCA1N and AVALCAT1
  mutate(AVALCA1N = format_avalcat1n(param = PARAMCD, aval = AVAL)) %>%
  derive_vars_merged(dataset_add = avalcat_lookup, by_vars = vars(PARAMCD, AVALCA1N)) %>%
  # Derive PARAM and PARAMN
  derive_vars_merged(dataset_add = select(param_lookup, -VSTESTCD), by_vars = vars(PARAMCD))

# The Finale ----
# Add all ADSL variables, apply spec properties, create xpt ----
advs <- advs_seq %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = vars(STUDYID, USUBJID)) %>%
  select(advs_spec$variable) %>%
  xportr_type(advs_spec, "ADVS") %>%
  xportr_label(advs_spec, "ADVS") %>% # What happens when you don't subset?
  xportr_format(advs_spec, "ADVS") %>%
  xportr_length(advs_spec, "ADVS") %>%
  xportr_write("advs.xpt", label = "Vital Signs Analysis")

message("This is a custom message that I use to remind myslef.")
warning("This is a custom warning that I use to remind myslef.")

# Run log command just in console not in the script.
# logrx::axecute(file = "/cloud/project/mod_advs.R", to_report = c("messages"))
