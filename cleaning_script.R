library(dplyr)
library(lubridate)
library(readxl)
library(readr)
library(openxlsx)
library(sf)
library(raster)
library(tidyverse)
library(data.table)
# library(hypegrammaR)

source("functions/audit_function_full.R")
source("functions/function_handler.R")
source("functions/to_alphanumeric_lowercase.R")

WGS84 <- crs("+init=epsg:4326")
UTM38N <- crs("+init=epsg:32638")

assessment_start_date <- as.Date("2020-07-15")

# set min time and max # interviews per day
time_limit <- 12
flag_time_limit <- 25
max_interv <- 10

###########################################################################################################

# read data from excel file
df <-
  read.csv("input/raw_data/household_merged.csv", encoding = "UTF-8") %>%
  mutate(
    calc_noteligible = as.numeric(calc_noteligible),
    uuid = X_uuid,
    `_uuid` = X_uuid
  )

names(df)[names(df) == "X.U.FEFF.start"] <- "start"

###########################################################################################################
# time check from audit files
# today
df$today <- as.Date(df$start, "%Y-%m-%d")

df <-
  time_check_audit(audit_dir_path = "audit/", df,  "_uuid", time_limit = time_limit)
# df <- time_check(df, time_limit)

df <- df %>%
  mutate(
    time_validity = case_when(
      interview_duration < flag_time_limit &
        interview_duration > time_limit ~ "Flagged",
      TRUE ~ time_validity
    )
  )

###########################################################################################################

# when survey does not continue to hh member calculation, these surveys are not valid
df <- df %>%
  mutate(calc_noteligible = case_when(is.na(num_hh_member) ~ 1,
                                      TRUE ~ calc_noteligible))

# add population group
df <- df %>%
  mutate(
    population_group = case_when(
      calc_noteligible == 1 ~ "not_eligible",
      calc_idp == 1 ~ "idp_out_camp",
      calc_returnee == 1 ~ "returnee",
      !is.na(camp_name) ~ "idp_in_camp",
      TRUE ~ "not_eligible"
    )
  )


# import individual loop data
indiv_df <-
  read.csv("input/raw_data/loop_merged.csv", encoding = "UTF-8") %>%
  mutate(`_index` = X_index,
         relationship = X.U.FEFF.relationship)

#############################################################################################################
########## SAMPLE FRAMES ########################
# In person sample
in_person_sample_df <-
  read_excel("input/sample/inperson_sf_14072020.xlsx")

in_person_sample_summarized <- in_person_sample_df %>%
  group_by(pop_group_dist) %>%
  summarise(num_surveys = sum(num_surveys))


# phone interview sample
phone_int_sample_df <-
  read_excel("input/sample/phone_int_sf_21072020.xlsx")

###########################################################################################################
# SPATIAL district check  for in person data collection #
############# Data frames for tracking ##############

in_person_points <- df %>%
  filter(dc_method == "in_person") %>%
  filter(!is.na(X_gpslocation_latitude),
         !is.na(X_gpslocation_longitude)) %>%
  dplyr::select(
    `_uuid`,
    governorate_mcna,
    cluster_location_id,
    enumerator_num,
    population_group,
    lat = X_gpslocation_latitude,
    long = X_gpslocation_longitude
  ) %>%
  as_tibble()

points_sf <- in_person_points %>%
  dplyr::select(
    `_uuid`,
    governorate_mcna,
    cluster_location_id,
    enumerator_num,
    population_group,
    lat,
    long
  ) %>%
  st_as_sf(
    coords = c("long", "lat"),
    agr = "constant",
    crs = WGS84,
    stringsAsFactors = FALSE,
    remove = TRUE
  )

######### IRQ Dist boundary ###################

irq_dist <- st_read("input/spatial_data/irq_dist.geojson") %>%
  mutate(district_gps = irq_dist_lookup_dist_kobo,
         District = irq_dist_lookup_dist_kobo)

irq_dist <-
  sf::st_as_sf(irq_dist, coords = c("long", "lat"), crs = 4326)
irq_dist <- sf::st_cast(irq_dist, "POLYGON")

# spatial join points to get true districts of GPS locations
district_join <- st_join(points_sf, irq_dist) %>%
  dplyr::select(`_uuid`:geometry, population_group, district_gps)

##
# convert back to df
district_join_df <- as.data.frame(district_join) %>%
  dplyr::select(`_uuid`, district_gps) %>%
  mutate(uuid = `_uuid`,
         district_gps = as.character(district_gps))

# merge back with main df to add actual district
df <- merge(df, district_join_df, all = T)

# set district_mcna column to true district for in person surveys
df <- df %>%
  mutate(district_mcna = case_when(!is.na(district_gps) ~ district_gps,
                                   TRUE ~ district_mcna))

# add full strata
df <- df %>%
  mutate(strata = paste0(district_mcna, "_", population_group))

##########################################################################################
# Deleted interviews column
df <- df %>%
  mutate(
    deleted = case_when(
      time_validity == "Deleted" |
        consent == "no" | population_group == "not_eligible" ~ "yes",
      TRUE ~ "no"
    )
  )

###############################################################################
# flag surveys for deletion if wrong strata or data collection method

df <- df %>%
  mutate(deleted = case_when((district_mcna == "al.ramadi" & population_group == "idp_out_camp" &
                                dc_method == "remote") |
                               (district_mcna == "al.ramadi" & population_group == "returnee" &
                                  dc_method == "remote") |
                               (district_mcna == "al.falluja" & population_group == "idp_out_camp" &
                                  dc_method == "remote") |
                               (district_mcna == "al.falluja" & population_group == "returnee" &
                                  dc_method == "remote") |
                               (district_mcna == "al.kadhmiyah" & population_group == "idp_out_camp" &
                                  dc_method == "in_person") |
                               (district_mcna == "al.kadhmiyah" & population_group == "returnee" &
                                  dc_method == "in_person") |
                               (strata == "khanaqin_idp_out_camp" &
                                  dc_method == "remote") |
                               (strata == "khanaqin_returnee" &
                                  dc_method == "remote") |
                               (strata == "erbil_idp_out_camp" &
                                  dc_method == "remote") |
                               (strata == "al.shikhan_idp_out_camp" &
                                  dc_method == "remote") |
                               (strata == "al.shikhan_returnee" &
                                  dc_method == "remote") |
                               (strata == "tilkaef_idp_out_camp" &
                                  dc_method == "in_person") |
                               (strata == "al.shirqat_returnee" &
                                  dc_method == "remote") |
                               (strata == "beygee_returnee" &
                                  dc_method == "remote") |
                               (strata == "tooz.khurmato_idp_out_camp" &
                                  dc_method == "remote") |
                               (strata == "al.kut_idp_out_camp" &
                                  dc_method == "remote") ~ "yes",
                             TRUE ~ deleted
  ))

# correct Mosul IDP surveys from Sept 18 that were actually for Hatra

df <- df %>%
  mutate(
    district_mcna = case_when(
      date_assessment == "9/18/2020" & strata == "al.mosul_idp_out_camp"
      ~ "al.hatra",
      TRUE ~ district_mcna
    ),
    strata = case_when(
      date_assessment == "9/18/2020" & strata == "al.mosul_idp_out_camp"
      ~ "al.hatra_idp_out_camp",
      TRUE ~ strata
    )
  )

###############################################################################
#DELETE NOTE COLUMNS
df[, c(
  "note_missing_documents_above_18",
  "note_missing_documents_under_18",
  "referral_contract",
  "note_returnee",
  "note_host",
  "note_idp",
  "note_ineligible",
  "note_idp_ineligible",
  "note_returnee_ineligible",
  "calc_note",
  "tot_calc_note",
  "not_residing_reason_note",
  "food_security_note",
  "disabilities_note",
  "covid_job_note",
  "exp_note",
  "note_inelegible",
  "note_returnee_inelegible"
)] <- list(NULL)

########################################################################################
###########################################################################################################
# calculate new variables

######### PARENT SHEET ################################################
#CHANGE SKIPPED QUESTIONS FOR DISCIPLINARY MEASURES TO DECLINED TO ANSWER
df$disciplinary_measures <- ifelse(
  df$disciplinary_measures == "" |
    df$disciplinary_measures == "no_answer",
  "decline_to_answer",
  df$disciplinary_measures
)

df$disciplinary_measures.explained <-
  ifelse(
    df$disciplinary_measures == "decline_to_answer",
    0,
    df$disciplinary_measures.explained
  )

df$disciplinary_measures.forbid_privileges <-
  ifelse(
    df$disciplinary_measures == "decline_to_answer",
    0,
    df$disciplinary_measures.forbid_privileges
  )

df$disciplinary_measures.shouted <-
  ifelse(
    df$disciplinary_measures == "decline_to_answer",
    0,
    df$disciplinary_measures.shouted
  )

df$disciplinary_measures.spanked <-
  ifelse(
    df$disciplinary_measures == "decline_to_answer",
    0,
    df$disciplinary_measures.spanked
  )

df$disciplinary_measures.no_answer <-
  ifelse(
    df$disciplinary_measures == "decline_to_answer",
    1,
    df$disciplinary_measures.no_answer
  )


#CHANGE NAMES FOR NOT_RESIDING_REASON VARIABLE
names(df)[names(df) == "married"] <- "not_residing_reason.married"
names(df)[names(df) == "seek_employment"] <-
  "not_residing_reason.seek_employment"
names(df)[names(df) == "study"] <- "not_residing_reason.study"
names(df)[names(df) == "family"] <- "not_residing_reason.family"
names(df)[names(df) == "armed_actors"] <-
  "not_residing_reason.armed_actors"
names(df)[names(df) == "kidnapped"] <-
  "not_residing_reason.kidnapped"
names(df)[names(df) == "missing"] <- "not_residing_reason.missing"
names(df)[names(df) == "detained"] <- "not_residing_reason.detained"

# Change "stealing" in the food_source variable to "local_charity" (error in the ODK)
df$food_source[df$food_source == "stealing"] <- "local_charity"

#VALUE FOR NOT_RESIDING_NUM = 15 BUT ONLY ONE INDICATED FOR REASONS
df$not_residing_num <- ifelse(df$not_residing_num == 15, 1,
                              df$not_residing_num)

# rename primary livelihood vars
df <- df %>%
  rename(
    "primary_livelihood_employment" = "primary_livelihood.employment",
    "primary_livelihood_retirement_pension" = "primary_livelihood.retirement_pension"
  )

# Re-position the shelter_type variable to be with the other snfi variables
df <- df %>% relocate(c("shelter_type"), .before = "shelter_type_other")


# Re-position the tank capacity variables
df <- df %>% relocate(c("access_private_shared_watertank", "tank_capacity", "refill_times", "people_share_tank"), .before = "latrines")



######### INDIVIDUAL SHEET ################################################

# group indiv data and join with hh level
indiv_df <- indiv_df %>%
  filter(relationship != "error")
q <- indiv_df[duplicated(indiv_df[, c(1:32, 34:39)]),]
q <- q %>% filter(relationship %in% c("head", "spouse"))
q <- as.numeric(as.vector(q$`_index`))
indiv_df <- indiv_df %>%
  filter(!`_index` %in% q)

# deal with more than 1 hoh
indiv_df$is_head <- ifelse(indiv_df$relationship == "head", 1, 0)
grouped_head_hh <- indiv_df %>%
  group_by(`X_submission__uuid`) %>%
  summarize(max_age = max(age),
            num_head_hh = sum(is_head))

indiv_df <- indiv_df %>%
  mutate(
    relationship =
      case_when(
        grouped_head_hh$num_head_hh[match(indiv_df$X_submission__uuid,
                                          grouped_head_hh$X_submission__uuid)] > 1 &
          sex == "female" &
          marital_status == "married" &
          is_head == 1 ~ "spouse",
        grouped_head_hh$num_head_hh[match(indiv_df$X_submission__uuid,
                                          grouped_head_hh$X_submission__uuid)] > 1 &
          is_head == 1 &
          grouped_head_hh$max_age[match(indiv_df$X_submission__uuid,
                                        grouped_head_hh$X_submission__uuid)] > age ~ "child",
        TRUE ~ relationship
      )
  )
indiv_df <- indiv_df[, 1:39]

# recode school attendance into binary
indiv_df <- indiv_df %>%
  mutate(
    attend_formal_ed = case_when(
      attend_formal_ed == "yes" ~ 1,
      attend_formal_ed == "no" ~ 0,
      TRUE ~ NA_real_
    ),
    uuid = X_submission__uuid
  )


names(indiv_df)[names(indiv_df) == "X.U.FEFF.relationship"] <-
  "relationship"

indiv_df <- indiv_df %>%
  mutate(head_household = case_when(relationship == "head" ~ 1, TRUE ~ 0),
         uuid = X_submission__uuid)


indiv_analysis <- indiv_df %>%
  group_by(uuid) %>%
  summarize(
    count_attend_formal_ed = sum(attend_formal_ed, na.rm = T),
    child_not_school = sum(as.numeric(child_not_school), na.rm = T),
    adult_no_work = sum(as.numeric(adult_no_work), na.rm = T),
    head_household = sum(as.numeric(head_household), na.rm = T)
  )

# join with parent dataset

df <- left_join(df, indiv_analysis, by = "uuid")

#correcting negative ages
indiv_df <- indiv_df %>%
  mutate(age = case_when(as.numeric(age) < 0 ~ (-1 * as.numeric(age)),
                         TRUE ~ as.numeric(age)))

# cannot be spouse and widowed
indiv_df <- indiv_df %>%
  mutate(
    relationship =
      case_when(
        relationship == "spouse" &
          marital_status == "widowed" &
          age == df$age_respondent[match(indiv_df$X_submission__uuid, df$`_uuid`)] &
          sex == df$gender_respondent[match(indiv_df$X_submission__uuid, df$`_uuid`)] ~ "head",
        TRUE ~ relationship
      )
  )

# spouse cannot be less than 14 yrs old
indiv_df <- indiv_df %>%
  mutate(
    relationship =
      case_when(
        relationship == "spouse" & age < 14 &
          df$age_respondent[match(indiv_df$X_submission__uuid, df$`_uuid`)] > 25 ~ "child",
        TRUE ~ relationship
      )
  )

# spouse cannot be same gender
indiv_df <- indiv_df %>%
  mutate(
    relationship =
      case_when(
        relationship == "spouse" &
          sex == df$gender_respondent[match(indiv_df$`X_submission__uuid`, df$`_uuid`)] &
          age <= 16 ~ "childinlaw",
        TRUE ~ relationship
      )
  )

#eliminating empty values
indiv_df[indiv_df$marital_status == "", "marital_status"] <- "NA"
indiv_df[indiv_df$pregnant_lactating == "", "pregnant_lactating"] <-
  "NA"
indiv_df[indiv_df$health_issue_other == "", "health_issue_other"] <-
  "NA"
indiv_df <- indiv_df %>%
  mutate(
    attend_formal_ed = case_when(
      attend_formal_ed == 0 ~ "no",
      attend_formal_ed == 1 ~ "yes",
      TRUE ~ "NA"
    )
  )
indiv_df[indiv_df$attend_informal_ed == "", "attend_informal_ed"] <-
  "NA"
indiv_df[indiv_df$work == "", "work"] <- "NA"
indiv_df[indiv_df$actively_seek_work == "", "actively_seek_work"] <-
  "NA"

trash_uuids <-
  read.csv("input/extras/X_uuid_tobe deleted_loop.csv", as.is = TRUE)
indiv_df <- indiv_df %>%
  filter(!(X_submission__uuid %in% trash_uuids |
             X_submission__uuid == ""))

# recalculate hh members with duplicate entries removed
temp <- indiv_df %>%
  group_by(X_submission__uuid) %>%
  summarize(num_family_member = n())
df <- df %>%
  mutate(num_family_member = temp$num_family_member[match(df$uuid, indiv_df$X_submission__uuid)])

df$num_hh_member <- as.integer(df$num_hh_member)

# df <- full_join(df, indiv_analysis, by = c("_uuid"="_submission__uuid"))
###########################################################################################################
#  snowballing and PII columns
pii_col <- c(
  "snowballing_question",
  "snowballing_willing",
  "snowballing_name",
  "snowballing_telephone",
  "snowballing_same_district",
  "snowballing_governorate",
  "snowballing_district",
  "snowballing_popgroup",
  "further_contact_willing",
  "contact_details_name",
  "contact_details_telephone",
  "gpslocation",
  "X_gpslocation_latitude",
  "X_gpslocation_longitude",
  "X_gpslocation_altitude",
  "X_gpslocation_precision",
  "cluster_location_id"
)

# outlier cleaning for parent sheet
df <- df %>% 
  mutate(num_hh_member = ifelse(num_hh_member > 46, NA_real_, num_hh_member),
         inc_employment_pension = ifelse(inc_employment_pension >= 7400000, NA_real_, inc_employment_pension),
         head_household = ifelse(head_household > 1, NA_real_, head_household),
         how_much_debt = ifelse(how_much_debt >= 100000000, NA_real_, how_much_debt),
         tot_expenses = ifelse(tot_expenses >= 2100000, NA_real_, tot_expenses))

#################################################################################################################
# read log csv file after AO has indicated decision on flagged data

log_df <-
  read.csv(
    "input/filled_cleaning_logs/cleaning_log_2020-09-23_cleaned_AF.csv",
    as.is = TRUE,
    encoding = "UTF-8"
  ) %>% 
  filter(!is.na(X_uuid))
  
replaced_df <- replaced_df(df, log_df)
#
# # replace translation logs
log_indiv_df <-
  read.csv(
    "input/filled_cleaning_logs/cleaning_log_individual_22092020.csv",
    as.is = TRUE,
    encoding = "UTF-8"
  ) %>%
  mutate(`_index` = X_index)

log_indiv_df <- rename(log_indiv_df, x_index = `_index`)

indiv_replaced <- replaced_indiv_df(indiv_df, log_indiv_df)

###############################################################################################################
# deleted surveys

# take uuids of deleted surveys and remove from cleaned dataset
deleted_surveys <- replaced_df %>%
  filter(deleted == "yes")

# drop pii columns from deleted dataset
deleted_surveys <- deleted_surveys %>%
  dplyr::select(-pii_col)

deleted_uuids <- deleted_surveys %>%
  dplyr::select(uuid, deleted = time_validity)

# join with individual dataset to remove deleted uuids from loop
indiv_replaced <- left_join(indiv_replaced, deleted_uuids, by = "uuid")

# take uuids of deleted loop surveys and remove from cleaned dataset
deleted_loop <- indiv_replaced %>%
  filter(!is.na(deleted))

indiv_cleaned <- indiv_replaced %>%  filter(is.na(deleted))

# remove deleted surveys from cleaned dataset
replaced_df %<>% filter(deleted == "no")

# drop pii columns from dataset
replaced_df <- replaced_df %>%
  dplyr::select(-pii_col)


# replace / character with . in colnames for final dataset
names(replaced_df) <-
  gsub(x = names(replaced_df),
       pattern = "/",
       replacement = "\\.")
names(indiv_replaced) <-
  gsub(x = names(indiv_replaced),
       pattern = "/",
       replacement = "\\.")

# select columns for deletion log
deleted_redacted <- deleted_surveys %>%
  dplyr::select(
    uuid,
    date_assessment,
    enumerator_num,
    governorate_mcna,
    district_mcna,
    duration_minutes,
    time_validity,
    population_group,
    strata,
    deleted
  )

#DELETE REF COLUMNS
replaced_df[, c("audit", "ref_evic", "ref_docs", "ref_child")] <-
  list(NULL)


#DELETE COLUMNS
replaced_df[, c(
  "calc_idp",
  "calc_returnee",
  "calc_host",
  "calc_noteligible",
  "ind_level",
  "calc_separated",
  "exp_compare",
  "calc_expenditure",
  "X_uuid",
  "referral_contact",
  "thanks"
  
)] <- list(NULL)

replaced_df$`_uuid` <- NULL

# export clean data
readr::write_excel_csv(
  replaced_df,
  sprintf(
    "output/cleaned_data/mcna_data_clean_parent_%s.csv",
    today()
  )
)
readr::write_excel_csv(indiv_replaced,sprintf("output/cleaned_data/mcna_data_clean_loop_%s.csv", today()))

# export to one spreadsheet
mcna_datasets <-
  list(
    "MCNA_VIII_2020" = replaced_df,
    "member" = indiv_replaced,
    "cleaning_log_hh" = log_df,
    "cleaning_log_loop" = log_indiv_df,
    "deletion_log" = deleted_redacted
  )

write.xlsx(mcna_datasets, (file = sprintf("output/cleaned_data/mcna_data_clean_%s.xlsx", today())))

# export deletion log
mcna_deleted <-
  list("MCNA_VIII_2020" = deleted_surveys, "member" = deleted_loop)
write.xlsx(mcna_deleted, (
  file = sprintf("output/deletion_log/mcna_data_deleted_2020-09-24.xlsx", today())
))

###########################################################################################################
###########################################################################################################

# harmonize entries after any manual cleaning

parent <-
  read.xlsx("output/cleaned_data/mcna_data_clean_2020-09-24.xlsx", na.strings=c(""," ","NA", "#N/A", "N/A"))

parent <- parent %>%
  mutate(
    reasons_not_attend.school_closed = case_when(
      resons_not_attend_other == "school_closed" ~ 1 ,
      TRUE ~ as.numeric(reasons_not_attend.school_closed)
    ),
    reasons_not_attend.cannot_go_physically  = case_when(
      resons_not_attend_other == "cannot_go_physically" ~ 1 ,
      TRUE ~ as.numeric(reasons_not_attend.cannot_go_physically)
    ),
    reasons_not_attend.children_working  = case_when(
      resons_not_attend_other == "children_working" ~ 1 ,
      TRUE ~ as.numeric(reasons_not_attend.children_working)
    ),
    reasons_not_attend.curriculum  = case_when(
      resons_not_attend_other == "curriculum" ~ 1,
      TRUE ~ as.numeric(reasons_not_attend.curriculum)
    ),
    reasons_not_attend.impossible_to_enrol  = case_when(
      resons_not_attend_other == "impossible_to_enroll" ~ 1,
      TRUE ~ as.numeric(reasons_not_attend.impossible_to_enrol)
    ),
    reasons_not_attend.lack_doc  = case_when(
      resons_not_attend_other == "lack_doc" ~ 1,
      TRUE ~ as.numeric(reasons_not_attend.lack_doc)
    ),
    reasons_not_attend.not_safe  = case_when(
      resons_not_attend_other == "not_safe" ~ 1,
      TRUE ~ as.numeric(reasons_not_attend.not_safe)
    ),
    reasons_not_attend.poor_infrastructure  = case_when(
      resons_not_attend_other == "poor_infrastructure" ~ 1,
      TRUE ~ as.numeric(reasons_not_attend.poor_infrastructure)
    ),
    reasons_not_attend.other  = case_when(
      resons_not_attend_other %in% c("to be enrolled next year", "COVID 19 related") ~ 1,
      TRUE ~ as.numeric(reasons_not_attend.other)
    )
  )
parent <- parent %>%
  mutate(
    hh_main_risks.lack_funds  = case_when(
      hh_main_risks_other == "lack_funds" ~ 1,
      TRUE ~ as.numeric(hh_main_risks.lack_funds)
    ),
    hh_main_risks.inadequate  = case_when(
      hh_main_risks_other == "inadequate" ~ 1,
      TRUE ~ as.numeric(hh_main_risks.inadequate)
    ),
    hh_main_risks.no_agreement  = case_when(
      hh_main_risks_other == "no_agreement" ~ 1,
      TRUE ~ as.numeric(hh_main_risks.no_agreement)
    )
  )
parent <- parent %>%
  mutate(
    primary_livelihood_employment  = case_when(
      primary_livelihood_other == "employment" ~ 1,
      TRUE ~ as.numeric(primary_livelihood_employment)
    ),
    primary_livelihood_retirement_pension  = case_when(
      primary_livelihood_other == "retirement_pension" ~ 1,
      TRUE ~ as.numeric(primary_livelihood_retirement_pension)
    ),
    primary_livelihood.social_service  = case_when(
      primary_livelihood_other == "social_service" ~ 1,
      TRUE ~ as.numeric(primary_livelihood.social_service)
    ),
    info_aid.aid  = case_when(info_aid_other == "aid" ~ 1, TRUE ~ as.numeric(info_aid.aid)),
    info_provider.other  = case_when(
      is.na(info_provider_other) ~ as.numeric(info_provider.other),
      TRUE ~ 1
    ),
    aid_not_satisfied.quality  = case_when(
      aid_not_satisfied_other == "quality" ~ 1,
      TRUE ~ as.numeric(aid_not_satisfied.quality)
    )
  )

parent <- parent %>%
  mutate(
    nfi_priority_needs.mattresses_sleeping_mats  = case_when(
      grepl("mattresses_sleeping_mats", nfi_priority_needs_other) ~ 1,
      TRUE ~ as.numeric(nfi_priority_needs.mattresses_sleeping_mats)
    ),
    nfi_priority_needs.cooking_stove  = case_when(
      grepl("cooking_stove", nfi_priority_needs_other) ~ 1,
      TRUE ~ as.numeric(nfi_priority_needs.cooking_stove)
    ),
    nfi_priority_needs.winter_heaters  = case_when(
      grepl("winter_heaters", nfi_priority_needs_other) ~ 1,
      TRUE ~ as.numeric(nfi_priority_needs.winter_heaters)
    ),
    nfi_priority_needs.heating_cooking_fuel  = case_when(
      grepl("heating_cooking_fuel", nfi_priority_needs_other) ~ 1,
      TRUE ~ as.numeric(nfi_priority_needs.heating_cooking_fuel)
    )
  )
parent <- parent %>%
  mutate(
    nfi_priority_needs.none  = case_when(
      as.numeric(nfi_priority_needs.bedding_items) == 0 &
        as.numeric(nfi_priority_needs.mattresses_sleeping_mats) == 0 &
        as.numeric(nfi_priority_needs.blankets) == 0 &
        as.numeric(nfi_priority_needs.cooking_utensils) == 0 &
        as.numeric(nfi_priority_needs.cooking_stove) == 0 &
        as.numeric(nfi_priority_needs.winter_heaters) == 0 &
        as.numeric(nfi_priority_needs.clothing) == 0 &
        as.numeric(nfi_priority_needs.heating_cooking_fuel) == 0 &
        as.numeric(nfi_priority_needs.other) == 0 ~ 1,
      TRUE ~ as.numeric(nfi_priority_needs.none)
    )
  )


loop <-
  read.xlsx("output/cleaned_data/mcna_data_clean_2020-09-24.xlsx",
            sheet = "member", na.strings=c(""," ","NA")) %>% 
  mutate(uuid = X_submission__uuid)


loop <- loop %>%
  mutate(
    health_issue.conflict_related  = case_when(
      health_issue_other == "conflict_related" ~ 1,
      TRUE ~  health_issue.conflict_related
    ),
    health_issue.chronic  = case_when(health_issue_other == "chronic" ~ 1, TRUE ~  health_issue.chronic),
    health_issue.communicable  = case_when(
      health_issue_other == "communicable" ~ 1,
      TRUE ~  health_issue.communicable
    )
  )
loop <- loop %>%
  mutate(
    health_issue.other  = case_when(
      health_issue.conflict_related  == 0 &
        health_issue.communicable  == 0 &
        health_issue.chronic  == 0 &
        !is.na(health_issue_other) ~ 1,
      TRUE ~  health_issue.other
    ),
    health_issue.none  = case_when(
      health_issue.conflict_related  == 0 &
        health_issue.communicable  == 0 &
        health_issue.chronic  == 0 &
        health_issue.other  == 0 ~ 1,
      TRUE ~  health_issue.none
    )
  )



individual_to_HH_numeric_sadd <- function(loop, parent, varname) {
  r <- loop[,c("uuid", varname)]
  r <-r[complete.cases(r), ]
  r = aggregate(r[,c(2)],
                by = list(r$uuid),
                FUN = sum, na.rm = T)
  names(r) <- c("uuid", "age_var")
  parent <- merge(parent, r, by="uuid", all = T)
  parent[,c(varname)] <- parent$'age_var'
  parent$'age_var' <- NULL
  loop$'varname' <- NULL
  return(parent)
}

#MALE
loop$male_2_calc <- ifelse(loop$sex == "male" & loop$age <= 2, 1 ,0)
loop$male_3_5_calc <- ifelse(loop$sex == "male" & loop$age <= 5 & loop$age >= 3, 1 ,0)
loop$male_6_17_calc <- ifelse(loop$sex == "male" & loop$age <= 17 & loop$age >= 6, 1 ,0)
loop$male_18_59_calc <- ifelse(loop$sex == "male" & loop$age <= 59 & loop$age >= 18, 1 ,0)
loop$male_60_calc <- ifelse(loop$sex == "male" & loop$age >= 60, 1 ,0)
loop$tot_male <- ifelse(loop$sex == "male", 1 ,0)

#FEMALE
loop$female_2_calc <- ifelse(loop$sex == "female" & loop$age <= 2, 1 ,0)
loop$female_3_5_calc <- ifelse(loop$sex == "female" & loop$age <= 5 & loop$age >= 3, 1 ,0)
loop$female_6_17_calc <- ifelse(loop$sex == "female" & loop$age <= 17 & loop$age >= 6, 1 ,0)
loop$female_18_59_calc <- ifelse(loop$sex == "female" & loop$age <= 59 & loop$age >= 18, 1 ,0)
loop$female_60_calc <- ifelse(loop$sex == "female" & loop$age >= 60, 1 ,0)
loop$tot_female <- ifelse(loop$sex == "female", 1 ,0)

#AGE
loop$tot_child_012 <- ifelse(loop$age <= 12 & loop$age >= 0, 1 ,0)
loop$tot_child_1317 <- ifelse(loop$age <= 17 & loop$age >= 13, 1 ,0)
loop$tot_child <- ifelse(loop$age <= 17 & loop$age >= 0, 1 ,0)
loop$tot_6_above <- ifelse(loop$age >= 6, 1 ,0)

#TOTAL FAMILY SIZE
loop$num_family_member <- 1


#FUN FUNCTION
#MALE
parent <- individual_to_HH_numeric_sadd(loop, parent, "male_2_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "male_3_5_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "male_6_17_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "male_18_59_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "male_60_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "tot_male")
#FEMALE
parent <- individual_to_HH_numeric_sadd(loop, parent, "female_2_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "female_3_5_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "female_6_17_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "female_18_59_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "female_60_calc")
parent <- individual_to_HH_numeric_sadd(loop, parent, "tot_female")
#AGE
parent <- individual_to_HH_numeric_sadd(loop, parent, "tot_child_012")
parent <- individual_to_HH_numeric_sadd(loop, parent, "tot_child_1317")
parent <- individual_to_HH_numeric_sadd(loop, parent, "tot_child")
parent <- individual_to_HH_numeric_sadd(loop, parent, "tot_6_above")
#TOTAL FAMILY SIZE
parent <- individual_to_HH_numeric_sadd(loop, parent, "num_family_member")


parent <- parent %>%
  mutate(representation = ifelse(dc_method == "remote" | (district_mcna == "al.amadiya" & population_group == "idp_out_camp"),
                                 "indicative", "representative"))

indicative <- parent %>%
  filter(representation == "indicative")

representative <- parent %>%
  filter(representation == "representative")


# export to one spreadsheet
mcna_datasets <-
  list(
    "representative" = representative,
    "indicative" = indicative,
    "Indiv_loop" = loop,
    "cleaning_log_hh" = log_df,
    "cleaning_log_loop" = log_indiv_df,
    "deletion_log" = deleted_redacted
  )

write.xlsx(mcna_datasets, file = "output/cleaned_data/mcna_data_clean_2020-09-24_finalprelim.xlsx")

