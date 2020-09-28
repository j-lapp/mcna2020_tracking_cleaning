





compare_columns_valuesLimit_col <- function(df,
                                            cn,
                                            condition,
                                            question_label,
                                            issue_label,
                                            columns,
                                            old_value,
                                            action_label,
                                            index) {
  varname <- paste("", cn , sep = "")
  data <- df %>%
    mutate(
      issue = issue_label,
      flag_index = index,
      log_number = 0,
      question.name = question_label,
      changed = NA,
      old.value = case_when(eval(parse(text = condition)) ~ as.character(eval(
        parse(text = old_value)
      )),
      TRUE ~ "Okay"),
      new.value = NA,!!varname := case_when(eval(parse(text = condition)) ~ action_label,
                                            TRUE ~ "Okay")
    )
  
  filterDate <- data %>%
    dplyr::select(columns) %>%  filter(eval(parse(text =  paste("data$", cn , sep =
                                                                  ""))) == 'Flagged' |
                                         eval(parse(text =  paste("data$", cn , sep = ""))) == 'Deleted'
                                       |
                                         eval(parse(text =  paste("data$", cn , sep =
                                                                    ""))) == 'Translate')
  
  return(filterDate)
}

compare_columns_values <- function(df, cn, condition) {
  varname <- paste("", cn , sep = "")
  
  data <- df %>%
    mutate(!!varname := case_when(eval(parse(text = condition)) ~ "Flagged",
                                  TRUE ~ "Okay"))
  
  return(data)
}

# this function is handling datetime diffirence
interview_time_handler <- function(df) {
  new_df <- df %>%
    mutate(time_spend = difftime(
      as.POSIXct(df$end, format = "%Y-%m-%dT%H:%M:%OS"),
      as.POSIXct(df$start, format = "%Y-%m-%dT%H:%M:%OS"),
      units = 'mins'
    ))
  return(new_df)
}

# this function is handling date diffirence
interview_date_handler <- function(df) {
  new_df <- df %>%
    mutate(arrival_displace_date_diff = difftime(
      as.POSIXct(df$arrival_date_idp, format = "%Y-%m-%d"),
      as.POSIXct(df$displace_date_idp, format = "%Y-%m-%d"),
      units = 'days'
    ))
  
  return(new_df)
}

read_conditions_from_excel_column <- function(df, conditionDf) {
  counter <- 0
  for (row in 1:nrow(conditionDf)) {
    result_col <- conditionDf[row, "new_column_name"]
    conditions  <- conditionDf[row, "conditions"]
    type  <- conditionDf[row, "type"]
    
    if (type == "date-time") {
      df_result <- interview_time_handler(df_result)
      df_result <-
        compare_columns_values(df_result, result_col, paste(conditions))
      
    } else if (type == "normal") {
      if (counter == 0) {
        df_result <-
          compare_columns_values(df, result_col, paste(conditions))
        
        counter <- counter + 1
        
      } else {
        df_result <-
          compare_columns_values(df_result, result_col, paste(conditions))
        
      }
    } else if (type == "date") {
      df_result <- interview_date_handler(df_result)
      df_result <-
        compare_columns_values(df_result, result_col, paste(conditions))
      
    }
  }
  
  return(df_result)
}

read_conditions_from_excel_limited_row <-
  function(df, conditionDf, idf) {
    df_total = data.frame()
    
    columns <-
      c(
        "log_number",
        "uuid",
        "cluster_location_id",
        "date_assessment",
        "governorate_mcna",
        "district_mcna",
        "enumerator_num",
        "question.name",
        "issue",
        "action",
        "changed",
        "old.value",
        "new.value",
        "flag_index"
      )
    
    for (row in 1:nrow(conditionDf)) {
      result_col <- conditionDf[row, "result_column_name"]
      conditions  <- conditionDf[row, "conditions"]
      type  <- conditionDf[row, "type"]
      
      question_label  <- conditionDf[row, "question.name"]
      
      issue_label  <- conditionDf[row, "issue_label"]
      
      old_value  <- conditionDf[row, "old_value"]
      
      action_label  <- conditionDf[row, "action_label"]
      
      
      if (type == "date-time") {
        df <- interview_time_handler(df)
        flagged_rows <-
          compare_columns_valuesLimit_col(
            df,
            result_col,
            paste(conditions),
            question_label,
            issue_label,
            columns,
            old_value,
            action_label,
            row
          )
        
        new_df <- data.frame(flagged_rows)
        df_total <- rbind(df_total, new_df)
      } else if (type == "normal") {
        flagged_rows <-
          compare_columns_valuesLimit_col(
            df,
            result_col,
            paste(conditions),
            question_label,
            issue_label,
            columns,
            old_value,
            action_label,
            row
          )
        
        new_df <- data.frame(flagged_rows)
        df_total <- rbind(df_total, new_df)
      } else if (type == "date") {
        df <- interview_date_handler(df)
        flagged_rows <-
          compare_columns_valuesLimit_col(
            df,
            result_col,
            paste(conditions),
            question_label,
            issue_label,
            columns,
            old_value,
            action_label,
            row
          )
        
        new_df <- data.frame(flagged_rows)
        df_total <- rbind(df_total, new_df)
      }
    }
    
    return(df_total)
  }

read_logs <- function(df, logDF, conditionDf) {
  df_total = data.frame()
  for (row in 1:nrow(logDF)) {
    flag_index <- logDF[row, "flag_index"]
    action  <- logDF[row, "action"]
    changed  <- logDF[row, "changed"]
    new_value  <- logDF[row, "new.value"]
    question_name <- logDF[row, "question.name"]
    
    current_condtion <-
      conditionDf[as.numeric(flag_index), "conditions"]
    current_type <- conditionDf[as.numeric(flag_index), "type"]
    old_value <- conditionDf[as.numeric(flag_index), "old_value"]
    X_uuid <-  logDF[row, "X_uuid"]
    
    print(row)
    
    aa <- paste("df$", question_name , sep = "")
    
    df[, question_name] <-
      ifelse(df$x_uuid == X_uuid, new_value, eval(parse(text = aa)))
  }
  
  
  return(df)
}
replaced_df <- function(df, translation) {
  df <- df %>%
    mutate(
      resons_not_attend_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "resons_not_attend_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      employment_primary_barriers_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "employment_primary_barriers_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      hh_main_risks_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "hh_main_risks_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      nfi_priority_needs_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "nfi_priority_needs_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      primary_livelihood_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "primary_livelihood_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      info_aid_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "info_aid_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      info_provider_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "info_provider_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      aid_not_satisfied_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "aid_not_satisfied_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      aid_workers_whyno = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "aid_workers_whyno" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      drinking_water_source_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "drinking_water_source_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      food_source_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "food_source_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      info_mode_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "info_mode_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      latrines_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "latrines_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      other_reason_for_debt = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "other_reason_for_debt" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      shelter_beter_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "shelter_beter_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      shelter_better_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "shelter_better_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      shelter_type_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "shelter_type_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      why_not_return_other = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "why_not_return_other" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ "NA"
      ),
      idp_first_place = case_when(
        translation$question.name[match(df$uuid, translation$X_uuid)] == "idp_first_place" ~ translation$new.value[match(df$uuid, translation$X_uuid)],
        TRUE ~ as.character(idp_first_place)
      )
    )
  df <- df %>%
    mutate(
      `reasons_not_attend.school_closed` = case_when(
        resons_not_attend_other == "school_closed" ~ 1 ,
        TRUE ~ as.numeric(`reasons_not_attend.school_closed`)
      ),
      `reasons_not_attend.cannot_go_physically` = case_when(
        resons_not_attend_other == "cannot_go_physically" ~ 1 ,
        TRUE ~ as.numeric(`reasons_not_attend.cannot_go_physically`)
      ),
      `reasons_not_attend.children_working` = case_when(
        resons_not_attend_other == "children_working" ~ 1 ,
        TRUE ~ as.numeric(`reasons_not_attend.children_working`)
      ),
      `reasons_not_attend.curriculum` = case_when(
        resons_not_attend_other == "curriculum" ~ 1,
        TRUE ~ as.numeric(`reasons_not_attend.curriculum`)
      ),
      `reasons_not_attend.impossible_to_enrol` = case_when(
        resons_not_attend_other == "impossible_to_enroll" ~ 1,
        TRUE ~ as.numeric(`reasons_not_attend.impossible_to_enrol`)
      ),
      `reasons_not_attend.lack_doc` = case_when(
        resons_not_attend_other == "lack_doc" ~ 1,
        TRUE ~ as.numeric(`reasons_not_attend.lack_doc`)
      ),
      `reasons_not_attend.not_safe` = case_when(
        resons_not_attend_other == "not_safe" ~ 1,
        TRUE ~ as.numeric(`reasons_not_attend.not_safe`)
      ),
      `reasons_not_attend.poor_infrastructure` = case_when(
        resons_not_attend_other == "poor_infrastructure" ~ 1,
        TRUE ~ as.numeric(`reasons_not_attend.poor_infrastructure`)
      ),
      `reasons_not_attend.other` = case_when(
        resons_not_attend_other %in% c("to be enrolled next year", "COVID 19 related") ~ 1,
        TRUE ~ as.numeric(`reasons_not_attend.other`)
      )
    )
  df <- df %>%
    mutate(
      `hh_main_risks.lack_funds` = case_when(
        hh_main_risks_other == "lack_funds" ~ 1,
        TRUE ~ as.numeric(`hh_main_risks.lack_funds`)
      ),
      `hh_main_risks.inadequate` = case_when(
        hh_main_risks_other == "inadequate" ~ 1,
        TRUE ~ as.numeric(`hh_main_risks.inadequate`)
      ),
      `hh_main_risks.no_agreement` = case_when(
        hh_main_risks_other == "no_agreement" ~ 1,
        TRUE ~ as.numeric(`hh_main_risks.no_agreement`)
      )
    )
  df <- df %>%
    mutate(
      `primary_livelihood_employment` = case_when(
        primary_livelihood_other == "employment" ~ 1,
        TRUE ~ as.numeric(`primary_livelihood_employment`)
      ),
      `primary_livelihood_retirement_pension` = case_when(
        primary_livelihood_other == "retirement_pension" ~ 1,
        TRUE ~ as.numeric(`primary_livelihood_retirement_pension`)
      ),
      `primary_livelihood.social_service` = case_when(
        primary_livelihood_other == "social_service" ~ 1,
        TRUE ~ as.numeric(`primary_livelihood.social_service`)
      ),
      `info_aid.aid` = case_when(info_aid_other == "aid" ~ 1, TRUE ~ as.numeric(`info_aid.aid`)),
      `info_provider.other` = case_when(
        is.na(info_provider_other) ~ as.numeric(`info_provider.other`),
        TRUE ~ 1
      ),
      `aid_not_satisfied.quality` = case_when(
        aid_not_satisfied_other == "quality" ~ 1,
        TRUE ~ as.numeric(`aid_not_satisfied.quality`)
      )
    )
  
  df <- df %>%
    mutate(
      `nfi_priority_needs.mattresses_sleeping_mats` = case_when(
        grepl("mattresses_sleeping_mats", nfi_priority_needs_other) ~ 1,
        TRUE ~ as.numeric(`nfi_priority_needs.mattresses_sleeping_mats`)
      ),
      `nfi_priority_needs.cooking_stove` = case_when(
        grepl("cooking_stove", nfi_priority_needs_other) ~ 1,
        TRUE ~ as.numeric(`nfi_priority_needs.cooking_stove`)
      ),
      `nfi_priority_needs.winter_heaters` = case_when(
        grepl("winter_heaters", nfi_priority_needs_other) ~ 1,
        TRUE ~ as.numeric(`nfi_priority_needs.winter_heaters`)
      ),
      `nfi_priority_needs.heating_cooking_fuel` = case_when(
        grepl("heating_cooking_fuel", nfi_priority_needs_other) ~ 1,
        TRUE ~ as.numeric(`nfi_priority_needs.heating_cooking_fuel`)
      ),
      `nfi_priority_needs.other` = case_when(
        grepl("freezer", `nfi_priority_needs.other`) |
          grepl("refrigerator", `nfi_priority_needs.other`) |
          grepl("water_filter", `nfi_priority_needs.other`) |
          grepl("airconditioner", `nfi_priority_needs.other`) |
          grepl("washing_machine", `nfi_priority_needs.other`) |
          grepl("closet", `nfi_priority_needs.other`) |
          grepl("water_boiler", `nfi_priority_needs.other`) |
          grepl("tv", `nfi_priority_needs.other`) |
          grepl("sewing_machine", `nfi_priority_needs.other`) ~ 1,
        TRUE ~ as.numeric(`nfi_priority_needs.other`)
      )
    )
  df <- df %>%
    mutate(
      `nfi_priority_needs.none` = case_when(
        as.numeric(`nfi_priority_needs.bedding_items`) == 0 &
          as.numeric(`nfi_priority_needs.mattresses_sleeping_mats`) == 0 &
          as.numeric(`nfi_priority_needs.blankets`) == 0 &
          as.numeric(`nfi_priority_needs.cooking_utensils`) == 0 &
          as.numeric(`nfi_priority_needs.cooking_stove`) == 0 &
          as.numeric(`nfi_priority_needs.winter_heaters`) == 0 &
          as.numeric(`nfi_priority_needs.clothing`) == 0 &
          as.numeric(`nfi_priority_needs.heating_cooking_fuel`) == 0 &
          as.numeric(`nfi_priority_needs.other`) == 0 ~ 1,
        TRUE ~ as.numeric(`nfi_priority_needs.none`)
      )
    )
  return(df)
}

replaced_indiv_df <- function(indiv_df, log_indiv_df) {
  indiv_df <- indiv_df %>%
    mutate(`health_issue_other` = log_indiv_df$new.value[match(indiv_df$`X_index`, log_indiv_df$X_index)])
  indiv_df <- indiv_df %>%
    mutate(
      `health_issue.conflict_related` = case_when(
        health_issue_other == "conflict_related" ~ 1,
        TRUE ~ as.numeric(`health_issue.conflict_related`)
      ),
      `health_issue.chronic` = case_when(
        health_issue_other == "chronic" ~ 1,
        TRUE ~ as.numeric(`health_issue.chronic`)
      ),
      `health_issue.communicable` = case_when(
        health_issue_other == "communicable" ~ 1,
        TRUE ~ as.numeric(`health_issue.communicable`)
      ),
    )
  indiv_df <- indiv_df %>%
    mutate(
      `health_issue.other` = case_when(
        `health_issue.conflict_related` == 0 &
          `health_issue.communicable` == 0 &
          `health_issue.chronic` == 0 &
          !is.na(health_issue_other) ~ 1,
        TRUE ~ as.numeric(`health_issue.other`)
      ),
      `health_issue.none` = case_when(
        `health_issue.conflict_related` == 0 &
          `health_issue.communicable` == 0 &
          `health_issue.chronic` == 0 &
          `health_issue.other` == 0 ~ 1,
        TRUE ~ as.numeric(`health_issue.none`)
      )
    )
  
  return(indiv_df)
}




recoded_handler <- function(df, indiv_df) {
  # get sex column form member sheet
  loop_hoh <- indiv_df[which(indiv_df$relationship == "head"), ]
  loop_females <- indiv_df[which(indiv_df$sex == "female"), ]
  loop_females <-
    indiv_df %>% mutate(plw = ifelse(pregnant_lactating == "yes", 1, 0))
  
  df$sex <-
    loop_hoh$sex[match(df$X_uuid, loop_hoh$X_submission__uuid)]
  loop_children <- indiv_df[which(indiv_df$age < 18), ]
  df$femal_hh <- ifelse(df$sex == "female", 1, 0)
  
  df$a7 <- ifelse(as.numeric(df$num_hh_member) %% 1 == 0, 1, 0)
  df$a8 <- ifelse(as.numeric(df$num_family_member) %% 1 == 0, 1,
                  0)
  df$a9_male <-
    as.numeric(df$tot_male) / (as.numeric(df$tot_male) + as.numeric(df$tot_female))
  df$a9_female <-
    as.numeric(df$tot_female) / (as.numeric(df$tot_male) + as.numeric(df$tot_female))
  
  df$a10_child <-
    as.numeric(df$tot_child) / (as.numeric(df$tot_male) +  as.numeric(df$tot_female))
  df$a10_adult <-
    (as.numeric(df$male_18_59_calc) +  as.numeric(df$female_18_59_calc)) / (as.numeric(df$tot_male) +  as.numeric(df$tot_female))
  
  df$a11 <-
    ifelse(
      loop_hoh[match(df$X_uuid, loop_hoh$X_submission__uuid), "marital_status"] %in%
        c("single", "separated", "widowed", "divorced"),
      1,
      ifelse(loop_hoh[match(df$X_uuid, loop_hoh$X_submission__uuid), "marital_status"] %in%
               c(NA, ""), NA, 0)
    )
  
  df$a12 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(loop_children$marital_status[which(loop_children$X_submission__uuid == x["X_uuid"])] == "married"), 1, 0)
    }
  )
  
  df$a13 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] < 18 &
                   indiv_df$work[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  
  df$a14 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$sex[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "female"), 1, 0)
    }
  )
  
  
  df$a16 <- ifelse(
    df$shelter_type_inperson %in%
      c(
        "unfinished_abandoned_building",
        "damaged_building",
        "tent",
        "religious_building",
        "public_building",
        "non_residential",
        "container",
        "makeshift_shelter"
      ),
    1,
    0
  )
  
  df$a17 <- ifelse(df$enclosure_issues %in% c("none"), 1, 0)
  
  df$a22 <-
    ifelse(df$displaced_again %in% c("decline_to_answer", 'no', NA), 0, 1)
  df$a24 <- ifelse(df$movement_intentions %in% c("remain"), 1, 0)
  df$a25 <- ifelse(df$movement_intentions12 %in% c("current"), 1, 0)
  df$a26 <- ifelse(df$movement_intentions12 %in% c("current"), 1, 0)
  df$a27 <- ifelse(df$movement_intentions_b %in% c("remain"), 1, 0)
  df$a28 <-
    ifelse(df$movement_intentions_b12 %in% c("remain"), 1, 0)
  df$a29 <-
    ifelse(df$local_integration %in% c("do_not_know", "decline_to_answer", "no"),
           0,
           1)
  
  
  # ################################################### b #######################################
  
  # df$b1 <- ifelse(rowSums(df[, c("inc_employment", "inc_pension")], na.rm=T) < 480000, 1, 0)
  
  df$b2 <-
    ifelse(df$primary_livelihood.ngo_charity_assistance == 1, 1, 0)
  df$b3 <- ifelse(df$inc_employment_pension %in% c(NA, 0), 0, 1)
  df$b5 <-
    ifelse(
      df$selling_assets %in% c("no_already_did", "yes") |
        df$borrow_debt  %in% c("no_already_did", "yes") |
        df$reduce_spending %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$b6 <-
    ifelse(
      df$selling_transportation_means %in% c("no_already_did", "yes") |
        df$change_place  %in% c("no_already_did", "yes") |
        df$child_work %in% c("no_already_did", "yes"),
      1,
      0
    )
  df$b7 <-
    ifelse(
      df$child_dropout_school %in% c("no_already_did", "yes") |
        df$adult_risky  %in% c("no_already_did", "yes") |
        df$family_migrating %in% c("no_already_did", "yes") |
        df$child_forced_marriage %in% c("no_already_did", "yes"),
      1,
      0
    )
  
  # ################################################### c #######################################
  
  # df$c2 <- ifelse(df$disab_explos %in% c("a_lot_of_difficulty", "cannot_do_at_all"), 1, 0)
  
  df$c3_i <-
    ifelse(df$difficulty_seeing %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
           1,
           0)
  df$c3_ii <-
    ifelse(df$difficulty_hearing %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
           1,
           0)
  df$c3_iii <-
    ifelse(df$difficulty_walking %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
           1,
           0)
  df$c3_iv <-
    ifelse(df$difficulty_remembering %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
           1,
           0)
  df$c3_v <-
    ifelse(df$difficulty_washing %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
           1,
           0)
  df$c3_vi <-
    ifelse(
      df$difficulty_communicating %in% c("a_lot_of_difficulty", "cannot_do_at_all"),
      1,
      0
    )
  
  df$c9 <- ifelse(df$difficulty_accessing_services == "yes", 1, 0)
  
  PLW <-
    as.data.frame(loop_females %>% dplyr::group_by(X_submission__uuid) %>% dplyr::summarize(sum(plw)))
  df$c11 <- PLW[match(df$X_uuid, PLW$X_submission__uuid), 2]
  
  
  # ################################################### d ########################################
  
  df$d1_i <- ifelse(df$info_aid.aid == 1, 1, 0)
  df$d1_ii <- ifelse(df$info_aid.safety == 1, 1, 0)
  df$d1_iii <- ifelse(df$info_aid.housing == 1, 1, 0)
  df$d1_iv <- ifelse(df$info_aid.livelihoods == 1, 1, 0)
  df$d1_v <- ifelse(df$info_aid.water == 1, 1, 0)
  df$d1_vi <- ifelse(df$info_aid.electricity == 1, 1, 0)
  df$d1_vii <- ifelse(df$info_aid.education == 1, 1, 0)
  df$d1_viii <- ifelse(df$info_aid.healthcare == 1, 1, 0)
  df$d1_ix <- ifelse(df$info_aid.legal == 1, 1, 0)
  df$d1_x <- ifelse(df$info_aid.property == 1, 1, 0)
  df$d1_xi <- ifelse(df$info_aid.uxo == 1, 1, 0)
  df$d1_xii <- ifelse(df$info_aid.documentation == 1, 1, 0)
  df$d1_xiii <- ifelse(df$info_aid.none == 1, 1, 0)
  df$d2_i <- ifelse(df$info_provider.ngo == 1, 1, 0)
  df$d2_ii <- ifelse(df$info_provider.friends_in_aoo == 1, 1, 0)
  df$d2_iii <-
    ifelse(df$info_provider.friends_visited_aoo == 1, 1, 0)
  df$d2_iv <-
    ifelse(df$info_provider.friends_not_been_in_aoo == 1, 1, 0)
  df$d2_v <- ifelse(df$info_provider.local_authorities == 1, 1, 0)
  df$d2_vi <-
    ifelse(df$info_provider.national_authorities == 1, 1, 0)
  df$d2_vii <- ifelse(df$info_provider.religious == 1, 1, 0)
  df$d2_viii <- ifelse(df$info_provider.mukhtars == 1, 1, 0)
  df$d2_ix <- ifelse(df$info_provider.sector_leaders == 1, 1, 0)
  df$d2_x <- ifelse(df$info_provider.schools == 1, 1, 0)
  df$d3_i <- ifelse(df$info_mode.mobile == 1, 1, 0)
  df$d3_ii <- ifelse(df$info_mode.direct_obs == 1, 1, 0)
  df$d3_iii <- ifelse(df$info_mode.face_cmmunic == 1, 1, 0)
  df$d3_iv <- ifelse(df$info_mode.television == 1, 1, 0)
  df$d3_v <- ifelse(df$info_mode.telephone == 1, 1, 0)
  df$d3_vi <- ifelse(df$info_mode.facebook_app == 1, 1, 0)
  df$d3_vii <- ifelse(df$info_mode.facebook_messenger == 1, 1, 0)
  df$d3_viii <- ifelse(df$info_mode.whatsapp == 1, 1, 0)
  df$d3_ix <- ifelse(df$info_mode.viber == 1, 1, 0)
  df$d3_x <- ifelse(df$info_mode.other_social == 1, 1, 0)
  df$d3_xi <- ifelse(df$info_mode.notice_board == 1, 1, 0)
  df$d3_xii <- ifelse(df$info_mode.newspapers == 1, 1, 0)
  df$d3_xiii <- ifelse(df$info_mode.leaflet == 1, 1, 0)
  df$d3_xiv <- ifelse(df$info_mode.loud_speakers == 1, 1, 0)
  df$d3_xv <- ifelse(df$info_mode.radio == 1, 1, 0)
  df$d4 <- ifelse(df$aid_received == "yes", 1, 0)
  df$d5_i <- ifelse(df$aid_type.cash == 1, 1, 0)
  df$d5_ii <- ifelse(df$aid_type.food == 1, 1, 0)
  df$d5_iii <- ifelse(df$aid_type.water == 1, 1, 0)
  df$d5_iv <- ifelse(df$aid_type.fuel == 1, 1, 0)
  df$d5_v <- ifelse(df$aid_type.shelter == 1, 1, 0)
  df$d5_vi <- ifelse(df$aid_type.seasonal_items == 1, 1, 0)
  df$d5_vii <- ifelse(df$aid_type.healthcare == 1, 1, 0)
  df$d5_viii <- ifelse(df$aid_type.other_nfi == 1, 1, 0)
  df$d5_ix <- ifelse(df$aid_type.education == 1, 1, 0)
  df$d5_x <- ifelse(df$aid_type.protection == 1, 1, 0)
  df$d6 <-
    ifelse(df$aid_satisfaction == "yes",
           1,
           ifelse(
             df$aid_satisfaction %in% c("decline_to_answer", "do_not_know", "no"),
             0,
             NA
           ))
  df$d7 <- ifelse(df$aid_not_satisfied.quantity == 1, 1, 0)
  df$d10 <-
    ifelse(df$aid_workers_satisfied == "",
           NA,
           ifelse(df$aid_workers_satisfied == "no", 1, 0))
  
  df$d15_i <- ifelse(df$covid_info_need == 'yes', 1, 0)
  df$d15_ii <-
    ifelse(df$covid_info_type %in% c("prevention measures", "prevention_measures"),
           1,
           0)
  
  # ################################################### g ########################################
  
  df$g4 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$attend_formal_ed[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "no"), 1, 0)
    }
  )
  df$g5 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$attend_informal_ed[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "no"), 1, 0)
    }
  )
  df$g4 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$attend_formal_ed[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "no"), 1, 0)
    }
  )
  
  df$g7_i <-
    ifelse(df$reasons_not_attend.cannot_afford %in% c(NA, 0), 0, 1)
  df$g7_ii <-
    ifelse(df$reasons_not_attend.school_closed %in% c(NA, 0), 0, 1)
  df$g7_iii <-
    ifelse(df$reasons_not_attend.not_safe %in% c(NA, 0), 0, 1)
  df$g7_iv <-
    ifelse(df$reasons_not_attend.impossible_to_enrol %in% c(NA, 0), 0, 1)
  df$g7_v <-
    ifelse(df$reasons_not_attend.cannot_go_physically %in% c(NA, 0),
           0,
           1)
  df$g7_vi <-
    ifelse(df$reasons_not_attend.overcrowded %in% c(NA, 0), 0, 1)
  df$g7_vii <-
    ifelse(df$reasons_not_attend.lack_of_staff %in% c(NA, 0), 0, 1)
  df$g7_viii <-
    ifelse(df$reasons_not_attend.poor_infrastructure %in% c(NA, 0), 0, 1)
  df$g7_ix <-
    ifelse(df$reasons_not_attend.curriculum %in% c(NA, 0), 0, 1)
  df$g7_x <-
    ifelse(df$reasons_not_attend.children_working %in% c(NA, 0), 0, 1)
  df$g7_xi <-
    ifelse(df$reasons_not_attend.parental_refusal %in% c(NA, 0), 0, 1)
  df$g7_xii <-
    ifelse(df$reasons_not_attend.uninterested %in% c(NA, 0), 0, 1)
  
  df$g8 <-
    ifelse(
      df$primary_school_place %in% c("between_2_5", "within_2km") &
        df$secondary_school_place %in% c("between_2_5", "within_2km"),
      1,
      0
    )
  df$g9 <- ifelse(df$covid_dropout %in% c(NA, 0), 0, 1)
  
  df$g14_i <- ifelse(df$cereals > 1, 1, 0)
  df$g14_ii <- ifelse(df$nuts_seed > 1, 1, 0)
  df$g14_iii <- ifelse(df$milk_dairy > 1, 1, 0)
  df$g14_iv <- ifelse(df$meat > 1, 1, 0)
  df$g14_v <- ifelse(df$vegetables > 1, 1, 0)
  df$g14_vi <- ifelse(df$fruits > 1, 1, 0)
  df$g14_vii <- ifelse(df$oil_fats > 1, 1, 0)
  df$g14_vii <- ifelse(df$sweets > 1, 1, 0)
  df$g14_ix <- ifelse(df$spices_condiments > 1, 1, 0)
  
  
  df$g14_x <- ifelse(
    df$food_source %in% c(
      "cash",
      "cash_assistance",
      "cridet",
      "gifts_family_friends",
      "inkind_labour",
      "begging",
      "aid_un_ngo_assistance",
      "food_assistance_gov",
      "own_production",
      "other"
    ),
    1,
    0
  )
  
  df$g15_i <- ifelse(df$no_food == "yes", 1, 0)
  df$g15_ii <-
    ifelse(df$no_food == "yes" & df$no_food_freq == "often", 1, 0)
  df$g15_iii <- ifelse(df$hungry == "yes", 1, 0)
  df$g15_iv <-
    ifelse(df$hungry == "yes" & df$hungry_freq == "often", 1, 0)
  df$g15_v <- ifelse(df$not_eating == "yes", 1, 0)
  df$g15_vi <-
    ifelse(df$not_eating == "yes" &
             df$not_eating_freq == "often", 1, 0)
  
  
  df$g19_i <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$attend_formal_ed[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  df$g19_ii <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$attend_informal_ed[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  df$g19_iii <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$work[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "yes"), 1, 0)
    }
  )
  
  
  df$g25 <-
    ifelse(df$distance_hospital %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  df$g26_i <-
    ifelse(df$distance_hospital %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  df$g26_ii <- ifelse(df$hospital_emergency_ser == "yes", 1, 0)
  df$g26_iii <- ifelse(df$hospital_maternity_ser == "yes", 1, 0)
  df$g26_iv <- ifelse(df$hospital_surgical_ser == "yes", 1, 0)
  df$g26_v <- ifelse(df$hospital_pediatric_ser == "yes", 1, 0)
  
  # df$g28 <- ifelse(df$distance_health_service %in% c("less_15", "less_30", "less_hour"), 1, 0)
  df$g29 <-
    ifelse(df$distance_hospital %in% c("less_15", "less_30", "less_hour"),
           1,
           0)
  df$g32 <- ifelse(df$women_specialised_services == "yes", 1, 0)
  
  df$g34_i <-
    ifelse(df$health_barriers.civ_docs_problems %in% c(NA, 0), 0, 1)
  df$g34_ii <- ifelse(df$health_barriers.cost %in% c(NA, 0), 0, 1)
  df$g34_iii <-
    ifelse(df$health_barriers.unqualified_staff %in% c(NA, 0), 0, 1)
  df$g34_iv <-
    ifelse(df$health_barriers.refused_treatment %in% c(NA, 0), 0, 1)
  df$g34_v <-
    ifelse(df$health_barriers.no_medicine %in% c(NA, 0), 0, 1)
  df$g34_vi <-
    ifelse(df$health_barriers.not_inclusive %in% c(NA, 0), 0, 1)
  df$g34_vii <-
    ifelse(df$health_barriers.no_offered_treatment %in% c(NA, 0), 0, 1)
  df$g34_viii <-
    ifelse(df$health_barriers.no_referral_phc %in% c(NA, 0), 0, 1)
  df$g34_ix <-
    ifelse(df$health_barriers.phc_closed %in% c(NA, 0), 0, 1)
  df$g34_x <-
    ifelse(df$health_barriers.distance_to_treatmentcenter %in% c(NA, 0),
           0,
           1)
  
  df$g37 <- ifelse(df$how_much_debt > 505000, 1, 0)
  df$g38 <-
    ifelse(
      df$reasons_for_debt %in% c("basic_hh_expenditure", "education", "food", "health"),
      1,
      0
    )
  
  df$g39_i <- ifelse(df$covid_loss_job == "yes", 1, 0)
  df$g39_ii <- ifelse(df$covid_loss_job_permanent %in% c(NA, 0) &
                        df$covid_loss_job_temp %in% c(NA, 0) ,
                      1,
                      0)
  
  df$g41 <-
    ifelse(df$market_place %in% c("less_15", "less_30"), 1, 0)
  
  df$g45_i <-
    ifelse(df$employment_primary_barriers.increased_competition == 1,
           1,
           0)
  df$g45_ii <-
    ifelse(df$employment_primary_barriers.jobs_far == 1, 1, 0)
  df$g45_iii <-
    ifelse(df$employment_primary_barriers.only_low_available == 1, 1, 0)
  df$g45_iv <-
    ifelse(df$employment_primary_barriers.underqualified_for_jobs == 1,
           1,
           0)
  df$g45_v <-
    ifelse(df$employment_primary_barriers.lack_of_connections == 1,
           1,
           0)
  df$g45_vi <-
    ifelse(df$employment_primary_barriers.none == 1, 1, 0)
  
  df$g35 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(sum(indiv_df$health_issue.chronic[which(indiv_df$X_submission__uuid == x["X_uuid"])], na.rm = T) > 0, 1, 0)
    }
  )
  
  df$g44 <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(
        any(
          indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] > 17 &
            indiv_df$work[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "no" &
            indiv_df$actively_seek_work[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "yes"
        ),
        1,
        0
      )
    }
  )
  
  df$g46 <- ifelse(df$employment_seasonal == "yes", 1, 0)
  
  df$g47_i <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] > 17 &
                   indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] < 60), 1, 0)
    }
  )
  
  df$g47_ii <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(
        indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] > 17 &
          indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] < 60 &
          indiv_df$sex[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "male"
      ),
      1,
      0)
    }
  )
  
  df$g47_iii <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(
        indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] > 17 &
          indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] < 60 &
          indiv_df$sex[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "female"
      ),
      1,
      0)
    }
  )
  df$g47_iv <- apply(
    df,
    1,
    FUN = function(x) {
      ifelse(any(
        indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] > 17 &
          indiv_df$age[which(indiv_df$X_submission__uuid == x["X_uuid"])] < 60 &
          indiv_df$work[which(indiv_df$X_submission__uuid == x["X_uuid"])] == "yes"
      ),
      1,
      0)
    }
  )
  
  
  df$g51 <- ifelse(
    df$pds_card == "no" |  df$id_card_a18 == "no" |
      df$nationality_cert_a18 == "no" |
      df$id_card_u18 == "no" |
      df$nationality_cert_u18 == "no" |
      df$birth_cert_u18 == "no" |
      df$birth_cert_u18 == "no",
    1,
    0
  )
  
  df$g52 <-
    ifelse(rowSums(df[, c(
      "disciplinary_measures.explained",
      "disciplinary_measures.forbid_privileges",
      "disciplinary_measures.shouted",
      "disciplinary_measures.spanked",
      "disciplinary_measures.no_answer"
    )], na.rm = T) > 0, 1, 0)
  
  df$g53a <-
    ifelse(df$hh_dispute %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g53a1 <-
    ifelse(df$hh_dispute %in% c("yes") &
             df$not_residing_num >= 1, 1, 0)
  
  df$g54_i <-
    ifelse(df$hh_dispute %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g54_ii <-
    ifelse(df$hh_dispute %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g54_iii <- ifelse(df$restriction_clearance %in% c(NA), 0, 1)
  df$g54_iv <-
    ifelse(
      df$restriction_clearance == "yes" &
        (
          df$restriction_clearance_covid == "similar" |
            df$restriction_clearance_covid == "no"
        ),
      1,
      0
    )
  
  df$g54_v <-
    ifelse(df$restriction_clearance_covid %in% c("no", "similer"), 1, 0)
  df$g54_vi <-
    ifelse(df$restriction_documents %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g54_vii <- ifelse(
    df$restriction_documents == "yes" &
      (
        df$restriction_documents_covid == "similar" |
          df$restriction_documents_covid == "no"
      ),
    1,
    0
  )
  df$g54_viii <-
    ifelse(df$restriction_time %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g54_viii <-
    ifelse(
      df$restriction_time_covid %in% c("decline_to_answer", 'no', "do_not_know", NA),
      0,
      1
    )
  df$g54_viii <-
    ifelse(df$restriction_reason %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g54_ix <-
    ifelse(df$restriction_reason_covid %in% c("similer"), 1, 0)
  df$g54_x <-
    ifelse(df$restriction_physical %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g54_xi <-
    ifelse(df$restriction_physical_covid %in% c("similer"), 1, 0)
  df$g54_xii <-
    ifelse(df$restriction_other %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  
  
  df$g56 <-
    ifelse(df$child_distress_number < 1 |
             is.na(df$child_distress_number),
           0,
           1)
  df$g57 <-
    ifelse(df$adult_distress_number < 1 |
             is.na(df$adult_distress_number),
           0,
           1)
  
  df$g63_i <-
    ifelse(df$feel_unsafe %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g63_ii <-
    ifelse(rowSums(df[, c(
      "unsafe_areas.facilities",
      "unsafe_areas.water_points",
      "unsafe_areas.social_areas",
      "unsafe_areas.distribution_areas",
      "unsafe_areas.markets",
      "unsafe_areas.way_to_centers",
      "unsafe_areas.way_to_school"
    )], na.rm = T) > 0, 1, 0)
  
  df$g61 <-
    ifelse(df$security_incident %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g62_i <-
    ifelse(df$security_incident_gender %in% c("male", 'female', "both"),
           1,
           1)
  
  df$g64 <- ifelse(df$hh_risk_eviction == "yes", 1, 0)
  
  df$g65_i <-
    ifelse(df$hh_main_risks.authorities_request == 1, 1, 0)
  df$g65_ii <- ifelse(df$hh_main_risks.lack_funds == 1, 1, 0)
  df$g65_iii <- ifelse(df$hh_main_risks.no_longer_hosted == 1, 1, 0)
  df$g65_iv <-
    ifelse(df$hh_main_risks.unaccepted_by_community == 1, 1, 0)
  df$g65_v <- ifelse(df$hh_main_risks.owner_request == 1, 1, 0)
  df$g65_vi <- ifelse(df$hh_main_risks.no_agreement == 1, 1, 0)
  df$g65_vii <- ifelse(df$hh_main_risks.inadequate == 1, 1, 0)
  df$g65_viii <- ifelse(df$hh_main_risks.occupied == 1, 1, 0)
  df$g65_ix <- ifelse(df$hh_main_risks.confiscation == 1, 1, 0)
  df$g65_x <- ifelse(df$hh_main_risks.dispute == 1, 1, 0)
  
  df$g66 <- ifelse(df$hlp_document == "yes", 1, 0)
  
  df$g67 <-
    ifelse(rowSums(df[, c(
      "why_not_return.fear_trauma",
      "why_not_return.lack_of_security_forces",
      "why_not_return.presence_of_mines",
      "why_not_return.discrimination",
      "why_not_return.lack_security_women",
      "why_not_return.lack_court",
      "why_not_return.movement_restrictions",
      "why_not_return.no_personal_id",
      "why_not_return.no_transport_return",
      "why_not_return.no_money_return",
      "why_not_return.lack_livelihoods_aoo",
      "why_not_return.house_damaged_destroyed",
      "why_not_return.house_land_occupied",
      "why_not_return.hh_assets_stolen_damaged",
      "why_not_return.local_markets_not_working",
      "why_not_return.basic_services_not_enough",
      "why_not_return.lack_of_education_oppotunities",
      "why_not_return.immediate_family_wont_return",
      "why_not_return.health_conditions",
      "why_not_return.children_enrolled_in_displacement",
      "why_not_return.living_conditions_better",
      "why_not_return.other",
      "why_not_return.do_not_know",
      "why_not_return.decline_to_answer"
    )], na.rm = T) > 0, 1, 0)
  
  df$g68 <-
    ifelse(df$hh_dispute %in% c("decline_to_answer", 'no', "do_not_know", NA),
           0,
           1)
  df$g73 <-
    ifelse(df$why_not_return.presence_of_mines %in% c(NA, 0), 0, 1)
  df$g74 <-
    ifelse(df$risk_education %in% c('no', "do_not_know", NA), 0, 1)
  
  df$g85 <- ifelse(rowSums(df[, c(
    "nfi_priority_needs.bedding_items",
    "nfi_priority_needs.mattresses_sleeping_mats",
    "nfi_priority_needs.blankets",
    "nfi_priority_needs.cooking_utensils",
    "nfi_priority_needs.cooking_stove",
    "nfi_priority_needs.winter_heaters",
    "nfi_priority_needs.clothing",
    "nfi_priority_needs.heating_cooking_fuel",
    "nfi_priority_needs.none"
    ,
    "nfi_priority_needs.other"
  )], na.rm = T) > 0, 1, 0)
  
  df$g89 <- ifelse(rowSums(df[, c(
    "shelter_better.protec_hazards",
    "shelter_better.improve_safety",
    "shelter_better.improve_privacy",
    "shelter_better.protect_climate",
    "shelter_better.none",
    "shelter_better.other"
  )], na.rm = T) > 0, 1, 0)
  
  df$g94_i <- ifelse(df$sufficient_water_drinking == "yes", 1, 0)
  df$g94_ii <- ifelse(df$sufficient_water_cooking == "yes", 1, 0)
  df$g94_iii <- ifelse(df$sufficient_water_hygiene == "yes", 1, 0)
  df$g94_iv <- ifelse(df$sufficient_water_other == "yes", 1, 0)
  
  df$g95 <- ifelse(rowSums(df[, c(
    "treat_drink_water_why.not_clear",
    "treat_drink_water_why.tastes_unpleasant",
    "treat_drink_water_why.smells_unpleasant",
    "treat_drink_water_why.none"
  )], na.rm = T) > 0, 1, 0)
  
  # df$g96 <- ifelse(df$treat_drink_water_how == "not_necessary", 0, 1)
  # df$g97 <- ifelse(rowSums(df[, c("latrines.flush", "latrines.vip_pit")], na.rm = T) > 0, 1, 0)
  df$g98 <-
    ifelse(df$access_hygiene_items  %in% c("satisfied", "very_satisfied"),
           1,
           0)
  df$g99 <-
    ifelse(df$access_hygiene_items == "yes" &
             df$use_of_soap.handwashing == 1,
           1,
           0)
  
  df$g100_i <- ifelse(df$tot_expenses %in% c(NA, 0), 0, 1)
  df$g100_ii <- ifelse(df$food_exp %in% c(NA, 0), 0, 1)
  df$g100_ii <- ifelse(df$rent_exp %in% c(NA, 0), 0, 1)
  df$g100_ii <- ifelse(df$medical_exp %in% c(NA, 0), 0, 1)
  
  df$g102_i <- ifelse(df$tot_expenses %in% c(NA, 0), 0, 1)
  df$g102_ii <- ifelse(df$food_exp %in% c(NA, 0), 0, 1)
  
  return(df)
}
