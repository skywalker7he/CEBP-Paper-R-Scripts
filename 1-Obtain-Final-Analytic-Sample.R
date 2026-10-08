# Set working directory

# Set path to data
# data_path <- 

dat <- read.csv(paste0(data_path, 
                       "/IrinotecanAndOpiates_DATA_2025-06-24_2337.csv"))
dat$seersitedesc[dat$seersitedesc == ""] <- "pancreatic"
dat$chronic_opiate_use[dat$record_id == 238] <- 0

dat_real <- dat %>% filter(
  !comment %in% c(
    "*MRN does not pull up a patient",
    "*MRN does not pull up patient",
    "did not pull a chart",
    "MRN does not pull up patient",
    "MRN does not pull up a patient",
    "NO CHART ATTACHED TO MRN ",
    "MRN DOES NOT PULL CHART",
    "NO CHART FOR MRN ",
    "no chart attached to mrn ",
    "mrn not attached to chart",
    "NO CHART TO MRN ",
    "no records...",
    "no records ", 
    "records dont go that far back , cant confirm")
)

dat_da <- dat_real %>% 
  
  # Keep patients with non-missing chronic opiate use
  # and exclude primary cancer site = Liver only
  filter(
    !is.na(chronic_opiate_use)
    # ,
    # is.na(seersitedesc) |
    # str_trim(str_to_lower(seersitedesc)) != "liver"
  ) %>% 
  
  mutate(
    
    date_birth = as.Date(dob, format = "%Y-%m-%d"),
    date_start_irinotecan = as.Date(dt_mindt, format = "%Y-%m-%d"),
    date_end_irinotecan = as.Date(dt_maxdt, format = "%Y-%m-%d"),
    date_death = as.Date(dt_dod, format = "%Y-%m-%d"),
    
    
    race_3c = case_when(
      race == "Asian" ~ "Other",
      race == "Black" ~ "Black",
      race == "Black or African American" ~ "Black",
      race == "BLACK OR AFRICAN AMERICAN" ~ "Black",
      race == "Other" ~ "Other",
      race == "OTHER" ~ "Other",
      race == "Other,White" ~ "Other",
      race == "Unknown" ~ NA_character_,
      race == "White" ~ "White",
      race == "WHITE" ~ "White",
      TRUE ~ NA_character_
    ),
    

    Hispanic = case_when(
      ethnicity == "Hispanic, Latino/a, or Spanish origin" ~ 1,
      ethnicity == "Not Hispanic, Latino/a, or Spanish origin" ~ 0,
      ethnicity == "Unknown" ~ NA_real_,
      TRUE ~ NA_real_
    ),
    
    cancer_type = case_when(
  
      is.na(seersitedesc) |
        str_trim(seersitedesc) == "" ~ NA_character_,
      
      

      str_detect(
        seersitedesc,
        regex(
          "colon|rectum|rectosigmoid|sigmoid|cecum|appendix",
          ignore_case = TRUE
        )
      ) ~ "Colon",
      

      str_detect(
        seersitedesc,
        regex(
          "pancreas|pancreatic|pancreatic duct",
          ignore_case = TRUE
        )
      ) ~ "Pancreatic",
      
      

      str_detect(
        seersitedesc,
        regex(
          "esophagus|esophageal|ge junction",
          ignore_case = TRUE
        )
      ) ~ "Esophageal",
      
      str_detect(
        seersitedesc,
        regex(
          "stomach|gastric|cardia|antrum|curvature",
          ignore_case = TRUE
        )
      ) ~ "Gastric",
      
      str_detect(
        seersitedesc,
        regex(
          "small intestine|jejunum|duodenum|ileum",
          ignore_case = TRUE
        )
      ) ~ "Small intestine",
      
    
      # Hepatobiliary / ampulla
      #
      # Primary site "Liver" has already been excluded above.
      # Mentions of liver as a metastatic site do not affect
      # classification because Colon/Pancreatic rules occur first.
      str_detect(
        seersitedesc,
        regex(
          "liver|cholangiocarcinoma|bile duct|ampulla of vater",
          ignore_case = TRUE
        )
      ) ~ "Hepatobiliary",
    
      str_detect(
        seersitedesc,
        regex(
          "^other digestive",
          ignore_case = TRUE
        )
      ) ~ "Other digestive",
      
      
      TRUE ~ NA_character_
    ),
    
  
    diarrhea_grading = if_else(
      diarrhea_grading == 0,
      NA,
      diarrhea_grading
    ),
    
    cramping_severity = if_else(
      cramping_severity == 4,
      NA,
      cramping_severity
    )
  ) %>% 
  
  select(
    record_id,
    sex,
    race_3c,
    Hispanic,
    baseline_age = age_minage,
    cancer_type,
    chronic_opiate_use,
    post_op_opiates_only,
    dose_reduction_3_months,
    dose_delay,
    diarrhea,
    diarrhea_grading,
    hospitalization,
    cramping,
    cramping_severity,
    gi_infection,
    opidrugs,
    escalating_doses_after
  )


# Create final analysis dataset

dat_da_final <- dat_da %>% 
  
  # Exclude patients with post-op opiates only
  filter(
    is.na(post_op_opiates_only) |
      post_op_opiates_only == 0
  ) %>% 
  
  mutate(
    
    Sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Female", "Male")
    ),
    
    Race = factor(
      case_when(
        race_3c %in% c("Black", "Other") ~ "Black or other",
        race_3c == "White" ~ "White",
        TRUE ~ NA_character_
      ),
      levels = c(
        "White",
        "Black or other"
      )
    ),
    
    Cancer_type = factor(
      case_when(
        cancer_type == "Colon" ~ "Colon",
        
        cancer_type == "Pancreatic" ~ "Pancreatic",
        
        cancer_type %in% c(
          "Esophageal",
          "Gastric",
          "Hepatobiliary",
          "Small intestine",
          "Other digestive"
        ) ~ "Other GI malignancies",
        
        TRUE ~ NA_character_
      ),
      levels = c(
        "Colon",
        "Pancreatic",
        "Other GI malignancies"
      )
    ),
    
    
    Chronic_opiate_use = factor(
      case_when(
        chronic_opiate_use == 0 ~ "No",
        chronic_opiate_use == 1 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    
    Dose_reduction = factor(
      case_when(
        dose_reduction_3_months == 0 ~ "No",
        dose_reduction_3_months == 1 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    Dose_delay = factor(
      case_when(
        dose_delay == 0 ~ "No",
        dose_delay == 1 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    Diarrhea = factor(
      case_when(
        diarrhea == 0 ~ "No",
        diarrhea == 1 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    Hospitalization = factor(
      case_when(
        hospitalization == 0 ~ "No",
        hospitalization == 1 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    Cramping = factor(
      case_when(
        cramping == 0 ~ "No",
        cramping == 1 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    Opiates_increase_after_irinotecan = factor(
      case_when(
        escalating_doses_after == 0 ~ "No",
        escalating_doses_after == 1 ~ "Yes",
        TRUE ~ NA_character_
      ),
      levels = c(
        "No",
        "Yes"
      )
    )
  ) %>% 
  
  select(
    record_id,
    Sex,
    Race,
    Age = baseline_age,
    Cancer_type,
    Chronic_opiate_use,
    Dose_reduction,
    Dose_delay,
    Diarrhea,
    Hospitalization,
    Cramping,
    dose_delay,
    dose_reduction_3_months,
    cramping,
    Opiates_increase_after_irinotecan
  )
