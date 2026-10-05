#Install packages:
install.packages(c("haven", "dplyr", "labelled"))
library(haven)
library(dplyr)
library(labelled)

alspac_data <- read_sav ("C:/ALSPAC/B4708_Melough_21Nov25.sav")
alspac_addition <- read_sav("C:/ALSPAC/Melough_B4708_11May26.sav")

#Explore the datasets:
dim(alspac_data)
dim(alspac_addition)  #number of rows and columns are different#

names(alspac_data)
names(alspac_addition)  #some columns exist in both datasets#

shared_names <- intersect(names(alspac_data), names(alspac_addition))
shared_names

setdiff(names(alspac_data), names(alspac_addition))

table(alspac_data$in_f11, useNA = "always")
table(alspac_addition$in_F11, useNA = "always")


#why does the counts for same variables differ?
compare_df <- merge(
  alspac_data[, c("cidB4708", "qlet", "in_f11")],
  alspac_addition[, c("cidB4708", "qlet", "in_F11")],
  by = c("cidB4708", "qlet")
)

table(compare_df$in_f11, compare_df$in_F11, useNA = "always")  #alspac_addition is more updated#


#now, test another variable:
compare_chol <- merge(
  alspac_data[, c("cidB4708", "qlet", "Chol_F24")],
  alspac_addition[, c("cidB4708", "qlet", "Chol_F24")],
  by = c("cidB4708", "qlet")
)

sum(!is.na(compare_chol$Chol_F24.x))
sum(!is.na(compare_chol$Chol_F24.y))
sum(compare_chol$Chol_F24.x != compare_chol$Chol_F24.y, na.rm = TRUE) #identical in both datasets#


#check other shared variables as well:
id_cols <- c("cidB4708", "qlet")
check_cols <- setdiff(shared_names, id_cols)

merged_check <- merge(
  alspac_data[, c(id_cols, check_cols)],
  alspac_addition[, c(id_cols, check_cols)],
  by = id_cols
)

result <- sapply(check_cols, function(v) {
  x <- merged_check[[paste0(v, ".x")]]
  y <- merged_check[[paste0(v, ".y")]]
  all(x == y | (is.na(x) & is.na(y)), na.rm = TRUE) && sum(is.na(x) != is.na(y)) == 0
})

table(result)  #35 of the columns are different between two datasets#

names(result)[result == FALSE]

diff_cols <- names(result)[result == FALSE]

na_compare <- sapply(diff_cols, function(v) {
  x <- merged_check[[paste0(v, ".x")]]
  y <- merged_check[[paste0(v, ".y")]]
  c(n_nonmiss_data = sum(!is.na(x)),
    n_nonmiss_addition = sum(!is.na(y)))
})

t(na_compare)   #the differences for this 35 variables were not much.


#let's investigate the few differences:
mismatches <- merged_check[merged_check$kz011b.x != merged_check$kz011b.y |
                             (is.na(merged_check$kz011b.x) != is.na(merged_check$kz011b.y)),
                           c("cidB4708", "qlet", "kz011b.x", "kz011b.y")]
mismatches


#compare the old and the new variables we requested:
#first compare maternal education variables:
table(alspac_addition$c645a, useNA = "always")
val_labels(alspac_addition$c645a)

alspac_addition %>%
  count(c645a)


#create maternal education variable:
alspac_addition <- alspac_addition %>%
  mutate(
    mat_edu = case_when(
      c645a == 1 ~ "CSE",
      c645a == 2 ~ "Vocational",
      c645a == 3 ~ "O level",
      c645a == 4 ~ "A level",
      c645a == 5 ~ "Uni degree",
      TRUE ~ NA_character_
    ),
    mat_edu = factor(
      mat_edu,
      levels = c("CSE", "Vocational", "O level", "A level", "Uni degree"),
      ordered = TRUE
    ))

alspac_addition %>%
  count(mat_edu) #correct#

#take a look at the old requested variables:
n_items <- c("n4000","n4001","n4002","n4003","n4004","n4005","n4006",
             "n4007","n4008","n4009","n4010","n4011","n4012","n4013")

alspac_data %>%
  mutate(n_items_answered = rowSums(!is.na(across(all_of(n_items))))) %>%
  count(n_items_answered == 0)  #using c645a reduces missingness greatly##


#ethnicity variables:
val_labels(alspac_addition$YPH2012)
val_labels(alspac_addition$c804)

alspac_addition %>%
  count(YPH2012)

alspac_addition %>%
  count(c804)

#create ethnicity variable:
alspac_addition <- alspac_addition %>%
  mutate(
    ethn = case_when(
      YPH2012 == 1 ~ "White",
      YPH2012 == 2 ~ "non-white",
      c804 == 1 ~ "White",
      c804 == 2 ~ "non-white",
      TRUE ~ NA_character_
    ),
    ethn = factor(ethn,levels = c("White", "non-white"))
  )

alspac_addition %>%
  count(ethn)


#create household income variable:
var_label(alspac_addition$h470)
var_label(alspac_addition$j410)

alspac_addition %>% count(h470)
alspac_addition %>% count(j410)


#create the income variable:
alspac_addition <- alspac_addition %>%
  mutate(
    fam_income = case_when(
      !is.na(h470) & h470 == 1 ~ "<£100",
      !is.na(h470) & h470 == 2 ~ "£100 - £199",
      !is.na(h470) & h470 == 3 ~ "£200 - £299",
      !is.na(h470) & h470 == 4 ~ "£300 - £399",
      !is.na(h470) & h470 == 5 ~ "£400+",
      
      is.na(h470) & j410 == 1 ~ "<£100",
      is.na(h470) & j410 == 2 ~ "£100 - £199",
      is.na(h470) & j410 == 3 ~ "£200 - £299",
      is.na(h470) & j410 == 4 ~ "£300 - £399",
      is.na(h470) & j410 == 5 ~ "£400+",
      
      TRUE ~ NA_character_
    ),
    fam_income = factor(
      fam_income,
      levels = c("<£100", "£100 - £199", "£200 - £299", "£300 - £399", "£400+"),
      ordered = TRUE
    )
  )

alspac_addition %>% count(fam_income)


#create age at outcome and sex variables:
alspac_addition %>% count(as_factor(kz021))

alspac_addition %>%
  summarise(
    n = sum(!is.na(FKAR0010)),
    mean_months = mean(FKAR0010, na.rm = TRUE),
    min_months = min(FKAR0010, na.rm = TRUE),
    max_months = max(FKAR0010, na.rm = TRUE)
  )


alspac_addition <- alspac_addition %>%
  mutate(
    age_24_years = FKAR0010 / 12,
    sex = kz021
  )

alspac_addition %>%
  summarise(
    mean_years = mean(age_24_years, na.rm = TRUE),
    min_years = min(age_24_years, na.rm = TRUE),
    max_years = max(age_24_years, na.rm = TRUE)
  )

alspac_addition %>% count(sex)


#create smoking variable:
#ever smoked variables
alspac_addition %>% count(as_factor(YPD7000))
alspac_addition %>% count(as_factor(FKSM1010))

alspac_addition %>%
  filter(!is.na(YPD7000) & !is.na(FKSM1010)) %>%
  count(as_factor(YPD7000), as_factor(FKSM1010))

alspac_addition %>%
  mutate(ever_smoked_combined = coalesce(as_factor(FKSM1010), as_factor(YPD7000))) %>%
  count(ever_smoked_combined)


#create binary and 3-category smoking variables:
alspac_addition <- alspac_addition %>%
  mutate(
    ever_smoked = coalesce(as_factor(FKSM1010), as_factor(YPD7000)),
    smoked_30d  = coalesce(as_factor(FKSM1040), as_factor(YPD7010)),
    
    smoking_2cat = case_when(
      ever_smoked == "No"  ~ "Never",
      ever_smoked == "Yes" ~ "Ever",
      TRUE ~ NA_character_
    ),
    
    smoking_3cat = case_when(
      ever_smoked == "No"                          ~ "Never",
      ever_smoked == "Yes" & smoked_30d == "Yes"    ~ "Current",
      ever_smoked == "Yes" & smoked_30d == "No"     ~ "Former",
      TRUE ~ NA_character_
    )
  )

alspac_addition %>% count(smoking_2cat)   #10536 missing#
alspac_addition %>% count(smoking_3cat)   #10538 missing#


#create alcohol variable:
alspac_addition %>% count(as_factor(FKAL1010))
alspac_addition %>% count(as_factor(FKAL1020))
alspac_addition %>% count(as_factor(FKAL1021))
alspac_addition %>% count(as_factor(FKAL1022))

#create 5 (AUDIT-C) and 3 categories:
alspac_addition <- alspac_addition %>%
  mutate(
    audit_freq = case_when(
      FKAL1020 == 0 ~ 0,  # Never
      FKAL1020 == 1 ~ 1,  # Monthly or less
      FKAL1020 == 2 ~ 2,  # 2-4 times a month
      FKAL1020 == 3 ~ 3,  # 2-3 times a week
      FKAL1020 == 4 ~ 4,  # 4+ times a week
      TRUE ~ NA_real_
    )
  )

alspac_addition %>% count(FKAL1020, audit_freq)

alspac_addition <- alspac_addition %>%
  mutate(
    audit_quantity = case_when(
      FKAL1021 == 0 ~ 0,  # 1 or 2
      FKAL1021 == 1 ~ 1,  # 3 or 4
      FKAL1021 == 2 ~ 2,  # 5 or 6
      FKAL1021 == 3 ~ 3,  # 7 to 9
      FKAL1021 == 4 ~ 4,  # 10 or more
      TRUE ~ NA_real_
    ),
    audit_binge = case_when(
      FKAL1022 == 0 ~ 0,  # Never
      FKAL1022 == 1 ~ 1,  # Less than monthly
      FKAL1022 == 2 ~ 2,  # Monthly
      FKAL1022 == 3 ~ 3,  # Weekly
      FKAL1022 == 4 ~ 4,  # Daily or almost daily
      TRUE ~ NA_real_
    )
  )

alspac_addition %>% count(FKAL1021, audit_quantity)
alspac_addition %>% count(FKAL1022, audit_binge)

#now, build the total audit-c score and 3-category smoking variables:
alspac_addition <- alspac_addition %>%
  mutate(
    audit_c_total = audit_freq + audit_quantity + audit_binge,
    
    alcohol_freq_3cat = case_when(
      FKAL1020 == 0 | FKAL1020 == 1 ~ "Never or <monthly",
      FKAL1020 == 2                 ~ "2-4 times/month",
      FKAL1020 == 3 | FKAL1020 == 4 ~ "2+ times/week",
      TRUE ~ NA_character_
    )
  )

alspac_addition %>%
  summarise(
    n = sum(!is.na(audit_c_total)),
    mean = mean(audit_c_total, na.rm = TRUE),
    min = min(audit_c_total, na.rm = TRUE),
    max = max(audit_c_total, na.rm = TRUE)
  )

alspac_addition %>% count(alcohol_freq_3cat)  #I use alcohol_freq_3cat##



#build physical activity for sensitivity analysis:
alspac_addition %>% count(as_factor(YPB2040))
alspac_addition %>% count(as_factor(YPB2050))
alspac_addition %>% count(YPB2040)

#we use YPB2040:
alspac_addition <- alspac_addition %>%
  mutate(
    pa_freq_4cat = case_when(
      YPB2040 %in% c(1, 2) ~ "<=1/month",          # Never, Less than monthly
      YPB2040 %in% c(3, 4) ~ "2-4 times/month",    # Once a fortnight, Weekly
      YPB2040 == 5          ~ "2-4 times/week",
      YPB2040 == 6          ~ "5+/week",
      TRUE ~ NA_character_   # includes code 7, "unsure on frequency"
    ),
    pa_freq_4cat = factor(
      pa_freq_4cat,
      levels = c("<=1/month", "2-4 times/month", "2-4 times/week", "5+/week"),
      ordered = TRUE
    )
  )

alspac_addition %>% count(pa_freq_4cat)
##########################################################################
#check if everyone in the original data exists in the new data:
ids_data <- alspac_data %>% select(cidB4708, qlet)
ids_addition <- alspac_addition %>% select(cidB4708, qlet)

nrow(anti_join(ids_data, ids_addition, by = c("cidB4708", "qlet")))  #yes#


#create an identifier variable:
alspac_addition <- alspac_addition %>%
  mutate(id = paste(cidB4708, qlet, sep = "_"))

sum(duplicated(alspac_addition$id))  #no duplicates


#add one extra column to the alspac_addition dataset:
alspac_addition <- alspac_addition %>%
  left_join(alspac_data %>% select(cidB4708, qlet, in_cbl),
            by = c("cidB4708", "qlet"))

nrow(alspac_addition)
sum(!is.na(alspac_addition$in_cbl))

names(alspac_addition)


#Investigating twins:
alspac_addition %>%
  count(cidB4708) %>%
  filter(n>1) %>%
  nrow()

library(labelled)
val_labels(alspac_addition$mz014)


alspac_addition %>%
  count(in_phase5)
##########################################################################
#Vitamin D levels:
var_label(alspac_addition$vitdtot_F7)
var_label(alspac_addition$vitdtot_F9)
var_label(alspac_addition$vitdtot_F11)

alspac_addition %>%
  summarise(
    n_F7  = sum(!is.na(vitdtot_F7)),        #1054 kids#
    n_F9  = sum(!is.na(vitdtot_F9)),        #4437 kids#
    n_F11 = sum(!is.na(vitdtot_F11))        #1007 kids#
  )

alspac_addition %>%
  mutate(n_timepoints = rowSums(!is.na(across(c(vitdtot_F7, vitdtot_F9, vitdtot_F11))))) %>%
  count(n_timepoints)   #each child has only one measurement#

#create one single variable for vitD:
alspac_addition <- alspac_addition %>%
  mutate(vitd_combined = coalesce(vitdtot_F7, vitdtot_F9, vitdtot_F11))

sum(!is.na(alspac_addition$vitd_combined))  #6498 children had vitD measurement#

#descriptives:
alspac_addition %>%
  mutate(
    timepoint = case_when(
      !is.na(vitdtot_F7)  ~ "F7",
      !is.na(vitdtot_F9)  ~ "F9",
      !is.na(vitdtot_F11) ~ "F11",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(timepoint)) %>%
  group_by(timepoint) %>%
  summarise(
    n = n(),
    mean_vitd = mean(vitd_combined, na.rm = TRUE),
    sd_vitd = sd(vitd_combined, na.rm = TRUE)
  )  #mean(SD) of age 7 is very different from the other two time points. so, we create z-scores


#season adjusting vitamin D levels:
alspac_addition %>% count(f7001)
alspac_addition %>% count(f9001)
alspac_addition %>% count(fe001)


#combine month variables into one:
alspac_addition <- alspac_addition %>%
  mutate(
    vitd_month = case_when(
      !is.na(vitdtot_F7) ~ f7001,
      !is.na(vitdtot_F9) ~ f9001,
      !is.na(vitdtot_F11) ~ fe001,
      TRUE ~ NA_real_
    )
  )

sum(!is.na(alspac_addition$vitd_month))
sum(is.na(alspac_addition$vitd_month) & !is.na(alspac_addition$vitd_combined))
#3 kids with vitD measurement did not have month of measurement#

#see these kids:
alspac_addition %>%
  filter(is.na(vitd_month) & !is.na(vitd_combined)) %>%
  select(id, vitdtot_F7, vitdtot_F9, vitdtot_F11, f7001, f9001, fe001)


#create sine and cosine terms for month:
alspac_addition <- alspac_addition %>%
  mutate(
    month_sin = sin(2 * pi * vitd_month / 12),
    month_cos = cos(2 * pi * vitd_month / 12)
  )

alspac_addition %>%
  filter(!is.na(vitd_month)) %>%
  select(vitd_month, month_sin, month_cos) %>%
  distinct() %>%
  arrange(vitd_month)

#season-adjust month 9 measurement:
model_f9 <- lm(vitdtot_F9 ~ month_sin + month_cos, data = alspac_addition)
summary(model_f9)

alspac_addition <- alspac_addition %>%
  mutate(
    vitd_F9_adj = if_else(
      !is.na(vitdtot_F9) & !is.na(month_sin),
      residuals(model_f9)[match(id, alspac_addition$id[!is.na(vitdtot_F9) & !is.na(month_sin)])] + mean(vitdtot_F9, na.rm = TRUE),
      NA_real_
    )
  )

alspac_addition %>%
  summarise(
    n = sum(!is.na(vitd_F9_adj)),
    mean_before = mean(vitdtot_F9, na.rm = TRUE),
    mean_after = mean(vitd_F9_adj, na.rm = TRUE),
    sd_before = sd(vitdtot_F9, na.rm = TRUE),
    sd_after = sd(vitd_F9_adj, na.rm = TRUE)
  )

#season adjust month 7 and 11:
model_f7  <- lm(vitdtot_F7  ~ month_sin + month_cos, data = alspac_addition)
model_f11 <- lm(vitdtot_F11 ~ month_sin + month_cos, data = alspac_addition)

summary(model_f7)
summary(model_f11)


alspac_addition <- alspac_addition %>%
  mutate(
    vitd_F7_adj = if_else(
      !is.na(vitdtot_F7) & !is.na(month_sin),
      residuals(model_f7)[match(id, alspac_addition$id[!is.na(vitdtot_F7) & !is.na(month_sin)])] + mean(vitdtot_F7, na.rm = TRUE),
      NA_real_
    ),
    vitd_F11_adj = if_else(
      !is.na(vitdtot_F11) & !is.na(month_sin),
      residuals(model_f11)[match(id, alspac_addition$id[!is.na(vitdtot_F11) & !is.na(month_sin)])] + mean(vitdtot_F11, na.rm = TRUE),
      NA_real_
    )
  )

alspac_addition %>%
  summarise(
    n_F7  = sum(!is.na(vitd_F7_adj)),
    n_F11 = sum(!is.na(vitd_F11_adj))
  )

#check mean and SD:
alspac_addition %>%
  summarise(
    mean_F7_before = mean(vitdtot_F7, na.rm = TRUE),
    mean_F7_after  = mean(vitd_F7_adj, na.rm = TRUE),
    sd_F7_before   = sd(vitdtot_F7, na.rm = TRUE),
    sd_F7_after    = sd(vitd_F7_adj, na.rm = TRUE),
    
    mean_F11_before = mean(vitdtot_F11, na.rm = TRUE),
    mean_F11_after  = mean(vitd_F11_adj, na.rm = TRUE),
    sd_F11_before   = sd(vitdtot_F11, na.rm = TRUE),
    sd_F11_after    = sd(vitd_F11_adj, na.rm = TRUE)
  ) %>%
  View()

#create season-adjusted vitD column:
alspac_addition <- alspac_addition %>%
  mutate(
    vitd_adj_combined = coalesce(vitd_F7_adj, vitd_F9_adj, vitd_F11_adj)
  )

alspac_addition %>%
  summarise(
    n = sum(!is.na(vitd_adj_combined)),
    mean = mean(vitd_adj_combined, na.rm = TRUE),
    sd = sd(vitd_adj_combined, na.rm = TRUE)
  )

##########################################################################
#save the data so far:
saveRDS(alspac_addition, "alspac_addition_covariates_built.rds")
##########################################################################
#outcomes:
#lipid profile:
var_label(alspac_addition$Chol_F24)
var_label(alspac_addition$HDL_F24)
var_label(alspac_addition$LDL_F24)
var_label(alspac_addition$Trig_F24)

#convert them to mg/dl:
alspac_addition <- alspac_addition %>%
  mutate(
    Chol_F24_mgdl = Chol_F24 * 38.67,
    HDL_F24_mgdl  = HDL_F24  * 38.67,
    LDL_F24_mgdl  = LDL_F24  * 38.67,
    Trig_F24_mgdl = Trig_F24 * 88.57
  )


summary(alspac_addition$Chol_F24_mgdl)
summary(alspac_addition$HDL_F24_mgdl)
summary(alspac_addition$LDL_F24_mgdl)
summary(alspac_addition$Trig_F24_mgdl)   #TG and HDL have some weird values#

#check HDL and TG extreme values:
alspac_addition %>%
  filter(!is.na(HDL_F24_mgdl)) %>%
  arrange(HDL_F24_mgdl) %>%
  select(id, HDL_F24, HDL_F24_mgdl) %>%
  head(10)

alspac_addition %>%
  filter(!is.na(Trig_F24_mgdl)) %>%
  arrange(desc(Trig_F24_mgdl)) %>%
  select(id, Trig_F24, Trig_F24_mgdl) %>%
  head(10)

#create flag variables for these implausible values:
alspac_addition <- alspac_addition %>%
  mutate(
    hdl_extreme_flag  = if_else(!is.na(HDL_F24_mgdl)  & HDL_F24_mgdl  < 10,   TRUE, FALSE),
    trig_extreme_flag = if_else(!is.na(Trig_F24_mgdl) & Trig_F24_mgdl > 1000, TRUE, FALSE)
  )

alspac_addition %>% count(hdl_extreme_flag)
alspac_addition %>% count(trig_extreme_flag)


#check normal distribution:
install.packages("e1071")
library(e1071)

alspac_addition %>%
  summarise(
    across(c(Chol_F24_mgdl, HDL_F24_mgdl, LDL_F24_mgdl, Trig_F24_mgdl),
           list(skew = ~skewness(.x, na.rm = TRUE)))
  )                 #TG is severely skewed#


hist(alspac_addition$Chol_F24_mgdl)
hist(alspac_addition$HDL_F24_mgdl)
hist(alspac_addition$LDL_F24_mgdl)
hist(alspac_addition$Trig_F24_mgdl)  #skewed#


#log-transforming TG:
alspac_addition <- alspac_addition %>%
  mutate(Trig_F24_log = log(Trig_F24_mgdl))

alspac_addition %>%
  summarise(
    n = sum(!is.na(Trig_F24_log)),
    skew_before = skewness(Trig_F24_mgdl, na.rm = TRUE),
    skew_after = skewness(Trig_F24_log, na.rm = TRUE)
  )

hist(alspac_addition$Trig_F24_log)
##############################################
#glucose and insulin:
var_label(alspac_addition$Glucose_F24)
var_label(alspac_addition$Insulin_F24)


#convert glucose:
alspac_addition <- alspac_addition %>%
  mutate(Glucose_F24_mgdl = Glucose_F24 * 18.02)

alspac_addition %>%
  summarise(
    across(c(Glucose_F24_mgdl, Insulin_F24),
           list(n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm=TRUE),
                min = ~min(.x, na.rm=TRUE), max = ~max(.x, na.rm=TRUE),
                skew = ~skewness(.x, na.rm=TRUE)))
  ) %>%
  View()

summary(alspac_addition$Glucose_F24_mgdl)
hist(alspac_addition$Glucose_F24_mgdl)
hist(alspac_addition$Insulin_F24)

#take a closer look:
alspac_addition %>%
  filter(!is.na(Glucose_F24_mgdl)) %>%
  arrange(desc(Glucose_F24_mgdl)) %>%
  select(id, Glucose_F24, Glucose_F24_mgdl) %>%
  head(10)

alspac_addition %>%
  filter(!is.na(Insulin_F24)) %>%
  arrange(desc(Insulin_F24)) %>%
  select(id, Insulin_F24) %>%
  head(10)


#flag possible diabetic individuals for sensitivity analysis:
alspac_addition <- alspac_addition %>%
  mutate(
    glucose_diabetic_flag = if_else(!is.na(Glucose_F24_mgdl) & Glucose_F24_mgdl >= 126, TRUE, FALSE)
  )

alspac_addition %>% count(glucose_diabetic_flag)

#check glucose and insulin distribution without the flags:
alspac_addition %>%
  filter(!glucose_diabetic_flag) %>%
  summarise(
    n = sum(!is.na(Glucose_F24_mgdl)),
    mean = mean(Glucose_F24_mgdl, na.rm = TRUE),
    median = median(Glucose_F24_mgdl, na.rm = TRUE),
    skew = skewness(Glucose_F24_mgdl, na.rm = TRUE)
  )                     #not skewed without the flags#


alspac_addition %>%
  filter(!glucose_diabetic_flag) %>%
  summarise(
    n = sum(!is.na(Insulin_F24)),
    mean = mean(Insulin_F24, na.rm = TRUE),
    median = median(Insulin_F24, na.rm = TRUE),
    skew = skewness(Insulin_F24, na.rm = TRUE)
  )             #still skewed#


#log-transform insulin;
alspac_addition <- alspac_addition %>%
  mutate(
    Insulin_F24_log = log(Insulin_F24)
  )

alspac_addition %>%
  summarise(
    n = sum(!is.na(Insulin_F24_log)),
    skew = skewness(Insulin_F24_log, na.rm = TRUE)
  )

hist(alspac_addition$Insulin_F24_log)


#create HOMA-IR:
alspac_addition <- alspac_addition %>%
  mutate(
    HOMA_IR = (Glucose_F24 * Insulin_F24) / 22.5
  )

alspac_addition %>%
  summarise(
    n = sum(!is.na(HOMA_IR)),
    mean = mean(HOMA_IR, na.rm = TRUE),
    median = median(HOMA_IR, na.rm = TRUE),
    skew = skewness(HOMA_IR, na.rm = TRUE)
  )

hist(alspac_addition$HOMA_IR)  #skewed#

#log-transforming HOMA-IR:
alspac_addition <- alspac_addition %>%
  mutate(HOMA_IR_log = log(HOMA_IR))

alspac_addition %>%
  summarise(
    n = sum(!is.na(HOMA_IR_log)),
    skew_before = skewness(HOMA_IR, na.rm = TRUE),
    skew_after = skewness(HOMA_IR_log, na.rm = TRUE)
  )                         #fixed#
##############################################
#blood pressure:
alspac_addition %>%
  summarise(
    across(c(FKBP1030, FKBP1031),
           list(n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm=TRUE),
                min = ~min(.x, na.rm=TRUE), max = ~max(.x, na.rm=TRUE),
                skew = ~skewness(.x, na.rm=TRUE)))
  ) %>%
  View()

alspac_addition %>%
  filter(!is.na(FKBP1030)) %>%
  arrange(desc(FKBP1030)) %>%
  select(id, FKBP1030, FKBP1031) %>%
  head(10)

#rename the variables:
alspac_addition <- alspac_addition %>%
  mutate(
    sbp_24 = FKBP1030,
    dbp_24 = FKBP1031
  )
##############################################
#fat mass index:
var_label(alspac_addition$FKDX1001)
var_label(alspac_addition$FKMS1000)

#check the distributions:
alspac_addition %>%
  summarise(
    across(c(FKDX1001, FKMS1000),
           list(n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm=TRUE),
                min = ~min(.x, na.rm=TRUE), max = ~max(.x, na.rm=TRUE)))
  ) %>%
  View()   #fat mass has implausible values##

alspac_addition %>%
  filter(!is.na(FKDX1001)) %>%
  arrange(FKDX1001) %>%
  select(id, FKDX1001, FKMS1000) %>%
  head(10)

#flag one implausible value:
alspac_addition <- alspac_addition %>%
  mutate(
    fatmass_implausible_flag = if_else(id == "6135_A", TRUE, FALSE)
  )

alspac_addition %>% count(fatmass_implausible_flag)

#calculate FMI:
alspac_addition <- alspac_addition %>%
  mutate(
    fat_mass_kg = FKDX1001 / 1000,
    height_m    = FKMS1000 / 1000,
    fmi_24      = fat_mass_kg / (height_m^2)
  )

alspac_addition %>%
  summarise(
    n = sum(!is.na(fmi_24)),
    mean = mean(fmi_24, na.rm = TRUE),
    min = min(fmi_24, na.rm = TRUE),
    max = max(fmi_24, na.rm = TRUE),
    skew = skewness(fmi_24, na.rm = TRUE)
  )
hist(alspac_addition$fmi_24)
summary(alspac_addition$fmi_24)

#check skewness without the flagged one:
alspac_addition %>%
  filter(!fatmass_implausible_flag) %>%
  summarise(
    n = sum(!is.na(fmi_24)),
    min = min(fmi_24, na.rm = TRUE),
    mean = mean(fmi_24, na.rm = TRUE),
    median = median(fmi_24, na.rm = TRUE),
    skew = skewness(fmi_24, na.rm = TRUE)
  )        #the skewness did not change#

#log-transform:
alspac_addition <- alspac_addition %>%
  mutate(fmi_24_log = log(fmi_24))

alspac_addition %>%
  summarise(
    n = sum(!is.na(fmi_24_log)),
    skew_before = skewness(fmi_24, na.rm = TRUE),
    skew_after = skewness(fmi_24_log, na.rm = TRUE)
  )

hist(alspac_addition$fmi_24_log)
##############################################
#carotid intima media thickness:
#check left and right mean measurements:
alspac_addition %>%
  summarise(
    across(c(FKCV1131, FKCV2131),
           list(n = ~sum(!is.na(.x)), mean = ~mean(.x, na.rm=TRUE),
                min = ~min(.x, na.rm=TRUE), max = ~max(.x, na.rm=TRUE),
                skew = ~skewness(.x, na.rm=TRUE)))
  ) %>%
  View()

#create the average cIMT variable:
alspac_addition <- alspac_addition %>%
  mutate(
    cimt_mean_24 = rowMeans(across(c(FKCV1131, FKCV2131)), na.rm = TRUE),
    cimt_n_sides = rowSums(!is.na(across(c(FKCV1131, FKCV2131))))
  ) %>%
  mutate(cimt_mean_24 = if_else(cimt_n_sides == 0, NA_real_, cimt_mean_24))

alspac_addition %>% count(cimt_n_sides)

alspac_addition %>%
  filter(cimt_n_sides > 0) %>%
  summarise(
    n = n(),
    mean = mean(cimt_mean_24, na.rm = TRUE),
    skew = skewness(cimt_mean_24, na.rm = TRUE)
  )
##############################################
#pulse wave velocity:
alspac_addition %>%
  summarise(
    n = sum(!is.na(FKCV4200)),
    mean = mean(FKCV4200, na.rm = TRUE),
    min = min(FKCV4200, na.rm = TRUE),
    max = max(FKCV4200, na.rm = TRUE),
    skew = skewness(FKCV4200, na.rm = TRUE)
  )

hist(alspac_addition$FKCV4200)
summary(alspac_addition$FKCV4200)

alspac_addition %>%
  filter(!is.na(FKCV4200)) %>%
  arrange(desc(FKCV4200)) %>%
  select(id, FKCV4200) %>%
  head(10)

#log-transformation:
alspac_addition <- alspac_addition %>%
  mutate(pwv_24_log = log(FKCV4200))

alspac_addition %>%
  summarise(
    n = sum(!is.na(pwv_24_log)),
    skew_before = skewness(FKCV4200, na.rm = TRUE),
    skew_after = skewness(pwv_24_log, na.rm = TRUE)
  )
##############################################
#save progress:
saveRDS(alspac_addition, "alspac_addition_outcomes_built.rds")
##############################################
#create the flow diagram:

#alive at 1 year:
var_label(alspac_addition$kz011b)
alspac_addition %>%
  count(kz011b)      #36 was NA#

#explore the 36 NAs:
alspac_addition %>%
  filter(is.na(kz011b)) %>%
  select(id, in_core, in_phase2, in_phase3, in_phase4, preg_in_alsp, preg_in_core) %>%
  head(36) %>%
  View()

#check to see if 2=no and NAs are correct:
alspac_addition %>%
  mutate(
    alive_group = case_when(
      kz011b == 1 ~ "Alive at 1",
      kz011b == 2 ~ "Not alive at 1",
      is.na(kz011b) ~ "Unknown"
    ),
    has_any_measurement = if_else(
      !is.na(vitd_combined) | !is.na(Chol_F24_mgdl) | !is.na(Glucose_F24_mgdl) |
        !is.na(sbp_24) | !is.na(fmi_24) | !is.na(cimt_mean_24) | !is.na(FKCV4200),
      TRUE, FALSE
    )
  ) %>%
  count(alive_group, has_any_measurement)   #yes, they were correct
#so we had 14956 child alive at age 1#

alspac_addition <- alspac_addition %>%
  mutate(stage1_alive_at_1 = case_when(
    kz011b == 1 ~ TRUE,
    TRUE ~ FALSE
  ))

alspac_addition %>% count(stage1_alive_at_1)


#attended clinic at age 7,9, or 11:
alspac_addition <- alspac_addition %>%
  mutate(
    stage2_attended_7_9_11 = case_when(
      in_f07 == 1 | in_f09 == 1 | in_F11 == 1 ~ TRUE,
      TRUE ~ FALSE
    )
  )

alspac_addition %>%
  filter(stage1_alive_at_1) %>%
  count(stage2_attended_7_9_11)  #out of 14956 alive at age 1, 10070 attended at least one of F7/9/11 visits#
#and 4886 didn't attend any.


#had valid vitamin D measurement:
alspac_addition <- alspac_addition %>%
  mutate(
    stage3_valid_vitd = case_when(
      !is.na(vitd_combined) ~ TRUE,
      TRUE ~ FALSE
    )
  )

alspac_addition %>%
  filter(stage1_alive_at_1 & stage2_attended_7_9_11) %>%
  count(stage3_valid_vitd)        #out of 10070 children attended F7,9,or 11 visit, 6497 had vitD measurements
#and 3573 didn't have vitD#
#before we had 6498 vitD measurements. What happend to that 1 person?
alspac_addition %>%
  filter(!is.na(vitd_combined) & !(stage1_alive_at_1 & stage2_attended_7_9_11)) %>%
  select(id, kz011b, stage1_alive_at_1, in_f07, in_f09, in_F11, stage2_attended_7_9_11, vitd_combined, sbp_24, Chol_F24, Glucose_F24_mgdl) %>%
  View()   #no info on clinic attendance or month of attendance or outcomes#


#had vitamin D and at least one outcome:
alspac_addition <- alspac_addition %>%
  mutate(
    has_any_outcome = if_else(
      !is.na(Chol_F24_mgdl) | !is.na(HDL_F24_mgdl) | !is.na(LDL_F24_mgdl) | !is.na(Trig_F24_mgdl) |
        !is.na(Glucose_F24_mgdl) | !is.na(Insulin_F24) | !is.na(HOMA_IR_log) |
        !is.na(sbp_24) | !is.na(dbp_24) |
        !is.na(fmi_24) | !is.na(cimt_mean_24) | !is.na(FKCV4200),
      TRUE, FALSE
    ),
    stage4_vitd_and_outcome = case_when(
      stage3_valid_vitd & has_any_outcome ~ TRUE,
      TRUE ~ FALSE
    )
  )

alspac_addition %>%
  filter(stage1_alive_at_1 & stage2_attended_7_9_11 & stage3_valid_vitd) %>%
  count(stage4_vitd_and_outcome)   #2784 participants had vitamin D and at least one outcome#


#complete covariates:
alspac_addition <- alspac_addition %>%
  mutate(
    has_complete_covariates = if_else(
      !is.na(sex) & !is.na(age_24_years) & !is.na(mat_edu) & !is.na(ethn) &
        !is.na(fam_income) & !is.na(smoking_3cat) & !is.na(alcohol_freq_3cat),
      TRUE, FALSE
    ),
    stage5_final_sample = case_when(
      stage4_vitd_and_outcome & has_complete_covariates ~ TRUE,
      TRUE ~ FALSE
    )
  )

alspac_addition %>%
  filter(stage1_alive_at_1 & stage2_attended_7_9_11 & stage3_valid_vitd & stage4_vitd_and_outcome) %>%
  count(stage5_final_sample)  #2471 had vitamin D, at least one outcome, and complete covariates##


#missingness of each covariate:
alspac_addition %>%
  filter(stage4_vitd_and_outcome) %>%
  summarise(
    missing_sex        = sum(is.na(sex)),
    missing_age        = sum(is.na(age_24_years)),
    missing_mat_edu    = sum(is.na(mat_edu)),
    missing_ethnicity  = sum(is.na(ethn)),
    missing_income     = sum(is.na(fam_income)),
    missing_smoking    = sum(is.na(smoking_3cat)),
    missing_alcohol    = sum(is.na(alcohol_freq_3cat))
  ) %>%
  View()
##############################################
#save the progress:
saveRDS(alspac_addition, "alspac_addition_full.rds")
##############################################
#Table 1: 
#first create vitamin D categories:
alspac_addition <- alspac_addition %>%
  mutate(
    vitd_group_raw = case_when(
      vitd_combined < 50 ~ "Deficient",
      vitd_combined >= 50 ~ "Sufficient",
      TRUE ~ NA_character_
    ),
    vitd_group_adj = case_when(
      vitd_adj_combined < 50 ~ "Deficient",
      vitd_adj_combined >= 50 ~ "Sufficient",
      TRUE ~ NA_character_
    )
  )


alspac_addition %>% count(vitd_group_raw)
alspac_addition %>% count(vitd_group_adj)

#table 1: covariates (using season-adjusted vitD)
install.packages("gtsummary")
library(gtsummary)

alspac_addition <- alspac_addition %>%
  mutate(
    sex = as_factor(kz021),
    sex=droplevels(sex))

alspac_addition %>% count(sex)

#create age at vitD measurement:
alspac_addition <- alspac_addition %>%
  mutate(
    age_at_vitd_weeks = case_when(
      !is.na(vitdtot_F7)  ~ f7003b,
      !is.na(vitdtot_F9)  ~ f9003b,
      !is.na(vitdtot_F11) ~ fe003b,
      TRUE ~ NA_real_
    ),
    age_at_vitd_years = age_at_vitd_weeks / 52.18
  )

alspac_addition %>%
  summarise(
    n = sum(!is.na(age_at_vitd_years)),
    mean = mean(age_at_vitd_years, na.rm = TRUE),
    min = min(age_at_vitd_years, na.rm = TRUE),
    max = max(age_at_vitd_years, na.rm = TRUE)
  )


alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(vitd_group_adj, vitd_adj_combined, age_at_vitd_years, sex, age_24_years, mat_edu, ethn, fam_income,
         smoking_3cat, smoking_2cat, alcohol_freq_3cat) %>%
  tbl_summary(
    by = vitd_group_adj,
    statistic = list(c(age_24_years, age_at_vitd_years, vitd_adj_combined) ~ "{mean} ({sd})"),
    digits = list(
      all_categorical() ~ c(0,2), c(age_24_years, age_at_vitd_years, vitd_adj_combined) ~2 )
  ) %>%
  add_p(test= list(c(age_24_years, age_at_vitd_years, vitd_adj_combined) ~ "t.test")) %>%
  add_overall()


#with raw vitamin D:
alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(vitd_group_raw, age_at_vitd_years, sex, age_24_years, mat_edu, ethn, fam_income,
         smoking_3cat, smoking_2cat, alcohol_freq_3cat) %>%
  tbl_summary(
    by = vitd_group_raw,
    statistic = list(c(age_24_years, age_at_vitd_years) ~ "{mean} ({sd})")
  ) %>%
  add_p(test= list(c(age_24_years, age_at_vitd_years) ~ "t.test")) %>%
  add_overall()


#now, create htn, dyslipidemia, and dysglycemia variables:
alspac_addition <- alspac_addition %>%
  mutate(
    hypertension = case_when(
      sbp_24 >= 130 | dbp_24 >= 80 ~ "Hypertensive",
      !is.na(sbp_24) & !is.na(dbp_24) ~ "Normal",
      TRUE ~ NA_character_
    ),
    dyslipidemia = case_when(
      Chol_F24_mgdl >= 200 | LDL_F24_mgdl >= 130 | Trig_F24_mgdl >= 150 |
        (sex == "Male" & HDL_F24_mgdl < 40) | (sex == "Female" & HDL_F24_mgdl < 50) ~ "Yes",
      !is.na(Chol_F24_mgdl) & !is.na(LDL_F24_mgdl) & !is.na(Trig_F24_mgdl) & !is.na(HDL_F24_mgdl) ~ "No",
      TRUE ~ NA_character_
    ),
    homa_insulin_resistant = case_when(
      HOMA_IR >= 1.9 ~ "Yes",
      !is.na(HOMA_IR) ~ "No",
      TRUE ~ NA_character_
    ),
    dysglycemia = case_when(
      glucose_diabetic_flag | homa_insulin_resistant == "Yes" ~ "Yes",
      !is.na(Glucose_F24_mgdl) & !is.na(HOMA_IR) ~ "No",
      TRUE ~ NA_character_
    )
  )

alspac_addition %>% count(hypertension)
alspac_addition %>% count(dyslipidemia)
alspac_addition %>% count(homa_insulin_resistant)
alspac_addition %>% count(dysglycemia)


#now, add the outcome variables to table 1:
alspac_addition <- alspac_addition %>% mutate(pwv_24 = as.numeric(FKCV4200))  



alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(vitd_group_adj, hypertension, dyslipidemia, homa_insulin_resistant, dysglycemia,
         HOMA_IR, fmi_24, cimt_mean_24, pwv_24) %>%
  tbl_summary(
    by = vitd_group_adj,
    type = list(c(dyslipidemia, dysglycemia) ~ "categorical"),
    statistic = list(
      cimt_mean_24 ~ "{mean} ({sd})",
      c(fmi_24, pwv_24) ~ "{median} ({p25}, {p75})"
    ),
    digits = list(all_categorical() ~ c(0,2))
  ) %>%
  add_p(
    test = list(
      cimt_mean_24 ~ "t.test",
      c(fmi_24, pwv_24, HOMA_IR) ~ "wilcox.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_overall()


#with raw vitamin D:
alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(vitd_group_raw, hypertension, dyslipidemia, homa_insulin_resistant, dysglycemia,
         fmi_24, cimt_mean_24, pwv_24) %>%
  tbl_summary(
    by = vitd_group_raw,
    type = list(c(dyslipidemia, dysglycemia) ~ "categorical"),
    statistic = list(
      cimt_mean_24 ~ "{mean} ({sd})",
      c(fmi_24, pwv_24) ~ "{median} ({p25}, {p75})"
    )
  ) %>%
  add_p(
    test = list(
      cimt_mean_24 ~ "t.test",
      c(fmi_24, pwv_24) ~ "wilcox.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_overall()
##############################################
#main analyses:

#starting with HOMA-IR:

#checking the associations with continuous vitD:
alspac_addition <- alspac_addition %>%
  mutate(
    vitd_combined_per10 = vitd_combined/10,
    vitd_adj_combined_per10 = vitd_adj_combined /10
  )


#continuous adjusted vitD:  
#model 1:
model1_homa_adj <- lm(HOMA_IR_log ~ vitd_adj_combined_per10,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_homa_adj)

coef1 <- coef(model1_homa_adj)["vitd_adj_combined_per10"]
ci1 <- confint(model1_homa_adj)["vitd_adj_combined_per10", ]
pct_change1 <- (exp(coef1) - 1) * 100
pct_ci_lower1 <- (exp(ci1[1]) - 1) * 100
pct_ci_upper1 <- (exp(ci1[2]) - 1) * 100
cat("Model 1 % change:", round(pct_change1, 2), "% (95% CI:", round(pct_ci_lower1, 2), "to", round(pct_ci_upper1, 2), "%)\n")


#model 2:
model2_homa_adj <- lm(HOMA_IR_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_homa_adj)

coef2 <- coef(model2_homa_adj)["vitd_adj_combined_per10"]
ci2 <- confint(model2_homa_adj)["vitd_adj_combined_per10", ]
pct_change2 <- (exp(coef2) - 1) * 100
pct_ci_lower2 <- (exp(ci2[1]) - 1) * 100
pct_ci_upper2 <- (exp(ci2[2]) - 1) * 100
cat("Model 2 % change:", round(pct_change2, 2), "% (95% CI:", round(pct_ci_lower2, 2), "to", round(pct_ci_upper2, 2), "%)\n")


#model 3:
model3_homa_adj <- lm(HOMA_IR_log ~ vitd_adj_combined_per10 +  
                        sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_homa_adj)   
confint(model3_homa_adj)["vitd_adj_combined_per10", ]

coef_val <- coef(model3_homa_adj)["vitd_adj_combined_per10"]
ci_vals <- confint(model3_homa_adj)["vitd_adj_combined_per10", ]

pct_change <- (exp(coef_val) - 1) * 100
pct_ci_lower <- (exp(ci_vals[1]) - 1) * 100
pct_ci_upper <- (exp(ci_vals[2]) - 1) * 100

cat("% change in HOMA-IR per 10 nmol/L vitamin D:", round(pct_change, 2), "%\n")
cat("95% CI:", round(pct_ci_lower, 2), "% to", round(pct_ci_upper, 2), "%\n")

#sample size:
nobs(model3_homa_adj)


#Non-linearity check using rms::rcs()
install.packages("rms")
library(rms)

model_vars <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(HOMA_IR_log, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

options(contrasts = c("contr.treatment", "contr.treatment"))   
dd <- datadist(model_vars)
options(datadist = "dd")

model3_homair_rcs <- ols(HOMA_IR_log ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                           smoking_3cat + alcohol_freq_3cat,
                         data = model_vars)

anova(model3_homair_rcs)     #no evidence of non-linearity##
ggplot(Predict(model3_homair_rcs, vitd_combined_per10))
##############################################
#glucose:

#continuous adjusted vitD:
#model 1:
model1_glucose_adj <- lm(Glucose_F24_mgdl ~ vitd_adj_combined_per10,
                         data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_glucose_adj)
confint(model1_glucose_adj)["vitd_adj_combined_per10", ]

#model 2:
model2_glucose_adj <- lm(Glucose_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                         data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_glucose_adj)
confint(model2_glucose_adj)["vitd_adj_combined_per10", ]

#model 3:
model3_glucose_adj <- lm(Glucose_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                           smoking_3cat + alcohol_freq_3cat,
                         data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_glucose_adj)
confint(model3_glucose_adj)["vitd_adj_combined_per10", ]

#sample size:
nobs(model3_glucose_adj)


#Non-linearity check using rms::rcs()
model_vars_glucose <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(Glucose_F24_mgdl, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_glucose)
options(datadist = "dd")

model3_glucose_rcs <- ols(Glucose_F24_mgdl ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                            smoking_3cat + alcohol_freq_3cat,
                          data = model_vars_glucose)
anova(model3_glucose_rcs)

ggplot(Predict(model3_glucose_rcs, vitd_combined_per10))
##############################################
#SBP:

#continuous adjusted vitD:
#model 1:
model1_sbp_adj <- lm(sbp_24 ~ vitd_adj_combined_per10,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_sbp_adj)
confint(model1_sbp_adj)["vitd_adj_combined_per10", ]

#model 2:
model2_sbp_adj <- lm(sbp_24 ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_sbp_adj)
confint(model2_sbp_adj)["vitd_adj_combined_per10", ]

#model 3:
model3_sbp_adj <- lm(sbp_24 ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_sbp_adj)
confint(model3_sbp_adj)["vitd_adj_combined_per10", ]

#sample size
nobs(model3_sbp_adj)


#Non-linearity check using rms::rcs()
alspac_addition <- alspac_addition %>%
  mutate(sbp_24 = as.numeric(sbp_24))

model_vars_sbp <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(sbp_24, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_sbp)
options(datadist = "dd")

model3_sbp_rcs <- ols(sbp_24 ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = model_vars_sbp)
anova(model3_sbp_rcs)

ggplot(Predict(model3_sbp_rcs, vitd_combined_per10))    #not significant##
##############################################
#DBP:

#continuous adjusted vitD:
#model 1:
model1_dbp_adj <- lm(dbp_24 ~ vitd_adj_combined_per10,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_dbp_adj)
confint(model1_dbp_adj)["vitd_adj_combined_per10", ]

#model 2:
model2_dbp_adj <- lm(dbp_24 ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_dbp_adj)
confint(model2_dbp_adj)["vitd_adj_combined_per10", ]

#model 3:
model3_dbp_adj <- lm(dbp_24 ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_dbp_adj)
confint(model3_dbp_adj)["vitd_adj_combined_per10", ]


#sample size:
nobs(model3_dbp_adj)


#Non-linearity check using rms::rcs()
model_vars_dbp <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(dbp_24, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_dbp)
options(datadist = "dd")

model3_dbp_rcs <- ols(dbp_24 ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = model_vars_dbp)
anova(model3_dbp_rcs)

ggplot(Predict(model3_dbp_rcs, vitd_combined_per10))    #not significant##

##############################################
#TC:

#check some outliers:
model_used_tc <- alspac_addition %>% filter(stage5_final_sample)
model_used_tc$resid_tc <- NA
model_used_tc$resid_tc[as.numeric(names(residuals(model3_tc)))] <- residuals(model3_tc)

model_used_tc %>%
  filter(!is.na(resid_tc)) %>%
  arrange(desc(resid_tc)) %>%
  select(id, Chol_F24_mgdl, resid_tc, hdl_extreme_flag, trig_extreme_flag, glucose_diabetic_flag) %>%
  head(3) %>% View()


#continuous adjusted vitD:
#model 1:
model1_tc_adj <- lm(Chol_F24_mgdl ~ vitd_adj_combined_per10,
                    data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_tc_adj)
confint(model1_tc_adj)["vitd_adj_combined_per10", ]

#model 2:
model2_tc_adj <- lm(Chol_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                    data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_tc_adj)
confint(model2_tc_adj)["vitd_adj_combined_per10", ]

#model 3:
model3_tc_adj <- lm(Chol_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                      smoking_3cat + alcohol_freq_3cat,
                    data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_tc_adj)
confint(model3_tc_adj)["vitd_adj_combined_per10", ]


#sample size:
nobs(model3_tc_adj)


#Non-linearity check using rms::rcs()
model_vars_tc <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(Chol_F24_mgdl, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_tc)
options(datadist = "dd")

model3_tc_rcs <- ols(Chol_F24_mgdl ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = model_vars_tc)
anova(model3_tc_rcs)

ggplot(Predict(model3_tc_rcs, vitd_combined_per10))    #not significant##
##############################################
#LDL-C:

#check some outliers:
model_used_ldl <- alspac_addition %>% filter(stage5_final_sample)
model_used_ldl$resid_ldl <- NA
model_used_ldl$resid_ldl[as.numeric(names(residuals(model3_ldl)))] <- residuals(model3_ldl)

model_used_ldl %>%
  filter(!is.na(resid_ldl)) %>%
  arrange(desc(resid_ldl)) %>%
  select(id, LDL_F24_mgdl, resid_ldl, hdl_extreme_flag, trig_extreme_flag, glucose_diabetic_flag) %>%
  head(3) %>% View()


#continuous adjusted vitD:
#model 1:
model1_ldl_adj <- lm(LDL_F24_mgdl ~ vitd_adj_combined_per10,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_ldl_adj)
confint(model1_ldl_adj)["vitd_adj_combined_per10", ]

#model 2:
model2_ldl_adj <- lm(LDL_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_ldl_adj)
confint(model2_ldl_adj)["vitd_adj_combined_per10", ]

#model 3:
model3_ldl_adj <- lm(LDL_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_ldl_adj)
confint(model3_ldl_adj)["vitd_adj_combined_per10", ]

#sample size:
nobs(model3_ldl_adj)


#Non-linearity check using rms::rcs()
model_vars_ldl <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(LDL_F24_mgdl, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_ldl)
options(datadist = "dd")

model3_ldl_rcs <- ols(LDL_F24_mgdl ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = model_vars_ldl)
anova(model3_ldl_rcs)

ggplot(Predict(model3_ldl_rcs, vitd_combined_per10))   #not significant##
##############################################
#HDL-C:

#continuous adjusted vitD:
#model 1:
model1_hdl_adj <- lm(HDL_F24_mgdl ~ vitd_adj_combined_per10,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_hdl_adj)
confint(model1_hdl_adj)["vitd_adj_combined_per10", ]

#model 2:
model2_hdl_adj <- lm(HDL_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_hdl_adj)
confint(model2_hdl_adj)["vitd_adj_combined_per10", ]

#model 3:
model3_hdl_adj <- lm(HDL_F24_mgdl ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_hdl_adj)
confint(model3_hdl_adj)["vitd_adj_combined_per10", ]

#sample size:
nobs(model3_hdl_adj)


#Non-linearity check using rms::rcs()
model_vars_hdl <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(HDL_F24_mgdl, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_hdl)
options(datadist = "dd")

model3_hdl_rcs <- ols(HDL_F24_mgdl ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = model_vars_hdl)
anova(model3_hdl_rcs)

ggplot(Predict(model3_hdl_rcs, vitd_combined_per10))   #not significant##
##############################################
#TG:

#continuous adjusted vitD:
#model 1:
model1_trig_adj <- lm(Trig_F24_log ~ vitd_adj_combined_per10,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_trig_adj)

coef1 <- coef(model1_trig_adj)["vitd_adj_combined_per10"]
ci1 <- confint(model1_trig_adj)["vitd_adj_combined_per10", ]
pct_change1 <- (exp(coef1) - 1) * 100
pct_ci_lower1 <- (exp(ci1[1]) - 1) * 100
pct_ci_upper1 <- (exp(ci1[2]) - 1) * 100
cat("Model 1 % change:", round(pct_change1, 2), "% (95% CI:", round(pct_ci_lower1, 2), "to", round(pct_ci_upper1, 2), "%)\n")

#model 2:
model2_trig_adj <- lm(Trig_F24_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_trig_adj)

coef2 <- coef(model2_trig_adj)["vitd_adj_combined_per10"]
ci2 <- confint(model2_trig_adj)["vitd_adj_combined_per10", ]
pct_change2 <- (exp(coef2) - 1) * 100
pct_ci_lower2 <- (exp(ci2[1]) - 1) * 100
pct_ci_upper2 <- (exp(ci2[2]) - 1) * 100
cat("Model 2 % change:", round(pct_change2, 2), "% (95% CI:", round(pct_ci_lower2, 2), "to", round(pct_ci_upper2, 2), "%)\n")

#model 3:
model3_trig_adj <- lm(Trig_F24_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_trig_adj)

coef3 <- coef(model3_trig_adj)["vitd_adj_combined_per10"]
ci3 <- confint(model3_trig_adj)["vitd_adj_combined_per10", ]
pct_change3 <- (exp(coef3) - 1) * 100
pct_ci_lower3 <- (exp(ci3[1]) - 1) * 100
pct_ci_upper3 <- (exp(ci3[2]) - 1) * 100
cat("Model 3 % change:", round(pct_change3, 2), "% (95% CI:", round(pct_ci_lower3, 2), "to", round(pct_ci_upper3, 2), "%)\n")

nobs(model3_trig_adj)
#check the extreme values:
model3_trig_sens <- lm(Trig_F24_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                         smoking_3cat + alcohol_freq_3cat,
                       data = alspac_addition %>% filter(stage5_final_sample & !trig_extreme_flag))
coef_t <- coef(model3_trig_sens)["vitd_adj_combined_per10"]
ci_t <- confint(model3_trig_sens)["vitd_adj_combined_per10", ]
cat("Trig % change:", round((exp(coef_t)-1)*100, 2), "% (95% CI:", round((exp(ci_t[1])-1)*100,2), "to", round((exp(ci_t[2])-1)*100,2), "%)\n")
nobs(model3_trig_sens)


#sample size:
nobs(model3_trig_adj)



#Non-linearity check using rms::rcs()
model_vars_tg <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(Trig_F24_log, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_tg)
options(datadist = "dd")

model3_tg_rcs <- ols(Trig_F24_log ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = model_vars_tg)
anova(model3_tg_rcs)

ggplot(Predict(model3_tg_rcs, vitd_combined_per10))   #not significant##
##############################################
#FMI:

#continuous adjusted vitD:
#model 1:
model1_fmi_adj <- lm(fmi_24_log ~ vitd_adj_combined_per10,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_fmi_adj)

coef1 <- coef(model1_fmi_adj)["vitd_adj_combined_per10"]
ci1 <- confint(model1_fmi_adj)["vitd_adj_combined_per10", ]
pct_change1 <- (exp(coef1) - 1) * 100
pct_ci_lower1 <- (exp(ci1[1]) - 1) * 100
pct_ci_upper1 <- (exp(ci1[2]) - 1) * 100
cat("Model 1 % change:", round(pct_change1, 2), "% (95% CI:", round(pct_ci_lower1, 2), "to", round(pct_ci_upper1, 2), "%)\n")

#model 2:
model2_fmi_adj <- lm(fmi_24_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_fmi_adj)

coef2 <- coef(model2_fmi_adj)["vitd_adj_combined_per10"]
ci2 <- confint(model2_fmi_adj)["vitd_adj_combined_per10", ]
pct_change2 <- (exp(coef2) - 1) * 100
pct_ci_lower2 <- (exp(ci2[1]) - 1) * 100
pct_ci_upper2 <- (exp(ci2[2]) - 1) * 100
cat("Model 2 % change:", round(pct_change2, 2), "% (95% CI:", round(pct_ci_lower2, 2), "to", round(pct_ci_upper2, 2), "%)\n")

#model 3:
model3_fmi_adj <- lm(fmi_24_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_fmi_adj)

coef3 <- coef(model3_fmi_adj)["vitd_adj_combined_per10"]
ci3 <- confint(model3_fmi_adj)["vitd_adj_combined_per10", ]
pct_change3 <- (exp(coef3) - 1) * 100
pct_ci_lower3 <- (exp(ci3[1]) - 1) * 100
pct_ci_upper3 <- (exp(ci3[2]) - 1) * 100
cat("Model 3 % change:", round(pct_change3, 2), "% (95% CI:", round(pct_ci_lower3, 2), "to", round(pct_ci_upper3, 2), "%)\n")

#sample size:
nobs(model3_fmi_adj)


#Non-linearity check using rms::rcs()
options(contrasts = c("contr.treatment", "contr.treatment"))

model_vars_fmi <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(fmi_24_log, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_fmi)
options(datadist = "dd")

model3_fmi_rcs <- ols(fmi_24_log ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = model_vars_fmi)
anova(model3_fmi_rcs)

dev.off()
ggplot(Predict(model3_fmi_rcs, vitd_combined_per10))  #not significant##
##############################################
#c-IMT:

#continuous adjusted vitD:
#model 1:
model1_cimt_adj <- lm(cimt_mean_24 ~ vitd_adj_combined_per10,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_cimt_adj)
confint(model1_cimt_adj)["vitd_adj_combined_per10", ]

#model 2:
model2_cimt_adj <- lm(cimt_mean_24 ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_cimt_adj)
confint(model2_cimt_adj)["vitd_adj_combined_per10", ]

#model 3:
model3_cimt_adj <- lm(cimt_mean_24 ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                        smoking_3cat + alcohol_freq_3cat,
                      data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_cimt_adj)
confint(model3_cimt_adj)["vitd_adj_combined_per10", ]

#sample size:
nobs(model3_cimt_adj)


#Non-linearity check using rms::rcs()
model_vars_cimt <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(cimt_mean_24, vitd_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_cimt)
options(datadist = "dd")

model3_cimt_rcs <- ols(cimt_mean_24 ~ rcs(vitd_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                         smoking_3cat + alcohol_freq_3cat,
                       data = model_vars_cimt)
anova(model3_cimt_rcs)

ggplot(Predict(model3_cimt_rcs, vitd_combined_per10))
##############################################
#PWV:
alspac_addition <- alspac_addition %>%
  mutate(pwv_24 = as.numeric(FKCV4200),
         pwv_24_log = log(pwv_24))


#continuous adjusted vitD:
#model 1:
model1_pwv_adj <- lm(pwv_24_log ~ vitd_adj_combined_per10,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model1_pwv_adj)

coef1 <- coef(model1_pwv_adj)["vitd_adj_combined_per10"]
ci1 <- confint(model1_pwv_adj)["vitd_adj_combined_per10", ]
pct_change1 <- (exp(coef1) - 1) * 100
pct_ci_lower1 <- (exp(ci1[1]) - 1) * 100
pct_ci_upper1 <- (exp(ci1[2]) - 1) * 100
cat("Model 1 % change:", round(pct_change1, 2), "% (95% CI:", round(pct_ci_lower1, 2), "to", round(pct_ci_upper1, 2), "%)\n")

#model 2:
model2_pwv_adj <- lm(pwv_24_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model2_pwv_adj)

coef2 <- coef(model2_pwv_adj)["vitd_adj_combined_per10"]
ci2 <- confint(model2_pwv_adj)["vitd_adj_combined_per10", ]
pct_change2 <- (exp(coef2) - 1) * 100
pct_ci_lower2 <- (exp(ci2[1]) - 1) * 100
pct_ci_upper2 <- (exp(ci2[2]) - 1) * 100
cat("Model 2 % change:", round(pct_change2, 2), "% (95% CI:", round(pct_ci_lower2, 2), "to", round(pct_ci_upper2, 2), "%)\n")

#model 3:
model3_pwv_adj <- lm(pwv_24_log ~ vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat,
                     data = alspac_addition %>% filter(stage5_final_sample))
summary(model3_pwv_adj)

coef3 <- coef(model3_pwv_adj)["vitd_adj_combined_per10"]
ci3 <- confint(model3_pwv_adj)["vitd_adj_combined_per10", ]
pct_change3 <- (exp(coef3) - 1) * 100
pct_ci_lower3 <- (exp(ci3[1]) - 1) * 100
pct_ci_upper3 <- (exp(ci3[2]) - 1) * 100
cat("Model 3 % change:", round(pct_change3, 2), "% (95% CI:", round(pct_ci_lower3, 2), "to", round(pct_ci_upper3, 2), "%)\n")

#sample size:
nobs(model3_pwv_adj)


#Non-linearity check using rms::rcs()
options(contrasts = c("contr.treatment", "contr.treatment"))

model_vars_pwv_adj <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(pwv_24_log, vitd_adj_combined_per10, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_pwv_adj)
options(datadist = "dd")

model3_pwv_rcs_adj <- ols(pwv_24_log ~ rcs(vitd_adj_combined_per10, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                            smoking_3cat + alcohol_freq_3cat,
                          data = model_vars_pwv_adj)
anova(model3_pwv_rcs_adj)

ggplot(Predict(model3_pwv_rcs_adj, vitd_adj_combined_per10))

#get predicted PWV at meaningful vitamin D values:
vitd_points <- quantile(model_vars_pwv_adj$vitd_adj_combined_per10,
                        probs = c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE)
vitd_points

preds_adj <- Predict(model3_pwv_rcs_adj, vitd_adj_combined_per10 = vitd_points)

preds_adj$pwv_ms <- exp(preds_adj$yhat)
preds_adj$lower_ms <- exp(preds_adj$lower)
preds_adj$upper_ms <- exp(preds_adj$upper)

preds_adj[, c("vitd_adj_combined_per10", "pwv_ms", "lower_ms", "upper_ms")]
#PWV is lowest (5.96 m/s) at median vitD, and rises by about 1.3% at deficient levels (6.04) and 1.3% at very high levels (6.04) compared to the middle#

##############################################
#sex stratification analyses for all outcomes:
outcomes_list <- list(
  list(name = "HOMA-IR",   var = "HOMA_IR_log",       logged = TRUE),
  list(name = "Glucose",   var = "Glucose_F24_mgdl",  logged = FALSE),
  list(name = "SBP",       var = "sbp_24",             logged = FALSE),
  list(name = "DBP",       var = "dbp_24",             logged = FALSE),
  list(name = "TC",        var = "Chol_F24_mgdl",      logged = FALSE),
  list(name = "LDL",       var = "LDL_F24_mgdl",       logged = FALSE),
  list(name = "HDL",       var = "HDL_F24_mgdl",       logged = FALSE),
  list(name = "Trig",      var = "Trig_F24_log",       logged = TRUE),
  list(name = "FMI",       var = "fmi_24_log",         logged = TRUE),
  list(name = "cIMT",      var = "cimt_mean_24",       logged = FALSE),
  list(name = "PWV",       var = "pwv_24_log",         logged = TRUE)
)

covariates_formula <- "vitd_adj_combined_per10 + age_24_years + ethn + mat_edu + fam_income + smoking_3cat + alcohol_freq_3cat"
covariates_int <- "vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income + smoking_3cat + alcohol_freq_3cat"

results_sex <- data.frame()

for (o in outcomes_list) {
  formula_int <- as.formula(paste(o$var, "~", covariates_int, "+ vitd_adj_combined_per10*sex"))
  m_int <- lm(formula_int, data = alspac_addition %>% filter(stage5_final_sample))
  int_row <- summary(m_int)$coefficients
  int_pval <- int_row[grep(":sex", rownames(int_row)), "Pr(>|t|)"]
  
  formula_main <- as.formula(paste(o$var, "~", covariates_formula))
  
  m_male <- lm(formula_main, data = alspac_addition %>% filter(stage5_final_sample & sex == "Male"))
  m_female <- lm(formula_main, data = alspac_addition %>% filter(stage5_final_sample & sex == "Female"))
  
  extract_row <- function(model, group) {
    coef_val <- coef(model)["vitd_adj_combined_per10"]
    ci_val <- confint(model)["vitd_adj_combined_per10", ]
    pval <- summary(model)$coefficients["vitd_adj_combined_per10", "Pr(>|t|)"]
    n <- nobs(model)
    
    if (o$logged) {
      est <- (exp(coef_val) - 1) * 100
      lower <- (exp(ci_val[1]) - 1) * 100
      upper <- (exp(ci_val[2]) - 1) * 100
    } else {
      est <- coef_val
      lower <- ci_val[1]
      upper <- ci_val[2]
    }
    
    data.frame(outcome = o$name, group = group, n = n,
               estimate = round(est, 3), ci_lower = round(lower, 3), ci_upper = round(upper, 3),
               p_value = round(pval, 3), interaction_p = round(int_pval, 3))
  }
  
  results_sex <- bind_rows(results_sex, extract_row(m_male, "Male"))
  results_sex <- bind_rows(results_sex, extract_row(m_female, "Female"))
}

results_sex
#######################################################################################################################
#Check the sample size of the models with PA added:
pa_models <- list(
  HOMAIR = model3_homair_pa,
  Glucose = model3_glucose_pa,
  SBP = model3_sbp_pa,
  DBP = model3_dbp_pa,
  TC = model3_tc_pa,
  LDL = model3_ldl_pa,
  HDL = model3_hdl_pa,
  Trig = model3_tg_pa,
  FMI = model3_fmi_pa,
  cIMT = model3_cimt_pa,
  PWV = model3_pwv_pa
)

sapply(pa_models, nobs)


alspac_addition %>%
  filter(stage5_final_sample) %>%
  summarise(n_with_pa = sum(!is.na(pa_freq_4cat)))
#######################################################################################################################
# Rebuild the RCS model using raw vitD (nmol/L) for clean axis labeling
library(rms)

options(contrasts = c("contr.treatment", "contr.treatment"))

model_vars_pwv_plot <- alspac_addition %>%
  filter(stage5_final_sample) %>%
  select(pwv_24_log, vitd_adj_combined, sex, age_24_years, ethn, mat_edu, fam_income,
         smoking_3cat, alcohol_freq_3cat)

dd <- datadist(model_vars_pwv_plot)
options(datadist = "dd")

model_pwv_rcs_plot <- ols(pwv_24_log ~ rcs(vitd_adj_combined, 4) + sex + age_24_years + ethn + mat_edu + fam_income +
                            smoking_3cat + alcohol_freq_3cat,
                          data = model_vars_pwv_plot)

# Get predictions across the observed range of vitamin D
pred_data <- Predict(model_pwv_rcs_plot, vitd_adj_combined)
pred_df <- as.data.frame(pred_data)

# Convert from log(PWV) back to raw PWV (m/s)
pred_df$pwv_ms <- exp(pred_df$yhat)
pred_df$lower_ms <- exp(pred_df$lower)
pred_df$upper_ms <- exp(pred_df$upper)

# Build the publication-quality plot
library(ggplot2)

p <- ggplot(pred_df, aes(x = vitd_adj_combined, y = pwv_ms)) +
  geom_ribbon(aes(ymin = lower_ms, ymax = upper_ms), fill = "grey70", alpha = 0.5) +
  geom_line(linewidth = 1, color = "black") +
  labs(
    x = "Season-adjusted childhood 25(OH)D concentrations (nmol/L)",
    y = "Predicted pulse wave velocity (m/s)",
    title = "Non-linear association between childhood 25(OH)D concentrations\nand pulse wave velocity at age 24"
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11)
  )

p

# Save at high resolution for manuscript submission
ggsave("C:/ALSPAC/pwv_nonlinearity_plot2.png", plot = p,
       width = 7, height = 5, dpi = 300, units = "in")
#######################################################################################################################
#Models adjusted for physical activity:
#HOMA-IR:
model3_homa_pa <- lm(HOMA_IR_log ~ vitd_adj_combined_per10 +  
                       sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                     data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_homa_pa)  
confint(model3_homa_pa)["vitd_adj_combined_per10", ]

coef_val <- coef(model3_homa_pa)["vitd_adj_combined_per10"]
ci_vals <- confint(model3_homa_pa)["vitd_adj_combined_per10", ]

pct_change <- (exp(coef_val) - 1) * 100
pct_ci_lower <- (exp(ci_vals[1]) - 1) * 100
pct_ci_upper <- (exp(ci_vals[2]) - 1) * 100

cat("% change in HOMA-IR per 10 nmol/L vitamin D:", round(pct_change, 2), "%\n")
cat("95% CI:", round(pct_ci_lower, 2), "% to", round(pct_ci_upper, 2), "%\n")


#glucose:
model3_glucose_pa <- lm(Glucose_F24_mgdl ~ vitd_adj_combined_per10 +  
                          sex + age_24_years + ethn + mat_edu + fam_income +
                          smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                        data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_glucose_pa)  
confint(model3_glucose_pa)["vitd_adj_combined_per10", ]

nobs(model3_glucose_pa)


#SBP:
model3_sbp_pa <- lm(sbp_24 ~ vitd_adj_combined_per10 +  
                      sex + age_24_years + ethn + mat_edu + fam_income +
                      smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                    data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_sbp_pa)  
confint(model3_sbp_pa)["vitd_adj_combined_per10", ]

nobs(model3_sbp_pa)


#DBP:
model3_dbp_pa <- lm(dbp_24 ~ vitd_adj_combined_per10 +  
                      sex + age_24_years + ethn + mat_edu + fam_income +
                      smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                    data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_dbp_pa)  
confint(model3_dbp_pa)["vitd_adj_combined_per10", ]


#total cholesterol:
model3_tc_pa <- lm(Chol_F24_mgdl ~ vitd_adj_combined_per10 +  
                     sex + age_24_years + ethn + mat_edu + fam_income +
                     smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                   data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_tc_pa)  
confint(model3_tc_pa)["vitd_adj_combined_per10", ]

nobs(model3_tc_pa)


#LDL-C:
model3_ldl_pa <- lm(LDL_F24_mgdl ~ vitd_adj_combined_per10 +  
                      sex + age_24_years + ethn + mat_edu + fam_income +
                      smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                    data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_ldl_pa)  
confint(model3_ldl_pa)["vitd_adj_combined_per10", ]

nobs(model3_ldl_pa)


#HDL-C:
model3_hdl_pa <- lm(HDL_F24_mgdl ~ vitd_adj_combined_per10 +  
                      sex + age_24_years + ethn + mat_edu + fam_income +
                      smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                    data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_hdl_pa)  
confint(model3_hdl_pa)["vitd_adj_combined_per10", ] 

nobs(model3_hdl_pa)


#TG:
model3_trig_pa <- lm(Trig_F24_log ~ vitd_adj_combined_per10 +  
                       sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                     data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_trig_pa)  
confint(model3_trig_pa)["vitd_adj_combined_per10", ]

coef_val <- coef(model3_trig_pa)["vitd_adj_combined_per10"]
ci_vals <- confint(model3_trig_pa)["vitd_adj_combined_per10", ]

pct_change <- (exp(coef_val) - 1) * 100
pct_ci_lower <- (exp(ci_vals[1]) - 1) * 100
pct_ci_upper <- (exp(ci_vals[2]) - 1) * 100

cat("% change in Trig per 10 nmol/L vitamin D:", round(pct_change, 2), "%\n")
cat("95% CI:", round(pct_ci_lower, 2), "% to", round(pct_ci_upper, 2), "%\n")

nobs(model3_trig_pa)


#FMI:
model3_fmi_pa <- lm(fmi_24_log ~ vitd_adj_combined_per10 +  
                      sex + age_24_years + ethn + mat_edu + fam_income +
                      smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                    data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_fmi_pa)  
confint(model3_fmi_pa)["vitd_adj_combined_per10", ]

coef_val <- coef(model3_fmi_pa)["vitd_adj_combined_per10"]
ci_vals <- confint(model3_fmi_pa)["vitd_adj_combined_per10", ]

pct_change <- (exp(coef_val) - 1) * 100
pct_ci_lower <- (exp(ci_vals[1]) - 1) * 100
pct_ci_upper <- (exp(ci_vals[2]) - 1) * 100

cat("% change in FMI per 10 nmol/L vitamin D:", round(pct_change, 2), "%\n")
cat("95% CI:", round(pct_ci_lower, 2), "% to", round(pct_ci_upper, 2), "%\n")

nobs(model3_fmi_pa)


#cIMT:
model3_cimt_pa <- lm(cimt_mean_24 ~ vitd_adj_combined_per10 +  
                       sex + age_24_years + ethn + mat_edu + fam_income +
                       smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                     data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_cimt_pa)  
confint(model3_cimt_pa)["vitd_adj_combined_per10", ]

nobs(model3_cimt_pa)


#PWV:
model3_pwv_pa <- lm(pwv_24_log ~ vitd_adj_combined_per10 +  
                      sex + age_24_years + ethn + mat_edu + fam_income +
                      smoking_3cat + alcohol_freq_3cat + pa_freq_4cat,
                    data = alspac_addition %>% filter(stage5_final_sample))

summary(model3_pwv_pa)  
confint(model3_pwv_pa)["vitd_adj_combined_per10", ]

coef_val <- coef(model3_pwv_pa)["vitd_adj_combined_per10"]
ci_vals <- confint(model3_pwv_pa)["vitd_adj_combined_per10", ]

pct_change <- (exp(coef_val) - 1) * 100
pct_ci_lower <- (exp(ci_vals[1]) - 1) * 100
pct_ci_upper <- (exp(ci_vals[2]) - 1) * 100

cat("% change in PWV per 10 nmol/L vitamin D:", round(pct_change, 2), "%\n")
cat("95% CI:", round(pct_ci_lower, 2), "% to", round(pct_ci_upper, 2), "%\n")

nobs(model3_pwv_pa)
#######################################################################################################################
#analyses for the extreme values:
#HDL-C:
model3_hdl_extreme <- lm(HDL_F24_mgdl ~ vitd_adj_combined_per10 +  
                           sex + age_24_years + ethn + mat_edu + fam_income +
                           smoking_3cat + alcohol_freq_3cat,
                         data = alspac_addition %>% filter(stage5_final_sample & !hdl_extreme_flag))

summary(model3_hdl_extreme)  
confint(model3_hdl_extreme)["vitd_adj_combined_per10", ]
nobs(model3_hdl_extreme)


#HOMA-IR:
model3_homa_extreme <- lm(HOMA_IR_log ~ vitd_adj_combined_per10 +  
                            sex + age_24_years + ethn + mat_edu + fam_income +
                            smoking_3cat + alcohol_freq_3cat,
                          data = alspac_addition %>% filter(stage5_final_sample & !insulin_implausible_flag))

summary(model3_homa_extreme)  
confint(model3_homa_extreme)["vitd_adj_combined_per10", ]

coef_val <- coef(model3_homa_extreme)["vitd_adj_combined_per10"]
ci_vals <- confint(model3_homa_extreme)["vitd_adj_combined_per10", ]

pct_change <- (exp(coef_val) - 1) * 100
pct_ci_lower <- (exp(ci_vals[1]) - 1) * 100
pct_ci_upper <- (exp(ci_vals[2]) - 1) * 100

cat("% change in HOMA-IR per 10 nmol/L vitamin D:", round(pct_change, 2), "%\n")
cat("95% CI:", round(pct_ci_lower, 2), "% to", round(pct_ci_upper, 2), "%\n")
nobs(model3_homa_extreme)
#######################################################################################################################
#checking twins in the final sample size:
alspac_addition %>%
  filter(stage5_final_sample) %>%
  count(cidB4708) %>%
  filter(n>1) %>%
  nrow()          #0 twins#




#seeing these twins:
twin_ids <- alspac_addition %>%
  count(cidB4708) %>%
  filter(n>1) %>%
  pull(cidB4708)

alspac_addition %>%
  filter(cidB4708 %in% twin_ids) %>%
  select(cidB4708, qlet, id, stage5_final_sample) %>%
  arrange(cidB4708)


alspac_addition %>%
  count(cidB4708, qlet) %>%
  count(qlet)

# Confirm these variables exist
c("in_alsp", "in_core", "in_phase2", "in_phase3", "in_phase4", "tripquad") %in% names(alspac_addition)



alspac_addition %>%
  mutate(
    in_core_num = unclass(in_core),
    in_phase2_num = unclass(in_phase2),
    in_phase3_num = unclass(in_phase3),
    in_phase4_num = unclass(in_phase4)
  ) %>%
  summarise(
    in_core_n = sum(in_core_num == 1, na.rm = TRUE),
    in_phase2_n = sum(in_phase2_num == 1, na.rm = TRUE),
    in_phase3_n = sum(in_phase3_num == 1, na.rm = TRUE),
    in_phase4_n = sum(in_phase4_num == 1, na.rm = TRUE)
  )               #no problem in terms of twins and additional recruitment phases##
#######################################################################################################################
#Age-stratified sensitivity analysis:
data_f7 <- alspac_addition %>% filter(stage5_final_sample & !is.na(vitdtot_F7))
data_f9 <- alspac_addition %>% filter(stage5_final_sample & !is.na(vitdtot_F9))
data_f11 <- alspac_addition %>% filter(stage5_final_sample & !is.na(vitdtot_F11))

nrow(data_f7); nrow(data_f9); nrow(data_f11)


# Install if needed
if (!require(flextable)) install.packages("flextable")
if (!require(officer)) install.packages("officer")
library(flextable)
library(officer)

covs <- "vitd_adj_combined_per10 + sex + age_24_years + ethn + mat_edu + fam_income + smoking_3cat + alcohol_freq_3cat"

outcomes_order <- list(
  list(label = "Total cholesterol, mg/dL",   var = "Chol_F24_mgdl",     logged = FALSE),
  list(label = "LDL-C, mg/dL",               var = "LDL_F24_mgdl",      logged = FALSE),
  list(label = "HDL-C, mg/dL",               var = "HDL_F24_mgdl",      logged = FALSE),
  list(label = "Triglycerides, % change",    var = "Trig_F24_log",      logged = TRUE),
  list(label = "Fasting glucose, mg/dL",     var = "Glucose_F24_mgdl",  logged = FALSE),
  list(label = "HOMA-IR, % change",          var = "HOMA_IR_log",       logged = TRUE),
  list(label = "FMI, % change",              var = "fmi_24_log",        logged = TRUE),
  list(label = "SBP, mmHg",                  var = "sbp_24",            logged = FALSE),
  list(label = "DBP, mmHg",                  var = "dbp_24",            logged = FALSE),
  list(label = "cIMT, mm",                   var = "cimt_mean_24",      logged = FALSE),
  list(label = "PWV, % change",              var = "pwv_24_log",        logged = TRUE)
)

age_datasets <- list(F7 = data_f7, F9 = data_f9, F11 = data_f11)

results_table <- data.frame()

for (o in outcomes_order) {
  row <- data.frame(Outcome = o$label)
  for (age_name in names(age_datasets)) {
    d <- age_datasets[[age_name]]
    m <- lm(as.formula(paste(o$var, "~", covs)), data = d)
    cf <- coef(m)["vitd_adj_combined_per10"]
    ci <- confint(m)["vitd_adj_combined_per10", ]
    pval <- summary(m)$coefficients["vitd_adj_combined_per10", "Pr(>|t|)"]
    n <- nobs(m)
    
    if (o$logged) {
      est <- (exp(cf) - 1) * 100
      lo <- (exp(ci[1]) - 1) * 100
      hi <- (exp(ci[2]) - 1) * 100
    } else {
      est <- cf; lo <- ci[1]; hi <- ci[2]
    }
    
    cell <- sprintf("%.2f (%.2f, %.2f); n=%d; P=%.3f", est, lo, hi, n, pval)
    row[[age_name]] <- cell
  }
  results_table <- rbind(results_table, row)
}

colnames(results_table) <- c("Outcome", "Age 7", "Age 9", "Age 11")

# Build and export as a Word table
ft <- flextable(results_table) %>%
  set_caption("Supplemental Table Z. Sensitivity analysis: associations between childhood 25(OH)D (per 10 nmol/L) and cardiometabolic and vascular outcomes at age 24, stratified by age at vitamin D measurement.") %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "supplemental_table_age_stratified.docx")

cat("Saved to:", file.path(getwd(), "supplemental_table_age_stratified.docx"), "\n")
