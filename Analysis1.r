setwd("/...../Analysis")

# === 0. Install / load packages ===
required <- c("readxl","tidyverse","janitor","here","skimr","lubridate",
              "janitor","psych","likert","gtsummary","broom","MASS",
              "ordinal","tidytext","tm","topicmodels","quanteda","wordcloud2",
              "gt","writexl")
to_install <- setdiff(required, installed.packages()[,1])
if(length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))
-------------------------------------------------------------------
# === 1. Read data and initial inspection ===
-------------------------------------------------------------------
file <- "data_survey.xlsx"   # path to your uploaded file
sheets <- readxl::excel_sheets(file)

# default to first sheet
df_raw <- readxl::read_excel(file, sheet = sheets[1], guess_max = 10000)


# Clean column names to snake_case for ease
df <- df_raw %>% janitor::clean_names()
#df <- df[-546, ]
#df <- df[-905, ]
#df <- df[-1452, ]


##correct the monthly income responses
#df <- df %>%
  mutate(average_monthly_income = case_when(
      average_monthly_income %in% c("₹40,001 - ₹50,000","₹40,001 - ₹50,001",
                                    "₹40,001 - ₹50,002") ~ "₹40,000 - ₹50,000",
      average_monthly_income %in% c("Above ₹50,000","Above ₹50,001",
                                    "Above ₹50,002") ~ "Above ₹50,000",
      average_monthly_income %in% c("₹10,000 - ₹20,000", "₹10,000 - ₹20,001",
                                    "₹10,000 - ₹20,002") ~ "₹10,000 - ₹20,000",
      average_monthly_income %in% c("₹20,001 - ₹30,000",
                                    "₹20,001 - ₹30,001") ~ "₹20,000 - ₹30,000",
      average_monthly_income %in% c("Below ₹10,000",
                                    "Below ₹10,001") ~ "Below ₹10,000",
      average_monthly_income %in% c("₹30,001 - ₹40,000",
                                    "₹30,001 - ₹40,001") ~ "₹30,000 - ₹40,000",
      TRUE ~ average_monthly_income          # keep other values and NA as is
    )
  )

# convert to numeric and round to nearest whole number
df$community_towards_supporting_women_entrepreneurs <-
    round(as.numeric(df$community_towards_supporting_women_entrepreneurs), 0)

# Quick peek (prints to console)
cat("Rows:", nrow(df), "Columns:", ncol(df), "\n")
print(names(df))    # list column names so you can check variable mapping

# Save column list for manual reference if needed
write.csv(data.frame(col_index=seq_along(names(df)), col_name=names(df)),
          file = "column_names_preview.csv", row.names = FALSE)
# === Helper functions ===
# select columns by keyword(s), returns vector of names
select_by_kw <- function(df, keywords){
  pattern <- paste(keywords, collapse="|")
  names(df)[grepl(pattern, names(df), ignore.case = TRUE)]
}

# convert common Yes/No-like columns to binary 1/0
to_binary <- function(x){
  x <- as.character(x)
  x <- trimws(tolower(x))
  out <- ifelse(x %in% c("yes","y","true","1","agree","support"), 1,
                ifelse(x %in% c("no","n","false","0","disagree","not support"), 0, NA))
  return(out)
}

# safe numeric parse
safe_num <- function(x){
  as.numeric(gsub("[^0-9.-]", "", as.character(x)))
}
-------------------------------------------------------------------------------------
# ########### 2. Socio-demographic profiles (descriptives) ===########

-------------------------------------------------------------------------------------
  
# Identify common demographic columns by keywords; adjust keywords if needed
demo_cols <- select_by_kw(df, c("age","gender","educat","marital","occupation",
                                "household","residen","income","house","duration","how long"))
demo_cols
#demo_cols<-demo_cols[-11] #remove not required

# Frequency tables for categorical demographic vars
for(col in demo_cols){
  cat("\n---", col, "---\n")
  print(df %>% count(.data[[col]]) %>% mutate(pct = n / sum(n) * 100) %>% arrange(desc(n)))
}

##table preparation
#library(openxlsx)   # or writexl

freq_list <- lapply(demo_cols, function(col) {
  df %>%
    count(!!sym(col), name = "n") %>%                # frequency
    mutate(
      variable = col,
      pct = round(100 * n / sum(n), 2)
    ) %>%
    rename(level = !!sym(col)) %>%                   # rename the value column
    dplyr::select(variable, level, n, pct)
})

# Combine all tables into one data frame
freq_df <- bind_rows(freq_list)
#view(freq_df)
# Write to Excel
#openxlsx::write.xlsx(freq_df, "demographic_frequencies.xlsx")

# --- Build the plot object ---

p <- ggplot(freq_df, aes(x = fct_infreq(level), y = n, fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.2, size = 3) +
  # wrap facet labels to two lines if they’re long
  facet_wrap(
    ~ variable, ncol = 3, nrow = 4,
    scales = "free_x",
    labeller = label_wrap_gen(width = 20)   # adjust width for desired wrapping
  ) +
  labs(
    x = NULL,
    y = "Count",
    title = "Demographic Frequencies"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 9)    # facet strip text size
  )

print(p)



# --- Save to file ---
# choose your path/format; width & height in inches, dpi for resolution
#ggsave("demographic_frequencies.png", plot = p,
       #width = 10, height = 12, dpi = 300)

# You could also save as PDF:
# ggsave("demographic_frequencies.jpg", plot = p, width = 10, height = 7)

# Numeric summary for income if indeed numeric
income_col <- select_by_kw(df, c("monthly_income","income","average_monthly_household_income"))
income_col
if(length(income_col)){
  inc <- safe_num(df[[income_col[1]]])
  summary(inc)
  skimr::skim(inc)
  # create income groups
  df <- df %>% mutate(income_num = safe_num(.data[[income_col[1]]]),
                      income_group = cut(income_num, breaks=c(-Inf,5000,10000,20000,50000,Inf),
                                         labels=c("<5k","5-10k","10-20k","20-50k",">50k")))
}

-------------------------------------------------------------------------------------

# === 3. Livelihoods & gender roles ===
-------------------------------------------------------------------------------------
  
# Primary source of income, women's occupation, community support for women entrepreneurs
primary_income_col <- select_by_kw(df, c("primary_source_of_income","primary_source","source_of_income","what_is_your_primary_source"))
women_occupation_col <- select_by_kw(df, c("primary_occupation_of_women","occupation_of_women","women"))
women_support_col <- select_by_kw(df, c("support_towards_women","supportive_towards_women","how_supportive"))
#unique(df$community_towards_supporting_women_entrepreneurs)

# Frequency and cross-tab examples
if(length(primary_income_col)) {
  df %>% count(.data[[primary_income_col[1]]]) %>% mutate(pct = n/sum(n)*100)
}
if(length(women_occupation_col)) {
  df %>% count(.data[[women_occupation_col[1]]]) %>% mutate(pct = n/sum(n)*100)
}
# cross tab gender vs women's occupation (if relevant)
if("gender" %in% names(df) & length(women_occupation_col)) {
  tab <- table(df$gender, df[[women_occupation_col[1]]])
  print(tab)
  print(janitor::adorn_totals(as.data.frame.matrix(tab), "row"))
}
-------------------------------------------------------------------------------------
  
# === 4. Community awareness & attitudes (Likert / knowledge items) ===
-------------------------------------------------------------------------------------
  
# Auto-identify awareness/attitude items by keywords
awareness_cols <- select_by_kw(df, c("importance","aware","knowledge","threat","climate","willing","support","attitude","how_supportive"))
awareness_cols

for(col in awareness_cols){
    print(df %>% count(.data[[col]]) %>% mutate(pct = n/sum(n)*100))
 }

##table preparation
#library(openxlsx)   # or writexl

freq_list1 <- lapply(awareness_cols, function(col1) {
  df %>%
    count(!!sym(col1), name = "n") %>%                # frequency
    mutate(
      variable = col1,
      pct = round(100 * n / sum(n), 2)
    ) %>%
    rename(level = !!sym(col1)) %>%                   # rename the value column
    dplyr::select(variable, level, n, pct)
})

# Combine all tables into one data frame
#freq_df1 <- bind_rows(freq_list1)
freq_df1 <- freq_list1 %>%
  lapply(\(x) mutate(x, level = as.character(level))) %>%
  bind_rows()
# Write to Excel
#openxlsx::write.xlsx(freq_df1, "Community awareness & attitudes.xlsx")

# --- Build the plot object ---

p1 <- ggplot(freq_df1, aes(x = fct_infreq(level), y = n, fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.2, size = 3) +
  # wrap facet labels to two lines if they’re long
  facet_wrap(
    ~ variable, ncol = 3, nrow = 4,
    scales = "free_x",
    labeller = label_wrap_gen(width = 20)   # adjust width for desired wrapping
  ) +
  labs(
    x = NULL,
    y = "Count",
    title = "Community awareness & attitudes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 9)    # facet strip text size
  )

print(p1)



# --- Save to file ---
# choose your path/format; width & height in inches, dpi for resolution
#ggsave("Community awareness & attitudes.png", plot = p1,
       #width = 9, height = 12, dpi = 300)

-------------------------------------------------------------------------------------  
# === 5. Perceptions of governance & participation ===
-------------------------------------------------------------------------------------
govern_cols <- select_by_kw(df, c("government","administrat","community_involvement","willing_to_collaborate","collaborate_with_gov","perception"))
govern_cols

# descriptive
for(col in govern_cols){
  print(df %>% count(.data[[col]]) %>% mutate(pct = n/sum(n)*100))
}


##table preparation
#library(openxlsx)   # or writexl

freq_list2 <- lapply(govern_cols, function(col2) {
  df %>%
    count(!!sym(col2), name = "n") %>%                # frequency
    mutate(
      variable = col2,
      pct = round(100 * n / sum(n), 2)
    ) %>%
    rename(level = !!sym(col2)) %>%                   # rename the value column
    dplyr::select(variable, level, n, pct)
})

# Combine all tables into one data frame
#freq_df1 <- bind_rows(freq_list1)
freq_df2 <- freq_list2 %>%
  lapply(\(x) mutate(x, level = as.character(level))) %>%
  bind_rows()
# Write to Excel
#openxlsx::write.xlsx(freq_df2, "Perceptions of governance & participation.xlsx")

# --- Build the plot object ---

p2 <- ggplot(freq_df2, aes(x = fct_infreq(level), y = n, fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.2, size = 3) +
  # wrap facet labels to two lines if they’re long
  facet_wrap(
    ~ variable, ncol = 3, nrow = 4,
    scales = "free_x",
    labeller = label_wrap_gen(width = 20)   # adjust width for desired wrapping
  ) +
  labs(
    x = NULL,
    y = "Count",
    title = "Perceptions of governance & participation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 9)    # facet strip text size
  )

print(p2)



# --- Save to file ---
# choose your path/format; width & height in inches, dpi for resolution
#ggsave("Perceptions of governance & participation.png", plot = p2,
      # width = 9, height = 12, dpi = 300)

-------------------------------------------------------------------------------------  
# === 6. Marine and coastal ecological issues ===
-------------------------------------------------------------------------------------  

# detect reef/fish/pollution related columns
eco_cols <- select_by_kw(df, c("coral","reef","fish_availability","pollut","oil","threat_to","habitat","ecotour"))
eco_cols
for(col in eco_cols){
  print(df %>% count(.data[[col]]) %>% mutate(pct = n/sum(n)*100))
}

##table preparation
#library(openxlsx)   # or writexl

freq_list3 <- lapply(eco_cols, function(col3) {
  df %>%
    count(!!sym(col3), name = "n") %>%                # frequency
    mutate(
      variable = col3,
      pct = round(100 * n / sum(n), 2)
    ) %>%
    rename(level = !!sym(col3)) %>%                   # rename the value column
    dplyr::select(variable, level, n, pct)
})

# Combine all tables into one data frame
freq_df3 <- freq_list3 %>%
  lapply(\(x) mutate(x, level = as.character(level))) %>%
  bind_rows()
# Write to Excel
#openxlsx::write.xlsx(freq_df3, "ecological issues.xlsx")

# --- Build the plot object ---

p3 <- ggplot(freq_df3, aes(x = fct_infreq(level), y = n, fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.2, size = 3) +
  # wrap facet labels to two lines if they’re long
  facet_wrap(
    ~ variable, ncol = 3, nrow = 2,
    scales = "free_x",
    labeller = label_wrap_gen(width = 20)   # adjust width for desired wrapping
  ) +
  labs(
    x = NULL,
    y = "Count",
    title = "ecological issues"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 9)    # facet strip text size
  )

print(p3)

# --- Save to file ---
# choose your path/format; width & height in inches, dpi for resolution
#ggsave("ecological issues.png", plot = p3,
       #width = 9, height = 7, dpi = 300)


# Example: cross-tab perceived threat vs noticed fish availability changes
fish_change_col <- select_by_kw(df, c("changes_in_fish","notice_changes_in_fish","how_often_notice_changes_in_fish"))
threat_col <- select_by_kw(df, c("greatest_threat","threat_to_mangroves","threat_to_seagrass"))
if(length(fish_change_col) && length(threat_col)){
  tab <- table(df[[fish_change_col[1]]], df[[threat_col[1]]])
  print(tab)
  # chi-sq test (check expected frequencies)
  if(all(chisq.test(tab)$expected > 5)){
    print(chisq.test(tab))
  } else {
    print(fisher.test(tab))
  }
}
chisq.test(tab)
-------------------------------------------------------------------------------------  
# === 7. Adaptation to climate change & environmental change ===
-------------------------------------------------------------------------------------  
  
climate_cols <- select_by_kw(df, c("coastal_erosion","erosion","mitigate","climate_change","climate"))
climate_cols
for(col in climate_cols){
  print(df %>% count(.data[[col]]) %>% mutate(pct = n/sum(n)*100))
}

##table preparation
#library(openxlsx)   # or writexl

freq_list4 <- lapply(climate_cols, function(col4) {
  df %>%
    count(!!sym(col4), name = "n") %>%                # frequency
    mutate(
      variable = col4,
      pct = round(100 * n / sum(n), 2)
    ) %>%
    rename(level = !!sym(col4)) %>%                   # rename the value column
    dplyr::select(variable, level, n, pct)
})

# Combine all tables into one data frame
freq_df4 <- freq_list4 %>%
  lapply(\(x) mutate(x, level = as.character(level))) %>%
  bind_rows()
# Write to Excel
#openxlsx::write.xlsx(freq_df4, "Adaptation to climate change.xlsx")

# --- Build the plot object ---

p4 <- ggplot(freq_df4, aes(x = fct_infreq(level), y = n, fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.2, size = 3) +
  # wrap facet labels to two lines if they’re long
  facet_wrap(
    ~ variable, ncol = 3, nrow = 2,
    scales = "free_x",
    labeller = label_wrap_gen(width = 20)   # adjust width for desired wrapping
  ) +
  labs(
    x = NULL,
    y = "Count",
    title = "Adaptation to climate change"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 9)    # facet strip text size
  )

print(p4)

# --- Save to file ---
# choose your path/format; width & height in inches, dpi for resolution
#ggsave("Adaptation to climate change.png", plot = p4,
       #width = 9, height = 7, dpi = 300)

-------------------------------------------------------------------------------------  
# === 8. Tourism and sustainability ===
-------------------------------------------------------------------------------------  
tourism_cols <- select_by_kw(df, c("tourism","tourist","eco-tour","ecotour","sustainab"))
tourism_cols

#unique(df$what_variety_of_fish_are_relished_by_the_tourists_mention_according_to_the_ranking_of_the_preference)
#tourism_cols <- tourism_cols[-1]

for(col in tourism_cols){
  print(df %>% count(.data[[col]]) %>% mutate(pct = n/sum(n)*100))
}

##table preparation
#library(openxlsx)   # or writexl

freq_list5 <- lapply(tourism_cols, function(col5) {
  df %>%
    count(!!sym(col5), name = "n") %>%                # frequency
    mutate(
      variable = col5,
      pct = round(100 * n / sum(n), 2)
    ) %>%
    rename(level = !!sym(col5)) %>%                   # rename the value column
    dplyr::select(variable, level, n, pct)
})

# Combine all tables into one data frame
freq_df5 <- freq_list5 %>%
  lapply(\(x) mutate(x, level = as.character(level))) %>%
  bind_rows()
# Write to Excel
#openxlsx::write.xlsx(freq_df5, "Tourism and sustainability.xlsx")

# --- Build the plot object ---

p5 <- ggplot(freq_df5, aes(x = fct_infreq(level), y = n, fill = variable)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.2, size = 3) +
  # wrap facet labels to two lines if they’re long
  facet_wrap(
    ~ variable, ncol = 3, nrow = 2,
    scales = "free_x",
    labeller = label_wrap_gen(width = 20)   # adjust width for desired wrapping
  ) +
  labs(
    x = NULL,
    y = "Count",
    title = "Tourism and sustainability"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text  = element_text(size = 9)    # facet strip text size
  )

print(p5)

# --- Save to file ---
# choose your path/format; width & height in inches, dpi for resolution
#ggsave("Tourism and sustainability.png", plot = p5,
       #width = 9, height = 7, dpi = 300)

-------------------------------------------------------------------------------------  
# === 9. Cross-tabulations & subgroup comparisons ===
-------------------------------------------------------------------------------------  

# Example cross-tabs and chi-square tests you requested:
# gender vs willingness to participate in restoration
participate_cols <- select_by_kw(df, c("participat","would_you_like_to_participate","participation"))
participate_cols
if(length(participate_cols) && "gender" %in% names(df)){
  tab <- table(df$gender, to_binary(df[[participate_cols[1]]]))
  print(tab)
  # Fisher or chi-square
  if(all(chisq.test(tab)$expected > 5)) print(chisq.test(tab)) else print(fisher.test(tab))
}

# occupation vs perceived importance of seagrass/mangrove
importance_cols <- select_by_kw(df, c("importance_of_mangrove","importance_of_seagrass","how_important"))
occupation_col <- select_by_kw(df, c("^occupation$|occupation_"))
unique(df[[importance_cols[1]]])

if (length(occupation_col) && length(importance_cols)) {
  # get the first importance column
  imp <- df[[importance_cols[1]]]
  
  # convert text responses to numeric 1–4
  if (!is.numeric(imp)) {
    imp <- dplyr::case_when(
      tolower(trimws(imp)) %in% c("very important", "4")       ~ 4,
      tolower(trimws(imp)) %in% c("moderately important", "3") ~ 3,
      tolower(trimws(imp)) %in% c("less important", "2")       ~ 2,
      tolower(trimws(imp)) %in% c("not important", "1")        ~ 1,
      TRUE ~ NA_real_
    )
  }
  
  df$imp_numeric <- imp
  
  # run Kruskal–Wallis if we have at least 3 unique numeric levels
  if (length(unique(na.omit(df$imp_numeric))) > 2) {
    kruskal.test(imp_numeric ~ as.factor(df[[occupation_col[1]]]), data = df)
  }
}
-------------------------------
govt_sup<-select_by_kw(df, c("government_provides_support"))
clim_feel<-select_by_kw(df, c("feel_the_effects_of_climate_change"))
clim_prog<-select_by_kw(df, c("government_programmes_for_climate"))
grt_threat<-select_by_kw(df, c("greatest_threat_to_mangroves_and_seagrass"))
collab_govt<-select_by_kw(df, c("collaborate_with_government_officials"))

--------------------

# === 10. Regression / correlation analyses ===
# Example 1: Logistic regression, outcome = willingness to participate in restoration (binary)
if(length(participate_cols)){
  y <- to_binary(df[[participate_cols[1]]])
  # choose predictors: education, age_group, income_group, occupation, awareness_items
  pred_edu <- select_by_kw(df, c("educat"))
  pred_age <- select_by_kw(df, c("age_group"))
  pred_occ <- select_by_kw(df, c("occupation"))
  pred_inc <- select_by_kw(df, c("monthly_income"))
  # prepare modeling df
  model_df <- df %>%
    transmute(y = y,
              education = .data[[pred_edu[1]]],
              age_group = .data[[pred_age[1]]],
              occupation = .data[[pred_occ[1]]],
              income_num = if(!is.null(pred_inc)) .data[[pred_inc]] else NA_real_,
              #awareness = awareness_cols
              ) %>%
    mutate(across(c(education, age_group, occupation), as.factor))
  model_df <- na.omit(model_df)
  if(nrow(model_df) > 20){
    fit_glm <- glm(y ~ education + age_group + occupation + income_num 
                   #+ awareness
                   ,
                   data = model_df, family = binomial)
    summary(fit_glm)
    broom::tidy(fit_glm) %>% mutate(OR = exp(estimate), lower = exp(estimate - 1.96*std.error), upper = exp(estimate + 1.96*std.error))
    # check VIF
    if(requireNamespace("car", quietly = TRUE)) car::vif(fit_glm)
  } else cat("Not enough complete cases for logistic regression\n")
}

# extract coefficient summary as a data frame
coef_tbl1 <- broom::tidy(fit_glm) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value)%>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# write to CSV
#write_csv(coef_tbl1, "glm1_coefficients.csv")

# Example 2: logistic/binary regression, outcome = "would_you_support_regulations_limiting_boat_anchoring_near_seagrass_meadows" (binary)
support_col <- select_by_kw(df, c("support_a_community","support_regulations"))
if(length(support_col)){
  supp1 <- to_binary(df[[support_col[1]]])
  supp2 <- df[[support_col[2]]]

  model_df1 <- df %>%
    transmute(y1 = supp1,
              education = .data[[pred_edu[1]]],
              age_group = .data[[pred_age[1]]],
              occupation = .data[[pred_occ[1]]],
              income_num = if(!is.null(pred_inc)) .data[[pred_inc]] else NA_real_,
              #awareness = awareness_cols
    ) %>%
    mutate(across(c(education, age_group, occupation), as.factor))
  model_df1 <- na.omit(model_df1)
  if(nrow(model_df1) > 20){
    fit_glm1 <- glm(y1 ~ education + age_group + occupation + income_num 
                   #+ awareness
                   ,
                   data = model_df1, family = binomial)
    summary(fit_glm1)
    broom::tidy(fit_glm1) %>% mutate(OR = exp(estimate), lower = exp(estimate - 1.96*std.error), upper = exp(estimate + 1.96*std.error))
    # check VIF
    if(requireNamespace("car", quietly = TRUE)) car::vif(fit_glm1)
  } else cat("Not enough complete cases for logistic regression\n")
}

# extract coefficient summary as a data frame
coef_tbl2 <- broom::tidy(fit_glm1) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value)%>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# write to CSV
#write_csv(coef_tbl2, "glm2_coefficients.csv")

# Example 3: logistic/binary regression, outcome = "support_a_community_program_for_mangrove_seagrass_restoration" (binary)
support_col <- select_by_kw(df, c("support_a_community","support_regulations"))
if(length(support_col)){
  supp1 <- to_binary(df[[support_col[1]]])
  supp2 <- df[[support_col[2]]]
  
  model_df2 <- df %>%
    transmute(y2 = supp2,
              education = .data[[pred_edu[1]]],
              age_group = .data[[pred_age[1]]],
              occupation = .data[[pred_occ[1]]],
              income_num = if(!is.null(pred_inc)) .data[[pred_inc]] else NA_real_,
              #awareness = awareness_cols
    ) %>%
    mutate(across(c(education, age_group, occupation), as.factor))
  model_df2 <- na.omit(model_df2)
  if(nrow(model_df2) > 20){
    library(nnet)
    fit_glm2 <- multinom(y2 ~ education + age_group + occupation + income_num,
                         data = model_df2)
    
    summary(fit_glm2)
    broom::tidy(fit_glm2) %>% mutate(OR = exp(estimate), lower = exp(estimate - 1.96*std.error), upper = exp(estimate + 1.96*std.error))
    # check VIF
    if(requireNamespace("car", quietly = TRUE)) car::vif(fit_glm2)
  } else cat("Not enough complete cases for logistic regression\n")
}


# Correlation example: education level numeric proxy vs awareness score
if(length(pred_edu) && exists("df")){
  # try to make education numeric by years/levels if possible
  # user may need to manually map education level to numeric if text
  # crude mapping example:
  df <- df %>% mutate(education_num = case_when(
    grepl("primary|1-5|5th", .data[[pred_edu[1]]], ignore.case = TRUE) ~ 5,
    grepl("secondary|10th|high", .data[[pred_edu[1]]], ignore.case = TRUE) ~ 10,
    grepl("higher|graduate|bachelor", .data[[pred_edu[1]]], ignore.case = TRUE) ~ 15,
    grepl("post|master|phd|doctor", .data[[pred_edu[1]]], ignore.case = TRUE) ~ 18,
    TRUE ~ NA_real_))
  cor.test(df$education_num, df$awareness_items, use="complete.obs", method = "spearman")
}

# === 11. Qualitative analysis for open-ended responses ===
# Identify open text columns (likely long descriptive columns)
text_cols <- names(df)[sapply(df, function(x) any(nchar(as.character(x)) > 20, na.rm=TRUE))]
text_cols <- unique(c(text_cols, select_by_kw(df, c("if_others","specify","explain","what_changes","what_kind","please_specify"))))
text_cols

# Simple token counts and top words for a chosen open question
{open_q <- text_cols[1]   # change index to examine other open-ended questions
cat("Analysing column:", open_q, "\n")
#text_df <- df %>% dplyr::select(id = row_number(), text = .data[[open_q]]) %>% filter(!is.na(text))
text_df <- df %>%
  dplyr::mutate(
    id   = dplyr::row_number(),
    text = .data[[open_q]]
  ) %>%
  dplyr::filter(!is.na(text)) %>%
  dplyr::select(id, text)


# tidytext pipeline
#install.packages("SnowballC")      # run once
#library(SnowballC)
tokens <- text_df %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(), by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  mutate(word = wordStem(word))   # from SnowballC via tidytext (if available)

# top words
top_words <- tokens %>% count(word, sort = TRUE) %>% head(50)
print(top_words)

# wordcloud
if(nrow(top_words)) wordcloud2::wordcloud2(top_words, size=0.8)
}
# Keyword-based thematic coding: define dictionary and classify responses
dictionary <- list(
  pollution = c("pollut", "oil", "garbag", "wast", "sewage"),
  overfishing = c("overfish", "trawl", "trawling", "net", "catch"),
  tourism = c("touris", "boat", "resort", "visitor"),
  erosion = c("eros", "shore", "coast", "sand", "beach"),
  climate = c("climat", "temperature", "sea level", "storm")
)
theme_assign <- function(text){
  text_l <- tolower(as.character(text))
  found <- sapply(dictionary, function(kw) any(sapply(kw, function(p) grepl(p, text_l, fixed=FALSE))))
  if(any(found)) paste(names(which(found)), collapse=";") else "other"
}
text_df <- text_df %>% mutate(theme = sapply(text, theme_assign))
text_df %>% count(theme) %>% arrange(desc(n))

# LDA topic modelling for open responses
if(nrow(text_df) >= 20){
  corpus <- quanteda::corpus(text_df, text_field = "text")
  dfm <- quanteda::dfm(corpus, remove = stopwords("en"), remove_punct = TRUE) %>% dfm_trim(min_termfreq = 5)
  k <- 4  # choose 3-7 and inspect coherence
  lda <- topicmodels::LDA(convert(dfm, to = "topicmodels"), k = k, control = list(seed = 1234))
  terms(lda, 10)   # top 10 terms per topic
  topics <- topicmodels::topics(lda, 1)
  text_df$topic <- as.factor(topics)
  table(text_df$topic)
}

# === 12. Outputs: export tables and plots for reporting ===
# Example: demographic summary table
demo_summary <- df %>% select(all_of(intersect(names(df), demo_cols))) %>% summarize_all(~list(table(.)))
# Write cleaned dataset and summary
writexl::write_xlsx(list(cleaned_data = df, demo_table = as.data.frame(table(df[[demo_cols[1]]]))),
                    path = "survey_analysis_outputs.xlsx")

# Export regression results
if(exists("fit_glm")){
  glm_res <- broom::tidy(fit_glm, conf.int = TRUE) %>%
    mutate(OR = exp(estimate), OR_low = exp(conf.low), OR_high = exp(conf.high))
  write.csv(glm_res, "glm_results.csv", row.names = FALSE)
}

# Save sample plots
ggsave("demographic_barplot.png", width=8, height=5)

#################### THE END ###############################

