#pharmetrics and UiO does not have test negative information (take it out)
# specific populations
incidence_estimates_help <- rbind(CPRDGOLD[[2]],
                                  IMASIS[[2]],
                                  AUSOM[[2]],
                                  IPCI[[2]],
                                  eDOL_CHUM[[2]],
                                  CORIVA[[2]],
                                  CPRDAurum[[2]])

# Assign variant periods
assign_variant <- function(date) {
  case_when(
    date >= as.Date("2020-12-01") & date < as.Date("2021-06-30") ~ "Alpha",
    date >= as.Date("2021-06-30") & date < as.Date("2021-12-15") ~ "Delta",
    date >= as.Date("2021-12-15") & date < as.Date("2022-03-31") ~ "Omicron BA.1",
    date >= as.Date("2022-04-01")                               ~ "Omicron BA.2+",
    TRUE ~ "Other"
  )
}

monthly_wide_df <- incidence_estimates_help %>%
  filter(denominator_strata_cohort_name %in% c("infection", "test_negative"), 
         analysis_interval == "months",
         denominator_age_group == "0 to 150",
         denominator_sex == "Both") %>%
  pivot_wider(
    id_cols = c(database_name, denominator_age_group, denominator_sex, outcome_cohort_name, incidence_start_date),
    names_from = denominator_strata_cohort_name,
    values_from = c(n_persons, person_years, n_events, incidence_100000_pys)
  ) %>%
  filter( !is.na(incidence_100000_pys_infection), !is.na(incidence_100000_pys_test_negative)) %>% 
  janitor::clean_names() %>%
  ungroup() %>%
  mutate(variant = assign_variant(incidence_start_date))

# Function to compute IRR using metainc per variant period
compute_variant_IRR <- function(df, condition_name) {
  df %>%
    filter(outcome_cohort_name == condition_name) %>%
    group_by(database_name, variant) %>%
    summarise(
      n_events_infection = sum(n_events_infection, na.rm = TRUE),
      person_years_infection = sum(person_years_infection, na.rm = TRUE),
      n_events_test_negative = sum(n_events_test_negative, na.rm = TRUE),
      person_years_test_negative = sum(person_years_test_negative, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(condition = condition_name)
}

# Get unique conditions
conditions <- unique(monthly_wide_df$outcome_cohort_name)

# Loop over conditions
variant_IRR_df <- map_dfr(conditions, ~ compute_variant_IRR(monthly_wide_df, .x))

# Prepare an empty list to store meta-analysis results
meta_list <- list()

# Get unique combinations
all_conditions <- unique(variant_IRR_df$condition)
all_variants <- unique(variant_IRR_df$variant)

# Loop over each condition and variant
for (cond in all_conditions) {
  for (var in all_variants) {
    temp_df <- variant_IRR_df %>%
      filter(condition == cond, variant == var) %>%
      filter(person_years_infection != 0, person_years_test_negative != 0)
    
    if (nrow(temp_df) >= 1 && sum(temp_df$person_years_infection, na.rm = TRUE) > 0 && sum(temp_df$person_years_test_negative, na.rm = TRUE) > 0) {
      meta_obj <- tryCatch(
        meta::metainc(
          event.e = temp_df$n_events_infection,
          time.e = temp_df$person_years_infection,
          event.c = temp_df$n_events_test_negative,
          time.c = temp_df$person_years_test_negative,
          sm = "IRR",
          method.random.ci = "classic",
          method.tau = "REML"
        ),
        error = function(e) NULL
      )
      
      if (!is.null(meta_obj)) {
        meta_list[[length(meta_list) + 1]] <- tibble(
          condition = cond,
          variant = var,
          IRR_random = exp(meta_obj$TE.random),
          IRR_low = exp(meta_obj$lower.random),
          IRR_high = exp(meta_obj$upper.random)
        )
      }
    }
  }
}

# Combine all results
meta_summary_df <- bind_rows(meta_list)

# Order variant factor if you want them nicely in plots
meta_summary_df <- meta_summary_df %>%
  mutate(variant = factor(variant, levels = c("Alpha", "Delta", "Omicron BA.1", "Omicron BA.2+", "Other")))

# Plot
ggplot(meta_summary_df, aes(x = variant, y = IRR_random)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = IRR_low, ymax = IRR_high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ condition, scales = "free_y", ncol = 2) +
  theme_bw(base_size = 14) +
  labs(
    x = "Variant period",
    y = "IRR (Infection vs Test Negative)",
    title = "IRRs by COVID-19 variant period and condition"
  )   +
  scale_y_log10(labels = label_number(accuracy = 0.001)) +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( size = 10, angle = 90, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size=10),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

ggsave(here::here("Results_final","Figure3_variant_free.pdf"))

# Plot
ggplot(meta_summary_df, aes(x = variant, y = IRR_random)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = IRR_low, ymax = IRR_high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ condition, ncol = 2) +
  theme_bw(base_size = 14) +
  labs(
    x = "Variant period",
    y = "IRR (Infection vs Test Negative)",
    title = "IRRs by COVID-19 variant period and condition"
  )   +
  scale_y_log10(labels = label_number(accuracy = 0.1)) +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( size = 10, angle = 90, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size=10),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

ggsave(here::here("Results_final","Figure3_variant.pdf"))

# Specify the Excel file path
excel_file <- here::here("Results_final","variant_IRR_inf_testneg.xlsx")

# Write the list of tibbles to different sheets in Excel
write_xlsx(meta_summary_df, excel_file)


# MONTHLY
# Prepare an empty list to store results
meta_list_months <- list()

# Get unique combinations
all_conditions <- unique(monthly_wide_df$outcome_cohort_name)
all_dates <- unique(monthly_wide_df$incidence_start_date)

# Loop over each condition
for (cond in all_conditions) {
  for (date in all_dates) {
    temp_df <- monthly_wide_df %>%
      filter(outcome_cohort_name == cond, incidence_start_date == date) %>%
      filter(person_years_infection != 0, person_years_test_negative != 0)
    
    # Check there are at least some data (non-zero person-years)
    if (nrow(temp_df) >= 1 && sum(temp_df$person_years_infection, na.rm = TRUE) > 0 && sum(temp_df$person_years_test_negative, na.rm = TRUE) > 0) {
      # Try meta-analysis
      meta_obj <- tryCatch(
        meta::metainc(
          event.e = temp_df$n_events_infection,
          time.e = temp_df$person_years_infection,
          event.c = temp_df$n_events_test_negative,
          time.c = temp_df$person_years_test_negative,
          sm = "IRR",
          method.random.ci = "classic",
          method.tau = "REML"
        ),
        error = function(e) NULL
      )
      
      # Store results if valid
      if (!is.null(meta_obj)) {
        meta_list_months[[length(meta_list_months) + 1]] <- tibble(
          condition = cond,
          incidence_start_date = as.Date(date),
          IRR_random = exp(meta_obj$TE.random),
          IRR_low = exp(meta_obj$lower.random),
          IRR_high = exp(meta_obj$upper.random)
        )
      }
    }
  }
}

# Combine all into one data frame
meta_months_summary_df <- bind_rows(meta_list_months)

ggplot(meta_months_summary_df, aes(x = incidence_start_date, y = IRR_random)) +
  geom_point() +
  geom_errorbar(aes(ymin = IRR_low, ymax = IRR_high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ condition, scales = "free_y", ncol = 2) +
  theme_bw() +
  labs(
    x = "Month (cohort start date)",
    y = "IRR (Infection vs Test Negative)",
    title = "Monthly IRRs by condition"
  )   +
  scale_y_log10(labels = label_number(accuracy = 0.1)) +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( size = 10, angle = 90, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size=10),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

ggsave(here::here("Results_final","Figure3_month_free.pdf"))

ggplot(meta_months_summary_df, aes(x = incidence_start_date, y = IRR_random)) +
  geom_point() +
  geom_errorbar(aes(ymin = IRR_low, ymax = IRR_high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  facet_wrap(~ condition, ncol = 2) +
  theme_bw() +
  labs(
    x = "Month (cohort start date)",
    y = "IRR (Infection vs Test Negative)",
    title = "Monthly IRRs by condition"
  )   +
  scale_y_log10(labels = label_number(accuracy = 0.1)) +
  theme( text = element_text( family = "serif", color = "black"),
         axis.text.x = element_text( size = 10, angle = 90, vjust = 0.5, hjust = 1),
         axis.text.y = element_text( size=10),
         panel.grid.minor.x = element_blank(),
         legend.background = element_rect(fill='transparent'),
         legend.position = "top")

ggsave(here::here("Results_final","Figure3_month.pdf"))

# Specify the Excel file path
excel_file <- here::here("Results_final","monthly_IRR_inf_testneg.xlsx")

# Write the list of tibbles to different sheets in Excel
write_xlsx(meta_months_summary_df, excel_file)
