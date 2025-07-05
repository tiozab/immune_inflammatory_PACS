# Load libraries
library(dplyr)
library(tibble)
library(purrr)
library(sandwich)
library(lmtest)
library(meta)
library(ggplot2)
library(cowplot)
library(ggsci)

# ---- Prepare long data ----

infection_df <- incidence_estimates_help %>%
  filter(denominator_strata_cohort_name == "infection") %>%
  select(database_name, denominator_age_group, denominator_sex,
         outcome_cohort_name, n_events, person_years) %>%
  mutate(group = "infection")

testneg_df <- incidence_estimates_help %>%
  filter(denominator_strata_cohort_name == "test_negative") %>%
  select(database_name, denominator_age_group, denominator_sex,
         outcome_cohort_name, n_events, person_years) %>%
  mutate(group = "test_negative")

poisson_df <- bind_rows(infection_df, testneg_df) %>%
  mutate(
    group_binary = ifelse(group == "infection", 1, 0),
    age_group = as.factor(denominator_age_group),
    sex = as.factor(denominator_sex)
  )

databases_list <- unique(poisson_df$database_name)
outcomes_list <- unique(poisson_df$outcome_cohort_name)

# ---- Function to compute adjusted log IRR per database ----

adjusted_IRR_per_database <- function(data, db, outcome_name, adjust_vars = NULL) {
  
  df_db <- data %>%
    filter(database_name == db, outcome_cohort_name == outcome_name)
  
  if (nrow(df_db) == 0 || sum(df_db$n_events) == 0) {
    return(tibble(database_name = db, logIRR = NA, se = NA))
  }
  
  formula_text <- if (!is.null(adjust_vars)) {
    paste0("n_events ~ group_binary + ", paste(adjust_vars, collapse = " + "))
  } else {
    "n_events ~ group_binary"
  }
  formula_obj <- as.formula(formula_text)
  
  # Try-catch wrapper
  out <- tryCatch({
    model <- glm(
      formula_obj,
      offset = log(person_years),
      family = poisson(link = "log"),
      data = df_db
    )
    
    rob_se <- sandwich::vcovHC(model, type = "HC0")
    coefs <- lmtest::coeftest(model, vcov = rob_se)
    
    log_IRR <- coefs["group_binary", "Estimate"]
    se_log_IRR <- coefs["group_binary", "Std. Error"]
    
    tibble(database_name = db, logIRR = log_IRR, se = se_log_IRR)
    
  }, error = function(e) {
    # If error: return NA row
    tibble(database_name = db, logIRR = NA_real_, se = NA_real_)
  })
}

# ---- Meta-analysis for each outcome ----

meta_adj_results_all <- function(adjust_vars = NULL) {
  map_dfr(outcomes_list, function(outc) {

    per_db_df <- map_dfr(databases_list, function(db) {
      adjusted_IRR_per_database(poisson_df, db, outc, adjust_vars)
    }) %>% filter(!is.na(logIRR))
    
    if (nrow(per_db_df) == 0) {
      return(tibble(outcome = outc, IRR_adj = NA, IRR_low_adj = NA, IRR_high_adj = NA))
    }
    
    meta_adj <- meta::metagen(
      TE = per_db_df$logIRR,
      seTE = per_db_df$se,
      sm = "IRR",
      hakn = TRUE,
      method.tau = "DL"
    )
    
    tibble(
      outcome = outc,
      IRR_adj = exp(meta_adj$TE.random),
      IRR_low_adj = exp(meta_adj$lower.random),
      IRR_high_adj = exp(meta_adj$upper.random)
    )
  })
}

# ---- Compute crude, age-adj, sex-adj ----

crude_IRRs <- meta_adj_results_all(adjust_vars = NULL)
age_adj_IRRs <- meta_adj_results_all(adjust_vars = c("age_group"))
sex_adj_IRRs <- meta_adj_results_all(adjust_vars = c("sex"))

# ---- Add condition factor levels ----

condition_levels <- c("t1dm","mis","ibd","sle","juvenile_arthritis","ra","me_cfs_symptoms","me_cfs","dysautonomia","pots")
condition_labels <- c("T1DM","MIS","IBD","SLE","Juvenile arthritis","RA","ME/CFS symptoms","ME/CFS diagnosis","POTS symptoms","POTS diagnosis")

crude_IRRs <- crude_IRRs %>%
  mutate(conditions = factor(outcome, levels = condition_levels, labels = condition_labels))

age_adj_IRRs <- age_adj_IRRs %>%
  mutate(conditions = factor(outcome, levels = condition_levels, labels = condition_labels))

sex_adj_IRRs <- sex_adj_IRRs %>%
  mutate(conditions = factor(outcome, levels = condition_levels, labels = condition_labels))

# ---- Plot function ----

plot_adjusted <- function(df, title_text) {
  ggplot(df, aes(x = IRR_adj, y = conditions)) + 
    geom_point(size = 1.5) +
    geom_errorbar(aes(xmin = IRR_low_adj, xmax = IRR_high_adj), width = 0.3) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    scale_x_continuous(limits = c(0.3, 4), trans = scales::log2_trans()) +
    ggsci::scale_color_lancet(alpha = 0.7) +
    labs(x = "IRR", y = "", title = title_text) +
    theme_bw() +
    theme(
      text = element_text(family = "serif"),
      axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size = 9),
      panel.grid.minor.x = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 11)
    )
}

# ---- Create plots ----

plot_crude <- plot_adjusted(crude_IRRs, "Crude IRR (meta-analysis)")
plot_age   <- plot_adjusted(age_adj_IRRs, "Age-adjusted IRR (meta-analysis)")
plot_sex   <- plot_adjusted(sex_adj_IRRs, "Sex-adjusted IRR (meta-analysis)")
plot_meta  <- plot_list$All + labs(title = "Stratified meta IRR (original)") + theme(plot.title = element_text(hjust = 0.5, size = 11))

# ---- Combine plots ----

final_plot <- plot_grid(
  plot_crude,
  plot_age,
  plot_sex,
  plot_meta,
  nrow = 2,
  labels = c("A", "B", "C", "D"),
  label_size = 12
)

# ---- Save ----

ggsave("combined_IRR_meta_plots.pdf", final_plot, width = 12, height = 10)


