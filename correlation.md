install.packages("haven")  
library(haven)
library(janitor)
library(tidyverse)
library(psych)       
library(ggcorrplot)  
library(car)        
library(lmtest)
library(ggcorrplot)



data <- read_dta("EKNA_head_with_TNC.dta")

# livelihood data section


livelihood_vars <- c("hproblem_job", "Watt_tour", "Fhhinc_timber", "Fhhinc_nontimber",
                     "Fincomeprod_1", "Fincomeprod_1_oth", "Fincomeprod_2", "Fincomeprod_2_oth",
                     "Parkatt_help", "Parkatt_employ", "Parkatt_meat", "Parkatt_adverse",
                     "Hfrstserv_ben_6", "Hunt_prestige")
livelihood_data <- data %>%
  select(any_of(livelihood_vars)) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.)))) %>%

library(psych)
describe(livelihood_data)

ggplot(livelihood_data, aes(x = hproblem_job)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Frequency that Jobs/Income is a Problem in the Village", x = "Significance of Job/Income Concern", y = "Number of Villages") +
  theme_minimal()

livelihood_data <- data %>%
  select(any_of(livelihood_vars))

livelihood_data <- livelihood_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

livelihood_data_clean <- livelihood_data_numeric %>%
  drop_na()

class(livelihood_data)

livelihood_data <- as.data.frame(livelihood_data)
sapply(livelihood_data, class)

glimpse(livelihood_data)


describe(livelihood_data_clean)

library(psych)
describe(livelihood_data_numeric)


# land clearing data section

data <- read_dta("EKNA_head_with_TNC.dta")
land_clearing_vars <- c(
  "ftimber_yn", "ftimber_punishyn", "ftimber_punish", "ftimber_rulebreak", "ftimber_rulefollow",
  "ftimber_who", "ftimber_whomost", "ffire_yn", "ffire_punishyn", "ffire_punish",
  "ffire_rulebreak", "ffire_rulefollow", "ffire_who", "fhuntfire_yn", "fhuntfire_punishyn",
  "fhuntfire_punish", "fhuntfire_rulebreak", "fhuntfire_rulefollow", "fhuntfire_who",
  "fhuntfire_whomost", "fclear_yn", "fclear_punishyn", "fclear_punish", "fclear_rulebreak",
  "fclear_rulefollow", "fclear_who", "fclear_whomost"
)
land_clearing_data <- data %>%
  select(any_of(land_clearing_vars)) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
  sapply(land_clearing_data, class)

  glimpse(land_clearing_data)
  describe(land_clearing_data)

  pair_counts <- outer(colnames(land_clearing_data), colnames(land_clearing_data), Vectorize(function(x, y) {
    sum(complete.cases(land_clearing_data[[x]], land_clearing_data[[y]]))
  }))
  dimnames(pair_counts) <- list(colnames(land_clearing_data), colnames(land_clearing_data))
  print(pair_counts)
  colSums(is.na(land_clearing_data))
  land_clearing_data_filtered <- land_clearing_data %>%
    select(where(~ sum(!is.na(.)) >= 30))
  no_variation <- land_clearing_data_filtered %>%
    select(where(~ sd(., na.rm = TRUE) == 0))
  print(colnames(no_variation))
  land_clearing_data_filtered <- land_clearing_data %>%
    select(where(~ sum(!is.na(.)) >= 30)) %>%  
    select(where(~ sd(., na.rm = TRUE) > 0))   
  
  cor_matrix <- cor(land_clearing_data_filtered, use = "complete.obs", method = "spearman")
  ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE)
  ggcorrplot(cor_matrix,
             method = "square",
             type = "lower",
             lab = FALSE,                    
             tl.cex = 10,
             tl.srt = 45,
             colors = c("darkred", "white", "darkblue"),
             title = "Spearman Correlation: Land Clearing Variables",
             ggtheme = ggplot2::theme_minimal())
  
  library(psych)
  cor_test <- corr.test(land_clearing_data_filtered, method = "spearman", use = "complete.obs")
  p_mat <- cor_test$p
  
  ggcorrplot(cor_matrix,
             p.mat = p_mat,
             sig.level = 0.05,
             insig = "blank",                
             lab = FALSE,                   
             method = "square",
             type = "lower",
             tl.cex = 10,
             tl.srt = 45,
             colors = c("darkred", "white", "darkblue"),
             title = "Significant Spearman Correlations (p < 0.05)",
             ggtheme = ggplot2::theme_minimal())
  
  
  
  
