if (!requireNamespace("QuantPsyc", quietly = TRUE)) {
  install.packages("QuantPsyc")
}

multiple_regression <- function(data) {
  model <- lm(hinge ~ age + residuals, data = data)
  model_summary <- summary(model)
  
  # Obtain standardized coefficients
  standardized_coefficients <- QuantPsyc::lm.beta(model)
  
  age_coeff <- round(standardized_coefficients["age"], 3)
  residuals_coeff <- round(standardized_coefficients["residuals"], 3)
  
  age_p_value <- round(model_summary$coefficients["age", "Pr(>|t|)"], 3)
  residuals_p_value <- round(model_summary$coefficients["residuals", "Pr(>|t|)"], 3)
  
  adjusted_r_squared <- round(model_summary$adj.r.squared, 2)
  
  result <- data.frame(Standardized_Coeff_Age = age_coeff,
                       Standardized_Coeff_Residuals = residuals_coeff,
                       P_value_Age = age_p_value,
                       P_value_Residuals = residuals_p_value,
                       Adjusted_R_squared = adjusted_r_squared)
  
  return(result)
}
