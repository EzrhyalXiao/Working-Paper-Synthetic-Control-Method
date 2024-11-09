install.packages("Synth")
library(Synth)

# Import data
data <- read.csv("scm_light_data.csv")

# Define treated city and create treatment indicator
treated_city <- "Shanghai"
data$treated <- ifelse(data$city == treated_city, 1, 0)

# Define outcome and predictor variables
outcome_var <- "nightlight_intensity"
predictor_vars <- c("tourism_revenue", "urban_infrastructure", "light_industry_sales")

# Prepare data for synthetic control
data_long <- reshape(data, idvar = "city", timevar = "year", direction = "long")
X <- data_long[, predictor_vars]
Y <- data_long[, outcome_var]
treated_unit <- which(data_long$city == treated_city)[1]

# Run synthetic control
synth_result <- synth(Y = Y, X = X, Tr = data_long$treated, Trunit = treated_unit)

# Output results
summary(synth_result)
plot(synth_result)

# Save results
saveRDS(synth_result, "synth_results.rds")

# Further analysis
data_long$synth_nightlight_intensity <- synth_result$Yhat
data_long$difference <- data_long$nightlight_intensity - data_long$synth_nightlight_intensity
t_test_result <- t.test(data_long$difference)
print(t_test_result)

# Placebo test
for (i in 2:35) {
  data_long$placebo_treated <- ifelse(data_long$city == paste("City", i, sep = ""), 1, 0)
  placebo_result <- synth(Y = Y, X = X, Tr = data_long$placebo_treated, Trunit = which(data_long$city == paste("City", i, sep = ""))[1])
  print(summary(placebo_result))
}

# Sensitivity analysis with new predictor variables
predictor_vars2 <- c("tourism_revenue2", "urban_infrastructure2", "light_industry_sales2")
X2 <- data_long[, predictor_vars2]
synth_result2 <- synth(Y = Y, X = X2, Tr = data_long$treated, Trunit = treated_unit)
summary(synth_result2)

# Sensitivity analysis with restricted control units
control_units <- 2:8
X_control <- X[control_units, ]
Y_control <- Y[control_units]
synth_result_control <- synth(Y = Y_control, X = X_control, Tr = data_long$treated, Trunit = treated_unit)
summary(synth_result_control)
