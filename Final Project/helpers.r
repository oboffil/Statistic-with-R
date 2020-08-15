# Calculates the Root Mean Squared Error (RMSE) of a model evaluated on a dataset
calc_rmse = function(model, tst_data){
  sqrt(mean((predict(model, tst_data) - tst_data$total_UPDRS) ^ 2))
}

# Calculates the Mean Absolute Error (MAE) of a model evaluated on a dataset
calc_mae = function(model, tst_data){
  mean(abs(predict(model, tst_data) - tst_data$total_UPDRS))
}

# Performs the Breusch-Pagan test on a model
get_bp = function(model) {
  unname(bptest(model)$p.value)
}

# Performs the Shapiro-Wilk test on a model
get_sw = function(model) {
  unname(shapiro.test(resid(model))$p.value)
}

# Extract the number of coefficients of a model
get_num_params = function(model) {
  length(coef(model))
}

# Calculates the Leave One Out Cross Validation (LOOCV) RMSE of a model
get_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

# Calculates the Adjusted R^2 of a model
get_adj_r2 = function(model) {
  summary(model)$adj.r.squared
}

# Performs all the above evaluations on a model and dataset, and returns a list of results
eval_model = function(model, tst_data){
  list(b_pagan = get_bp(model),
       shap_wilk = get_sw(model),
       rmse_loocv = get_loocv_rmse(model),
       rmse_trn = sqrt(mean(model$residuals^2)),
       rmse_tst = calc_rmse(model, tst_data),
       adj_r2 = get_adj_r2(model),
       num_p = get_num_params(model),
       mae_tst = calc_mae(model, tst_data))
}

# Creates Fitted vs Residuals and QQ plots
diagnostic_plots = function(model, pcol="grey", lcol="dodgerblue"){
  par(mfrow = c(1,2))
  plot(fitted(model), resid(model), col = pcol, 
       xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals")
  abline(h=0, col = lcol)
  qqnorm(resid(model), col = pcol, main = "Normal Q-Q plot")
  qqline(resid(model), col = lcol)
}

# Creates a filter of all non-influential points of a model
non_influential_filter = function(model){
  cooks.distance(model) <= (4 / length(cooks.distance(model)))
}

# Creates a plot of Predicted vs Actuals
plot_pva = function(predicted, actual){
  plot(actual, predicted, 
     col = "darkgrey",
     xlab = "Actual",
     ylab = "Predicted",
     main = "Predicted vs Actual")
  grid()
  abline(0, 1, col = "dodgerblue")
}
