

# Test that this works in the software. GLM model 1 should be equivalent to GLM model 2
# In GLM model 1, the offset is applied through the offset term
# in GLM model 2, the offset is applied manually to the incurred loss and exposure
# Since HDtweedie does not support offsets, we will manually apply the offset in this way. 

glm_model_1 <- glm(formula = incurred_loss ~ 
                     # driver_age_18_38_hinge_ln + driver_age_38_76_hinge_ln + driver_age_76_99_hinge_ln + 
                     # vehicle_age_0_10_hinge_mod1_ln + vehicle_age_10_99_hinge_mod1_ln + 
                     driver_age_18_38_hinge + driver_age_38_76_hinge + driver_age_76_99_hinge + 
                     vehicle_age_0_10_hinge + vehicle_age_10_99_hinge + 
                     C(MultiPolicy) + C(xTreme_TurnSignal) + C(car_weight) +C(industry_code), 
                   family = tweedie(1.6, link.power = 0),
                   weights = exposure,
                   offset = log(cw_model_complement_of_credibility), # Here the offset is applied through the 
                   # offset logic and not the target/exposure terms
                   data = large_state_training_data)

glm_model_2 <- glm(formula = (incurred_loss*(cw_model_complement_of_credibility^(1-1.6)) /(cw_model_complement_of_credibility^(2-1.6))) ~ 
                     # driver_age_18_38_hinge_ln + driver_age_38_76_hinge_ln + driver_age_76_99_hinge_ln + 
                     # vehicle_age_0_10_hinge_mod1_ln + vehicle_age_10_99_hinge_mod1_ln + 
                     driver_age_18_38_hinge + driver_age_38_76_hinge + driver_age_76_99_hinge + 
                     vehicle_age_0_10_hinge + vehicle_age_10_99_hinge + 
                     C(MultiPolicy) + C(xTreme_TurnSignal) + C(car_weight) +C(industry_code), 
                   family = tweedie(1.6, link.power = 0),
                   weights = exposure*(cw_model_complement_of_credibility^(2-1.6)),
                   data = large_state_training_data)
# This is identical within a material tolerance. All coefficients are identical at the thousandths place,
# and therefore equivalent for insurance implementation purposes
glm_model_1$coefficients
glm_model_2$coefficients





# Large State Model, no Offset

# fit the lasso model with an offset
large_state_no_offset_lasso_model <- cv.HDtweedie(x = large_state_training_data_matrix, 
                                                  y = (large_state_training_loss_vector), 
                                                  group = NULL, p = 1.6, 
                                                  weights = large_state_training_data$exposure, lambda = NULL, 
                                                  pred.loss = "deviance", 
                                                  nfolds = 4, foldid = large_state_training_cv_vector,
                                                  standardize = TRUE)

# Fit the GLM model without an offset
large_state_glm_model <- glm(formula = incurred_loss ~ 
                               driver_age_18_38_hinge + driver_age_38_76_hinge + driver_age_76_99_hinge + 
                               vehicle_age_0_10_hinge + vehicle_age_10_99_hinge + 
                               C(MultiPolicy) + C(xTreme_TurnSignal) + C(car_weight) +C(industry_code), 
                             family = tweedie(1.6, link.power = 0),
                             weights = exposure,
                             # offset = cw_model_complement_of_credibility_vector, - Note, no complement here.
                             data = large_state_training_data)

# add the predictions without a complement to our training data
large_state_training_data[,glm_prediction := predict.glm(large_state_glm_model, large_state_training_data, type = "response")]
large_state_training_data[,lasso_prediction := predict(large_state_no_offset_lasso_model, large_state_training_data_matrix, s="lambda.min",
                                                       weights = exposure) ]

large_state_validate_data[,glm_prediction := predict.glm(large_state_glm_model, large_state_validate_data, type = "response")]
large_state_validate_data[,lasso_prediction := predict(large_state_no_offset_lasso_model, large_state_validate_data_matrix, s="lambda.min", 
                                                       weights = exposure) ]
summary(large_state_validate_data)

large_state_coeff_table <- create_coeff_table(unpenalized_model = large_state_glm_model, 
                                              penalized_model = large_state_no_offset_lasso_model)

relativity_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "industry_code"))
relativity_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "MultiPolicy"))
relativity_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "xTreme_TurnSignal"))
relativity_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "car_weight"))

prediction_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "industry_code"))
prediction_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "MultiPolicy"))
prediction_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "xTreme_TurnSignal"))
prediction_plot(get_cat_var_table(large_state_training_data, large_state_coeff_table, "car_weight"))





large_state_driver_age_table <- get_driver_var_table(large_state_training_data, large_state_coeff_table, variable_name = "driver_age")
large_state_vehicle_age_table <- get_vehicle_var_table(large_state_training_data, large_state_coeff_table, variable_name = "vehicle_age")


prediction_plot(large_state_driver_age_table)
relativity_plot(large_state_driver_age_table)

# cw_coeff_table
prediction_plot(large_state_vehicle_age_table)
relativity_plot(large_state_vehicle_age_table)



# Fit a GLM with an offset to see the difference in behavior. 

large_state_offset_glm_model <- glm(formula = incurred_loss ~
                                      driver_age_18_38_hinge + driver_age_38_76_hinge + driver_age_76_99_hinge +
                                      vehicle_age_0_10_hinge + vehicle_age_10_99_hinge +
                                      C(MultiPolicy) + C(xTreme_TurnSignal) + C(car_weight) +C(industry_code),
                                    family = tweedie(1.6, link.power = 0),
                                    weights = exposure,
                                    offset = log(cw_model_complement_of_credibility_vector),
                                    data = large_state_training_data)



# First, let's compare the GLM models with and without the complement of credibility
original_predictions <- predict.glm(large_state_glm_model, large_state_training_data, type = "response")
with_offset_predictions <- predict.glm(large_state_offset_glm_model, large_state_training_data, type = "response") # uses the offset
cbind(original_predictions, with_offset_predictions)
# They're exactly the same!
# This is because the GLM treats the data with full credibility. 
# as long as our complement can be cancelled out by our selected predictors, the model predictions will not change. 
# We will no longer both model and offset variables in a GLM. 

