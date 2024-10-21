


create_coeff_table <- function(unpenalized_model, penalized_model){
  
  #glm and penalized model output is in different formats, this turns them into a data table with correct names.
  lasso_coeff <- as.data.table(as.table(coef(penalized_model, s=c("lambda.min"), complete = TRUE)))[,V2 := NULL]
  colnames(lasso_coeff) <- c("Variable", "Lasso_Coefficient")
  glm_coeff <- as.data.table(stack(unpenalized_model$coefficients))
  colnames(glm_coeff) <- c("GLM_Coefficient", "Variable")
  
  # Rename variables
  glm_coeff[Variable == "C(MultiPolicy)multi_yes", Variable := "multi_yes"]
  glm_coeff[Variable == "C(xTreme_TurnSignal)xTreme_yes", Variable := "xTreme_yes"]
  glm_coeff[Variable == "C(car_weight)weight_heavy", Variable := "weight_heavy"]
  glm_coeff[Variable == "C(car_weight)weight_light", Variable := "weight_light"]
  glm_coeff[Variable == "C(car_weight)weight_extra_light", Variable := "weight_extra_light"]
  glm_coeff[Variable == "C(industry_code)ind_construction", Variable := "ind_construction"]
  glm_coeff[Variable == "C(industry_code)ind_farming", Variable := "ind_farming"]
  glm_coeff[Variable == "C(industry_code)ind_finance_and_insurance", Variable := "ind_finance_and_insurance"]
  glm_coeff[Variable == "C(industry_code)ind_fine_arts", Variable := "ind_fine_arts"]
  glm_coeff[Variable == "C(industry_code)ind_fireworks", Variable := "ind_fireworks"]
  glm_coeff[Variable == "C(industry_code)ind_food_services", Variable := "ind_food_services"]
  glm_coeff[Variable == "C(industry_code)ind_health_care", Variable := "ind_health_care"]
  glm_coeff[Variable == "C(industry_code)ind_real_estate", Variable := "ind_real_estate"]
  glm_coeff[Variable == "C(industry_code)ind_retail", Variable := "ind_retail"]
  # glm_coeff[Variable == "C(subset)large_state", Variable := "ind_large_state"]
  # glm_coeff[Variable == "C(subset)medium_state", Variable := "ind_medium_state"]
  # glm_coeff[Variable == "C(subset)small_state", Variable := "ind_small_state"]
  # glm_coeff[Variable == "C(subset)small_cw_dist_state", Variable := "ind_small_state_cw_dist"]
  
  #merging is safer than a column-bind method. 
  coef_table <- merge(glm_coeff, lasso_coeff, by = "Variable")
  coef_table[,GLM_Coefficient := round(GLM_Coefficient, 5)]
  coef_table[,Lasso_Coefficient := round(Lasso_Coefficient, 5)]

  return(coef_table)
}

get_cat_var_table <- function(data, coef_table, cat_var) {
  
  # Create a summary of predictions and pure premiums from the dataset
  true_relativity_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", cat_var, "_relativity"))),
                                    experienced_pure_premium = sum(incurred_loss)/length(incurred_loss), 
                                    glm_prediction = sum(glm_prediction)/length(glm_prediction),
                                    lasso_prediction = sum(lasso_prediction)/length(lasso_prediction),
                                    true_pure_premium = sum(true_risk)/length(true_risk)), by = cat_var]

  # Get the modeled relativities from the Coef table
  modeled_relativity_table <- coef_table[Variable %in% unique(data[,get(cat_var)]),]
  # Merge the tables together
  relativity_table <- merge(true_relativity_table, modeled_relativity_table, by.x = cat_var, by.y = "Variable", all = TRUE)
  # Account for any missing values
  relativity_table[is.na(relativity_table)] <- 0
  # Make sure that exp is numeric
  relativity_table[,exp := as.numeric(exposure)]
  # re-name and exponentiate the coefficients so that they can be easily plotted
  relativity_table[,GLM_factor := as.numeric(exp(GLM_Coefficient))]
  relativity_table[,Lasso_factor := as.numeric(exp(Lasso_Coefficient))]
  # get rid of the coefficients before returning the relativities
  relativity_table[,c("GLM_Coefficient", "Lasso_Coefficient") := NULL]
  
  return(relativity_table)
  
}

relativity_plot <- function(data_table){
  
  # This function plots relativities for our full dataset
  # it does not include the ability to plot our complement of credibility.
  fig <- plot_ly()
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~exposure,
                           type = 'bar',name = 'exposure', marker = list(color = 'rgba(0,175,254,0.4)', line = list(color = 'rgb(8,48,107)', width = 1.5)))
  # add traces, these are the individual factors and true relativities. 
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~GLM_factor, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'GLM Relativity')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~Lasso_factor, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Lasso Relativity')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~true_relativity, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'True Relativity')
  ay <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "left",
    title = "<b>Relativity</b>")
  # Basic formatting
  fig <- fig %>% layout(
    title = paste("Relativity Plot", colnames(data_table)[1]), yaxis2 = ay,
    xaxis = list(title=colnames(data_table)[1]),
    yaxis = list(title="<b>Exposure Count</b>", side = "right")) %>%
    layout(xaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff')
    )
  
  return(fig)
}


prediction_plot <- function(data_table){
  
  # Plot the raw predictions. These are not re-balanced
  fig <- plot_ly()
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~exposure,
                           type = 'bar',name = 'exposure', marker = list(color = 'rgba(0,175,254,0.4)', line = list(color = 'rgb(8,48,107)', width = 1.5)))
  # add traces
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~experienced_pure_premium,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Experienced Pure Premium')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~glm_prediction,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'GLM Prediction')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~lasso_prediction,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Lasso Prediction')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~true_pure_premium,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'True Pure Premium')
  ay <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "left",
    title = "<b>Pure Premium</b>")
  
  fig <- fig %>% layout(
    title = paste("Prediction Plot", colnames(data_table)[1]), yaxis2 = ay,
    xaxis = list(title=colnames(data_table)[1]),
    yaxis = list(title="<b>Exposure Count</b>", side = "right")) %>%
    layout(xaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff')
    )
  
  return(fig)
}




get_driver_var_table <- function(data, coef_table, variable_name = "driver_age", 
                                 variable_transformations = c("driver_age_18_38_hinge", "driver_age_38_76_hinge", "driver_age_76_99_hinge"), 
                                 base_level = (38)){
  # As this is a continuous variable, it is not as straightforward as categorical variable
  # First, we will provide a base level for rebalancing if there is none. 
  if(is.null(base_level)) {
    base_level <- mean(data$variable_name)
  }
  
  # Create a summary table by driver age
  relativity_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", variable_name, "_relativity"))),
                               experienced_pure_premium = sum(incurred_loss)/length(incurred_loss), 
                               glm_prediction = sum(glm_prediction)/length(glm_prediction),
                               lasso_prediction = sum(lasso_prediction)/length(lasso_prediction),
                               true_pure_premium = sum(true_risk)/length(true_risk)), by = variable_name]

  # rebalance true relativity to base level
  relativity_table[,true_relativity := true_relativity/relativity_table[,true_relativity][match(base_level, relativity_table[, get(variable_name)] )]]
  
  # Get the coefficients for each variable transformation
  modeled_relativity_table <- coef_table[Variable %in% variable_transformations,]
  glm_age_18_38_coeff <- modeled_relativity_table[,GLM_Coefficient][match("driver_age_18_38_hinge", modeled_relativity_table$Variable)]
  glm_age_38_76_coeff <- modeled_relativity_table[,GLM_Coefficient][match("driver_age_38_76_hinge", modeled_relativity_table$Variable)]
  glm_age_76_99_coeff <- modeled_relativity_table[,GLM_Coefficient][match("driver_age_76_99_hinge", modeled_relativity_table$Variable)]
  # Combine all coefficients with their respective variables to get the aggregate prediction for all age variables
  relativity_table[,GLM_factor := exp(glm_age_18_38_coeff*pmin(driver_age, (38)) + 
                                        glm_age_38_76_coeff*pmin(pmax(driver_age, (38)), (76)) + 
                                        glm_age_76_99_coeff* pmax(driver_age, (76)))/
                     exp(glm_age_18_38_coeff*min((38), base_level) + 
                           glm_age_38_76_coeff*max(min(base_level, (76)), (38))  + 
                           glm_age_76_99_coeff * max(base_level, (76)))]
  
  # Combine all coefficients with their respective variables to get the aggregate prediction for all age variables
  lasso_age_18_38_coeff <- modeled_relativity_table[,Lasso_Coefficient][match("driver_age_18_38_hinge", modeled_relativity_table$Variable)]
  lasso_age_38_76_coeff <- modeled_relativity_table[,Lasso_Coefficient][match("driver_age_38_76_hinge", modeled_relativity_table$Variable)]
  lasso_age_76_99_coeff <- modeled_relativity_table[,Lasso_Coefficient][match("driver_age_76_99_hinge", modeled_relativity_table$Variable)]
  # Combine all coefficients with their respective variables to get the aggregate prediction for all age variables
  relativity_table[,Lasso_factor := exp(lasso_age_18_38_coeff*pmin(driver_age, (38)) + 
                                          lasso_age_38_76_coeff*pmin(pmax(driver_age, (38)), (76)) + 
                                          lasso_age_76_99_coeff* pmax(driver_age, (76)))/
                     exp(lasso_age_18_38_coeff*min((38), base_level) + 
                           lasso_age_38_76_coeff*max(min(base_level, (76)), (38)) + 
                           lasso_age_76_99_coeff * max(base_level, (76)))]
  
  return(relativity_table[order(driver_age)])
  
}



get_vehicle_var_table <- function(data, coef_table, variable_name = "vehicle_age", 
                                  variable_transformations = c("vehicle_age_0_10_hinge", "vehicle_age_10_99_hinge"), 
                                  base_level = (10)){
  # As this is a continuous variable, it is not as straightforward as categorical variable
  # First, we will provide a base level for rebalancing if there is none. 
  if(is.null(base_level)) {
    base_level <- mean(data$variable_name)
  }
  # aggregate the data to get average pure premiums and predictions per category
  relativity_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", variable_name, "_relativity"))),
                               experienced_pure_premium = sum(incurred_loss)/length(incurred_loss), 
                               glm_prediction = sum(glm_prediction)/length(glm_prediction),
                               lasso_prediction = sum(lasso_prediction)/length(lasso_prediction),
                               true_pure_premium = sum(true_risk)/length(true_risk)), by = variable_name]

  # rebalance true relativity to base level
  relativity_table[,true_relativity := true_relativity/relativity_table[,true_relativity][match(base_level, relativity_table[, get(variable_name)] )]]
  # select our relevant coefficients
  modeled_relativity_table <- coef_table[Variable %in% variable_transformations,]
  glm_vehicle_0_10_coeff <- modeled_relativity_table[,GLM_Coefficient][match("vehicle_age_0_10_hinge", modeled_relativity_table$Variable)]
  glm_vehicle_10_99_coeff <- modeled_relativity_table[,GLM_Coefficient][match("vehicle_age_10_99_hinge", modeled_relativity_table$Variable)]
  # Calculate the relativity when combining all vehicle age variables. 
  relativity_table[,GLM_factor := exp(glm_vehicle_0_10_coeff*pmin(vehicle_age, (10)) + 
                                        glm_vehicle_10_99_coeff*pmax(vehicle_age, (10)))/
                     exp(glm_vehicle_0_10_coeff*min(base_level, (10)) + glm_vehicle_10_99_coeff*max(base_level, (10)))]
  
  lasso_vehicle_0_10_coeff <- modeled_relativity_table[,Lasso_Coefficient][match("vehicle_age_0_10_hinge", modeled_relativity_table$Variable)]
  lasso_vehicle_10_99_coeff <- modeled_relativity_table[,Lasso_Coefficient][match("vehicle_age_10_99_hinge", modeled_relativity_table$Variable)]
  # Calculate the relativity when combining all vehicle age variables. 
  relativity_table[,Lasso_factor := exp(lasso_vehicle_0_10_coeff*pmin(vehicle_age, (10)) + 
                                          lasso_vehicle_10_99_coeff*pmax(vehicle_age, (10)))/
                     exp(lasso_vehicle_0_10_coeff*min(base_level, (10)) + lasso_vehicle_10_99_coeff*max(base_level, (10)))]
  
  return(relativity_table[order(vehicle_age)])
  
}


create_credibility_coeff_table <- function(glm_model, penalized_model, complement_model){
  # This function creates a coefficient table containing all coefficients from both models as well as the complement of credibility
  
  #glm and penalized model output is in different formats, this turns them into a data table with correct names.
  lasso_coeff <- as.data.table(as.table(coef(penalized_model, s=c("lambda.min"), complete = TRUE)))[,V2 := NULL]
  colnames(lasso_coeff) <- c("Variable", "Lasso_Coefficient")
  complement_coeff <- as.data.table(as.table(coef(complement_model, s=c("lambda.min"), complete = TRUE)))[,V2 := NULL]
  colnames(complement_coeff) <- c("Variable", "Complement_Coefficient")
  glm_coeff <- as.data.table(stack(glm_model$coefficients))
  colnames(glm_coeff) <- c("GLM_Coefficient", "Variable")
  
  # Rename variables
  glm_coeff[Variable == "C(MultiPolicy)multi_yes", Variable := "multi_yes"]
  
  glm_coeff[Variable == "C(xTreme_TurnSignal)xTreme_yes", Variable := "xTreme_yes"]
  
  glm_coeff[Variable == "C(car_weight)weight_heavy", Variable := "weight_heavy"]
  glm_coeff[Variable == "C(car_weight)weight_light", Variable := "weight_light"]
  glm_coeff[Variable == "C(car_weight)weight_extra_light", Variable := "weight_extra_light"]
  
  glm_coeff[Variable == "C(industry_code)ind_construction", Variable := "ind_construction"]
  glm_coeff[Variable == "C(industry_code)ind_farming", Variable := "ind_farming"]
  glm_coeff[Variable == "C(industry_code)ind_finance_and_insurance", Variable := "ind_finance_and_insurance"]
  glm_coeff[Variable == "C(industry_code)ind_fine_arts", Variable := "ind_fine_arts"]
  glm_coeff[Variable == "C(industry_code)ind_fireworks", Variable := "ind_fireworks"]
  glm_coeff[Variable == "C(industry_code)ind_food_services", Variable := "ind_food_services"]
  glm_coeff[Variable == "C(industry_code)ind_health_care", Variable := "ind_health_care"]
  glm_coeff[Variable == "C(industry_code)ind_real_estate", Variable := "ind_real_estate"]
  glm_coeff[Variable == "C(industry_code)ind_retail", Variable := "ind_retail"]
  
  #merging is safer than a column-bind method. 
  coef_table <- merge(glm_coeff, lasso_coeff, by = "Variable")
  coef_table <- merge(coef_table, complement_coeff, by = "Variable")
  # Remove the intercept of our complement of credibility
  coef_table["(Intercept)", "Complement_Coefficient" := 0]
  coef_table[,Credibility_Weighted_Coefficient := Lasso_Coefficient + Complement_Coefficient]
 
  coef_table[,GLM_Coefficient := round(GLM_Coefficient, 5)]
  coef_table[,Credibility_Weighted_Coefficient := round(Credibility_Weighted_Coefficient, 5)]
  coef_table[,Complement_Coefficient := round(Complement_Coefficient, 5)]
  coef_table[,Lasso_Coefficient := round(Lasso_Coefficient, 5)]
  
  return(coef_table)
}





get_credibility_cat_var_table <- function(data, coef_table, cat_var) {
  # This function creates a table of the GLM, Complement, and Lasso coefficients for categorical variables. 
  
  # First, summarize the data to our variable's level. 
  true_relativity_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", cat_var, "_relativity"))),
                                    experienced_pure_premium = sum(incurred_loss)/length(incurred_loss), 
                                    glm_prediction = sum(glm_prediction)/length(glm_prediction),
                                    complement_prediction = sum(complement_prediction)/length(complement_prediction),
                                    credibility_weighted_prediction = sum(credibility_weighted_prediction)/length(credibility_weighted_prediction),
                                    true_pure_premium = sum(true_risk)/length(true_risk)), by = cat_var]
  # Select the relevant coefficients
  modeled_relativity_table <- coef_table[Variable %in% unique(data[,get(cat_var)]),]
  # Combine our modeled and true relativities
  relativity_table <- merge(true_relativity_table, modeled_relativity_table, by.x = cat_var, by.y = "Variable", all = TRUE)
  # Account for NA values where a value may not be modeled
  relativity_table[is.na(relativity_table)] <- 0
  # Calculcate factors for our one-hot encoded variables. 
  relativity_table[,exp := as.numeric(exposure)]
  relativity_table[,GLM_factor := as.numeric(exp(GLM_Coefficient))]
  relativity_table[,Lasso_factor := as.numeric(exp(Lasso_Coefficient))]
  relativity_table[,Credibility_Weighted_factor := as.numeric(exp(Credibility_Weighted_Coefficient))]
  relativity_table[,Complement_factor := as.numeric(exp(Complement_Coefficient))]
  # Remove coefficients so we only export factors. 
  relativity_table[,c("GLM_Coefficient", "Lasso_Coefficient", "Complement_Coefficient", "Credibility_Weighted_Coefficient") := NULL]
  
  return(relativity_table)
  
}



credibility_relativity_plot <- function(data_table){

  # This function plots the relativities of our models including the complement of credibility
  
  # Create the plot
  fig <- plot_ly()
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~exposure,
                           type = 'bar',name = 'exposure', marker = list(color = 'rgba(0,175,254,0.4)', line = list(color = 'rgb(8,48,107)', width = 1.5)))
  # add traces
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~GLM_factor, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'GLM Relativity')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~Credibility_Weighted_factor, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Credibility Weighted Relativity')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~Complement_factor, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Complement Relativity')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~true_relativity, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'True Relativity')

  ay <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "left",
    title = "<b>Relativity</b>")
  
  # Basic Formatting
  fig <- fig %>% layout(
    title = paste("Relativity Plot", colnames(data_table)[1]), yaxis2 = ay,
    xaxis = list(title=colnames(data_table)[1]),
    yaxis = list(title="<b>Exposure Count</b>", side = "right")) %>%
    layout(xaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff')
    )
  
  return(fig)
}



credibility_prediction_plot <- function(data_table){
  
  # Create the prediction plots including our re-based complement of credibility
  
  # Create the re-balanced complement of credibility prediction (rebalanced for a different intercept)
  data_table[,complement_prediction_rebalanced := complement_prediction*(mean(credibility_weighted_prediction)/mean(complement_prediction))]
  
  fig <- plot_ly()
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~exposure,
                           type = 'bar',name = 'exposure', marker = list(color = 'rgba(0,175,254,0.4)', line = list(color = 'rgb(8,48,107)', width = 1.5)))
  # add traces
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~experienced_pure_premium,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Experienced Pure Premium')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~glm_prediction,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'GLM Prediction')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~credibility_weighted_prediction,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Credibility Weighted Prediction')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~complement_prediction_rebalanced,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'Complement Prediction Rebalanced')
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~true_pure_premium,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'True Pure Premium')
  

  ay <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "left",
    title = "<b>Pure Premium</b>")
  
  fig <- fig %>% layout(
    title = paste("Prediction Plot", colnames(data_table)[1]), yaxis2 = ay,
    xaxis = list(title=colnames(data_table)[1]),
    yaxis = list(title="<b>Exposure Count</b>", side = "right")) %>%
    layout(xaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff')
    )
  
  return(fig)
}



get_credibility_vehicle_var_table <- function(data, coef_table, variable_name = "vehicle_age", 
                                  variable_transformations = c("vehicle_age_0_10_hinge", "vehicle_age_10_99_hinge"), 
                                  base_level = (10)){
  
  # This creates a vehicle_variable table with relativities for all of our variables. 
  # This is more complicated than categorical variables since it is combining multiple variables onto a single plot
  
  if(is.null(base_level)) {
    base_level <- mean(data$variable_name)
  }
  # Create a summary of the data's predictions and pure premiums
  relativity_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", variable_name, "_relativity"))),
                               experienced_pure_premium = sum(incurred_loss)/length(incurred_loss), 
                               glm_prediction = sum(glm_prediction)/length(glm_prediction),
                               complement_prediction = sum(complement_prediction)/length(complement_prediction),
                               credibility_weighted_prediction = sum(credibility_weighted_prediction)/length(credibility_weighted_prediction),
                               true_pure_premium = sum(true_risk)/length(true_risk)), by = variable_name]
  
  # rebalance true relativity to base level
  relativity_table[,true_relativity := true_relativity/relativity_table[,true_relativity][match(base_level, relativity_table[, get(variable_name)] )]]
  modeled_relativity_table <- coef_table[Variable %in% variable_transformations,]
  glm_vehicle_0_10_coeff <- modeled_relativity_table[,GLM_Coefficient][match("vehicle_age_0_10_hinge", modeled_relativity_table$Variable)]
  glm_vehicle_10_99_coeff <- modeled_relativity_table[,GLM_Coefficient][match("vehicle_age_10_99_hinge", modeled_relativity_table$Variable)]
  # Combine the predictions for both vehicle age variables and rebalance to our base level. 
  relativity_table[,GLM_factor := exp(glm_vehicle_0_10_coeff*pmin(vehicle_age, (10)) + 
                                        glm_vehicle_10_99_coeff*pmax(vehicle_age, (10)))/
                     exp(glm_vehicle_0_10_coeff*min(base_level, (10)) + glm_vehicle_10_99_coeff*max(base_level, (10)))]
  # add on the complement of credibility
  complement_vehicle_0_10_coeff <- modeled_relativity_table[,Complement_Coefficient][match("vehicle_age_0_10_hinge", modeled_relativity_table$Variable)]
  complement_vehicle_10_99_coeff <- modeled_relativity_table[,Complement_Coefficient][match("vehicle_age_10_99_hinge", modeled_relativity_table$Variable)]
  # Combine the predictions for both vehicle age variables and rebalance to our base level. 
  relativity_table[,Complement_factor := exp(complement_vehicle_0_10_coeff*pmin(vehicle_age, (10)) + 
                                               complement_vehicle_10_99_coeff*pmax(vehicle_age, (10)))/
                     exp(complement_vehicle_0_10_coeff*min(base_level, (10)) + complement_vehicle_10_99_coeff*max(base_level, (10)))]
  
  # Credibility weighted predictions. 
  lasso_vehicle_0_10_coeff <- modeled_relativity_table[,Credibility_Weighted_Coefficient][match("vehicle_age_0_10_hinge", modeled_relativity_table$Variable)]
  lasso_vehicle_10_99_coeff <- modeled_relativity_table[,Credibility_Weighted_Coefficient][match("vehicle_age_10_99_hinge", modeled_relativity_table$Variable)]
  # Combine the predictions for both vehicle age variables and rebalance to our base level. 
  relativity_table[,Credibility_Weighted_factor := exp(lasso_vehicle_0_10_coeff*pmin(vehicle_age, (10)) + 
                                          lasso_vehicle_10_99_coeff*pmax(vehicle_age, (10)))/
                     exp(lasso_vehicle_0_10_coeff*min(base_level, (10)) + lasso_vehicle_10_99_coeff*max(base_level, (10)))]
  
  return(relativity_table[order(vehicle_age)])
  
}




get_credibility_driver_var_table <- function(data, coef_table, variable_name = "driver_age", 
                                 variable_transformations = c("driver_age_18_38_hinge", "driver_age_38_76_hinge", "driver_age_76_99_hinge"), 
                                 base_level = (40)){
  # Create a driver age factor table including our complement of credibility
  if(is.null(base_level)) {
    base_level <- mean(data$variable_name)
  }
  
  relativity_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", variable_name, "_relativity"))),
                               experienced_pure_premium = sum(incurred_loss)/length(incurred_loss), 
                               glm_prediction = sum(glm_prediction)/length(glm_prediction),
                               complement_prediction = sum(complement_prediction)/length(complement_prediction),
                               credibility_weighted_prediction = sum(credibility_weighted_prediction)/length(credibility_weighted_prediction),
                               true_pure_premium = sum(true_risk)/length(true_risk)), by = variable_name]
  
  # rebalance true relativity to base level
  relativity_table[,true_relativity := true_relativity/relativity_table[,true_relativity][match(base_level, relativity_table[, get(variable_name)] )]]

  modeled_relativity_table <- coef_table[Variable %in% variable_transformations,]
  # Obtain the GLM coefficients
  glm_age_18_38_coeff <- modeled_relativity_table[,GLM_Coefficient][match("driver_age_18_38_hinge", modeled_relativity_table$Variable)]
  glm_age_38_76_coeff <- modeled_relativity_table[,GLM_Coefficient][match("driver_age_38_76_hinge", modeled_relativity_table$Variable)]
  glm_age_76_99_coeff <- modeled_relativity_table[,GLM_Coefficient][match("driver_age_76_99_hinge", modeled_relativity_table$Variable)]
  # Combine the predictions for all driver age variables and rebalance to our base level. 
  relativity_table[,GLM_factor := exp(glm_age_18_38_coeff*pmin(driver_age, (38)) + 
                                        glm_age_38_76_coeff*pmin(pmax(driver_age, (38)), (76)) + 
                                        glm_age_76_99_coeff* pmax(driver_age, (76)))/
                     exp(glm_age_18_38_coeff*min((38), base_level) + 
                           glm_age_38_76_coeff*max(min(base_level, (76)), (38))  + 
                           glm_age_76_99_coeff * max(base_level, (76)))]
  # Complement Coefficient
  complement_age_18_38_coeff <- modeled_relativity_table[,Complement_Coefficient][match("driver_age_18_38_hinge", modeled_relativity_table$Variable)]
  complement_age_38_76_coeff <- modeled_relativity_table[,Complement_Coefficient][match("driver_age_38_76_hinge", modeled_relativity_table$Variable)]
  complement_age_76_99_coeff <- modeled_relativity_table[,Complement_Coefficient][match("driver_age_76_99_hinge", modeled_relativity_table$Variable)]
  # Combine the predictions for all driver age variables and rebalance to our base level. 
  relativity_table[,Complement_factor := exp(complement_age_18_38_coeff*pmin(driver_age, (38)) + 
                                                complement_age_38_76_coeff*pmin(pmax(driver_age, (38)), (76)) + 
                                                complement_age_76_99_coeff* pmax(driver_age, (76)))/
                     exp(complement_age_18_38_coeff*min((38), base_level) + 
                           complement_age_38_76_coeff*max(min(base_level, (76)), (38)) + 
                           complement_age_76_99_coeff * max(base_level, (76)))]
  # Get the Lasso Coefficients
  lasso_age_18_38_coeff <- modeled_relativity_table[,Credibility_Weighted_Coefficient][match("driver_age_18_38_hinge", modeled_relativity_table$Variable)]
  lasso_age_38_76_coeff <- modeled_relativity_table[,Credibility_Weighted_Coefficient][match("driver_age_38_76_hinge", modeled_relativity_table$Variable)]
  lasso_age_76_99_coeff <- modeled_relativity_table[,Credibility_Weighted_Coefficient][match("driver_age_76_99_hinge", modeled_relativity_table$Variable)]
  # Combine the predictions for all driver age variables and rebalance to our base level. 
  relativity_table[,Credibility_Weighted_factor := exp(lasso_age_18_38_coeff*pmin(driver_age, (38)) + 
                                          lasso_age_38_76_coeff*pmin(pmax(driver_age, (38)), (76)) + 
                                          lasso_age_76_99_coeff* pmax(driver_age, (76)))/
                     exp(lasso_age_18_38_coeff*min((38), base_level) + 
                           lasso_age_38_76_coeff*max(min(base_level, (76)), (38)) + 
                           lasso_age_76_99_coeff * max(base_level, (76)))]
  
  return(relativity_table[order(driver_age)])
  
}





double_lift_chart <- function(dataset = large_state_training_data,
                              lasso_credibility = "credibility_weighted_prediction",
                              glm = "glm_prediction",
                              actual = "true_risk",
                              exposure = "exposure",
                              normalize = FALSE){
  # # First, rebalance losses to the observed level to account for misses in the intercept
  dataset[,lasso_credibility_rebalanced := get(lasso_credibility)* sum(get(actual))/sum(get(lasso_credibility))]
  dataset[,glm_rebalanced := get(glm)* sum(get(actual))/sum(get(glm))]
  # lasso_rebalancing_factor <- sum(dataset$lasso_credibility)
  
  # Now, order the dataset
  dataset[,ordering := get(glm)/get(lasso_credibility)]
  dataset <- dataset[order(ordering),]
  # Create deciles
  dataset[,decile := 1+floor((as.numeric(rownames(dataset))-1)/nrow(dataset)*10)]
  # Summarize
  data_summary <- dataset[, .(exposure = .N, line1 = sum(lasso_credibility_rebalanced)/.N,
                              line2 = sum(glm_rebalanced)/.N, observed = sum(get(actual))/.N), by = decile]
  if(normalize == FALSE){
    data_summary <- dataset[, .(exposure = .N, line1 = sum(lasso_credibility_rebalanced)/.N,
                                line2 = sum(glm_rebalanced)/.N, observed = sum(get(actual))/.N), by = decile]
  }
  if(normalize == TRUE){
    data_summary <- dataset[, .(exposure = .N, line1 = sum(lasso_credibility_rebalanced)/sum(get(actual)),
                                line2 = sum(glm_rebalanced)/sum(get(actual)), observed = sum(get(actual))/sum(get(actual))), by = decile]
  }
  
  
  
  # if(normalize == TRUE){
  #   data_summary[,line1 := line1/observed]
  #   data_summary[,line2 := line2/observed]
  #   data_summary[,observed := observed/observed]
  # }
  
  
  fig <- plot_ly()
  fig <- fig %>% add_trace(data = data_summary , x = ~get(colnames(data_summary)[1]), y = ~exposure,
                           type = 'bar',name = 'exposure', marker = list(color = 'rgba(0,175,254,0.4)', line = list(color = 'rgb(8,48,107)', width = 1.5)))
  # add traces
  fig <- fig %>% add_trace(data = data_summary , x = ~get(colnames(data_summary)[1]), y = ~line1,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = lasso_credibility)
  fig <- fig %>% add_trace(data = data_summary , x = ~get(colnames(data_summary)[1]), y = ~line2,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = glm)
  fig <- fig %>% add_trace(data = data_summary , x = ~get(colnames(data_summary)[1]), y = ~observed,
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = actual)
  ay <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "left",
    title = "<b>Pure Premium</b>")
  
  fig <- fig %>% layout(
    title = "Two-way lift chart", 
    yaxis2 = ay,
    xaxis = list(title=colnames(data_summary)[1]),
    yaxis = list(title="<b>Exposure Count</b>", side = "right")) %>%
    layout(xaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff')
    )
  return(fig)
  
}




description_plot_table <- function(data, cat_var){
  true_relativity_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", cat_var, "_relativity"))),
                                    true_pure_premium = sum(true_risk)/length(true_risk)), by = cat_var]
  true_relativity_table <- true_relativity_table[order(get(cat_var))]
  return(true_relativity_table)
}
description_plot <- function(data, cat_var){
  
  data_table <- data[, .(exposure = .N, true_relativity = mean(get(paste0("true_", cat_var, "_relativity"))),
                         true_pure_premium = sum(true_risk)/length(true_risk)), by = cat_var]
  data_table <- data_table[order(get(cat_var))]
  
  # This function plots just the true relativity and exposures for a given description plot table. 
  fig <- plot_ly()
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~exposure,
                           type = 'bar',name = 'exposure', marker = list(color = 'rgba(0,175,254,0.4)', line = list(color = 'rgb(8,48,107)', width = 1.5)))
  # add traces, these are the individual factors and true relativities. 
  
  fig <- fig %>% add_trace(data = data_table , x = ~get(colnames(data_table)[1]), y = ~true_relativity, 
                           type = 'scatter', mode = 'lines+markers',  yaxis = "y2", name = 'True Relativity')
  ay <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "left",
    title = "<b>Relativity</b>")
  # Basic formatting
  fig <- fig %>% layout(
    title = paste("Relativity Plot", colnames(data_table)[1]), yaxis2 = ay,
    xaxis = list(title=colnames(data_table)[1]),
    yaxis = list(title="<b>Exposure Count</b>", side = "right")) %>%
    layout(xaxis = list(
      zerolinecolor = '#ffff',
      zerolinewidth = 2,
      gridcolor = 'ffff'),
      yaxis = list(
        zerolinecolor = '#ffff',
        zerolinewidth = 2,
        gridcolor = 'ffff')
    )
  
  return(fig)
}


create_credibility_coeff_table_alternate_lambda <- function(glm_model, penalized_model, complement_model, alternate_lambda){
  # This function creates a coefficient table containing all coefficients from both models as well as the complement of credibility
  
  #glm and penalized model output is in different formats, this turns them into a data table with correct names.
  lasso_coeff <- as.data.table(as.table(coef(penalized_model, s=c(alternate_lambda), complete = TRUE)))[,V2 := NULL]
  colnames(lasso_coeff) <- c("Variable", "Lasso_Coefficient")
  complement_coeff <- as.data.table(as.table(coef(complement_model, s=c("lambda.min"), complete = TRUE)))[,V2 := NULL]
  colnames(complement_coeff) <- c("Variable", "Complement_Coefficient")
  glm_coeff <- as.data.table(stack(glm_model$coefficients))
  colnames(glm_coeff) <- c("GLM_Coefficient", "Variable")
  
  # Rename variables
  glm_coeff[Variable == "C(MultiPolicy)multi_yes", Variable := "multi_yes"]
  
  glm_coeff[Variable == "C(xTreme_TurnSignal)xTreme_yes", Variable := "xTreme_yes"]
  
  glm_coeff[Variable == "C(car_weight)weight_heavy", Variable := "weight_heavy"]
  glm_coeff[Variable == "C(car_weight)weight_light", Variable := "weight_light"]
  glm_coeff[Variable == "C(car_weight)weight_extra_light", Variable := "weight_extra_light"]
  
  glm_coeff[Variable == "C(industry_code)ind_construction", Variable := "ind_construction"]
  glm_coeff[Variable == "C(industry_code)ind_farming", Variable := "ind_farming"]
  glm_coeff[Variable == "C(industry_code)ind_finance_and_insurance", Variable := "ind_finance_and_insurance"]
  glm_coeff[Variable == "C(industry_code)ind_fine_arts", Variable := "ind_fine_arts"]
  glm_coeff[Variable == "C(industry_code)ind_fireworks", Variable := "ind_fireworks"]
  glm_coeff[Variable == "C(industry_code)ind_food_services", Variable := "ind_food_services"]
  glm_coeff[Variable == "C(industry_code)ind_health_care", Variable := "ind_health_care"]
  glm_coeff[Variable == "C(industry_code)ind_real_estate", Variable := "ind_real_estate"]
  glm_coeff[Variable == "C(industry_code)ind_retail", Variable := "ind_retail"]
  
  #merging is safer than a column-bind method. 
  coef_table <- merge(glm_coeff, lasso_coeff, by = "Variable")
  coef_table <- merge(coef_table, complement_coeff, by = "Variable")
  # Remove the intercept of our complement of credibility
  coef_table["(Intercept)", "Complement_Coefficient" := 0]
  coef_table[,Credibility_Weighted_Coefficient := Lasso_Coefficient + Complement_Coefficient]
  
  coef_table[,GLM_Coefficient := round(GLM_Coefficient, 5)]
  coef_table[,Credibility_Weighted_Coefficient := round(Credibility_Weighted_Coefficient, 5)]
  coef_table[,Complement_Coefficient := round(Complement_Coefficient, 5)]
  coef_table[,Lasso_Coefficient := round(Lasso_Coefficient, 5)]
  
  return(coef_table)
}

plot_lasso_model_cv_error <- function(lasso_model) {
  # Preparing the data for plotting
  data <- data.frame(
    lambda = lasso_model$lambda,
    cvm = lasso_model$cvm,
    cvsd = lasso_model$cvsd
  )
  data$cvlower = data$cvm - data$cvsd
  data$cvupper = data$cvm + data$cvsd

  # Plotting
  ggplot(data, aes(x = lambda, y = cvm)) +
    geom_line() +
    geom_ribbon(aes(ymin = cvlower, ymax = cvupper), alpha = 0.2) +
    labs(x = "Lambda", y = "Mean Cross-Validated Error") +
    theme_minimal()
}

# Usage example:
# plot_lasso_model_cv_error(large_state_NOoffset_lasso_model)






