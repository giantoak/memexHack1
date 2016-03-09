# Function to see if var is at a particular level (variable list is ok)
isTableAtThisLevel <- function(dataset, varinquotes) {
  dataset <- as.data.frame(dataset)
  isTableAtThisLevel_yn <- nrow(as.data.frame(unique(dataset[, varinquotes]))) == nrow(dataset)
  return(isTableAtThisLevel_yn)
}





#Clustered standard errors
# sourced from here: https://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
# which quotes this guy http://www.ne.su.se/polopoly_fs/1.216115.1426234213!/menu/standard/file/clustering1.pdf








#Top code
topCodeThis <- function(data, topCodeValue, varToTopCode , resultingVar = 'NULL') {
  if (is.null(resultingVar)) {
    resultingVar <- paste0(varToTopCode, "_tc", topCodeValue)
  }
  data[[resultingVar]] <- ifelse(data[[varToTopCode]] > topCodeValue,
                                 topCodeValue, 
                                 data[[varToTopCode]])
  return(data)
  
}



#Write function to verify .x and .y vars are the same


are_merged_repeat_vars_same <- function(data) {
  merged_vars <- gsub('\\.x', '', colnames(data)[grepl('\\.x', colnames(data))])
  for (var in merged_vars) {
    x_var <- paste0(var, '.x')
    y_var <- paste0(var, '.y')
    if (all(data[[x_var]] == data[[y_var]]))  {
      data[[var]] <- data[[x_var]]
      data[[x_var]] <- NULL
      data[[y_var]] <- NULL
    } else {
      print(paste(x_var, "and", y_var, "aren't the same"))
      print(table(data[[x_var]] == data[[y_var]]))
    }
  }
  return(data)
}

#Function to remove missing variables from dataset based on formula variables

no_missings_subset <- function(df, specify_vars_with_missings, usual_filters = TRUE) {
    
    if (usual_filters) {
        
  df <- df[df$spam == FALSE & df$sex_ad == 1 & df$is_massage_parlor_ad == 0 & df$price_per_hour < 1000 & df$year >2010,]
    }
  
  df <- as.data.frame(df)[complete.cases(as.data.frame(df)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly
  
  return(df)
}





cl  <- function(data,fm, cluster) {
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  cluster <- data[[cluster]]
  
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  clustered.se <- sqrt(diag(vcovCL))
  return(clustered.se)
}

cl_coeftest  <- function(data,fm, cluster) {
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  cluster <- data[[cluster]]
  
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  clustered.se <- sqrt(diag(vcovCL))
  return(coeftest(fm, vcovCL))
}


tgg_reg <- function(df,formula,clustering_variable) {
  
  reg <- lm(data = df, formula)
  
  clustered.se <- cl(df,reg,clustering_variable)
  
  
 
   return(list(reg = reg, clustered.se = clustered.se))
  }











#Function: Residual checks
residual_checks <- function(lm_object) {
  
  residual_plot <- data.frame(cbind(predict(lm_object), lm_object$residuals))
  names(residual_plot) <- c('fitted_values', 'residuals')
  
  print(ggplot(residual_plot, aes(x = fitted_values, y = residuals)) + geom_point())
  
  qqnorm(lm_object$residuals)
  qplot(lm_object$residuals)
  
}

# 
# 
# cl   <- function(fm, cluster) {
#   require(sandwich, quietly = TRUE)
#   require(lmtest, quietly = TRUE)
#   
#   M <- length(unique(cluster))
#   N <- length(cluster)
#   K <- fm$rank
#   dfc <- (M/(M-1))*((N-1)/(N-K))
#   uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
#   vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
#   coeftest(fm, vcovCL) 
# }
# 
# #Regression Function
# 
# tgg_reg <- function(df,formula,clustering_variable)
# {
#   
#   reg <- lm(data = df, formula)
#   
#   
#   
#   #Cluster standard errors by MSA
#   reg <- cl(reg, df[[clustering_variable]])
#   
#   return(reg)
# }
# 
