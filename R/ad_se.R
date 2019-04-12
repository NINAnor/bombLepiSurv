#' add_se
#' 
#' From 2017 analysis. Preparation for xtable. 
#'
#' I nicked this somewhere, don't remember where
#'
#'   
#'   
#' @param model: a lm or glm fitted model
#' @param name_f: a character string with the name of the categorical (factor) variable
#' @param name_x: a character string with the name fo the interacting variable, by default the intercept

add_se <- function(model, name_f, name_x = "Intercept"){
  #grab the standard error of the coefficients
  se_vec <- summary(model)$coefficients[,2]
  if(name_x=="Intercept"){
    #the stabdard error of the intercept
    se_x <- se_vec[1]
    #get the level-specific standard errors
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[-grep(":",names(se_f))]
    #get the covariance between the intercept and the level-specific parameters
    vcov_f <- vcov(model)[grep(name_f,rownames(vcov(model))),grep(name_x,colnames(vcov(model)))]
    vcov_f <- vcov_f[-grep(":",names(vcov_f))]
    #the estimated average value at each level
    coef_f <- fixef(model)[1] + fixef(model)[names(vcov_f)]
  }
  else{
    #similar code for the case of another variable than the intercept
    se_x <- se_vec[name_x]
    se_f <- se_vec[grep(name_f,names(se_vec))]
    se_f <- se_f[grep(":",names(se_f))]
    vcov_f <- vcov(model)[grep(name_f,rownames(vcov(model))),grep(name_x,colnames(vcov(model)))][,1]
    vcov_f <- vcov_f[grep(":",names(vcov_f))]
    coef_f <- c(fixef(model)[name_x], fixef(model)[name_x]+fixef(model)[names(vcov_f)])
  }
  #compute the summed SE
  se_f <- c(sqrt(se_x), sqrt(se_x**2+se_f**2+2*vcov_f))
  #create the output dataframe
  min2.5 <- coef_f - 1.96 * se_f
  max97.5 <- coef_f + 1.96 * se_f
  
  out <- data.frame(Coef=coef_f, SE=se_f, min2.5 = min2.5, max97.5 = max97.5)
  return(out)
}
