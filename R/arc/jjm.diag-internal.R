
# Internal functions of diagnostic ----------------------------------------
.createDataFrame = function(data, years, class){
  dims  = dim(data)
  res   = data.frame(year = rep(years, dims[2]), data = c(data), class = rep(class, each = dims[1]))
  return(res)}

.bubbles         = function(x, data, bub.scale = 2.5, col = c("black", "black"),...){
  dots = list(...)
  dots$data = data
  dots$cex = bub.scale*(abs(data$resids)/max(abs(data$resids), na.rm = TRUE)) + bub.scale*0.1
  dots$col = ifelse(data$resids > 0, col[1], col[2])
  dots$pch = ifelse(data$resids>0, 19, 1)
  dots$panel = function(x, y, ..., cex, subscripts){
    panel.grid(h = -1, v = -1)
    panel.xyplot(x, y, cex = cex[subscripts], ...)
  }
  call.list = c(x = x, dots)
  ans = do.call("xyplot", call.list)
  ans
}

.ac   = function(x) {return(as.character(x))}
.an   = function(x) {return(as.numeric(x))}
.anf  = function(x) {return(as.numeric(as.character(x)))}

.setOutputNames = function(out){
  
  names(out)     = c("Years", "Total Fishing mortality", "Total biomass no Fishing", "SSB no fishing", "Total biomass",
                      "SSB in the future under scenario 1", "SSB in the future under scenario 2",
                      "SSB in the future under scenario 3", "SSB in the future under scenario 4",
                      "SSB in the future under scenario 5", "Catch in the future under scenario 1",
                      "Catch in the future under scenario 2", "Catch in the future under scenario 3",
                      "Catch in the future under scenario 4", "Catch in the future under scenario 5",
                      "SSB", "Recruitment", "Numbers at age", "F at age fishery 1", "F at age fishery 2",
                      "F at age fishery 3", "F at age fishery 4", "Fishery names", "Indices names",
                      "Observations at age survey 1", "Observations at age survey 2", "Observations at age survey 3",
                      "Observations at age survey 4", "Observations at age survey 5", "Observations at age survey 6",
                      "Observations at age survey 7", "Observations at age survey 8", "Observations at age survey 9",
                      "Survey catchabilities", "Proportions at age fishery 1 observed",
                      "Proportions at age fishery 2 observed", "Proportions at age fishery 3 observed",
                      "Proportions at age fishery 4 observed", "Proportions at age fishery 1 predicted",
                      "Proportions at age fishery 2 predicted", "Proportions at age fishery 3 predicted",
                      "Proportions at age fishery 4 predicted", "Proportions at age survey 1 observed",
                      "Proportions at age survey 4 observed", "Proportions at age survey 1 predicted",
                      "Proportions at age survey 4 predicted", "Total catch fishery 1 observed",
                      "Total catch fishery 1 predicted", "Total catch fishery 2 observed",
                      "Total catch fishery 2 predicted", "Total catch fishery 3 observed",
                      "Total catch fishery 3 predicted", "Total catch fishery 4 observed",
                      "Total catch fishery 4 predicted", "F_fsh_1", "F_fsh_2", "F_fsh_3", "F_fsh_4", 
                      "Selectivity fishery 1", "Selectivity fishery 2", "Selectivity fishery 3", "Selectivity fishery 4",
                      "Selectivity survey 1", "Selectivity survey 2", "Selectivity survey 3", "Selectivity survey 4",
                      "Selectivity survey 5", "Selectivity survey 6", "Selectivity survey 7", "Selectivity survey 8",
                      "Selectivity survey 9", "Stock recruitment", "Stock recruitment curve", "Likelihood composition",
                      "Likelihood composition names", "Sel_Fshry_1", "Sel_Fshry_2", "Sel_Fshry_3", "Sel_Fshry_4",
                      "Survey_Index_1", "Survey_Index_2", "Survey_Index_3", "Survey_Index_4", "Survey_Index_5",
                      "Survey_Index_6", "Survey_Index_7", "Survey_Index_8", "Survey_Index_9", "Age_Survey_1",
                      "Age_Survey_2", "Age_Survey_3", "Age_Survey_4", "Age_Survey_5", "Age_Survey_6", "Age_Survey_7",
                      "Age_Survey_8", "Age_Survey_9", "Sel_Survey_1", "Sel_Survey_2", "Sel_Survey_3", "Sel_Survey_4",
                      "Sel_Survey_5", "Sel_Survey_6", "Sel_Survey_7", "Sel_Survey_8", "Sel_Survey_9",
                      "Recruitment penalty", "F penalty", "Survey 1 catchability penalty",
                      "Survey 1 catchability power function", "Survey 2 catchability penalty",
                      "Survey 2 catchability power function", "Survey 3 catchability penalty",
                      "Survey 3 catchability power function", "Survey 4 catchability penalty",
                      "Survey 4 catchability power function", "Survey 5 catchability penalty",
                      "Survey 5 catchability power function", "Survey 6 catchability penalty",
                      "Survey 6 catchability power function", "Survey 7 catchability penalty",
                      "Survey 7 catchability power function", "Survey 8 catchability penalty",
                      "Survey 8 catchability power function", "Survey 9 catchability penalty",
                      "Survey 9 catchability power function", "Natural mortality", "Steepness of recruitment",
                      "Sigma recruitment", "Number of parameters estimated", "Steepness prior", "Sigma recruitment prior",
                      "Rec_estimated_in_styr_endyr", "SR_Curve_fit__in_styr_endyr", "Model_styr_endyr",
                      "Natural mortality prior", "q prior", "q power prior", "cv catch biomass", "Projection year range",
                      "Fsh_sel_opt_fish", "Survey_Sel_Opt_Survey", "Phase_survey_Sel_Coffs", "Fishery selectivity ages",
                      "Survey selectivity ages", "Phase_for_age_spec_fishery", "Phase_for_logistic_fishery",
                      "Phase_for_dble_logistic_fishery", "Phase_for_age_spec_survey", "Phase_for_logistic_survey",
                      "Phase_for_dble_logistic_srvy", "EffN_Fsh_1", "EffN_Fsh_2", "EffN_Fsh_3", "EffN_Fsh_4",
                      "C_fsh_1", "C_fsh_2", "C_fsh_3", "C_fsh_4", "Weight at age in the population","Maturity at age",
                      "Weight at age in fishery 1", "Weight at age in fishery 2", "Weight at age in fishery 3",
                      "Weight at age in fishery 4", "Weight at age in survey 1", "Weight at age in survey 2",
                      "Weight at age in survey 3", "Weight at age in survey 4", "Weight at age in survey 5",
                      "Weight at age in survey 6", "Weight at age in survey 7", "Weight at age in survey 8",
                      "Weight at age in survey 9", "EffN_Survey_1", "EffN_Survey_4")
  return(out)}

.readYPR = function(fileName){
  if(!file.exists(fileName)) {
    x = rep(NA, 501)
    jjm.ypr = data.frame(F=x, SSB=x, Yld=x, Recruit=x, SPR=x, B=x)
    return(jjm.ypr)
  }
  
  jjm.ypr            = read.table(fileName, sep = " ", skip = 4, header = TRUE, fill = TRUE)
  jjm.ypr[1,]        = jjm.ypr[1, c(1, 8, 2, 3, 4, 5, 6, 7)]
  colnames(jjm.ypr)  = c("F", "X", "SSB", "Yld", "Recruit", "SPR", "B", "X2")
  jjm.ypr            = jjm.ypr[,-grep("X", colnames(jjm.ypr))]
  return(jjm.ypr)
}

# Function that have not been used
.plot.bubbles    = function(x, xlab = " ", ylab = " ", main = " ", factx = 0, facty = 0, amplify = 1){
  my.col = c("white", " ", "black")
  xval = c(col(x)) + factx
  yval = c(row(x)) + facty
  
  # area of bubble is proportional to value.
  plot(x = xval, y = yval, cex = amplify*c(sqrt(abs(x/pi))), xlab = xlab, ylab = ylab, main = main,
       pch = 19, xaxt = "n", yaxt = "n", col = my.col[2 + check.zero(c(x)/check.zero(abs(c(x))))])
  
  # area of bubble is proportional to value.
  points(x = xval, y = yval, cex = amplify*c(sqrt(abs(x/pi))))
}

# Function that have not been used
.check.zero      = function(x){
  ## checks if there are zeros and replaces them with 1.
  x[x == 0] = 1
  return(x)
}

# .diagnostic function -----------------------------------------------------
.diagnostics = function(jjm.info, jjm.out, jjm.in, ...){
  
  # Get model name
  model = jjm.info$model
  #jjm.out = jjm.out
  
  #- Generic attributes of the stock assessment
  Nfleets   = length(c(jjm.out$Fshry_names))
  Nsurveys  = length(c(jjm.out$Index_names))
  ages      = jjm.in$ages[1]:jjm.in$ages[2]
  lengths   = jjm.in$lengths[1]:jjm.in$lengths[2]
  
  #- Get the age-structured fleets and length-structured fleets out
  if(length(grep("pobs_fsh_", names(jjm.out))) > 0){
    ageFleets = unlist(strsplit(names(jjm.out)[grep("pobs_fsh_", names(jjm.out))], split = "_"))
    ageFleets = ageFleets[seq(3, length(ageFleets), 3)]
  } else { ageFleets = 0}
  
  if(length(grep("pobs_len_fsh_", names(jjm.out))) > 0){
    lgtFleets = unlist(strsplit(names(jjm.out)[grep("pobs_len_fsh_", names(jjm.out))], split = "_"))
    lgtFleets = lgtFleets[seq(4, length(lgtFleets), 4)]
  } else {lgtFleets = 0}
  
  #- Get the age-structured surveys and length-structured surveys out
  if(length(grep("pobs_ind_", names(jjm.out))) > 0){
    ageSurveys = unlist(strsplit(names(jjm.out)[grep("pobs_ind_", names(jjm.out))], "_"))
    ageSurveys = ageSurveys[seq(3, length(ageSurveys), 3)]
  } else {ageSurveys = 0}
  
  if(length(grep("pobs_len_ind_", names(jjm.out))) > 0){
    lgtSurveys = unlist(strsplit(names(jjm.out)[grep("pobs_len_ind_", names(jjm.out))], "_"))
    lgtSurveys = lgtSurveys[seq(4, length(lgtSurveys), 4)]
  } else {lgtSurveys = 0}
  
  # Plots of the INPUT data
  allPlots = list()
  
  inputPlots = list()
  
  # 1: Weight in the fishery by fleet
  inputPlots$weightFishery = .input_weightFisheryFUN(Nfleets, jjm.out, ages,
                                                      lwd = 1, xlab = "Years", ylab = "Weight",
                                                      main = "Weight at age in the fishery",
                                                      scales = list(alternating = 1, tck = c(1,0)))
  
  # 2: Weight at age in the survey
  inputPlots$weightAge = .input_weightAgeFUN(Nsurveys, jjm.out, ages,
                                              type = "l", lwd = 1,
                                              xlab = "Years", ylab = "Weight", main = "Weight at age in the survey",
                                              auto.key = list(space = "right", points = FALSE, lines = TRUE, type = "b"),
                                              scales = list(alternating = 1, tck = c(1,0)))
  
  
  # 3: Weight by cohort in the fleet  
  inputPlots$weightByCohortFleet = .input_weightByCohortFleetFUN(Nfleets, jjm.out, ages,
                                                                  type = "b", lwd = 1, pch = 19, cex = 0.6,
                                                                  xlab = "Age", ylab = "Weight",
                                                                  main = "Weight at age by cohort in the fleet",
                                                                  auto.key = list(space = "right", points = FALSE,
                                                                                  lines = TRUE, type = "b"), 
                                                                  scales = list(alternating = 1, tck = c(1,0)))
  
  # 4: Weight by cohort in the survey  
  inputPlots$weightByCohortSurvey = .input_weightByCohortSurveyFUN(Nsurveys, jjm.out, ages,
                                                                    type = "l", lwd = 1, pch = 19, cex = 0.6,
                                                                    xlab = "Age", ylab = "Weight",
                                                                    main = "Weight at age by cohort in the survey",
                                                                    auto.key = list(space = "right", points = FALSE,
                                                                                    lines = TRUE, type = "b"),
                                                                    scales = list(alternating = 1, tck = c(1,0)))
  
  # 5: Age composition of the catch
  if(.an(ageFleets)[1] != 0){
    inputPlots$ageFleets1 = .input_ageFleetsFUN(jjm.in, ageFleets, ages,
                                                 main = "Age composition in fleets", 
                                                 as.table = TRUE, ylab = "Proportion at age")    
    
    cols = rev(heat.colors(11))
    inputPlots$ageFleets2 = .input_ageFleets2FUN(jjm.in, ageFleets, cols, ages,
                                                  main = "Age composition in fleets",
                                                  zlab = "", ylab = "")    
    
    cols       = rainbow(length(ages))
    inputPlots$ageFleetsPlots = .input_ageFleetsPlotsFUN(jjm.in, ages, cols, ageFleets,
                                                          xlab = "Age", ylab = "Proportion at age")
  }
  
  # 6: Age composition of the survey
  if(length(which(jjm.in$Inumageyears > 0)) > 0){
    inputPlots$ageCompositionSurvey1 = .input_ageCompositionSurvey1FUN(jjm.in, ages,
                                                                        scales = list(rotation = 90,
                                                                                      alternating = 3,
                                                                                      y = list(axs = "i")),
                                                                        horizontal = FALSE, strip = FALSE, 
                                                                        strip.left = strip.custom(style = 1),
                                                                        main = "Age composition in surveys", 
                                                                        ylab = "Proportion at age")
    
    cols  = rev(heat.colors(11))
    inputPlots$ageCompositionSurvey2 = .input_ageCompositionSurvey2FUN(jjm.in, cols, ages,
                                                                        main = "Age composition in surveys",
                                                                        zlab = "", ylab = "",
                                                                        scales = list(rotation = 90, alternating = 3))
  }
  
  # 7: Weight in the population
  inputPlots$weightPopulation = .input_weightPopulationFUN(jjm.in, ages,
                                                            xlab = "Age", ylab = "Weight (kg)",
                                                            main = "Weight in the stock")
  
  # 8: Maturity at age in the population  
  inputPlots$maturityPopulation = .input_maturityPopulationFUN(jjm.in, ages,
                                                                xlab = "Age", ylab = "Proportion mature",
                                                                main = "Maturity in the stock")
  
  # 9: Length composition of the catch
  if(.an(lgtFleets)[1] != 0){
    cols  = rev(heat.colors(11))
    inputPlots$lengthComposition1 = .input_lengthComposition1FUN(cols, lgtFleets, jjm.in, lengths,
                                                                  zlab = "", ylab = "")    
    
    inputPlots$lengthComposition2 = .input_lengthComposition2FUN(lgtFleets, jjm.in, lengths,
                                                                  xlab = "Length", ylab = "Proportion at length")
  }
  
  # Plots of the fit of the catch data
  outPlots = list()
  
  # 9a: Trend in catch
  outPlots$totalCatch = .fit_totalCatchFUN(Nfleets, jjm.out,
                                            xlab = "Years", ylab = "Catch in kt", main = "Total catch", 
                                            scales = list(alternating = 1, tck = c(1,0), y = list(axs = "i")))
  
  #9b: trends in catch by fleet as polygon
  if(Nfleets > 1){
    outPlots$totalCatchByFleet = .fit_totalCatchByFleetFUN(jjm.out, Nfleets,
                                                            xlab = "Years", ylab = "Catch by fleet in kt", 
                                                            main = "Total catch by fleet",
                                                            scales = list(alternating = 1, tck = c(1,0),
                                                                          y = list(axs = "i")))
  }
  
  # 10: Log residual total catch by fleet
  outPlots$catchResidualsByFleet = .fit_catchResidualsByFleetFUN(Nfleets, jjm.out,
                                                                 xlab = "Years", ylab = "Residuals", 
                                                                 main = "Catch residuals by fleet",
                                                                 scales = list(alternating = 1, tck = c(1,0)),
                                                                 lwd = 3, cex.axis = 1.2, font = 2)
  
  # 11: Absolute residual catch by fleet
  outPlots$absoluteResidualCatchByFleet = .fit_absoluteResidualCatchByFleetFUN(Nfleets, jjm.out,
                                                                                scales = list(y = list(draw = FALSE), 
                                                                                              alternating = 3))
  
  # 12a: Proportions catch by age modelled and observed
  if(.an(ageFleets)[1] != 0){        
    outPlots$residualsCatchAtAgeByFleet = .fit_residualsCatchAtAgeByFleetFUN(ageFleets, jjm.out, ages,
                                                                             xlab = "Years", ylab = "Absolute residual catch", 
                                                                             main = "Absolute residual catch by fleet",
                                                                             scales = list(alternating = 1, tck = c(1, 0)))
  }
  
  # 12b: Proportions catch by length modelled and observed
  if(.an(lgtFleets)[1] != 0){    
    outPlots$residualsCatchAtLengthByFleet = .fit_residualsCatchAtLengthByFleetFUN(lgtFleets, jjm.out, lengths, Nfleets,
                                                                                   xlab = "Years", ylab = "Length", 
                                                                                   main = "Residuals catch-at-length by fleet",
                                                                                   scales = list(alternating = 3))
  }
  
  # 13a: Fitted age by year by fleet
  
  if(.an(ageFleets)[1] != 0){
    outPlots$ageFitsCatch = .fit_ageFitsCatchFUN(ageFleets, jjm.out, ages,
                                                  xlab = "Age", ylab = "Proportion at age", 
                                                  scales = list(alternating = 3))
  }
  
  # 13b: Fitted length by year by fleet
  if(.an(lgtFleets)[1] != 0){
    outPlots$lengthFitsCatch = .fit_lengthFitsCatchFUN(lgtFleets, jjm.out, lengths,
                                                        xlab = "Length", ylab = "Proportion at length")
  }
  
  # 14: Absolute catch by fleet modelled and observed
  cols  = rainbow(11)  
  outPlots$predictedObservedCatchesByFleet = .fit_predictedObservedCatchesByFleetFUN(Nfleets, cols, jjm.out,
                                                                                     scales = list(alternating = 1, tck = c(1,0)),
                                                                                     main = "Predicted and observed catches by fleet",
                                                                                     xlab = "Years", ylab = "Thousand tonnes")
  
  #-----------------------------------------------------------------------------
  #- Plots of the fit of the survey data
  #-----------------------------------------------------------------------------
  
  # 15: Standardized indices observed with error and modelled  
  outPlots$predictedObservedIndices = .fit_predictedObservedIndicesFUN(Nsurveys, jjm.out,
                                                                        main = "Predicted and observed indices",
                                                                        xlab = "Years", ylab = "Normalized index value", 
                                                                        ylim = c(-0.2, 2),
                                                                        scales = list(alternating = 3, y = list(draw = FALSE)))
  
  # 15b: Fitted age by year by survey
  if(.an(ageSurveys)[1] != 0){
    outPlots$ageFitsSurvey = .fit_ageFitsSurveyFUN(ageSurveys, jjm.out, ages, ageFleets,
                                                    xlab = "Age", ylab = "Proportion at age", 
                                                    scales = list(alternating = 3))
  }
  
  # 16: Log residuals in survey
  cols  = rainbow(length(ages))  
  outPlots$standardizedSurveyResiduals = .fit_standardizedSurveyResidualsFUN(Nsurveys, jjm.out, cols,
                                                                              xlab = "Years", ylab = "Log residuals", 
                                                                              main = "Standardized survey residuals",
                                                                              lwd = 3, cex.axis = 1.2, font = 2,
                                                                              scales = list(y = list(draw = FALSE),
                                                                                            alternating = 1, tck = c(1, 0)))
  
  # 16b: standard deviation of time series variances
  outPlots$sdPerInputSeries = .fit_sdPerInputSeriesFUN(jjm.out,
                                                        main = "SD per input series", ylab = "SD", xlab = "Years",
                                                        scales = list(alternating = 1, tck = c(1, 0)))
  
  #-----------------------------------------------------------------------------
  #- Plots of selectivity in fleet and survey + F's
  #-----------------------------------------------------------------------------  
  # 17: Selectivity at age in the fleet
  outPlots$selectivityFisheryByPentad = .fit_selectivityFisheryByPentadFUN(Nfleets, jjm.out, ages,
                                                                            scale = list(alternating = 3),
                                                                            main = "Selectivity of the Fishery by Pentad",
                                                                            xlab = "Age", ylab = "Selectivity")
  
  # 18: selecitivity at age in the survey
  outPlots$selectivitySurveyByPentad = .fit_selectivitySurveyByPentadFUN(Nsurveys, jjm.out, ages,
                                                                          scale = list(alternating = 3),
                                                                          main = "Selectivity of the survey by Pentad",
                                                                          xlab = "Age", ylab = "Selectivity")
  
  # 19a: F at age
  cols  = rev(heat.colors(11))  
  outPlots$fAtAGe = .fit_fAtAGeFUN(jjm.out, ages, cols,
                                   scale = list(alternating = 1, tck = c(1,0)),
                                   xlab = "Age", ylab = "Years", main = "F at age")
  
  # 19b: Prop F at age
  cols  = rainbow(length(ages))
  outPlots$fProportionAtAGe = .fit_fProportionAtAGeFUN(jjm.out, ages, cols,
                                                       xlab = "Years", ylab = "Proportion of F at age", 
                                                       main = "F proportion at age",
                                                       ylim = c(0, 1), 
                                                       scales = list(alternating = 1, tck = c(1,0),
                                                                     y = list(axs = "i")))
  
  #19b: N at age
  cols = rev(heat.colors(11))  
  outPlots$nAtAGe = .fit_nAtAGeFUN(jjm.out, cols,
                                   scales = list(alternating = 1, tck = c(1,0)),
                                   xlab = "Age", ylab = "Years", main = "N at age")
  
  #19c: Prop N at age
  cols  = rainbow(length(ages))
  outPlots$nProportionAtAGe = .fit_nProportionAtAGeFUN(jjm.out, cols, ages,
                                                       xlab = "Years", ylab = "Proportion of N at age", 
                                                       main = "Proportion at age",
                                                       ylim = c(0, 1),
                                                       scales = list(alternating = 1, tck = c(1,0),
                                                                     y = list(axs = "i")))
  
  #20: Fisheries mean age  
  if(.an(ageFleets)[1] != 0){
    outPlots$fisheryMeanAge = .fit_fisheryMeanAgeFUN(jjm.out, ageFleets,
                                                     lwd = 3, lty = c(1, 3), col = 1,
                                                     ylab = "Age", xlab = "Years", main = "Fishery mean age",
                                                     scales = list(alternating = 1, tck = c(1,0),
                                                                   y = list(axs = "i")))
  } 
  
  #20: Fisheries mean length  
  if(.an(lgtFleets)[1] != 0){
    outPlots$fisheryMeanLength = .fit_fisheryMeanLengthFUN(lgtFleets, jjm.out,
                                                           lwd = 3, lty = c(1, 3), col = 1,
                                                           ylab = "Length (cm)", xlab = "Years", 
                                                           main = "Fishery mean length",
                                                           scales = list(alternating = 1, tck = c(1,0)))
  }
  
  #21: Survey mean age
  if(.an(ageFleets)[1] != 0){
    outPlots$surveyMeanAge = .fit_surveyMeanAgeFUN(Nsurveys, jjm.out,
                                                   lwd = 3, lty = c(1, 3), col = 1,
                                                   ylab = "Age", xlab = "Years", main = "Survey mean age",
                                                   scales = list(alternating = 1, tck = c(1,0)))
  }
  
  #-----------------------------------------------------------------------------
  #- Plots of stock summary
  #-----------------------------------------------------------------------------
  # 22a: summary sheet with SSB, R, F and Biomass and Catch
  outPlots$summarySheet = .fit_summarySheetFUN(jjm.out,
                                                main = NA,
                                                scales = list(alternating = 1, tck = c(1,0), 
                                                              y = list(relation = "free", rot = 0),
                                                              axs = "i"))
  
  # 22b: Summary sheet 2
  outPlots$summarySheet2 = .fit_summarySheet2FUN(jjm.out,
                                                  main = NA, 
                                                  scales = list(alternating = 1, tck = c(1,0), 
                                                                y = list(relation = "free", rot = 0),
                                                                axs = "i"))
  
  # 22b Uncertainties of key parameters
  outPlots$uncertaintyKeyParams = .fit_uncertaintyKeyParamsFUN(jjm.out,
                                                               auto.key = list(space = "right", 
                                                                               points = FALSE, 
                                                                               lines = FALSE, 
                                                                               type = "l", col = 1:3),
                                                               col = 1:3, lwd = 2, 
                                                               main = "Uncertainty of key parameters")
                                                               
  # 23: Mature - immature ratio
  cols  = rainbow(length(ages))
  outPlots$matureInmatureFishes = .fit_matureInmatureFishesFUN(jjm.out, 
                                                                lwd = 3, lty = c(1, 3), col = 1,
                                                                ylab = "Biomass in kt", xlab = "Years", 
                                                                main = "Mature - Immature fish")
  
  # 24: Stock-recruitment
  if(length(grep("SR_Curve_years", names(jjm.out))) == 0 ){
  outPlots$stockRecruitment = .fit_stockRecruitmentFUN(jjm.out,
                                                        ylab = "Recruitment", xlab = "Spawning Stock Biomass", 
                                                        main = "Stock Recruitment")
  } else {
  outPlots$stockRecruitment = .fit_stockRecruitmentFUN2(jjm.out, cols,
                                                 ylab = "Recruitment", xlab = "Spawning Stock Biomass", 
                                                 main = "Stock Recruitment")
  }
  # 25: SSB not fished over SSB fished
  outPlots$fishedUnfishedBiomass = .fit_fishedUnfishedBiomassFUN(jjm.out,
                                                                  ylab = "Total biomass", xlab = "Years", 
                                                                  main = "Fished vs. unfished biomass",
                                                                  col = c(1, 1), lwd = 3, lty = c(1, 3))
  
  # Plots of catch and ssb projections
  #projectionsPlots = list()
  
  # 25: SSB projections
  outPlots$ssbPrediction = .projections_ssbPredictionFUN(jjm.out,
                                                                  ylab = "Spawning Stock Biomass", xlab = "Years", 
                                                                  main = "SSB prediction")
  
  # 26: Catch projections    
  outPlots$catchPrediction = .projections_catchPredictionFUN(jjm.out,
                                                                      ylab = "Catch", xlab = "Years", 
                                                                      main = "Catch prediction")
   
  
  # Plots of yield per recruit and yield biomass and kobe plot
  #yprPlots = list()
  
  outPlots$yieldSsbPerRecruit = .ypr_yieldSsbPerRecruitFUN(jjm.out,
                                                            main = "Yield and spawing stock biomass per recruit",
                                                            xlab = "Fishing mortality", ylab = "Spawing biomass / Yield per recruit",
                                                            scales = list(alternating = 1, tck = c(1,0),
                                                                          y = list(relation = "free", rot = 0)))
  

  outPlots$kobePlot = .kobeFUN(jjm.out)
  
  if(length(grep("SR_Curve_years", names(jjm.out))) == 0 ){
		outPlots$recDev = NULL
	} else {
  outPlots$recDev = .recDevFUN(jjm.out, cols, breaks = 10)
  }

  # Join all plots
  plotTree = list(model = model, data = names(inputPlots), output = names(outPlots))
            #       projections = names(projectionsPlots), ypr = names(yprPlots))
  allPlots = list(info = plotTree, data = inputPlots, output = outPlots)
					#, projections = projectionsPlots, ypr = yprPlots,
                    # )
  
#   class(allPlots) = "jjm.diag"
  
  return(allPlots)
}


# Plots of diagnostic function --------------------------------------------
.input_weightFisheryFUN = function(Nfleets, jjm.out, ages, ...)
{
  
  wt_fsh = grep(pattern ="wt_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in 1:Nfleets){
    res = .createDataFrame(jjm.out[[wt_fsh[iFleet]]][,-1], jjm.out$Yr, ages)
    
    if(iFleet == 1)
      tot = cbind(res, c(jjm.out$Fshry_names[iFleet]))
    
    if(iFleet != 1)
      tot = rbind(tot,cbind(res,c(jjm.out$Fshry_names[iFleet])))
  }
  colnames(tot) = c("year","data","age","fleet"); res = tot
  
  pic = xyplot(data~year|fleet, data = res, groups = age,
                type = "l",
                auto.key = list(space = "right", points = FALSE, lines = TRUE, type = "b"),
                par.settings = list(superpose.symbol = list(pch = as.character(ages), cex = 1)),
                ...)
  
  return(pic)
}

.input_weightAgeFUN = function(Nsurveys, jjm.out, ages, ...)
{
  
  wt_ind = grep(pattern ="wt_ind_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iSurvey in 1:Nsurveys){
    res = .createDataFrame(jjm.out[[wt_ind[iSurvey]]][,-1], jjm.out$Yr, ages)
    
    if(iSurvey == 1)
      tot = cbind(res, c(jjm.out$Index_names[iSurvey]))
    
    if(iSurvey != 1)
      tot = rbind(tot, cbind(res, c(jjm.out$Index_names[iSurvey])))
  }
  colnames(tot) = c("year","data","age","survey"); res = tot
  
  pic = xyplot(data~year|survey, data = res, groups = age,
                par.settings = list(superpose.symbol = list(pch = as.character(ages), cex = 1)), ...)
  
  return(pic)
}

.input_weightByCohortFleetFUN = function(Nfleets, jjm.out, ages, ...)
{
  
  wt_fsh = grep(pattern ="wt_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in 1:Nfleets){
    res = .createDataFrame(jjm.out[[wt_fsh[iFleet]]][,-1], jjm.out$Yr, ages)
    
    if(iFleet == 1)
      tot = cbind(res, c(jjm.out$Fshry_names[iFleet]))
    
    if(iFleet != 1)
      tot = rbind(tot, cbind(res, c(jjm.out$Fshry_names[iFleet])))
  }
  colnames(tot) = c("year","data","age","fleet"); res = tot
  res$cohort = res$year - res$age
  yrs = sort(rev(jjm.out$Yr)[1:13])
  
  pic = xyplot(data ~ age|fleet, data = subset(res, cohort%in%yrs), groups = cohort,
                par.settings = list(superpose.symbol = list(pch = .ac(ages), cex = 1)),
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                }, ...)
  
  return(pic)
}

.input_weightByCohortSurveyFUN = function(Nsurveys, jjm.out, ages, ...)
{
  
  wt_ind = grep(pattern ="wt_ind_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iSurvey in 1:Nsurveys){
    res = .createDataFrame(get("jjm.out")[[wt_ind[iSurvey]]][,-1], jjm.out$Yr, ages)
    if(iSurvey == 1) tot = cbind(res,c(jjm.out$Index_names[iSurvey]))
    if(iSurvey != 1) tot = rbind(tot,cbind(res,c(jjm.out$Index_names[iSurvey])))
  }
  colnames(tot) = c("year","data","age","survey"); res = tot
  res$cohort = res$year - res$age
  
  yrs = sort(rev(jjm.out$Yr)[1:13])
  
  
  pic = xyplot(data ~ age|survey, data = subset(res, cohort %in% yrs), groups = cohort,
                par.settings = list(superpose.symbol = list(pch = as.character(ages), cex = 1)),                
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                }, ...)
  
  return(pic)
}

.input_ageFleetsFUN = function(jjm.in, ageFleets, ages, ...)
{
  for(iFleet in seq_along(.an(ageFleets))){
    res = .createDataFrame(sweep(jjm.in$Fagecomp[,,iFleet], 1,
                                  apply(jjm.in$Fagecomp[,,iFleet], 1, sum, na.rm = T), "/"),
                            jjm.in$years[1]:jjm.in$years[2],
                            ages)
    
    res = cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == seq_along(.an(ageFleets))[1])
      tot = res
    
    if(iFleet != seq_along(.an(ageFleets))[1])
      tot = rbind(tot,res)
  }
  
  colnames(tot) = c("year","data","age","fleet"); res = tot
  yrs           = rev(sort(unique(res$year)))[1:10]
  
  pic = barchart(data ~ age | fleet* as.factor(year), data = subset(res, year %in% yrs),
                 xlim = range(pretty(c(min(res$year), max(res$year)))),
                 scales = list(rotation = 90, alternating = 3, y = list(axs = "i")), horizontal = FALSE,
                 groups = fleet, strip = FALSE, strip.left = strip.custom(style = 1), reverse.rows = TRUE,
                 panel = function(...){
                   panel.grid(h = -1, v = -1)
                   panel.barchart(...)
                 }, ...)
  
  
  return(pic)
}

.input_ageFleets2FUN = function(jjm.in, ageFleets, cols, ages, ...)
{
  for(iFleet in seq_along(.an(ageFleets))){
    res = .createDataFrame(sweep(jjm.in$Fagecomp[,,iFleet], 1,
                                  apply(jjm.in$Fagecomp[,,iFleet], 1, sum, na.rm = T), "/"),
                            jjm.in$years[1]:jjm.in$years[2],
                            ages)
    
    res = cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == seq_along(.an(ageFleets))[1])
      tot = res
    
    if(iFleet != seq_along(.an(ageFleets))[1])
      tot = rbind(tot,res)
  }
  
  colnames(tot) = c("year","data","age","fleet"); res = tot
  
  pic = levelplot(data ~ age*year | fleet, data = res, col.regions = cols, cuts = 10,
                   colorkey = TRUE, layout = c(length(ageFleets), 1), ...)
  
  return(pic)
}

.input_ageFleetsPlotsFUN = function(jjm.in, ages, cols, ageFleets, ...)
{
  for(iFleet in seq_along(.an(ageFleets))){
    res = .createDataFrame(sweep(jjm.in$Fagecomp[,,iFleet], 1,
                                  apply(jjm.in$Fagecomp[,,iFleet], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2],
                            ages)
    
    res = cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == seq_along(.an(ageFleets))[1])
      tot = res
    
    if(iFleet != seq_along(.an(ageFleets))[1])
      tot = rbind(tot,res)
  }
  
  colnames(tot) = c("year","data","age","fleet"); res = tot
  
  res$cohort = (res$year - res$age) %% length(ages) + 1
  ageFleetsPlots = list()
  for(iFleet in unique(res$fleet)){
    tmpres = subset(res, fleet == iFleet)
    tmpres$data[tmpres$data <= 1e-2 & tmpres$age == 1] = 0
    
    pic = xyplot(data ~ age | as.factor(year), data = tmpres,
                  main = paste("Age composition in fleets", iFleet),
                  as.table = TRUE,
                  panel = function(x,y){
                    yr      = names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  = tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x, y, horizontal = FALSE, origin = 0, box.width = 1,
                                   col = cols[tmpres$cohort[colidx]])
                  }, ...)
    
    ageFleetsPlots[[iFleet]] = pic
  }
  
  return(ageFleetsPlots)
}

.input_ageCompositionSurvey1FUN = function(jjm.in, ages, ...)
{
  for(iSurvey in which(jjm.in$Inumageyears>0)){
    res = .createDataFrame(sweep(jjm.in$Ipropage[,,iSurvey], 1,
                                  apply(jjm.in$Ipropage[,,iSurvey], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], ages)
    res = cbind(res, jjm.in$Inames[iSurvey])
    
    yrs = rev(sort(unique(res$year)))[1:10]
    
    if(iSurvey == 1)
      tot = res
    
    if(iSurvey != 1)
      tot = rbind(tot, res)
  }
  
  colnames(tot) = c("year", "data", "age", "survey"); res = tot
  
  pic = barchart(data ~ age | survey * as.factor(year), data = subset(res, year %in% yrs),
                  
                  groups = survey, 
                  reverse.rows = TRUE,
                  as.table = TRUE,
                  panel = function(...){
                    panel.grid(h = -1, v = -1)
                    panel.barchart(...)
                  }, ...)
  
  return(pic)
}

.input_ageCompositionSurvey2FUN = function(jjm.in, cols, ages, ...)
{
  for(iSurvey in which(jjm.in$Inumageyears > 0)){
    res = .createDataFrame(sweep(jjm.in$Ipropage[,,iSurvey], 1,
                                  apply(jjm.in$Ipropage[,,iSurvey], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], ages)
    res = cbind(res, jjm.in$Inames[iSurvey])
    
    yrs = rev(sort(unique(res$year)))[1:10]
    
    if(iSurvey == 1)
      tot = res
    
    if(iSurvey != 1)
      tot = rbind(tot, res)
  }
  
  colnames(tot) = c("year", "data", "age", "survey"); res = tot
  
  pic = levelplot(data ~ age*year | survey, data = res,
                   col.regions = cols, cuts = 10,
                   colorkey = TRUE, as.table = FALSE, ...)
  
  return(pic)
}

.input_weightPopulationFUN = function(jjm.in, ages, ...)
{
  res = data.frame(year = 1, data = jjm.in$wt_temp, age = ages)
  pic = xyplot(data~age, data = res,
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., type = "b", pch = 19, cex = 0.6, lwd = 2, col = 1)
                }, ...)
  
  return(pic)
}

.input_maturityPopulationFUN = function(jjm.in, ages, ...)
{
  res = data.frame(year = 1, data = jjm.in$mt_temp, age = ages)
  pic = xyplot(data~age, data = res,
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., type = "b", pch = 19, cex = 0.6, lwd = 2, col = 1)
                }, ...)
  
  return(pic)
}

.input_lengthComposition1FUN = function(cols, lgtFleets, jjm.in, lengths, ...)
{
  for(iFleet in .an(lgtFleets)){
    res = .createDataFrame(sweep(jjm.in$Flengthcomp[,,iFleet], 1,
                                  apply(jjm.in$Flengthcomp[,,iFleet], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], lengths)
    res = cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == .an(lgtFleets)[1])
      tot = res
    
    if(iFleet != .an(lgtFleets)[1]) 
      tot = rbind(tot, res)
  }
  colnames(tot) = c("year", "data", "length", "fleet"); res = tot
  
  lengthComposition1 = list()
  for(iFleet in unique(tot$fleet)){
    pic = levelplot(data ~ length*year | fleet, data = subset(tot, fleet == iFleet), as.table = TRUE,
                     col.regions = cols, cuts = 10,
                     main = paste("Length composition in fleet", iFleet),
                     colorkey = TRUE, ...)
    
    lengthComposition1[[iFleet]] = pic
  }
  
  return(lengthComposition1)
}

.input_lengthComposition2FUN = function(lgtFleets, jjm.in, lengths, ...)
{
  for(iFleet in .an(lgtFleets)){
    res = .createDataFrame(sweep(jjm.in$Flengthcomp[,,iFleet], 1,
                                  apply(jjm.in$Flengthcomp[,,iFleet], 1, sum, na.rm = TRUE), "/"),
                            jjm.in$years[1]:jjm.in$years[2], lengths)
    res = cbind(res, jjm.in$Fnames[iFleet])
    
    if(iFleet == .an(lgtFleets)[1])
      tot = res
    
    if(iFleet != .an(lgtFleets)[1]) 
      tot = rbind(tot, res)
  }
  colnames(tot) = c("year", "data", "length", "fleet"); res = tot
  
  tot$cohort = (tot$year - tot$length) %% length(lengths) + 1
  lengthComposition2 = list()
  for(iFleet in unique(tot$fleet)){
    tmpres = subset(tot, fleet == iFleet)
    
    pic = xyplot(data ~ length | as.factor(year), data = tmpres,
                  main = paste("Length composition in fleets", iFleet),
                  as.table = TRUE,
                  panel = function(x, y){
                    yr      = names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  = tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x, y, horizontal = FALSE, origin = 0, box.width = 1, col = tmpres$cohort[colidx],
                                   border = 'transparent')
                  }, ...)
    
    lengthComposition2[[iFleet]] = pic
  }
  
  return(lengthComposition2)
}

.fit_totalCatchFUN = function(Nfleets, jjm.out, ...)
{
  Obs_catch = grep(pattern ="Obs_catch_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in 1:Nfleets){
    res = cbind(jjm.out$Yr, jjm.out[[Obs_catch[iFleet]]])
   
    if(Nfleets == 1) res = cbind(res, jjm.out$Fshry_names[iFleet])
    if(Nfleets > 1) res = cbind(res, jjm.out$Fshry_names[iFleet, 1])
    if(iFleet == 1) tot = res
    if(iFleet != 1) tot = rbind(tot, res)
  }
  colnames(tot) = c("year", "catch", "fleet"); res = data.frame(tot)
  res$catch = as.numeric(as.character(res$catch))
  
  totcatch = numeric()
  for(iYr in sort(unique(res$year))){
    totcatch[which(iYr == sort(unique(res$year)))] = sum(subset(res, year == iYr)$catch)
  }
  
  pic = xyplot(totcatch~jjm.out$Yr, ylim = c(0, 1.1*max(totcatch)),
                
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.barchart(..., horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                }, ...)
  
  return(pic)
}

.fit_totalCatchByFleetFUN = function(jjm.out, Nfleets, ...)
{
  Obs_catch = grep(pattern = "Obs_catch_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iFleet in 1:Nfleets){
    res = cbind(jjm.out$Yr, jjm.out[[Obs_catch[iFleet]]])
    
    if(Nfleets == 1) res = cbind(res, jjm.out$Fshry_names[iFleet])
    if(Nfleets > 1) res = cbind(res, jjm.out$Fshry_names[iFleet, 1])
    if(iFleet == 1) tot = res
    if(iFleet != 1) tot = rbind(tot, res)
  }
  colnames(tot) = c("year", "catch", "fleet"); res = data.frame(tot)
  res$catch = as.numeric(as.character(res$catch))
  
  res$year = .an(.ac(res$year))
  
  for(iFleet in 2:Nfleets){
    idx = which(res$fleet == jjm.out$Fshry_names[iFleet])
    res[idx,"catch"] = subset(res, fleet == jjm.out$Fshry_names[iFleet])$catch +
      subset(res, fleet == jjm.out$Fshry_names[iFleet - 1])$catch
  }
  
  pic = xyplot(catch ~ year, data = res, groups = fleet,
                type = "l",
                auto.key = list(space = "right", points = FALSE, lines = FALSE, col = 1:Nfleets),
                panel = function(...){
                  lst = list(...)
                  idx = mapply(seq,
                                from = seq(1, length(lst$y), length(lst$y)/Nfleets),
                                to = seq(1, length(lst$y), length(lst$y)/Nfleets) + (length(lst$y)/Nfleets - 1))
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., col = "white")
                  for(iFleet in Nfleets:1){
                    panel.polygon(x = c(lst$x[idx[,iFleet]], rev(lst$x[idx[,iFleet]])),
                                  y = c(rep(0, length(lst$y[idx[,iFleet]])), rev(lst$y[idx[,iFleet]])),
                                  col = (Nfleets:1)[iFleet], border = 0)
                  }
                  
                }, ...)
  
  return(pic)
}

.fit_catchResidualsByFleetFUN = function(Nfleets, jjm.out, ...)
{
  Obs_catch  = grep(pattern = "Obs_catch_[0-9]*", x = names(jjm.out), value=TRUE)
  Pred_catch = grep(pattern = "Pred_catch_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in 1:Nfleets){
    res = data.frame(year = jjm.out$Yr, obs = jjm.out[[Obs_catch[iFleet]]],
                      model = jjm.out[[Pred_catch[iFleet]]], fleet = jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot = res
    if(iFleet != 1) tot = rbind(tot, res)
  }
  tot$obs[tot$obs<=0]   = NA
  tot$obs[tot$model<=0] = NA
  res                   = tot
  resids                = log(res$obs + 1) - log(res$model + 1); res$resids = resids
  scalar                = 3/max(resids, na.rm = TRUE)
  residRange            = range(resids, na.rm = TRUE)
  ikey                  = simpleKey(text = .ac(round(seq(residRange[1], residRange[2], length.out = 6), 2)),
                                     points = TRUE, lines = FALSE, columns = 2)
  ikey$points$cex       = abs(round(seq(residRange[1], residRange[2], length.out = 6), 2))*scalar
  ikey$points$col       = 1
  ikey$points$pch       = ifelse(test = round(seq(residRange[1], residRange[2], length.out = 6), 2) > 0,
                                  yes = 19, no = 1)
  
  
  pic = xyplot(resids*scalar ~ year | as.factor(fleet), data = res,
                prepanel = function(...) {list(ylim = c(1, 1))},
                layout = c(1, Nfleets),
                type = "p", key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  panel.points(x, 1, cex = abs(y), col = ifelse(test = y > 0, yes = "black",  "white"), pch = 19)
                  panel.points(x, 1, cex = abs(y), col = 1, pch = 1)
                }, ...)
  
  return(pic)
}

.fit_absoluteResidualCatchByFleetFUN = function(Nfleets, jjm.out, ...)
{
  
  Obs_catch  = grep(pattern = "Obs_catch_[0-9]*", x = names(jjm.out), value=TRUE)
  Pred_catch = grep(pattern = "Pred_catch_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in 1:Nfleets){
#     res = data.frame(year = jjm.out$Yr, obs = jjm.out[[paste("Obs_catch_", iFleet, sep = "")]],
#                       model = jjm.out[[paste("Pred_catch_", iFleet, sep = "")]], fleet = jjm.out$Fshry_names[iFleet])

    res = data.frame(year = jjm.out$Yr, obs = jjm.out[[Obs_catch[iFleet]]],
                     model = jjm.out[[Pred_catch[iFleet]]], fleet = jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot = res
    if(iFleet != 1) tot = rbind(tot, res)
  }
  tot$obs[tot$obs <= 0]   = NA
  tot$obs[tot$model <= 0] = NA
  res                   = tot
  
  pic = xyplot(model - obs ~ year | fleet, data = res, allow.multiple = TRUE,
                panel = function(...){
                  panel.grid(h = -1, v = -1, lty = 3)
                  panel.barchart(..., horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                }, ...)
  
  return(pic)
}

.fit_residualsCatchAtAgeByFleetFUN = function(ageFleets, jjm.out, ages, ...)
{
  pobs_fsh = grep(pattern = "pobs_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  phat_fsh = grep(pattern = "phat_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in seq_along(.an(ageFleets))) {
    obs = .createDataFrame(jjm.out[[pobs_fsh[iFleet]]][,-1],
                           jjm.out[[pobs_fsh[iFleet]]][,1], ages)
    mod = .createDataFrame(jjm.out[[phat_fsh[iFleet]]][,-1],
                           jjm.out[[phat_fsh[iFleet]]][,1], ages)
    
    if(iFleet == seq_along(.an(ageFleets))[1]) tot = cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet, 1], nrow(obs)))
    if(iFleet != seq_along(.an(ageFleets))[1]) tot = rbind(tot, cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet,1], nrow(obs))))
  }
  
  colnames(tot)   = c("year","obs","age","model","fleet")
  res             = tot
  
  resids          = log(res$obs + 1) - log(res$model + 1)
  res$resids      = resids
  scalar          = 3/max(resids,na.rm=T)
  residRange      = range(resids,na.rm=T)
  ikey            = simpleKey(text=as.character(round(seq(residRange[1],residRange[2],length.out=6),2)),
                               points=T,lines=F,columns = 2, cex = 1.5)
  ikey$points$cex = abs(round(seq(residRange[1],residRange[2],length.out=6),2))*scalar
  ikey$points$col = 1
  ikey$points$pch = ifelse(round(seq(residRange[1],residRange[2],length.out=6),2)>0,19,1)
  
  pic = .bubbles(age ~ year|fleet, data = res, allow.multiple = TRUE,
                  key = ikey, ...)
  
  return(pic)
}

.fit_residualsCatchAtLengthByFleetFUN = function(lgtFleets, jjm.out, lengths, Nfleets, ...)
{
  pobs_len_fsh = grep(pattern = "pobs_len_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  phat_len_fsh = grep(pattern = "phat_len_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in seq_along(.an(lgtFleets))) {
    obs = .createDataFrame(jjm.out[[pobs_len_fsh[iFleet]]][,-1],
                           jjm.out[[pobs_len_fsh[iFleet]]][,1], lengths)
    mod = .createDataFrame(jjm.out[[phat_len_fsh[iFleet]]][,-1],
                           jjm.out[[phat_len_fsh[iFleet]]][,1], lengths)
    
    if(Nfleets == 1){
      if(iFleet == seq_along(.an(lgtFleets))[1]) tot = cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet], nrow(obs)))
      if(iFleet != seq_along(.an(lgtFleets))[1]) tot = rbind(tot, cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet], nrow(obs))))
    }
    if(Nfleets != 1){
      if(iFleet == seq_along(.an(lgtFleets))[1]) tot = cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet,1], nrow(obs)))
      if(iFleet != seq_along(.an(lgtFleets))[1]) tot = rbind(tot, cbind(obs, mod$data, rep(jjm.out$Fshry_names[iFleet,1], nrow(obs))))
    }
  }
  colnames(tot)   = c("year", "obs", "length", "model", "fleet")
  res             = tot
  
  resids          = log(res$obs + 1) - log(res$model + 1)
  res$resids      = resids
  scalar          = 3/max(resids, na.rm = TRUE)
  residRange      = range(resids, na.rm = TRUE)
  ikey            = simpleKey(text = .ac(round(seq(residRange[1], residRange[2], length.out = 6), 2)),
                               points = TRUE, lines = FALSE, columns = 2, cex = 1.5)
  ikey$points$cex = abs(round(seq(residRange[1], residRange[2], length.out = 6), 2))*scalar
  ikey$points$col = 1
  ikey$points$pch = ifelse(test = round(seq(residRange[1], residRange[2], length.out = 6),2) > 0, yes = 19, no = 1)
  
  pic = .bubbles(length ~ year|fleet, data = res, allow.multiple = TRUE,
                  key = ikey, ...)
  
  return(pic)
}

.fit_ageFitsCatchFUN = function(ageFleets, jjm.out, ages, ...)
{
  pobs_fsh = grep(pattern = "pobs_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  phat_fsh = grep(pattern = "phat_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in seq_along(.an(ageFleets))){
#     obs = .createDataFrame(jjm.out[[paste("pobs_fsh_", iFleet, sep = "")]][,-1],
#                             jjm.out[[paste("pobs_fsh_", iFleet, sep = "")]][,1], ages)
#     mod = .createDataFrame(jjm.out[[paste("phat_fsh_", iFleet, sep = "")]][,-1],
#                             jjm.out[[paste("phat_fsh_", iFleet, sep = "")]][,1], ages)

    obs = .createDataFrame(jjm.out[[pobs_fsh[iFleet]]][,-1],
                           jjm.out[[pobs_fsh[iFleet]]][,1], ages)
    mod = .createDataFrame(jjm.out[[phat_fsh[iFleet]]][,-1],
                           jjm.out[[phat_fsh[iFleet]]][,1], ages)
    
    if(iFleet == seq_along(.an(ageFleets))[1]) {
      x = cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) = c("year", "data", "age", "class", "fleet")
      
      y = cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) = c("year", "data", "age", "class", "fleet")
      
      tot = rbind(x,y)
    }
    
    if(iFleet != seq_along(.an(ageFleets))[1]){
      x = cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) = c("year", "data", "age", "class", "fleet")
      
      y = cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) = c("year", "data", "age", "class", "fleet")
      
      res = rbind(x,y)
      tot = rbind(tot,res)
    }
  }
  res = tot
  res$cohort = (res$year - res$age) %% length(ages) + 1
  
  ikey          = simpleKey(text = c("Observed", "Predicted"),
                             points = TRUE, lines = FALSE, rect = TRUE, columns = 2, cex = 1.5)
  ikey$rectangles$alpha = c(1, 0)
  ikey$rectangles$col   = "white"
  ikey$rectangles$lty   = c(1, 0)
  ikey$points$pch       = c(-1, 19)
  ikey$points$col       = c("white", "black")
  ikey$points$cex       = c(0, 1.1)
  
  cols  = rainbow(length(ages))
  ageFitsCatch = list()
  for(iFleet in c(jjm.out$Fshry_names)[seq_along(.an(ageFleets))]){
    tmpres  = subset(res, fleet == iFleet)
    tmpres$data[tmpres$data <= 1e-2 & tmpres$age == 1] = 0
    
    pic = xyplot(data ~ age | factor(year), data = tmpres,
                  groups = class,
                  main = paste("Age fits", iFleet),
                  key = ikey,
                  as.table = TRUE,
                  panel = function(x, y){
                    idx     = mapply(seq, from = seq(1, length(y), length(ages)),
                                      to = seq(1, length(y), length(ages)) + (length(ages) - 1))
                    first   = c(idx[,seq(from = 1, to = dim(idx)[2], by = 3)])
                    second  = c(idx[,seq(from = 2, to = dim(idx)[2], by = 3)])
                    #cols = tmpres$cohort[which(is.na(pmatch(tmpres$data,y))==F & is.na(pmatch(tmpres$age,x))==F)]
                    yr      = names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  = tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x[first], y[first], horizontal = FALSE, origin = 0, box.width = 1, col = cols[colidx])
                    panel.points(x[second], y[second], pch = 19, col = 1, cex = 0.5)
                  }, ...)
    
    ageFitsCatch[[iFleet]] = pic
  }
  
  return(ageFitsCatch)
}

.fit_lengthFitsCatchFUN = function(lgtFleets, jjm.out, lengths, ...)
{
  pobs_len_fsh = grep(pattern = "pobs_len_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  phat_len_fsh = grep(pattern = "phat_len_fsh_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iFleet in seq_along(.an(lgtFleets))){
#     obs = .createDataFrame(jjm.out[[paste("pobs_len_fsh_", iFleet, sep = "")]][,-1],
#                             jjm.out[[paste("pobs_len_fsh_", iFleet, sep = "")]][,1], lengths)
#     mod = .createDataFrame(jjm.out[[paste("phat_len_fsh_", iFleet, sep = "")]][,-1],
#                             jjm.out[[paste("phat_len_fsh_", iFleet, sep = "")]][,1], lengths)
    
    obs = .createDataFrame(jjm.out[[pobs_len_fsh[iFleet]]][,-1],
                           jjm.out[[pobs_len_fsh[iFleet]]][,1], lengths)
    mod = .createDataFrame(jjm.out[[phat_len_fsh[iFleet]]][,-1],
                           jjm.out[[phat_len_fsh[iFleet]]][,1], lengths)
    
    if(iFleet == seq_along(.an(lgtFleets))[1]) {
      x = cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) = c("year", "data", "length", "class", "fleet")
      
      y = cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) = c("year", "data", "length", "class", "fleet")
      
      tot = rbind(x, y)
    }
    
    if(iFleet != seq_along(.an(lgtFleets))[1]){
      x = cbind(obs, rep("obs", nrow(obs)), jjm.out$Fshry_names[iFleet])
      colnames(x) = c("year", "data", "length", "class", "fleet")
      
      y = cbind(mod, rep("model", nrow(mod)), jjm.out$Fshry_names[iFleet])
      colnames(y) = c("year", "data", "length", "class", "fleet")
      
      res = rbind(x, y)
      tot = rbind(tot, res)
    }
  }
  res = tot
  res$cohort = res$length
  
  ikey                  = simpleKey(text = c("Observed", "Predicted"),
                                     points = TRUE, lines = FALSE, rect = TRUE, columns = 2, cex = 1.5)
  ikey$rectangles$alpha = c(1, 0)
  ikey$rectangles$col   = "white"
  ikey$rectangles$lty   = c(1, 0)
  ikey$points$pch       = c(-1, 19)
  ikey$points$col       = c("white", "black")
  ikey$points$cex       = c(0, 1.1)
  
  cols  = rainbow(length(lengths))
  lengthFitsCatch = list()
  for(iFleet in c(jjm.out$Fshry_names)[seq_along(.an(lgtFleets))]){
    tmpres  = subset(res, fleet == iFleet)
    pic = xyplot(data ~ length | factor(year), data = tmpres,
                  groups = class,
                  main = paste("Length fits", iFleet),
                  key = ikey,
                  as.table = TRUE,
                  panel = function(x,y){
                    idx     = mapply(seq, from = seq(1, length(y), length(lengths)),
                                      to = (seq(from = 1, to = length(y), by = length(lengths)) + length(lengths) - 1))
                    first   = c(idx[,seq(1, dim(idx)[2], 3)])
                    second  = c(idx[,seq(2, dim(idx)[2], 3)])
                    #cols = tmpres$cohort[which(is.na(pmatch(tmpres$data,y))==F & is.na(pmatch(tmpres$age,x))==F)]
                    yr      = names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                    colidx  = tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                    panel.barchart(x[first], y[first], horizontal = FALSE, origin = 0, box.width = 1, col = cols[colidx])
                    panel.points(x[second], y[second], col = 1, pch = 19, cex = 0.25)
                  }, ...)
    
    lengthFitsCatch[[iFleet]] = pic
  }
  
  return(lengthFitsCatch)
}

.fit_predictedObservedCatchesByFleetFUN = function(Nfleets, cols, jjm.out, ...)
{
  Obs_catch  = grep(pattern = "Obs_catch_[0-9]*", x = names(jjm.out), value = TRUE)
  Pred_catch = grep(pattern = "Pred_catch_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iFleet in 1:Nfleets){
    res = data.frame(year = jjm.out$Yr, obs = jjm.out[[Obs_catch[iFleet]]],
                      model = jjm.out[[Pred_catch[iFleet]]],
                      fleet = jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot = rbind(cbind(res$year, res$obs, .ac(res$fleet), rep("obs", nrow(res))),
                                 cbind(res$year, res$model, .ac(res$fleet), rep("model", nrow(res))))
    if(iFleet != 1) tot = rbind(tot, rbind(cbind(res$year, res$obs, .ac(res$fleet), rep("obs",nrow(res))),
                                            cbind(res$year, res$model, .ac(res$fleet), rep("model", nrow(res)))))
  }
  colnames(tot) = c("year", "data", "fleet", "classing")
  res = data.frame(tot, stringsAsFactors = FALSE)
  res$year = .an(.ac(res$year))
  res$data = .an(.ac(res$data))
  
  ikey                  = simpleKey(text = c("Observed","Predicted"), cex = 1.5,
                                    points = FALSE, lines = TRUE, rect = TRUE, columns = 2)
  ikey$rectangles$alpha = c(1,0)
  ikey$rectangles$col   = "grey"
  ikey$rectangles$lty   = c(1,0)
  ikey$lines$lty        = c(0,1)
  ikey$lines$col        = 1
  ikey$lines$lwd        = 3
  
  pic = xyplot(data ~ year | as.factor(fleet), data = res,
               groups = as.factor(classing),
               sclaes = list(alternating = 1, tck = c(1,0)),
                key = ikey,
                panel = function(x, y){
                  first = 1:length(jjm.out$Yr)
                  second = (length(jjm.out$Yr) + 1):(length(jjm.out$Yr)*2)
                  panel.grid(h = -1, v = -1)
                  panel.barchart(x[first], y[second], horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                  panel.xyplot(x[first], y[second], type = "l", lwd = 5, col = 1, lty = 1)
                }, ...)
  
  return(pic)
}

.fit_predictedObservedIndicesFUN = function(Nsurveys, jjm.out, ...)
{
  Obs_Survey = grep(pattern = "Obs_Survey_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iSurvey in 1:Nsurveys) {
    if(any(jjm.out$Yr %in% jjm.out[[Obs_Survey[iSurvey]]][,1])){
      addToDF = jjm.out$Yr[which(!jjm.out$Yr %in% jjm.out[[Obs_Survey[iSurvey]]][,1])]
      addToDF = as.data.frame(rbind(cbind(addToDF, NA, "model"), 
                                     cbind(addToDF, NA, "obs"),
                                     cbind(addToDF, NA, "sd"),
                                     cbind(addToDF, NA, "stdres"),
                                     cbind(addToDF, NA, "lstdres")))
      colnames(addToDF) = c("year", "data", "class")
      addToDF$year = .an(.ac(addToDF$year))
      addToDF$data = .an(.ac(addToDF$data))
      addToDF$class = .ac(addToDF$class)
    }
    
    res = .createDataFrame(jjm.out[[Obs_Survey[iSurvey]]][,-1],
                           jjm.out[[Obs_Survey[iSurvey]]][,1],
                           c("obs", "model", "sd", "stdres", "lstdres"))
    res$class = .ac(res$class)
    res$data  = res$data/max(subset(res, class %in% c("model", "obs", "sd"))$data, na.rm = TRUE)
    resSort   = rbind(res, addToDF)
    resSort   = orderBy(~year + class, data = resSort)
    
    if(iSurvey == 1)
      tot   = cbind(resSort, rep(jjm.out$Index_names[iSurvey, 1], nrow(resSort)))
    
    if(iSurvey != 1){
      res2  = cbind(resSort, rep(jjm.out$Index_names[iSurvey, 1], nrow(resSort)))
      tot   = rbind(tot, res2)
    }
  }
  
  colnames(tot)               = c("year", "data", "classing", "surveys")
  tot                         = tot[duplicated(paste(tot$year, tot$classing, tot$surveys)) == FALSE,]
  tot$data[which(tot$data<0)] = NA
  res                         = tot
  
  ikey            = simpleKey(text = c("Observed", "Predicted"), points = TRUE, lines = TRUE, columns = 2, cex = 1.5)
  
  ikey$lines$lty  = c(0, 1)
  ikey$lines$lwd  = c(0, 2)
  ikey$lines$col  = c(0, 1)
  ikey$points$pch = c(19, -1)
  ikey$points$col = c("grey", "white")
  ikey$points$cex = 0.9
  
  pic = xyplot(data ~ year|as.factor(surveys), data = subset(res, classing %in% c("obs", "model", "sd")),
                groups = classing,
                key = ikey,
                panel = function(...){
                  tmp     = list(...)
                  first   = which(tmp$groups[1:length(tmp$x)] == "model")
                  second  = which(tmp$groups[1:length(tmp$x)] == "obs")
                  third   = which(tmp$groups[1:length(tmp$x)] == "sd")
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(tmp$x[first], tmp$y[first], type = "l", col = "black", lwd = 3)
                  panel.points(tmp$x[second], tmp$y[second], col = "grey", pch = 19, cex = 0.6)
                  panel.segments(tmp$x[third], c(tmp$y[second] + 1.96*tmp$y[third]),
                                 tmp$x[third], c(tmp$y[second] - 1.96*tmp$y[third]))
                  panel.lines(tmp$x[first], tmp$y[first], col = "black", lwd = 3)
                }, ...)
  
  return(pic)
}

.fit_ageFitsSurveyFUN = function(ageSurveys, jjm.out, ages, ageFleets, ...)
{
  pobs_ind = grep(pattern = "pobs_ind_[0-9]*", x = names(jjm.out), value=TRUE)
  phat_ind = grep(pattern = "phat_ind_[0-9]*", x = names(jjm.out), value=TRUE)
  
  for(iSurvey in seq_along(.an(ageSurveys))) {
    obs = .createDataFrame(jjm.out[[pobs_ind[iSurvey]]][,-1],
                           jjm.out[[pobs_ind[iSurvey]]][,1], ages)
    mod = .createDataFrame(jjm.out[[phat_ind[iSurvey]]][,-1], 
                           jjm.out[[phat_ind[iSurvey]]][,1], ages)
    
    if(iSurvey == .an(ageSurveys)[1]){
      x = cbind(obs, rep("obs", nrow(obs)), jjm.out$Index_names[iSurvey])
      colnames(x) = c("year", "data", "age", "class", "survey")
      
      y = cbind(mod, rep("model", nrow(mod)), jjm.out$Index_names[iSurvey])
      colnames(y) = c("year", "data", "age", "class", "survey")
      
      tot = rbind(x, y)
    }
    
    if(iSurvey != .an(ageSurveys)[1]){
      x = cbind(obs, rep("obs", nrow(obs)), jjm.out$Index_names[iSurvey])
      colnames(x) = c("year", "data", "age", "class", "survey")
      
      y = cbind(mod, rep("model", nrow(mod)), jjm.out$Index_names[iSurvey])
      colnames(y) = c("year", "data", "age", "class", "survey")
      
      res = rbind(x, y)
      tot = rbind(tot, res)
    }
  }
  res = tot
  res$cohort = (res$year - res$age) %% length(ages) + 1
  
  ikey                  = simpleKey(text = c("Observed","Predicted"),
                                     points = TRUE, lines = FALSE, rect = TRUE, columns = 2, cex = 1.5)
  ikey$rectangles$alpha = c(1, 0)
  ikey$rectangles$col   = "white"
  ikey$rectangles$lty   = c(1, 0)
  ikey$points$pch       = c(-1, 19)
  ikey$points$col       = c("white", "black")
  ikey$points$cex       = c(0, 1.1)
  
  cols  = rainbow(length(ages))
  ageFitsSurvey = list()
  for(iSurvey in c(jjm.out$Index_names)[seq_along(.an(ageFleets))]){
    tmpres = subset(res, survey == iSurvey)
    tmpres$data[tmpres$data <= 1e-2 & tmpres$age == 1] = 0
    
    if(nrow(tmpres) > 0)
      pic = xyplot(data ~ age | factor(year), data = tmpres,
                    groups = class,
                    main = paste("Age fits", iSurvey),
                    key = ikey,
                    as.table = TRUE,
                    panel = function(x, y){
                      idx     = mapply(seq, from = seq(1, length(y), length(ages)),
                                        to = seq(from = 1, to = length(y), by = length(ages)) + (length(ages) - 1))
                      first   = c(idx[,seq(from = 1, to = dim(idx)[2], by = 3)])
                      second  = c(idx[,seq(from = 2, to = dim(idx)[2], by = 3)])
                      yr      = names(which.max(table(tmpres$year[which(tmpres$data %in% y)])))
                      colidx  = tmpres$cohort[which(tmpres$data %in% y & tmpres$year == .an(yr))]
                      panel.barchart(x[first], y[first], horizontal = FALSE, origin = 0, box.width = 1, col = cols[colidx])
                      panel.points(x[second], y[second], pch = 19, col = 1, cex = 0.5)
                    }, ...) else NULL
    
    ageFitsSurvey[[iSurvey]] = pic
  }
  
  return(ageFitsSurvey)
}

.fit_standardizedSurveyResidualsFUN = function(Nsurveys, jjm.out, cols, ...)
{
  Obs_Survey = grep(pattern = "Obs_Survey_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iSurvey in 1:Nsurveys){
    if(any(jjm.out$Yr %in% jjm.out[[Obs_Survey[iSurvey]]][,1] == FALSE)){
      addToDF = jjm.out$Yr[which(!jjm.out$Yr %in% jjm.out[[Obs_Survey[iSurvey]]][,1])]
      addToDF = data.frame(year = addToDF, obs = NA, model = NA, sd = NA, stdres = NA, lstdres = NA)
      colnames(addToDF) = c("year", "obs", "model", "sd", "stdres", "lstdres")
      addToDF$year = .an(.ac(addToDF$year))
    }
    
    res = .createDataFrame(jjm.out[[Obs_Survey[iSurvey]]][,-1],
                           jjm.out[[Obs_Survey[iSurvey]]][,1],
                           c("obs", "model", "sd", "stdres", "lstdres"))
    res = cbind(subset(res, class == "obs")[,1:2], 
                 subset(res, class == "model")$data,
                 subset(res, class == "sd")$data,
                 subset(res, class == "stdres")$data,
                 subset(res, class == "lstdres")$data)
    colnames(res) = c("year", "obs", "model", "sd", "stdres", "lstdres")
    res = rbind(res, addToDF)
    res = orderBy(~year, data = res)
    
    if(iSurvey == 1) 
      tot = cbind(res, rep(jjm.out$Index_names[iSurvey, 1], nrow(res)))
    
    if(iSurvey != 1){
      res2  = cbind(res, rep(jjm.out$Index_names[iSurvey, 1], nrow(res)))
      tot   = rbind(tot, res2)
    }
  }
  
  colnames(tot)           = c("year", "obs", "model", "sd", "stdres", "lstdres", "survey")
  tot                     = tot[duplicated(paste(tot$year, tot$survey) == FALSE),]
  tot$obs[tot$obs < 0]    = NA
  tot$obs[tot$model < 0]  = NA
  tot$obs[tot$sd < 0]     = NA
  res                     = tot
  scalar                  = 3/max(abs(res$lstdres), na.rm = TRUE)
  resRange                = range(res$lstdres, na.rm = TRUE)
  ikey                    = simpleKey(text = .ac(round(seq(resRange[1], resRange[2], length.out = 6), 2)),
                                       points = TRUE, lines = FALSE, columns = 2)
  ikey$points$cex         = abs(round(seq(resRange[1], resRange[2], length.out = 6), 2))*scalar
  ikey$points$col         = 1
  ikey$points$pch         = ifelse(test = round(seq(resRange[1], resRange[2], length.out = 6), 2) > 0,
                                    yes = 19, no = 1)
  
  
  pic = xyplot(lstdres*scalar ~ year | as.factor(survey), data = res,
                prepanel = function(...) {list(ylim = c(1, 1))},
                layout = c(1, Nsurveys),
                type = "p", col = cols, 
                key = ikey,
                panel = function(x, y){
                  panel.grid(v = -1, h = 1)
                  panel.points(x, 1, cex = abs(y), col = ifelse(y > 0, "black", "white"), pch = 19)
                  panel.points(x, 1, cex = abs(y), col = 1, pch = 1)
                }, ...)
  
  return(pic)
}

.fit_sdPerInputSeriesFUN = function(jjm.out, ...)
{
  for(iSDnr in names(jjm.out)[grep("sdnr", names(jjm.out))]){
    dat = jjm.out[[iSDnr]]
    if(length(grep("age", iSDnr)) > 0 & length(grep("fsh", iSDnr)) > 0)
      iName = paste("SD_age_", jjm.out$Fshry_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(length(grep("length", iSDnr)) > 0 & length(grep("fsh", iSDnr)) > 0)
      iName = paste("SD_length_", jjm.out$Fshry_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(length(grep("age", iSDnr)) > 0 & length(grep("ind", iSDnr)) > 0)
      iName = paste("SD_age_", jjm.out$Index_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(length(grep("length",iSDnr)) > 0 & length(grep("ind", iSDnr)) > 0) 
      iName = paste("SD_length_", jjm.out$Index_names[.an(substr(iSDnr, nchar(iSDnr), nchar(iSDnr)))], sep = "")
    
    if(iSDnr == names(jjm.out)[grep("sdnr", names(jjm.out))][1]){
      totdat = cbind(dat, iName)
    }else {
      totdat = rbind(totdat, cbind(dat, iName))
    }
  }
  
  totdat            = as.data.frame(totdat)
  colnames(totdat)  = c("year", "data", "class")
  totdat$year       = .an(.ac(totdat$year))
  totdat$data       = .an(.ac(totdat$data))
  res = totdat
  
  pic = xyplot(data ~ year | class, data = res, type = "h",
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.abline(h = 1, col = "blue", lty = 3)
                  panel.xyplot(..., col = 1)
                }, ...)
  
  return(pic)
}

.fit_selectivityFisheryByPentadFUN = function(Nfleets, jjm.out, ages, ...)
{
  sel_fsh = grep(pattern = "sel_fsh_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iFleet in 1:Nfleets){
    res = .createDataFrame(jjm.out[[sel_fsh[iFleet]]][,-c(1,2)], jjm.out$Yr, ages)
    res = cbind(res, jjm.out$Fshry_names[iFleet])
    
    if(iFleet == 1) tot = res
    if(iFleet != 1) tot = rbind(tot, res)
  }
  colnames(tot) = c("year", "data", "age", "fleet"); res = tot
  
  res$cohort = res$year - res$age
  pic = xyplot(data ~ age|sprintf("%i's", floor((year + 2)/5)*5) * fleet, data = res,
                groups = year, type = "l", as.table = TRUE, ...)
  
  return(pic)
}

.fit_selectivitySurveyByPentadFUN = function(Nsurveys, jjm.out, ages, ...)
{
  sel_ind = grep(pattern = "sel_ind_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iSurvey in 1:Nsurveys){
    res = .createDataFrame(jjm.out[[sel_ind[iSurvey]]][,-c(1,2)], jjm.out$Yr, ages)
    res = cbind(res, jjm.out$Index_names[iSurvey])
    
    if(iSurvey == 1) tot = res
    if(iSurvey != 1) tot = rbind(tot, res)
  }
  colnames(tot) = c("year", "data", "age", "survey"); res = tot
  
  res$cohort = res$year - res$age
  pic = xyplot(data ~ age|sprintf("%i's",floor((year+2)/5)*5) * survey, data = res,
                groups = year, type = "l", as.table = TRUE, ...)
  
  return(pic)
}

.fit_fAtAGeFUN = function(jjm.out, ages, cols, ...)
{
  res = jjm.out$TotF
  dimnames(res) = list(Years = jjm.out$Yr, Age = ages)
  
  pic = levelplot(t(res), col.regions = cols, cuts = 10, ...)
  
  return(pic)
}

.fit_fProportionAtAGeFUN = function(jjm.out, ages, cols, ...)
{
  res = .createDataFrame(t(apply(sweep(jjm.out$TotF, 1, rowSums(jjm.out$TotF), "/"), 1, cumsum)),
                          jjm.out$N[,1], class = ages)
  
  pic = xyplot(data ~ year, data = res, groups = class,
                type = "l",
                auto.key = list(space = "right", points = FALSE, lines = FALSE, col = cols, cex = 1.2),
                panel = function(...){
                  lst = list(...)
                  idx = mapply(seq, from = seq(1, length(lst$y), length(lst$y)/length(ages)),
                                to = seq(1, length(lst$y),
                                         length(lst$y)/length(ages)) + (length(lst$y)/length(ages) - 1))
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., col = "white")
                  for(iAge in rev(ages)){
                    panel.polygon(x = c(lst$x[idx[, iAge]], rev(lst$x[idx[,iAge]])),
                                  y = c(rep(0, length(lst$y[idx[,iAge]])), rev(lst$y[idx[,iAge]])),
                                  col = cols[iAge], border = 0)
                  }                  
                }, ...)
  
  return(pic)
}

.fit_nAtAGeFUN = function(jjm.out, cols, ...)
{
  res = .createDataFrame(jjm.out$N[,-1], jjm.out$N[,1], rep("Ns", prod(dim(jjm.out$N[,-1]))))
  pic = levelplot(t(jjm.out$N[,-1]), col.regions = cols, cuts = 10, ...)
  
  return(pic)
}

.fit_nProportionAtAGeFUN = function(jjm.out, cols, ages, ...)
{
  res = .createDataFrame(t(apply(sweep(jjm.out$N[,-1], 1, rowSums(jjm.out$N[,-1]), "/"), 1, cumsum)),
                          jjm.out$N[,1], class = ages)
  
  pic = xyplot(data ~ year, data = res, groups = class,
                type = "l", 
                auto.key = list(space = "right", points = FALSE, lines = FALSE, col = cols, cex = 1.2),
                panel = function(...){
                  lst = list(...)
                  idx = mapply(seq, from = seq(1, length(lst$y), length(lst$y)/length(ages)),
                                to = seq(1, length(lst$y), length(lst$y)/length(ages)) + (length(lst$y)/length(ages) - 1))
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(..., col = "white")
                  for(iAge in rev(ages)){
                    panel.polygon(x = c(lst$x[idx[,iAge]], rev(lst$x[idx[,iAge]])),
                                  y = c(rep(0, length(lst$y[idx[,iAge]])), rev(lst$y[idx[,iAge]])),
                                  col = cols[iAge], border = 0)
                  }
                }, ...)
  
  return(pic)
}

.fit_fisheryMeanAgeFUN = function(jjm.out, ageFleets, ...)
{
  
  EffN_Fsh = grep(pattern = "EffN_Fsh_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iFleet in seq_along(.an(ageFleets))){
    res = data.frame(jjm.out[[EffN_Fsh[iFleet]]][,c(1, 4, 5, 7, 8)])
    colnames(res) = c("Year", "Obs", "Model", "Obs5", "Obs95")
    
    for(i in 2:5){
      tot = data.frame(cbind(res[,1], res[,i]))
      tot$class = names(res)[i]
      tot$Fleet = jjm.out$Fshry_names[iFleet]
      if(iFleet == seq_along(.an(ageFleets))[1] & i == 2) total = tot
      if(iFleet != seq_along(.an(ageFleets))[1] | i != 2) total = rbind(total, tot)
    }
  }
  colnames(total) = c("year", "data", "class", "fleet")
  
  ikey            = simpleKey(text = c("Observed", "Modelled"),
                               points = TRUE, lines = TRUE, columns = 2)
  ikey$lines$col  = c("white","black")
  ikey$lines$lwd  = c(0, 2)
  ikey$lines$lty  = c(0, 1)
  ikey$lines$pch  = c(0, 0)
  ikey$points$pch = c(16, 0)
  ikey$points$col = c("grey", "white")
  
  pic = xyplot(data ~ year | fleet, data = total,
                type = "l", key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx = mapply(seq(length(x)/4, length(x), length.out = 4) - length(x)/4 + 1,
                                seq(length(x)/4, length(x), length.out = 4), FUN = seq)
                  obs   = idx[,1]
                  mod   = idx[,2]
                  obs5  = idx[,3]
                  obs95 = idx[,4]
                  
                  panel.xyplot(x[obs], y[obs], type = "p", col = "grey", pch = 19, cex = 0.6)
                  panel.segments(x[obs], y[obs5], x[obs], y[obs95])
                  panel.xyplot(x[obs], y[mod], type = "l", lwd = 2, col = "black")                    
                }, ...)
  
  return(pic)
}

.fit_fisheryMeanLengthFUN = function(lgtFleets, jjm.out, ...)
{
  EffN_Length_Fsh = grep(pattern = "EffN_Length_Fsh_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iFleet in seq_along(.an(lgtFleets))){
    res = data.frame(jjm.out[[EffN_Length_Fsh]][,c(1, 4, 5, 7, 8)])
    colnames(res) = c("Year", "Obs", "Model", "Obs5", "Obs95")
    
    for(i in 2:5){
      tot = data.frame(cbind(res[,1], res[,i]))
      tot$class = names(res)[i]
      tot$Fleet = jjm.out$Fshry_names[iFleet]
      
      if(iFleet == seq_along(.an(lgtFleets))[1] & i == 2) total = tot
      if(iFleet != seq_along(.an(lgtFleets))[1] | i != 2) total = rbind(total, tot)
    }
  }
  colnames(total) = c("year", "data", "class", "fleet")
  
  ikey           = simpleKey(text = c("Observed", "Modelled"),
                              points = TRUE, lines = TRUE, columns = 2)
  ikey$lines$col = c("white", "black")
  ikey$lines$lwd = c(0, 2)
  ikey$lines$lty = c(0, 1)
  ikey$lines$pch = c(0, 0)
  ikey$points$pch= c(16, 0)
  ikey$points$col= c("grey", "white")
  
  pic = xyplot(data ~ year | fleet, data = total,
                type = "l", key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx = mapply(seq(length(x)/4, length(x), length.out = 4) - length(x)/4 + 1,
                                seq(length(x)/4, length(x), length.out = 4), FUN = seq)
                  obs   = idx[,1]
                  mod   = idx[,2]
                  obs5  = idx[,3]
                  obs95 = idx[,4]
                  panel.xyplot(x[obs], y[obs], type = "p", col = "grey", pch = 19, cex = 0.6)
                  panel.segments(x[obs], y[obs5], x[obs], y[obs95])
                  panel.xyplot(x[obs], y[mod], type = "l", lwd = 2, col = "black")
                }, ...)
  
  return(pic)
}

.fit_surveyMeanAgeFUN = function(Nsurveys, jjm.out, ...)
{
  EffN_Survey = grep(pattern = "EffN_Survey_[0-9]*", x = names(jjm.out), value = TRUE)
  
  for(iSurvey in 1:Nsurveys){
    res = data.frame(jjm.out[[EffN_Survey[iSurvey]]][,c(1, 4, 5, 7, 8)])
    if(nrow(res) > 1){
      colnames(res) = c("Year", "Obs", "Model", "Obs5", "Obs95")
      
      for(i in 2:5){
        tot = data.frame(cbind(res[,1], res[,i]))
        tot$class = names(res)[i]
        tot$Survey = jjm.out$Index_names[iSurvey]
        if(iSurvey == 1 & i == 2) total = tot
        if(iSurvey != 1 | i != 2) total = rbind(total, tot)
      }
    }
  }
  colnames(total) = c("year", "data", "class", "survey")
  
  ikey           = simpleKey(text = c("Observed", "Modelled"),
                              points = TRUE, lines = TRUE, columns = 2)
  ikey$lines$col = c("white", "black")
  ikey$lines$lwd = c(0, 2)
  ikey$lines$lty = c(0, 1)
  ikey$lines$pch = c(0, 0)
  ikey$points$pch= c(16, 0)
  ikey$points$col= c("grey", "white")
  
  pic = xyplot(data ~ year | survey, data = total,
                type = "l", key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx = mapply(seq(length(x)/4, length(x), length.out = 4) - length(x)/4 + 1,
                                seq(length(x)/4, length(x), length.out = 4),FUN = seq)
                  obs   = idx[,1]
                  mod   = idx[,2]
                  obs5  = idx[,3]
                  obs95 = idx[,4]
                  
                  panel.xyplot(x[obs], y[obs], type = "p", col = "grey", pch = 19, cex = 0.6)
                  panel.segments(x[obs], y[obs5], x[obs], y[obs95])
                  panel.xyplot(x[obs], y[mod], type = "l", lwd = 2, col = "black")
                }, ...)
  
  return(pic)
}


.fit_summarySheetFUN = function(jjm.out, ...)
{
  TotCatch = 0
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    TotCatch    = jjm.out[[iFlt]] + TotCatch
  summaryData = rbind(cbind(jjm.out$Yr, jjm.out$R[,-1], "Recruitment"),
                       cbind(jjm.out$Yr, TotCatch, TotCatch, TotCatch, TotCatch, "Landings"),
                       cbind(jjm.out$SSB[which(jjm.out$SSB[,1] %in% jjm.out$Yr), 1],
                             jjm.out$SSB[which(jjm.out$SSB[,1] %in% jjm.out$Yr), -1], "SSB"),
                       cbind(jjm.out$Yr, cbind(rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1]),
                                               rowMeans(jjm.out$TotF[,-1]), rowMeans(jjm.out$TotF[,-1])),
                             "Fishing mortality"))
  
  summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"),
                       cbind(summaryData[,c(1, 4, 6)], "lower"),
                       cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) = c("year", "data", "class", "estim")
  summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$year = as.integer(summaryData$year)
  summaryData$data = as.numeric(summaryData$data)
  
  summaryData$class = factor(summaryData$class, levels = unique(summaryData$class))
  
  alpha.f = 0.5
  
  pic = xyplot(data ~ year | class, data = summaryData, groups = class,
                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
                layout = c(2, 2),
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  point = 1:length(jjm.out$Yr)
                  lower = (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                  upper = (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                  
                  # LANDINGS
                  if(panel.number() == 2){
                    panel.barchart(x[point], y[point], horizontal = FALSE, origin = 0, box.width = 1, col = "grey90")
#                     panel.lines(x[point], jjm.out$msy_mt[,8], lwd = 4, 
#                                 col = adjustcolor("blue", alpha.f = alpha.f))
                  }
                  
                  # Recruitment
                  if(panel.number() == 1){
                    panel.barchart(x[point], y[point], horizontal = FALSE, origin = 0, box.width = 1, col = "grey90")
                    panel.segments(x[lower], y[lower], x[lower], y[upper])
                  }
                  
                  # F
                  if(panel.number() == 4){
                    panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)
                    panel.lines(x[point], jjm.out$msy_mt[,5], lwd = 4, 
                                col = adjustcolor("blue", alpha.f = alpha.f))
                  }
                  
                  # SSB
                  if(panel.number() == 3){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey90", border = NA)
                    panel.xyplot(x[point], y[point], type = "l", lwd = 3, lty = 1, col = 1)
                    panel.lines(x[point], jjm.out$msy_mt[,10], lwd = 4, 
                                col = adjustcolor("blue", alpha.f = alpha.f))
                  }
                }, ...)
  
  return(pic)
}


.fit_summarySheet2FUN = function(jjm.out, ...)
{
  TotCatch = 0
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    TotCatch    = jjm.out[[iFlt]] + TotCatch
  
  summaryData = rbind(cbind(jjm.out$Yr, jjm.out$TotBiom[,-1], "Total biomass"),
                      cbind(jjm.out$Yr, cbind(rowMeans(jjm.out$TotF[,-1]), 
                                              rowMeans(jjm.out$TotF[,-1]), 
                                              rowMeans(jjm.out$TotF[,-1]), 
                                              rowMeans(jjm.out$TotF[,-1])), "Fishing mortality"),
                      cbind(jjm.out$Yr, jjm.out$R[,-1], "Recruitment"),
                      cbind(jjm.out$Yr, jjm.out$TotBiom_NoFish[,-1], "Unfished biomass"))
  
  
  summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"), 
                       cbind(summaryData[,c(1, 4, 6)], "lower"),
                       cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) = c("year", "data", "class", "estim")
  summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$class = factor(summaryData$class, ordered = FALSE,
                              levels = c("Unfished biomass", "Recruitment", "Fishing mortality", "Total biomass"))
  summaryData$year = as.integer(summaryData$year)
  summaryData$data = as.numeric(summaryData$data)
  
  alpha.f = 0.45
  
  pic = xyplot(data ~ year | class, data = summaryData,
                groups = class,
                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
                layout = c(1, 4),
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  point = 1:length(jjm.out$Yr)
                  lower = (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                  upper = (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                  
                  # NoFish
				          if(panel.number() == 1){
				            panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey", border = NA)
                    panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)

                  }

                  # Recruitment
                  if(panel.number() == 2){
                    panel.barchart(x[point], y[point], horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                    panel.segments(x[lower], y[lower], x[lower], y[upper])
                  }
                  
                  # Ftot
                  if(panel.number() == 3){
                    panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)
                  }
                  
                  # Total biomass
                  if(panel.number() == 4){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey",
                                  border = NA)
                    panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)
                  }
                }, ...)
  
  return(pic)
}

.fit_summarySheet3FUN = function(jjm.out, ...)
{
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    
    summaryData = rbind(cbind(jjm.out$Yr, jjm.out$TotBiom[ ,-1], "Total biomass"),
                        cbind(jjm.out$Yr, cbind(rowMeans(jjm.out$TotF[ ,-1]), 
                                                rowMeans(jjm.out$TotF[ ,-1]), 
                                                rowMeans(jjm.out$TotF[ ,-1]), 
                                                rowMeans(jjm.out$TotF[ ,-1])), "Fishing mortality"),                  
                        cbind(jjm.out$Yr, jjm.out$TotBiom_NoFish[ ,-1], "Unfished biomass"))
  
  summaryData = rbind(cbind(summaryData[,c(1:2, 6)], "point"), 
                      cbind(summaryData[,c(1, 4, 6)], "lower"),
                      cbind(summaryData[,c(1, 5, 6)], "upper"))
  
  colnames(summaryData) = c("year", "data", "class", "estim")
  summaryData = data.frame(summaryData, stringsAsFactors = FALSE)
  summaryData$class = factor(summaryData$class, ordered = FALSE,
                             levels = c("Unfished biomass", "Total biomass", "Fishing mortality"))
  summaryData$year   = as.integer(summaryData$year)
  summaryData$data   = as.numeric(summaryData$data)
  
  alpha.f = 0.45
  
  pic1 = xyplot(data ~ year | class, data = summaryData,
                groups = class,
                prepanel = function(...) {list(ylim = range(pretty(c(0, 1.1*list(...)$y))))},
                layout = c(3, 1),
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  point = 1:length(jjm.out$Yr)
                  lower = (length(jjm.out$Yr) + 1):(2*length(jjm.out$Yr))
                  upper = (2*length(jjm.out$Yr) + 1):(3*length(jjm.out$Yr))
                  
                  # Total biomass
                  if(panel.number() == 1){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey",
                                  border = NA)
                    panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)
                    #if(endvalue){
                    #  ltext(x=rev(x)[1], y=rev(y)[1], labels=round(rev(y)[1],0), pos=2, offset=1, cex=0.9,
                    #        font = 2)
                    #}
                  }
                  
                  
                  # Unfished biomass
                  if(panel.number() == 2){
                    panel.polygon(c(x[lower], rev(x[upper])), c(y[lower], rev(y[upper])), col = "grey", border = NA)
                    panel.xyplot(x[point], y[point], lwd = 2, lty = 1, type = "l", col = 1)
                    #if(endvalue){
                    #  ltext(x=rev(x)[1], y=rev(y)[1], labels=round(rev(y)[1],0), pos=2, offset=1, cex=0.9,
                    #        font = 2)
                    #}
                  }
                  
                  
                  # F
                  if(panel.number() == 3){
                    panel.barchart(x[point], y[point], horizontal = FALSE, origin = 0, box.width = 1, col = "grey")
                    panel.segments(x[lower], y[lower], x[lower], y[upper]) 
                  }
                  
                }, ...)
  
  
  if(length(grep("SR_Curve_years", names(jjm.out))) == 0 ){
    pic2 = .fit_stockRecruitmentFUN(jjm.out, 
                                    ylab = "Recruitment", xlab = "Spawning Stock Biomass",
                                    main = "Stock Recruitment")
  
  } else {
	pic2 = .fit_stockRecruitmentFUN2(jjm.out, cols, 
	                                 ylab = "Recruitment", xlab = "Spawning Stock Biomass", 
	                                 main = "Stock Recruitment")
								  }
  
  pic3 = arrangeGrob(pic1, pic2)
  return(pic3)
}



.fit_uncertaintyKeyParamsFUN = function(jjm.out, ...)
{
  res = rbind(data.frame(CV = jjm.out$SSB[,3]/jjm.out$SSB[,2], years = jjm.out$SSB[,1], class = "SSB"),
               data.frame(CV = jjm.out$TotBiom[,3]/jjm.out$TotBiom[,2], years = jjm.out$TotBiom[,1], class = "TSB"),
               data.frame(CV = jjm.out$R[,3]/jjm.out$R[,2], years = jjm.out$R[,1], class = "R"))
  
  pic = xyplot(CV ~ years, data = res, groups = class,
               scales = list(alternating = 1, tck = c(1,0)), type = "l", ...)
  
  return(pic)
}

.fit_matureInmatureFishesFUN = function(jjm.out, ...)
{
  N   = jjm.out$N[,-1]
  Mat = jjm.out$mature_a
  Wt  = jjm.out$wt_a_pop
  
  MatureBiom = rowSums(sweep(N, 2, Mat*Wt, "*"))
  ImmatureBiom = rowSums(sweep(N, 2, (1 - Mat)*Wt, "*"))
  
  res = data.frame(rbind(cbind(jjm.out$Yr, MatureBiom, "Mature"),
                          cbind(jjm.out$Yr, ImmatureBiom, "Immature")), stringsAsFactors = FALSE)
  colnames(res) = c("year", "data", "classing")
  res$data = as.numeric(res$data)
  res$year = as.integer(res$year)
  res$classing = as.factor(res$classing)
  
  ikey           = simpleKey(text = c("Mature", "Immature"),
                              points = FALSE, lines = TRUE, columns = 2, cex = 1.5)
  ikey$lines$col = 1
  ikey$lines$lwd = 3
  ikey$lines$lty = c(3, 1)
  ikey$points$col = "white"
  
  pic = xyplot(data ~ year, data = res, groups = classing,
               scales = list(alternating = 1, tck = c(1,0)),
                type = "l", key = ikey,
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                }, ...)
  
  return(pic)
}


.fit_stockRecruitmentFUN <- function(jjm.out, ...)
{
  res1 <- data.frame(jjm.out[["Stock_Rec"]][,c(2, 4)])
  res1 <- res1[1:(nrow(res1) - 1),]
  res1$class <- "observed"
  
  res2 <- data.frame(jjm.out[["stock_Rec_Curve"]])
  res2 <- res2[1:(nrow(res2) - 1),]
  res2$class <- "modelled"
  
  res  <- rbind(res1, res2)
  colnames(res) <- c("SSB", "Rec", "class")
  
  #ikey           <- simpleKey(text = c("Observed", "Modelled"),
  #                            points = TRUE, lines = TRUE, columns = 2)
  #ikey$lines$col <- c(1, rev(cols)[1])
  #ikey$lines$lwd <- c(2, 3)
  #ikey$lines$lty <- c(3, 2)
  
  #ikey$points$pch <- c(19, 0)
  #ikey$points$col <- c("darkgrey", "white")
  
  pic <- xyplot(Rec ~ SSB, data = res, groups = class,
                scales = list(alternating = 1, tck = c(1,0)),
                #key = ikey,
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idxobs <- which(res$SSB %in% x & res$class == "observed")
                  idxmod <- which(res$SSB %in% x & res$class == "modelled")
                  #panel.xyplot(x[idxobs], y[idxobs], type = "l", lwd = 3, col = 1, lty = 3)
                  panel.points(x[idxobs], y[idxobs], type = "p", cex = 1.5, pch = 19, col = "darkgrey")
                  panel.xyplot(x[idxmod], y[idxmod], type = "l", lwd = 4, col = "black", lty = 1)
                },
				
				auto.key = list(title = "", 
                               text = "Simulated",
                               x = 0.85, y = 1, cex = 1.25,
                               points = FALSE, border = FALSE, 
                               lines = FALSE, col = "darkgrey")

				, ...)
  
  return(pic)
}


.fit_stockRecruitmentFUN2 = function(jjm.out, cols, ...)
{
  
  cols  = rainbow(10)
  county = grep("SR_Curve_years_", names(jjm.out))
  colyear = NULL
  for(i in seq_along(county)){
    namesy = paste("SR_Curve_years_", i, sep = "")
    colyear[[i]] = jjm.out[[namesy]]
  }
  
  res1 = data.frame(jjm.out[["Stock_Rec"]][, c(2, 4)])
  res1$class = "Simulated"
  res1$year = jjm.out[["Stock_Rec"]][, 1]
  
  res1$color = numeric(nrow(res1))
  for(i in seq_along(colyear)){
    res1$color[which(res1$year %in% colyear[[i]])] = i
    res1$color[which(res1$year %in% colyear[[i]])] = i
  }
  res1 = res1[1:(nrow(res1) - 1), ]
  
  
  count = grep("stock_Rec_Curve", names(jjm.out))
  res2 = NULL
  for(i in seq_along(count)){
    namesres2 = paste("stock_Rec_Curve_", i, sep = "")
    res2[[i]] = data.frame(jjm.out[[namesres2]])
    res2[[i]] = res2[[i]][1:(nrow(res2[[i]])-1), ] 
    res2[[i]]$class = paste("Regime", i, sep ="")
    res2[[i]]$year = NA
    res2[[i]]$color = NA
  }
  res2 = do.call("rbind", res2)
  
  res  = rbind(res1, res2) 
  colnames(res) = c("SSB", "Rec", "class", "year", "col")
  
  labelLeg = NULL
  for(i in seq_along(colyear)){
    labelLeg[1] = "Simulated"
    labelLeg[i+1] = paste(colyear[[i]][1], " - ", rev(colyear[[i]])[1], sep = "")
  }
  
  labelCol = NULL
  labelCol[1] = "darkgrey"
  labelCol[seq_along(colyear) + 1] = rev(cols)[seq_along(colyear)]
  
  idxobs = list()
  pic = xyplot(Rec ~ SSB, data = res, groups = class,
               scales = list(alternating = 1, tck = c(1,0)),
               panel = function(x, y, subscripts){
                 panel.grid(h = -1, v = -1)
                 
                 for(i in c(0, seq_along(colyear))){
                   idxobs[[i+1]] = which(res$SSB %in% x & res$class == "Simulated" & res$col == i)
                 }
                 
                 panel.text(x[res$class=="Simulated"], y[res$class=="Simulated"], labels=res$year[res$class=="Simulated"][subscripts], 
                            cex = 1, pos = 3, offset = 1, srt = 0, adj = c(1,1))
                 
                 countm = grep("Regime", unique(res$class))
                 idxmod = NULL
                 
                 for(i in seq_along(idxobs)){
                   if(i == 1) {panel.points(x[idxobs[[i]]], y[idxobs[[i]]], type = "p", cex = 1.5, pch = 19, col = "darkgrey")}
                   else {panel.points(x[idxobs[[i]]], y[idxobs[[i]]], type = "p", cex = 1.5, pch = 19, col = rev(cols)[i-1])}
                 }
                 
                 for(i in seq_along(countm)){
                   namesid = paste("Regime", i, sep = "")
                   idxmod = which(res$SSB %in% x & res$class == namesid)
                   panel.xyplot(x[idxmod], y[idxmod], type = "l", lwd = 4, col = rev(cols)[i], lty = 1)                   
                 }
               },
               
               auto.key = list(title = "", 
                               text = labelLeg,
                               x = 0.85, y = 1, cex = 1.25,
                               points = FALSE, border = FALSE, 
                               lines = FALSE, col = labelCol), ...)
  
  return(pic)
}



.fit_fishedUnfishedBiomassFUN = function(jjm.out, ...)
{
  BnoFish = jjm.out$TotBiom_NoFish[,2]
  BFish   = jjm.out$TotBiom[,2]
  res     = as.data.frame(rbind(cbind(jjm.out$TotBiom[,1], BnoFish, "notfished"),
                                 cbind(jjm.out$TotBiom[,1], BFish, "fished")), stringsAsFactors = FALSE)
  colnames(res) = c("year", "data", "class")
  res$data      = .an(res$data)
  res$year      = .an(res$year)
  
  ikey           = simpleKey(text = c("Fished", "Unfished"),
                              points = FALSE, lines = TRUE, columns = 2, cex = 1.5)
  ikey$lines$col = c(1, 1)
  ikey$lines$lwd = c(2, 2)
  ikey$lines$lty = c(1, 3)
  
  pic = xyplot(data ~ year, data = res, groups = class,
               scales = list(alternating = 1, tck = c(1,0)),
                key = ikey, type = "l",
                panel = function(...){
                  panel.grid(h = -1, v = -1)
                  panel.xyplot(...)
                }, ...)
  
  return(pic)
}

.projections_ssbPredictionFUN = function(jjm.out, ...)
{
  lastYear = jjm.out$R[nrow(jjm.out$R), 1]
  Nfutscen  = length(grep("SSB_fut_", names(jjm.out)))
  scenarios = c(paste0("F", lastYear ," SQ"), 
				paste0("F", lastYear, " 0.75x"), 
				paste0("F", lastYear, " 1.25x"), 
				paste0("F", lastYear, " 0.5x"), 
				paste0("F", lastYear, " 0x"))
 #[1:Nfutscen]
  
  SSB_fut = grep(pattern = "SSB_fut_", x = names(jjm.out), value = TRUE)
  for(iScen in 1:length(scenarios)){
    #idx = nrow(get("jjm.out")[["SSB"]][,c(1, 2, 4, 5)])
    #tot = rbind(get("jjm.out")[["SSB"]][-idx,c(1, 2, 4, 5)],
    #             get("jjm.out")[[paste("SSB_fut_", iScen, sep = "")]][,c(1, 2, 4, 5)])
    #colnames(tot) = c("year", "SSB", "SSB5", "SSB95")
	idx = nrow(get("jjm.out")[["SSB"]][,c(1, 2)])
    tot = rbind(get("jjm.out")[["SSB"]][-idx,c(1, 2)],
                 get("jjm.out")[[SSB_fut[iScen]]][,c(1, 2)])
    colnames(tot) = c("year", "SSB")
    
    #for(i in 2:4){
      #if(iScen == 1 & i == 2){
	  if(iScen == 1){
        #totres = data.frame(cbind(tot[,1], tot[,2]))
        #totres$class = colnames(tot)[i]
        #totres$scenario = scenarios[iScen]
		totres = data.frame(tot)
		totres$scenario = scenarios[iScen]
      } else {
      #if(iScen != 1 | i != 2){
        #res = data.frame(cbind(tot[,1], tot[,i]))
        #res$class = colnames(tot)[i]
        #res$scenario = scenarios[iScen]
        #totres = rbind(totres, res)
		res = data.frame(tot)
        res$scenario = scenarios[iScen]
        totres  = rbind(totres, res)
      }
    #}
  }
  #colnames(totres) = c("year", "data", "class", "scenario")
  colnames(totres) = c("year", "data", "scenario")
  
  #ikey           = simpleKey(text = scenarios, points = FALSE, lines = TRUE, columns = 2)
  #ikey$lines$col = 1:length(scenarios)
  #ikey$lines$lwd = 4
  #ikey$lines$lty = 1
  
  #pic = xyplot(data ~ year, data = totres, type = "l", groups = scenario,
  #              xlim = c(2000, max(totres$year)),
#                 key = ikey,
  #              prepanel = function(...) {list(ylim = c(0, max(totres$data, na.rm = TRUE)))},
  #              panel = function(x, y){
  #                panel.grid(h = -1, v = -1)
  #                idx = mapply(seq(length(x)/Nfutscen, length(x), length.out = Nfutscen) - length(x)/Nfutscen + 1,
  #                              seq(length(x)/Nfutscen, length(x), length.out = Nfutscen), FUN = seq)
     #             idx2 = mapply(seq(length(idx[,1])/3, length(idx[,1]), length.out = 3) - length(idx[,1])/3 + 1,
     #                            seq(length(idx[,1])/3, length(idx[,1]), length.out = 3), FUN = seq)
     #             
     #             for(iScen in 2:Nfutscen){
     #               panel.xyplot(x[idx[,iScen][idx2[,1]]], y[idx[,iScen][idx2[,1]]], type = "l", col = iScen, lwd = 3)
     #               iCol  = col2rgb(iScen)
     #               iCol  = rgb(iCol[1]/255, iCol[2]/255, iCol[3]/255, 0.25)
     #               panel.polygon(c(x[idx[,iScen][idx2[,2]]], rev(x[idx[,iScen][idx2[,3]]])),
     #                             c(y[idx[,iScen][idx2[,2]]], rev(y[idx[,iScen][idx2[,3]]])), col = iCol, border = iCol)
     #               panel.lines(x[idx[,iScen][idx2[,1]]], y[idx[,iScen][idx2[,1]]], col = iScen, lwd = 3)
     #             }
     #             panel.xyplot(x[idx[,1][idx2[,1]]], y[idx[,1][idx2[,1]]], type = "l", col = 1, lwd = 4)
     #             iCol  = col2rgb(1)
    #              iCol  = rgb(iCol[1]/255, iCol[2]/255, iCol[3]/255, 0.15)
   #               panel.polygon(c(x[idx[,1][idx2[,2]]], rev(x[idx[,1][idx2[,3]]])),
  #                              c(y[idx[,1][idx2[,2]]], rev(y[idx[,1][idx2[,3]]])), col = iCol, border = iCol)
 #                 panel.lines(x[idx[,1][idx2[,1]]], y[idx[,1][idx2[,1]]], col = 1, lwd = 4)
  #              })
				
  ikey           = simpleKey(text=scenarios, points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col = 1:length(scenarios)
  ikey$lines$lwd = 4
  ikey$lines$lty = 1
  
  pic = xyplot(data ~ year, data = totres, type = "l", groups = scenario,
                key = ikey, scales = list(alternating = 1, tck = c(1,0)),
                prepanel = function(...) {list(ylim = c(0, max(totres$data, na.rm = TRUE)))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx = mapply(seq(length(x)/Nfutscen, length(x), length.out = Nfutscen) - length(x)/Nfutscen + 1,
                                seq(length(x)/Nfutscen, length(x), length.out = Nfutscen), FUN = seq)
                  #scen1 = idx[,1]; scen2 = idx[,2]; scen3 = idx[,3]; scen4 = idx[,4]; scen5 = idx[,5]
                  for(iScen in 2:Nfutscen) panel.xyplot(x[idx[,iScen]], y[idx[,iScen]], type = "l", col = iScen, lwd = 3)
                  panel.xyplot(x[idx[,1]], y[idx[,1]], type = "l", col = 1, lwd = 4)                  
                }, ...)
  
  return(pic)
}

.projections_catchPredictionFUN = function(jjm.out, ...)
{
  lastYear = jjm.out$R[nrow(jjm.out$R), 1]
  Nfutscen  = length(grep("SSB_fut_", names(jjm.out)))
  scenarios = c(paste0("F", lastYear ," SQ"), 
				paste0("F", lastYear, " 0.75x"), 
				paste0("F", lastYear, " 1.25x"), 
				paste0("F", lastYear, " 0.5x"), 
				paste0("F", lastYear, " 0x"))
  totCatch  = 0
  for(iFlt in grep("Obs_catch_", names(jjm.out)))
    totCatch = jjm.out[[iFlt]] + totCatch
  
  totCatch  = cbind(jjm.out$Yr, totCatch)
  colnames(totCatch) = c("year", "catch")
  
  Catch_fut = grep(pattern = "Catch_fut_[0-9]*", x = names(jjm.out), value=TRUE)
    
  for(iScen in 1:length(scenarios)){
#     tot = rbind(totCatch, jjm.out[[paste("Catch_fut_", iScen, sep = "")]])
    tot = rbind(totCatch, jjm.out[[Catch_fut[iScen]]])
    colnames(tot) = c("year", "catch")
    if(iScen == 1){
      totres = data.frame(tot)
      totres$scenario = scenarios[iScen]
    }else {
      res = data.frame(tot)
      res$scenario = scenarios[iScen]
      totres  = rbind(totres, res)
    }
  }
  
  colnames(totres) = c("year", "data", "scenario")
  
  ikey           = simpleKey(text=scenarios, points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col = 1:length(scenarios)
  ikey$lines$lwd = 4
  ikey$lines$lty = 1
  
  pic = xyplot(data ~ year, data = totres, type = "l", groups = scenario,
               key = ikey, scales = list(alternating = 1, tck = c(1,0)),
                prepanel = function(...) {list(ylim = c(0, max(totres$data, na.rm = TRUE)))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx = mapply(seq(length(x)/Nfutscen, length(x), length.out = Nfutscen) - length(x)/Nfutscen + 1,
                                seq(length(x)/Nfutscen, length(x), length.out = Nfutscen), FUN = seq)
                  #scen1 = idx[,1]; scen2 = idx[,2]; scen3 = idx[,3]; scen4 = idx[,4]; scen5 = idx[,5]
                  for(iScen in 2:Nfutscen) panel.xyplot(x[idx[,iScen]], y[idx[,iScen]], type = "l", col = iScen, lwd = 3)
                  panel.xyplot(x[idx[,1]], y[idx[,1]], type = "l", col = 1, lwd = 4)                  
                }, ...)
  
  return(pic)
}

.ypr_yieldSsbPerRecruitFUN = function(jjm.out, ...)
{
  jjm.ypr = jjm.out$YPR

  if(is.null(jjm.ypr)) return(invisible(NULL))
  res = rbind(data.frame(cbind(jjm.ypr$F, jjm.ypr$SSB), class = "SSB"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$Yld), class = "Yield"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$Recruit), class = "Recruit"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$SPR), class = "SPR"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$B), class = "Biomass"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$Yld/jjm.ypr$Recruit), class = "YPR"),
               data.frame(cbind(jjm.ypr$F, jjm.ypr$SSB/jjm.ypr$Recruit), class = "SpawPR"))
  colnames(res) = c("F", "data", "class")
  res           = subset(res, class %in% c("YPR", "SpawPR"))
  
  pic = xyplot(data ~ F | class, data = res, type = "l",
               prepanel = function(...) {list(ylim = range(pretty(c(0, list(...)$y))))},
                layout = c(1, 2),
                panel = function(...){
                  lst = list(...)
                  panel.grid(h = -1, v = -1)
                  if(panel.number() == 1) panel.xyplot(lst$x, lst$y, type = "l", lwd = 3, lty = 1, col = 1)
                  if(panel.number() == 2) panel.xyplot(lst$x, lst$y, type = "l", lwd = 3, lty = 1, col = 1)
                }, ...)
  
  return(pic)
}

.kobeFUN = function(jjm.out) {
  kob = jjm.out$msy_mt
  col = "black"
  
  F_Fmsy = kob[, 4]
  B_Bmsy = kob[, 13]
  years  = kob[, 1]
  
  n = length(B_Bmsy)
  
  xlim = range(pretty(c(0, B_Bmsy)))
  ylim = range(pretty(c(0, F_Fmsy)))

  x = seq(0, max(xlim), by = 0.1)
  y = seq(0, max(ylim), by = 0.1)

  y = y[1:length(x)]

  mypanel<-function(x,y,...){
  panel.xyplot(x, y, ...)
  panel.text(B_Bmsy[c(1,n)] + 0.05, F_Fmsy[c(1,n)] + 0.2, labels = range(years), cex = 0.8)
  }

  b = xyplot(F_Fmsy[c(1,n)] ~ B_Bmsy[c(1,n)], type = "p", col = col, pch = c(15, 17), panel = mypanel, cex = 0.8)
  c = xyplot(F_Fmsy ~ B_Bmsy, type = "b", col = col, pch = 19, cex = 0.5)

  pic = xyplot(y ~ x, type="n", xlim = xlim, ylim = ylim, xlab = toExpress("B/B[msy]"), ylab = toExpress("F/F[msy]"),
               main="Kobe plot", scales = list(alternating = 1, tck = c(1,0))) + 
    layer_(panel.xblocks(x, x < 1, col = rgb(1, 0, 0, alpha = 0.5), block.y = 1)) +
		layer_(panel.xblocks(x, x < 1, col = rgb(1, 1, 0, alpha = 0.5), block.y = 1, vjust = 1)) +
		layer_(panel.xblocks(x, x >= 1, col = rgb(1, 1, 0, alpha = 0.5), block.y = 1)) +
		layer_(panel.xblocks(x, x >= 1, col = rgb(0, 1, 0, alpha = 0.5), block.y = 1, vjust = 1)) +
		as.layer(b)+
		as.layer(c)

  return(pic)
 
}


.kobeFUN2 = function(obj, cols, endvalue, ...) {
  
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  
  dataxy = NULL
  
  fMx = numeric(length(obj))
  bMx = numeric(length(obj))
  
  for(i in seq_along(obj)){
    
    for(j in seq_along(obj[[i]]$output)){
      
      fMx[i] = max(obj[[i]]$output[[j]]$msy_mt[,4])	
      bMx[i] = max(obj[[i]]$output[[j]]$msy_mt[,13])
      
    }
    
  }

	posFmax = which(fMx == max(fMx))
	posBmax = which(bMx == max(bMx))
  
	for(i in seq_along(obj)){
	  for(j in seq_along(obj[[i]]$output)){
	    xlim = range(pretty(c(0, obj[[posBmax]]$output[[j]]$msy_mt[,13])))
	    ylim = range(pretty(c(0, obj[[posFmax]]$output[[j]]$msy_mt[,4])))    
	  }
	}
  
  x <- seq(0, max(xlim), by = 0.1)
  y <- seq(0, max(ylim), by = 0.1)

  y = y[1:length(x)]
  
  b = list()
  c = list()
  
  for(i in seq_along(obj)){
    for(j in seq_along(obj[[i]]$output)){
      kob = obj[[i]]$output[[j]]$msy_mt
      
      F_Fmsy = kob[,4]
      B_Bmsy = kob[,13]
      years  = kob[,1]
      name = names(obj[[i]]$output[j])
      model = names(obj)
      
      n = length(B_Bmsy)
          
      data1 = as.data.frame(cbind(x, y))
      data1 = cbind(data1, model, name)
      dataxy = rbind(dataxy, data1)
           
      mypanel<-function(x,y,...){
        panel.xyplot(x, y, ...)
        panel.text(x + 0.05, y + 0.2, labels = range(years), ...)
      }
      
      if(endvalue) {b[[j]] <- xyplot(F_Fmsy[c(1,n)] ~ B_Bmsy[c(1,n)], type = "p", col = cols[j], panel = mypanel, pch = c(15, 17), cex = 1)}
      else {b[[j]] <- xyplot(F_Fmsy[c(1,n)] ~ B_Bmsy[c(1,n)], type = "p", col = cols[j], pch = c(15, 17), cex = 1)}
      c[[j]] <- xyplot(F_Fmsy ~ B_Bmsy, type = "b", col = cols[j], pch = 19, cex = 0.5)
      
    }   
    
  }
  
  for(i in seq_along(obj)){
    for(j in seq_along(obj[[i]]$output)){
  
          pic = xyplot(y ~ x, type = "n", xlim = xlim, ylim = ylim, 
                       xlab = toExpress("B/B[msy]"), ylab = toExpress("F/F[msy]"),
                       scales = list(alternating = 1, tck = c(1, 0)),
                       key = list(lines = list(col = cols[1:length(obj[[i]]$output)], lwd = 3),
                                  text = list(names(obj[[i]]$output)), ...
                   ), ...) + 
        layer_(panel.xblocks(x, x < 1, col = rgb(1, 0, 0, alpha = 0.5), block.y = 1)) +
        layer_(panel.xblocks(x, x < 1, col = rgb(1, 1, 0, alpha = 0.5), block.y = 1, vjust = 1)) +
        layer_(panel.xblocks(x, x >= 1, col = rgb(1, 1, 0, alpha = 0.5), block.y = 1)) +
        layer_(panel.xblocks(x, x >= 1, col = rgb(0, 1, 0, alpha = 0.5), block.y = 1, vjust = 1)) 
      
    }
  }
  
#   for(i in seq_along(obj)){
#     for(j in seq_along(obj[[i]]$output)){
      pic = pic + as.layer(b[[j]])
      pic = pic + as.layer(c[[j]])  
#     }
#   }
  
  return(pic)
 
}


.kobeFUN3 = function(obj, cols, endvalue, ...) {
  
  if(is.null(cols)) cols = "black"
  cols = cols[1]

  listStocks = NULL
  for(i in seq_along(obj)){
    listStocks = obj[[i]]$output 
  }
  
  pic = list()
  
  for(i in seq_along(obj)){
    
    for(j in seq_along(obj[[i]]$output)){
      
      dataT = NULL
      dataxy = NULL
      datalab = NULL
      
      kob = obj[[i]]$output[[j]]$msy_mt
      
      F_Fmsy = kob[,4]
      B_Bmsy = kob[,13]
      years  = kob[,1]
      name = names(obj[[i]]$output[j])
      model = names(obj)
      xlim = range(pretty(c(0, B_Bmsy)))
      ylim = range(pretty(c(0, F_Fmsy)))
      
      x <- c(0, 1, max(xlim))
      y <- c(0, 1, max(ylim))
      y = y[1:length(x)]
      
      n = length(years)
      
      data = as.data.frame(cbind(F_Fmsy, B_Bmsy, years))
      data = cbind(data, model, name)
      dataT = rbind(dataT, data)
      
      data1 = as.data.frame(cbind(x, y))
      data1 = cbind(data1, model, name)
      dataxy = rbind(dataxy, data1)
      
      data2 = as.data.frame(cbind(F_Fmsy[c(1,n)], B_Bmsy[c(1,n)], range(years)))
      data2 = cbind(data2, model, name)
      datalab = rbind(datalab, data2)
      
      pic[[j]] = xyplot(y ~ x | name + model, dataxy, type = "n", xlab = toExpress("B/B[msy]"), ylab = toExpress("F/F[msy]"),
                        scales = list(alternating = 1, tck = c(1, 0)), 
                        xlim = range(pretty(c(0, B_Bmsy))),
                        ylim = range(pretty(c(0, F_Fmsy))),
                   ...) +  
        layer_(panel.xblocks(x, x < 1, col = rgb(1, 0, 0, alpha = 0.5), block.y = 1)) +
        layer_(panel.xblocks(x, x < 1, col = rgb(1, 1, 0, alpha = 0.5), block.y = 1, vjust = 1)) +
        layer_(panel.xblocks(x, x >= 1, col = rgb(1, 1, 0, alpha = 0.5), block.y = 1)) +
        layer_(panel.xblocks(x, x >= 1, col = rgb(0, 1, 0, alpha = 0.5), block.y = 1, vjust = 1))  
      if(endvalue) pic[[j]] = pic[[j]] + as.layer(xyplot(V1 ~ V2 | name, datalab, type = "p", col = "black", 
                                               panel = function(x,y,subscripts, ...){
                                                 panel.xyplot(x, y, ...)
                                                 panel.text(x + 0.05, y + 0.2, labels = datalab$V3[subscripts], ...)
                                               } , 
                                               pch = c(15, 17), cex = 1))
      if(endvalue == FALSE) pic[[j]] = pic[[j]] + as.layer(xyplot(V1 ~ V2 | name, datalab, type = "p", col = cols, pch = c(15, 17), cex = 1))
      pic[[j]] = pic[[j]] + as.layer(xyplot(F_Fmsy ~ B_Bmsy | name, dataT, type = "b", col = cols, pch = 19, cex = 0.5)) 
      
    }
    
  }

  return(pic)
 
}


.recDevFUN = function(jjm.out, cols, ...)
{
  
  county = grep("SR_Curve_years_", names(jjm.out))
  colyear = NULL
  for(i in seq_along(county)){
    namesy = paste("SR_Curve_years_", i, sep = "")
    colyear[[i]] = jjm.out[[namesy]]
  }
  
  res = data.frame(jjm.out[["rec_dev"]])
  colnames(res) = c("year", "value")
  
  res$color = numeric(nrow(res))
  for(i in seq_along(colyear)){
    res$color[which(res$year %in% colyear[[i]])] = i
  }
  
  res$class = character(nrow(res))
  res$class[which(res$color==0)] = "Simulated"
  for(i in unique(res$color[which(res$color!=0)])){
    res$class[res$color == i] = paste("Regime", i, sep="")
  }
  
  labelLeg = NULL
  for(i in seq_along(colyear)){
    labelLeg[1] = "Simulated"
    labelLeg[i+1] = paste(colyear[[i]][1], " - ", rev(colyear[[i]])[1], sep = "")
  }
  
  colBar = NULL
  colBar[1] = "darkgrey"
  colBar[seq_along(colyear) + 1] = rev(cols)[seq_along(colyear)]
  colBar = colBar[order(unique(res$class))]
  
  labelCol = NULL
  labelCol[1] = "darkgrey"
  labelCol[seq_along(colyear) + 1] = rev(cols)[seq_along(colyear)]
  
  Bar = barchart(value ~ as.character(year), data = res, groups = class, horizontal = FALSE,
                 origin = 0, col = colBar,  box.width = 1, ylab = "Deviation",
                 par.settings = list(superpose.polygon = list(col = labelCol)),
                 scales = list(alternating = 1, tck = c(1,0), x = list(rot = 90)),
                 auto.key = list(title = "", space = "right",
                                 text = labelLeg,
                                 x = 0.85, y = 1, cex = 1.5,
                                 points = FALSE, border = FALSE, 
                                 lines = FALSE))
  
  Hist = histogram( ~ value | class, data = res[res$class != "Simulated", ], groups = class,
                   xlab = "Deviation", type = "density",
                   scales = list(alternating = 1, tck = c(1,0)),
                   ylim = c(0, 1.5*max(density(res$value[res$class != "Simulated"])[[2]])),
                   xlim = c(min(density(res$value[res$class != "Simulated"])[[1]]),
                            max(density(res$value[res$class != "Simulated"])[[1]])),
                   panel = function(x, col = colBar, ...){
                     panel.histogram(x = x, col = colBar[packet.number()],...)
                     panel.densityplot(x = x, darg = list(bw = 0.2, kernel = "gaussian"), 
                                       col = "black", lwd = 2, ...)
                     meanstat = round(mean(x), 3)
                     sdstat = round(sd(x), 3)
                     ssqstat = round(sum((x)^2), 3)
                     ssqTot = round(sum((res$value[res$class != "Simulated"])^2), 3)
                     panel.text(x = 0.8* max(density(res$value[res$class != "Simulated"])[[1]]), 
                                y = 0.85*max(density(res$value)[[2]]), labels = paste("mean = ", meanstat))
                     panel.text(x = 0.8*max(density(res$value[res$class != "Simulated"])[[1]]), 
                                y = 0.8*max(density(res$value)[[2]]), labels = paste("std = ", sdstat))
                     panel.text(x = 0.8*max(density(res$value[res$class != "Simulated"])[[1]]),
                                y = 0.75*max(density(res$value)[[2]]), labels = paste("ssq = ", ssqstat))
                     panel.text(x = 0.8*max(density(res$value[res$class != "Simulated"])[[1]]),
                                y = 0.7*max(density(res$value)[[2]]), labels = paste("ssqT = ", ssqTot))
                   })
  
  out = list(Bar = Bar, Hist = Hist)
  
  return(out)
} 


.plotDiagVar = function(x, fleet=NULL, plot=TRUE, ...) {
  if(!is.null(fleet) & all(fleet %in% names(x))) x = x[fleet]
  if(inherits(x, "trellis")) {
    x = update(x, ...) 
    if(isTRUE(plot)) print(x) 
  } else x = lapply(x, FUN=.plotDiagVar, fleet=NULL, plot=plot, ...)
  
  out = if(isTRUE(plot)) NULL else x
  
  return(invisible(out))
}

.plotDiag = function(x, var=NULL, fleet=NULL, plot=TRUE, ...) {
  
  if(is.null(var)) var = names(x)
  
  indVar = var %in% names(x)
  
  if(any(!indVar)) {
    msg = if(sum(!indVar)==1) 
      paste("Variable", sQuote(var[!indVar]), "does not exist.") else
        paste("Variables", sQuote(var[!indVar]), "do not exist.")
    var = var[indVar]
    if(length(var)==0) return(invisible())
    warning(msg)
  }
  
  if(isTRUE(plot)) {
    for(ivar in var) .plotDiagVar(x[[ivar]], fleet=fleet, plot=plot, ...)
    return(invisible())
  }
  
  out = list()
  
  for(i in seq_along(var)) 
    out[[i]] = .plotDiagVar(x[[var[i]]], fleet=fleet, plot=plot, ...)
  
  return(invisible(out))
  
}
