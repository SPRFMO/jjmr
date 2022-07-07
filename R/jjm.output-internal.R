.reshapeJJM = function(x, what = "biomass", total = FALSE){
  
  var = switch(what,
               biomass     = "TotBiom",
               ftot        = "Ftot",
               recruitment = "R",
               ssb         = "SSB",
               noFishTB    = "TotBiom_NoFish"
  )
  
  out = NULL
  
  for(i in seq_along(x)) {
    
    jjm.stocks = x[[i]]$output
    
    for(j in seq_along(jjm.stocks)){
      
      jjm.out = jjm.stocks[[j]]
      
      jjm.out$Ftot = cbind(jjm.out$Yr, rowMeans(jjm.out$TotF[, -1]), 
                           apply(jjm.out$TotF[, -1], 1, sd), 
                           rowMeans(jjm.out$TotF[, -1]),
                           rowMeans(jjm.out$TotF[, -1]))   
      
      jjm.in  = x[[i]]$data
      jjm.ypr = jjm.stocks[[j]]$YPR
      model   = x[[i]]$info$output$model
      stocks  = as.list(names(jjm.stocks))[[j]]
      
      outVar = jjm.out[[var]]
      year   = jjm.out$Yr
      ind    = which(outVar[, 1] %in% year) 
      outVar = outVar[ind, ]
      outVar = data.frame(outVar, model = model, stocks = stocks)
      
      out    = rbind(out, outVar)
      
    }
    
  }
  
  colnames(out) = c("year", "mean", "sd", "lower", "upper", "model", "stocks")
  
  if(what == "biomass" & isTRUE(total)){
    
    meanTotal = aggregate(mean ~ year + model, data = out, FUN = sum)
    sdTotal   = aggregate(sd ~ year + model, data = out, FUN = function(x) sqrt(sum(x^2)))
    outTotal  = merge(meanTotal, sdTotal, all = TRUE)
    outTotal$lower  = outTotal$mean - outTotal$sd
    outTotal$upper  = outTotal$mean + outTotal$sd
    outTotal$stocks = "Total"
    
    out = merge(out, outTotal, all = TRUE, sort = FALSE)
    
  }
  
  if(what == "noFishTB" & isTRUE(total)){
    
    meanTotal = aggregate(mean ~ year + model, data = out, FUN = sum)
    sdTotal   = aggregate(sd ~ year + model, data = out, FUN = function(x) sqrt(sum(x^2)))
    outTotal  = merge(meanTotal, sdTotal, all = TRUE)
    outTotal$lower  = outTotal$mean - outTotal$sd
    outTotal$upper  = outTotal$mean + outTotal$sd
    outTotal$stocks = "Total"
    
    out = merge(out, outTotal, all = TRUE, sort = FALSE)
    
  }
  
  if(what == "ssb" & isTRUE(total)){
    
    meanTotal = aggregate(mean ~ year + model, data = out, FUN = sum)
    sdTotal   = aggregate(sd ~ year + model, data = out, FUN = function(x) sqrt(sum(x^2)))
    outTotal  = merge(meanTotal, sdTotal, all = TRUE)
    outTotal$lower  = outTotal$mean - outTotal$sd
    outTotal$upper  = outTotal$mean + outTotal$sd
    outTotal$stocks = "Total"
    
    out = merge(out, outTotal, all = TRUE, sort = FALSE)
    
  }
  
  if(what == "recruitment" & isTRUE(total)){
    
    meanTotal = aggregate(mean ~ year + model, data = out, FUN = sum)
    sdTotal   = aggregate(sd ~ year + model, data = out, FUN = function(x) sqrt(sum(x^2)))
    outTotal  = merge(meanTotal, sdTotal, all = TRUE)
    outTotal$lower  = outTotal$mean - outTotal$sd
    outTotal$upper  = outTotal$mean + outTotal$sd
    outTotal$stocks = "Total"
    
    out = merge(out, outTotal, all = TRUE, sort = FALSE)
    
  }
  
  return(out)
}


.reshapeJJM2 = function(x, what, ...){
  
  var = switch(what,
               catchProj     = "Catch_fut_",
               ssbProj       = "SSB_fut_"
  )
  
  out = NULL
  
  for(i in seq_along(x)) {
    
    jjm.stocks = x[[i]]$output
    
    for(j in seq_along(jjm.stocks)){
      
      jjm.out = jjm.stocks[[j]]
      
      lastYear = jjm.out$R[nrow(jjm.out$R), 1]
      Nfutscen  = length(grep("SSB_fut_", names(jjm.out)))
      scenarios = c(paste0("F", lastYear ," SQ"), 
                    paste0("F", lastYear, " 0.75x"), 
                    paste0("F", lastYear, " 1.25x"), 
                    paste0("F", lastYear, " 0.5x"), 
                    paste0("F", lastYear, " 0x"))
      
      if(var == "Catch_fut_") {
        totCatch  = 0
        for(iFlt in grep("Obs_catch_", names(jjm.out)))
          totCatch = jjm.out[[iFlt]] + totCatch
        
        totCatch  = cbind(jjm.out$Yr, totCatch)
        colnames(totCatch) = c("year", "catch")
      }
      
      for(iScen in 1:length(scenarios)){
        
        if(var == "SSB_fut_") {
          idx = nrow(get("jjm.out")[["SSB"]][,c(1, 2)])
          tot = rbind(get("jjm.out")[["SSB"]][-idx,c(1, 2)],
                      get("jjm.out")[[paste(var, iScen, sep = "")]][,c(1, 2)])
          colnames(tot) = c("year", "SSB")
        }
        
        if(var == "Catch_fut_") {
          tot = rbind(totCatch, jjm.out[[paste(var, iScen, sep = "")]])
          colnames(tot) = c("year", "catch")
        }
        
        if(iScen == 1){
          totres = data.frame(tot)
          totres$scenario = scenarios[iScen]
        } else {
          res = data.frame(tot)
          res$scenario = scenarios[iScen]
          totres  = rbind(totres, res)
        }
        
      }
      
      colnames(totres) = c("year", "data", "scenario")	
      
      model   = x[[i]]$info$output$model 
      totres$model = model
      totres$stocks  = as.list(names(jjm.stocks))[[j]]
      
      out    = rbind(out, totres)
    }
    
    colnames(out) = c("year", "data", "scenario", "model", "stocks")
    
  }
  
  return(out)
  
}

.reshapeJJM3 = function(x, what, ...){
  
  var = switch(what,
               catchProjScen     = "Catch_fut_",
               ssbProjScen       = "SSB_fut_"
  )
  
  out = NULL
  
  scenarios = c(1, 0.75, 1.25, 0.5, 0)
  
  for(i in seq_along(x)) {
    
    jjm.stocks = x[[i]]$output
    
    for(j in seq_along(jjm.stocks)){
      
      jjm.out = jjm.stocks[[j]]
      
      lastYear = jjm.out$R[nrow(jjm.out$R), 1]
      Nfutscen  = length(grep("SSB_fut_", names(jjm.out)))
      
      if(var == "Catch_fut_") {
        meanCatch = NULL
        idx = grep(var, names(jjm.out))
        stocks = as.list(names(jjm.stocks))[[j]]
        for(k in seq_along(idx)){
          meanCatch[k] = mean(jjm.out[[idx[k]]][,2])
        }
        totres    = data.frame(scen = scenarios, data = meanCatch, stocks = stocks)
        
      }
      
      if(var == "SSB_fut_") {
        ssbLy = jjm.out$SSB[(nrow(jjm.out$SSB) - 1), 2]
        stocks = as.list(names(jjm.stocks))[[j]]
        ratio = NULL
        idx = grep(var, names(jjm.out))
        for(k in seq_along(idx)){
          ratio[k] = jjm.out[[idx[k]]][nrow(jjm.out[[idx[k]]]),2] / ssbLy
        }
        totres    = data.frame(scen = scenarios, data = ratio, stocks = stocks)
        
      }
      
      colnames(totres) = c("scen", "data", "stocks")	
      
      model   = x[[i]]$info$output$model 
      totres$model = model
      
      out    = rbind(out, totres)
      
    }
    
    colnames(out) = c("Scenario", "data", "stocks", "model")
    out = out[with(out, order(Scenario)),]
  }
  
  return(out)
  
}


.reshapeJJM4 = function(x, what, ...){
  
  var = switch(what,
               ratioSSB_F     = "SSB_NoFishR",
               ratioSSB       = "SSB"
  )
  
  out = NULL
  
  for(i in seq_along(x)) {
    
    jjm.stocks = x[[i]]$output
    
    for(j in seq_along(jjm.stocks)){
      
      jjm.out = jjm.stocks[[j]]
      
      if(var == "SSB_NoFishR") {
        idx1  = grep(var, names(jjm.out))
        idx2  = grep("TotF", names(jjm.out))
        xAxis = jjm.out[[idx1]][,2]
        yAxis = rowMeans(jjm.out[[idx2]])[-1]
        year  = jjm.out$Yr[-1]
        Data  = data.frame(year = year, xAxis = xAxis, yAxis = yAxis)
      }
      
      if(var == "SSB") {
        idx     = which(names(jjm.out) == var)
        fEst    = jjm.out[[idx]][1,2]
        indYear = which(jjm.out[[idx]][ ,1] == 1971)
        Years   = jjm.out[[idx]][ indYear:nrow(jjm.out[[idx]]), 1 ]
        Values  = jjm.out[[idx]][ indYear:nrow(jjm.out[[idx]]) , 2] / fEst
        Data    = data.frame(year = Years, xAxis = Years, yAxis = Values)
        
      }
      
      colnames(Data) = c("year", "scen", "data")	
      
      model       = x[[i]]$info$output$model 
      Data$model  = model
      Data$stocks = as.list(names(jjm.stocks))[[j]]
      
      out    = rbind(out, Data)
      
    }
    
    colnames(out) = c("year", "Scenario", "data", "model", "stocks")        
    
  }
  
  return(out)
  
}


.reshapeJJM5 = function(x, scen, cols, ...){
  
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  
  out = NULL
  
  if(!(scen %in% c(0, 0.5, 0.75, 1, 1.25))) stop("Only 0, 0.5, 0.75, 1 and 1.25 multipliers are allowed.")
  
  if(scen == 0) scenario = 5
  if(scen == 0.5) scenario = 4
  if(scen == 0.75) scenario = 2
  if(scen == 1) scenario = 1
  if(scen == 1.25) scenario = 3
  
  for(i in seq_along(x)) {
    
    jjm.stocks = x[[i]]$output
    
    for(j in seq_along(jjm.stocks)){
      
      jjm.out = jjm.stocks[[j]]
      
      idx = grep("Pred_catch_", names(jjm.out))
      
      idx_catch = grep(paste0("Catch_fut_", scenario), names(jjm.out))
      idx_ssb   = grep(paste0("SSB_fut_", scenario), names(jjm.out))
      
      totCatch = 0
      for(k in idx){
        totCatch = totCatch + jjm.out[[k]] 
      }
      
      dataCatch = data.frame(year = c(jjm.out$Yr, jjm.out[[idx_catch]][,1]), 
                             data = c(totCatch, jjm.out[[idx_catch]][,2]),
                             class = "catch")
      
      
      jjm.out$SSB = jjm.out$SSB[- nrow(jjm.out$SSB), ]
      
      dataSSB = rbind(jjm.out$SSB, jjm.out[[idx_ssb]])
      dataSSB = as.data.frame(dataSSB)
      names(dataSSB) = c("year", "ssb", "sd", "lower", "upper")
      
      dataS = cbind(dataSSB$year, dataSSB$ssb)
      dataS = as.data.frame(dataS)
      dataS$class = "ssb"
      
      dataL = cbind(dataSSB$year, dataSSB$lower)
      dataL = as.data.frame(dataL)
      dataL$class = "lower"
      
      dataU = cbind(dataSSB$year, dataSSB$upper)
      dataU = as.data.frame(dataU)
      dataU$class = "upper"
      
      dataT = rbind(dataS, dataL, dataU)
      dataT = as.data.frame(dataT)
      names(dataT) = c("year", "data", "class")
      
      Data = rbind(dataCatch, dataT)
      
      model   = x[[i]]$info$output$model 
      stocks  = as.list(names(jjm.stocks))[[j]]
      Data$model = model
      Data$stocks = stocks
      Data$color = cols[[j]]
      names(Data) = c("year", "data", "class", "model", "stocks", "color")
      
      out    = rbind(out, Data)
      
    }
    
  }
  
  return(out)
  
}



.reshapeJJMCol = function(x, cols, ...){
  
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  
  out = NULL
	
  for(i in seq_along(x)) {
    
    jjm.stocks = x[[i]]$output
    
    for(j in seq_along(jjm.stocks)){
      
      jjm.out = jjm.stocks[[j]]
      
      model   = x[[i]]$info$output$model
      stocks  = as.list(names(jjm.stocks))[[j]]
      
      Data = data.frame(model = model, stocks, class = c("catch", "ssb", "upper", "lower"),
                        color = cols[[j]])
     
      out    = rbind(out, Data)
    }
    
  }
  
  return(out)
  
}


.combineModels = function(...){
  
  modelList = list(...)
  
  for(i in modelList)
    if(class(i) != "jjm.output")
      stop("Objects must be of class 'jjm.output'.")
  
  modelList = c(...)
  
  # Remove repeated models from modelList 
  modelList2 = modelList
  for(i in seq_along(modelList[-length(modelList)]))
    for(j in seq(from = i + 1, to = length(modelList)))
      if(identical(modelList[[i]], modelList[[j]]))
        modelList2[[j]] = NULL
  
  modelList = modelList2; rm("modelList2")
  
  modelNames = NULL
    for(j in seq_along(modelList)){
      modelNames = c(modelNames, modelList[[j]]$info$output$model)
    }
    
  # Models
  models = list()
  for(i in seq_along(modelList)){
    models[[i]] = modelList[[i]][c("info", "data", "control", "parameters", "output")]
  }
  
  names(models) = modelNames

  output = models 

  class(output) = c("jjm.output")
  
  return(output)

}

.funPlotSeries = function(x, what, cols, stack, endvalue, poslegend, total, combine, ...){
  
  dataShape = .reshapeJJM(x, what = what, total = total)
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  mtheme = standard.theme("pdf", color=TRUE)
  mtheme$plot.line$lwd = 5
  mtheme$superpose.line$lwd = 5
  
  listStocks = NULL
  for(i in seq_along(x)){
    listStocks = x[[i]]$output 
  }
  
  if(combine == !TRUE){
    if(stack == !TRUE){
      pic = xyplot(mean ~ year | model, data = dataShape, groups = stocks, ylab = "", 
                   ylim = c(0.8*min(dataShape$lower), 1.1*max(dataShape$upper)),
                   xlim = c(min(dataShape$year - 1), max(dataShape$year + 1)),
                   scales = list(alternating = 1, tck = c(1, 0), y = list(relation = "free", rot = 0)),
                   key = list(lines = list(col = cols[1:length(names(listStocks))], lwd = 3),
                              text = list(names(listStocks))
                              , ...),                
                   par.settings = mtheme,
                   upper = dataShape$upper, lower = dataShape$lower,
                   panel = function(x, y, ...){
                     panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                     panel.xyplot(x, y, type ='l', cex = 0.6, lty = 1, lwd = 2, ...)
                     if(endvalue){
                       ltext(x = x[dataShape$year == max(dataShape$year)], 
                             y = y[dataShape$year == max(dataShape$year)], 
                             labels = round(y[dataShape$year == max(dataShape$year)],0), 
                             pos = 3, offset = 1, cex = 0.9,
                             font = 2, adj = 0)
                     }
                   }
                   , ...)
    } else {
      pic = xyplot(mean ~ year | stocks + model, data = dataShape, groups = stocks, ylab = "",
                         #ylim = c(0.8*min(dataShape$lower), 1.1*max(dataShape$upper)),
                         #xlim = c(min(dataShape$year - 1), max(dataShape$year + 1)),
                         scales = list(alternating = 1, tck = c(1, 0), y = list(relation = "free", rot = 0)),
                         upper = dataShape$upper, lower = dataShape$lower,
                         panel = function(x, y, ...){
                           panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                           panel.xyplot(x, y, type = 'l', cex = 0.6, lty = 1, lwd = 2, ...)
                           if(endvalue){
                             ltext(x = rev(x)[1], y = rev(y)[1], 
                                   labels = round(rev(y)[1],0), pos=3, offset=1, cex=0.9,
                                   font = 2, adj = 0)
                           }
                         }, ...)
    }
    
  } else {
    
    dataShape$combine=""
    dataShape$listStocks=paste(dataShape$model,dataShape$stocks)
    dataShape$listStocks=factor(dataShape$listStocks, levels = unique(dataShape$listStocks))
    
    if(stack == !TRUE){
      pic = xyplot(mean ~ year | combine, data = dataShape, groups = listStocks, ylab = "", 
                   ylim = c(0.8*min(dataShape$lower), 1.1*max(dataShape$upper)),
                   xlim = c(min(dataShape$year - 1), max(dataShape$year + 1)),
                   scales = list(alternating = 1, tck = c(1, 0), y = list(relation = "free", rot = 0)),
                   key = list(lines = list(col = cols[1:length(unique(dataShape$listStocks))], lwd = 3),
                              text = list(unique(as.character(dataShape$listStocks)))
                              , ...),                
                   par.settings = mtheme,
                   upper = dataShape$upper, lower = dataShape$lower,
                   panel = function(x, y, ...){
                     panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                     panel.xyplot(x, y, type ='l', cex = 0.6, lty = 1, lwd = 2, ...)
                     if(endvalue){
                       ltext(x = x[dataShape$year == max(dataShape$year)], 
                             y = y[dataShape$year == max(dataShape$year)], 
                             labels = round(y[dataShape$year == max(dataShape$year)],0), 
                             pos = 3, offset = 1, cex = 0.9,
                             font = 2, adj = 0)
                     }
                   }
                   , ...)
      
    } else {
      pic = xyplot(mean ~ year | listStocks, data = dataShape, groups = listStocks, ylab = "",
                   #ylim = c(0.8*min(dataShape$lower), 1.1*max(dataShape$upper)),
                   #xlim = c(min(dataShape$year - 1), max(dataShape$year + 1)),
                   scales = list(alternating = 1, tck = c(1, 0), y = list(relation = "free", rot = 0)),
                   upper = dataShape$upper, lower = dataShape$lower,
                   panel = function(x, y, ...){
                     panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                     panel.xyplot(x, y, type = 'l', cex = 0.6, lty = 1, lwd = 2, ...)
                     if(endvalue){
                       ltext(x = rev(x)[1], y = rev(y)[1], 
                             labels = round(rev(y)[1],0), pos=3, offset=1, cex=0.9,
                             font = 2, adj = 0)
                     }
                   }, ...)
      
    }

  }
  
  
  return(pic)
  
}


.funPlotKobe = function(x, what, cols, stack, endvalue, poslegend, ...){
  
  obj = x
  if(stack) {pic = .kobeFUN3(obj, cols = cols, endvalue = endvalue, ...)}
  else {pic = .kobeFUN2(obj, cols = cols, endvalue = endvalue, ...)}
  
  return(pic)
  
}


.funPlotProj = function(x, what, cols, stack, endvalue, poslegend, ...){
  
  if(!stack) warning("Series do not have to be in the same plot")
  
  listStocks = NULL
  for(i in seq_along(x)){
    for(j in seq_along(x[[i]]$output)){
      listStocks = x[[i]]$output[[j]]  
    }
  }
  
  Nfutscen  = length(grep("SSB_fut_", names(listStocks)))
  scenarios = c("F SQ", "F 0.75x", "F 1.25x", "F 0.5x", "F 0x")
  
  dataShape = .reshapeJJM2(x, what, ...)
  ikey           = simpleKey(text=scenarios, points = FALSE, lines = TRUE, columns = 2)
  ikey$lines$col = 1:length(scenarios)
  ikey$lines$lwd = 2
  ikey$lines$lty = 1
  
  pic = xyplot(data ~ year | stocks + model, data = dataShape, type = "l", groups = scenario,
				key = ikey, ylab = "",
				scales = list(alternating = 1, tck = c(1, 0), y = list(relation = "free", rot = 0)),
				#prepanel = function(...) {list(ylim = c(0, max(dataShape$data, na.rm = TRUE)))},
                panel = function(x, y){
                  panel.grid(h = -1, v = -1)
                  idx = mapply(seq(length(x)/Nfutscen, length(x), length.out = Nfutscen) - length(x)/Nfutscen + 1,
                                seq(length(x)/Nfutscen, length(x), length.out = Nfutscen), FUN = seq)
                  #scen1 = idx[,1]; scen2 = idx[,2]; scen3 = idx[,3]; scen4 = idx[,4]; scen5 = idx[,5]
                  for(iScen in 2:Nfutscen) panel.xyplot(x[idx[,iScen]], y[idx[,iScen]], type = "l", col = iScen, lwd = 2)
                  panel.xyplot(x[idx[,1]], y[idx[,1]], type = "l", col = 1, lwd = 2)                  
                }, ...)
  
  return(pic)
}


.funPlotScen = function(x, what, cols, stack, endvalue, poslegend, ...){
	
  dataShape = .reshapeJJM3(x, what = what)
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  mtheme = standard.theme("pdf", color=TRUE)
  mtheme$plot.line$lwd = 5
  mtheme$superpose.line$lwd = 5
  
  listStocks = NULL
  for(i in seq_along(x)){
    listStocks = x[[i]]$output
  }
  
  if(stack == !TRUE){
    pic = xyplot(data ~ Scenario | model, data = dataShape, groups = stocks,
				xlab="Fishing Mortality Multiplier", ylab = "",
				scales = list(alternating = 1, tck = c(1, 0)),
                 key = list(lines = list(col = cols[1:length(listStocks)], lwd = 3),
                            text = list(names(listStocks))
                            , ...),                
                 par.settings=mtheme,
                 panel = function(x, y, ...){
                   #panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                   panel.xyplot(x, y, type ='l', lty = 1, lwd = 2, ...)
				           panel.xyplot(x, y, type = 'p', cex = 1, pch = 19, ...)
                   if(endvalue){
                     ltext(x = x[dataShape$Scenario == max(dataShape$Scenario)], 
                           y = y[dataShape$Scenario == max(dataShape$Scenario)], 
                           labels = round(y[dataShape$Scenario == max(dataShape$Scenario)], 3),
                           pos=3, offset=1, cex=0.9, font = 2, adj = 0)
                   }
                 }
                 , ...)
  } else {pic = xyplot(data ~ Scenario | stocks + model, data = dataShape, groups = stocks,
                       xlab="Fishing Mortality Multiplier", ylab = "",
                       scales = list(alternating = 1, tck = c(1, 0),
                                     y = list(relation = "free", rot = 0)),
					   panel = function(x, y, ...){
                         #panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                         panel.xyplot(x, y, type = 'l', lty = 1, lwd = 2, ...)
                         panel.xyplot(x, y, type = 'p', cex = 1, pch = 19, ...)
						 if(endvalue){
                           ltext(x=rev(x)[1], y=rev(y)[1], labels=round(rev(y)[1],3), pos=3, offset=1, cex=0.9,
                                 font = 2, adj = 0)
                         }
                       }, ...)
  }
  
  return(pic)
	
}


.funPlotRatioSSB_F = function(x, what, cols, stack, endvalue, poslegend, ...){
	
  dataShape = .reshapeJJM4(x, what = what)
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  mtheme = standard.theme("pdf", color=TRUE)
  mtheme$plot.line$lwd = 5
  mtheme$superpose.line$lwd = 5
  
  listStocks = NULL
  for(i in seq_along(x)){
    listStocks = x[[i]]$output 
  }
  
  if(stack == !TRUE){
    pic = xyplot(data ~ Scenario | model, data = dataShape, groups = stocks,
				xlab = "Ratio", ylab = "Fishing Mortality",
				scales = list(alternating = 1, tck = c(1, 0)),
                 key = list(lines = list(col = cols[1:length(listStocks)], lwd = 3),
                            text = list(names(listStocks))
                            , ...),                
                 par.settings=mtheme,
                 panel = function(x, y, ...){
                   #panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                   panel.xyplot(x, y, type ='l', lty = 1, lwd = 2, ...)
				            panel.xyplot(x, y, type = 'p', cex = 1, pch = 19, ...)
                   if(endvalue){
                     ltext(x = x[dataShape$year == max(dataShape$year)], 
                           y = y[dataShape$year == max(dataShape$year)], 
                           labels = round(y[dataShape$year == max(dataShape$year)], 3),
                           pos=3, offset=1, cex=0.9, font = 2, adj = 0)
                   }
                 }
                 , ...)
  } else {pic = xyplot(data ~ Scenario | stocks + model, data = dataShape, groups = stocks,
                       xlab = "Ratio", ylab = "Fishing Mortality",
                       scales = list(alternating = 1, tck = c(1, 0),
                                     y = list(relation = "free", rot = 0)),
					   panel = function(x, y, ...){
                         #panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                         panel.xyplot(x, y, type = 'l', lty = 1, lwd = 2, ...)
                         panel.xyplot(x, y, type = 'p', cex = 1, pch = 19, ...)
						 if(endvalue){
                           ltext(x = rev(x)[1], y = rev(y)[1], labels = round(rev(y)[1], 3), pos = 3, offset = 1, cex = 0.9,
                                 font = 2, adj = 0)
                         }
                       }, ...)
  }
  
  return(pic)
	
}


.funPlotRatioSSB = function(x, what, cols, stack, endvalue, poslegend, ...){
	
  dataShape = .reshapeJJM4(x, what = what)
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  mtheme = standard.theme("pdf", color=TRUE)
  mtheme$plot.line$lwd = 5
  mtheme$superpose.line$lwd = 5
  
  listStocks = NULL
  for(i in seq_along(x)){
    listStocks = x[[i]]$output 
  }
  
  if(stack == !TRUE){
    pic = xyplot(data ~ Scenario | model, data = dataShape, groups = stocks,
				xlab = "year", ylab = "Ratio",
				scales = list(alternating = 1, tck = c(1, 0)),
                 key = list(lines = list(col = cols[1:length(listStocks)], lwd = 3),
                            text = list(names(listStocks))
                            , ...),                
                 par.settings=mtheme,
                 panel = function(x, y, ...){
                   #panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                   panel.xyplot(x, y, type ='l', lty = 1, lwd = 2, ...)
                   if(endvalue){
                     ltext(x = x[dataShape$Scenario == max(dataShape$Scenario)], 
                           y = y[dataShape$Scenario == max(dataShape$Scenario)], 
                           labels = round(y[dataShape$Scenario == max(dataShape$Scenario)], 3),
                           pos=3, offset=1, cex=0.9, font = 2, adj = 0)
                   }
                 }
                 , ...)
  } else {pic = xyplot(data ~ Scenario | stocks + model, data = dataShape, groups = stocks,
                       xlab = "year", ylab = "Ratio",
                       scales = list(alternating = 1, tck = c(1, 0),
                                     y = list(relation = "free", rot = 0)),
					   panel = function(x, y, ...){
                         #panel.superpose(x, y, panel.groups = .my.panel.bands, type = 'l', ...)
                         panel.xyplot(x, y, type = 'l', lty = 1, lwd = 2, ...)
						 if(endvalue){
                           ltext(x=rev(x)[1], y=rev(y)[1], labels = round(rev(y)[1], 3), pos=3, offset=1, cex=0.9,
                                 font = 2, adj = 0)
                         }
                       }, ...)
  }
  
  return(pic)
	
}



.funPlotTotProj = function(x, what, cols, stack, endvalue, poslegend, scen, ...){
	
  dataShape = .reshapeJJM5(x, scen, cols)
  dataCol = .reshapeJJMCol(x, cols)
  if(is.null(cols)) cols = rep(trellis.par.get("superpose.symbol")$col, 2)
  
  ikey           = simpleKey(text = unique(dataShape$stocks),
                             points = FALSE, lines = TRUE, columns = 2)
  
  ikey$lines$col = cols[1:length(unique(dataShape$stocks))]
  ikey$lines$lwd = 2
  ikey$lines$lty = 1
  
  pic = list()
  
  for(i in seq_along(length(unique(dataShape$stocks)))){  
    
    if(stack == TRUE){
      pic = xyplot(data ~ year | stocks + model , data = dataShape, groups = class,
                   xlab = "year", ylab = "",
                   scales = list(alternating = 1, tck = c(1, 0),
                                 y = list(relation = "free", rot = 0)),
                   #key = ikey,               
                   panel = function(x, y, ...){
                     panel.xyplot(x, y, type = 'l', lwd = 2, 
                                  lty = c(5, 1, 3, 3),
                                  col = rep(cols[1:length(unique(dataShape$stocks))], each = 8), ...)
                     if(endvalue){
                       ltext(x=rev(x)[1], y=rev(y)[1], labels=round(rev(y)[1], 3), pos=3, offset=1, cex=0.9,
                             font = 2, adj = 0)
                     }
                   }
                   , ...)
    } else {pic[[i]] = xyplot(data ~ year | model, data = dataShape, groups = stocks,
                         xlab = "year", ylab = "", type = "l",
                         scales = list(alternating = 1, tck = c(1, 0)),
                         par.settings = list(superpose.line = list(
                           lwd = 2, 
                           col = dataCol$color)),
                         if(endvalue){
                           ltext(x=rev(x)[1], y=rev(y)[1], labels = round(rev(y)[1], 3), pos=3, offset=1, cex=0.9,
                                 font = 2, adj = 0)
                         }
                         , ...)
    }
    
    
  }
  
  return(pic)
	
}

.getInfo = function(data, output, model) {
  info.data   = list(file = attr(data, "filename"), 
                     variables = length(names(data)), 
                     year=c(data$years[1], data$years[2]),
                     age = c(data$ages[1], data$ages[2]), 
                     length = c(data$lengths[1], data$lengths[2]),
                     version = attr(data, "version"))
  
  indices = NULL
  fisheries = NULL
  
  for(i in seq_along(output)) {
    tempI = output[[i]]$Index_names
    tempF = output[[i]]$Fshry_names
    indices = c(indices, tempI)
    fisheries = c(fisheries, tempF)
  }
  
  indices = unique(indices)
  fisheries = unique(fisheries)
  
  info.output = list(model=model, fisheryNames=fisheries, modelYears=output$Yr,
                     indexModel=indices, nStock=length(output))
  
  info = list(data = info.data, output = info.output)
  
  return(info)
}

.readOutputsJJM = function(files, yld=NULL) {
  ypr = if(!is.null(yld)) .readYPR(yld) else NULL # should be a list by stock
  outputs = list()
  for(i in seq_along(files)) {
    outputs[[i]] = PBSmodelling::readList(files[i]) # add validation
    outputs[[i]]$YPR = ypr
  }

  names(outputs) = paste0("Stock_", 1:length(files)) # Puede ser modificado cuando se lea el ctl
  return(outputs)
  
}
