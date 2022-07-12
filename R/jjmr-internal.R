
# Internal functions ------------------------------------------------------

# Code to read in final data ----------------------------------------------
.readDat = function(dat, version) {
  ###-Read in the raw datafile-###
  res1      = scan(file = dat, what = 'numeric', quiet = TRUE, sep = "\n",
                    comment.char = "#", allowEscapes = TRUE)
  res1      = strsplit(res1, "\t")

  for(i in seq_along(res1)){
    res1[[i]] = paste(res1[[i]], collapse = " ")
    res1[[i]] = strsplit(res1[[i]], " ")[[1]]
  }
  
  #- Get some initial dimensions
  nY        = length(.an(unlist(res1[[1]][1])):.an(unlist(res1[[2]][1]))) #number of years
  Ys        = na.omit(.an(unlist(res1[1:2]))) #Years
  nA        = length(.an(unlist(res1[[3]][1])):.an(unlist(res1[[4]][1]))) #number of ages
  As        = na.omit(.an(unlist(res1[3:4]))) #Ages
  nL        = na.omit(.an(unlist(res1[[5]]))) #number of lengths
  Ls        = na.omit(.an(unlist(strsplit(res1[[6]], "  ")))) #Lengths
  nF        = na.omit(.an(unlist(res1[[7]]))) #number of fisheries
  
  #- Define storage object
  cols      = list()
  
  ###-Fill cols with data from res1-###
  
  #-Common data
  cols$years        = matrix(NA, ncol = 2, nrow = 1 , dimnames = list("years", c("first year", "last year")))
  cols$years[]      = na.omit(.an(unlist(res1[1:2])))
  cols$ages         = matrix(NA, ncol = 2, nrow = 1, dimnames = list("age", c("age recruit", "oldest age")))
  cols$ages[]       = na.omit(.an(unlist(res1[3:4])))
  cols$lengths      = matrix(NA, ncol = 2, nrow = 1, dimnames = list("lengths", c("first length", "last length")))
  cols$lengths[]    = na.omit(c(min(Ls), max(Ls)))
  cols$nbins        = nL
  cols$lengthbin    = numeric(nL)
  cols$lengthbin[]  = Ls 
  
  #-Fisheries data
  cols$Fnum         = numeric()
  cols$Fnum         = na.omit(.an(unlist(res1[7])))
  
  #-Start of dynamic rows
  counter           = 8 #first dynamic row
  
  cols$Fnames       = list()
  cols$Fnames       = strsplit(unlist(res1[counter]), "%")[[1]]; counter = counter + 1
  cols$Fcaton       = matrix(NA, ncol = nF, nrow = nY,
                              dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  cols$Fcaton[]     = matrix(na.omit(.an(unlist(res1[counter:(counter + nF - 1)]))),
                              ncol = nF, nrow = nY); counter = counter + nF
  cols$Fcatonerr    = matrix(NA, ncol = nF, nrow = nY, dimnames = list(years = Ys[1]:Ys[2],
                                                                       paste("fishery", 1:nF, sep = "")))
  cols$Fcatonerr[]  = matrix(na.omit(.an(unlist(res1[counter:(counter + nF - 1)]))), 
                              ncol = nF, nrow = nY); counter = counter + nF
  cols$FnumyearsA   = matrix(NA, ncol = nF, nrow = 1, dimnames = list("years", paste("Fyears", 1:nF, sep = "")))
  cols$FnumyearsA[] = na.omit(.an(unlist(res1[counter:(counter+nF-1)]))); counter = counter + nF
  cols$FnumyearsL   = matrix(NA, ncol = nF, nrow = 1, dimnames = list("years", paste("Fyears", 1:nF, sep = "")))
  cols$FnumyearsL[] = na.omit(.an(unlist(res1[counter:(counter + nF - 1)]))); counter = counter + nF
  cols$Fageyears    = matrix(NA, ncol = nF, nrow = nY,
                              dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  
  for(iFs in 1:nF){
    if(cols$FnumyearsA[iFs] > 0){
      Fageyears = c(na.omit(.an(res1[[counter]])))
      wFyears   = pmatch(Fageyears, cols$years[1]:cols$years[2])
      cols$Fageyears[wFyears, paste("fishery", iFs, sep = "")] = Fageyears
      counter   = counter + 1
    }
  }
  cols$Flengthyears = matrix(NA, ncol = nF, nrow = nY, 
                              dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsL[iFs] > 0){
      Flengthyears  = c(na.omit(.an(res1[[counter]])))
      lFyears       = pmatch(Flengthyears, cols$years[1]:cols$years[2])
      cols$Flengthyears[lFyears,paste("fishery", iFs, sep = "")] = Flengthyears
      counter       = counter + 1
    }
  }
  
  cols$Fagesample = matrix(NA, ncol = nF, nrow = nY, 
                            dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsA[iFs] > 0){
      wFyears = rownames(cols$Fageyears)[which(is.na(cols$Fageyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Fagesample[wFyears, paste("fishery", iFs, sep = "")] = na.omit(.an(unlist(res1[counter])))
      counter = counter + 1
    }
  }
  
  cols$Flengthsample = matrix(NA, ncol = nF, nrow = nY, 
                               dimnames = list(years = Ys[1]:Ys[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsL[iFs] > 0){
      lFyears = rownames(cols$Flengthyears)[which(is.na(cols$Flengthyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Flengthsample[lFyears, paste("fishery", iFs, sep = "")] = na.omit(.an(unlist(res1[counter])))
      counter = counter + 1
    }
  }
  
  cols$Fagecomp = array(NA, dim = c(nY, nA, nF), 
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsA[iFs] > 0){
      wFyears = rownames(cols$Fageyears)[which(is.na(cols$Fageyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Fagecomp[wFyears,,paste("fishery", iFs, sep = "")] = 
        matrix(na.omit(.an(unlist(res1[counter:(counter + length(wFyears) - 1)]))), ncol = nA,
               nrow = length(wFyears), byrow = TRUE)
      counter = counter + length(wFyears)
    }
  }
  
  cols$Flengthcomp = array(NA, dim = c(nY, nL, nF), 
                            dimnames = list(years = Ys[1]:Ys[2], lengths = Ls[1]:Ls[length(Ls)],
                                            paste("fishery", 1:nF, sep = "")))
  for(iFs in 1:nF){
    if(cols$FnumyearsL[iFs] > 0){
      lFyears = rownames(cols$Flengthyears)[which(is.na(cols$Flengthyears[,paste("fishery", iFs, sep = "")]) == FALSE)]
      cols$Flengthcomp[lFyears,,paste("fishery", iFs, sep = "")] = 
        matrix(na.omit(.an(unlist(res1[counter:(counter + length(lFyears) - 1)]))),
               ncol = nL, nrow = length(lFyears), byrow = TRUE)
      counter = counter +length(lFyears)
    }
  }
  
  cols$Fwtatage = array(NA, dim = c(nY, nA, nF),
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("fishery",1:nF,sep="")))
  for(iFs in 1:nF){
    cols$Fwtatage[,,iFs] = matrix(na.omit(.an(unlist(res1[counter:(counter + nY - 1)]))),
                                   ncol = nA, nrow = nY, byrow = TRUE)
    counter = counter + nY
  } 
  
  #-Indices data
  nI = na.omit(.an(res1[[counter]]))
  cols$Inum = numeric()
  cols$Inum = na.omit(.an(res1[[counter]])); counter = counter + 1
  cols$Inames = list()
  cols$Inames = strsplit(res1[[counter]], "%")[[1]] 
  counter = counter + 1
  cols$Inumyears = matrix(NA, ncol = nI, nrow = 1, dimnames = list("years", paste("index", 1:nI, sep = "")))
  cols$Inumyears[] = na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter = counter + cols$Inum
  
  cols$Iyears = matrix(NA, ncol = nI, nrow = nY, dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumyears[iSu] > 0){
      Iyears = na.omit(.an(res1[[counter]])); wIyears = pmatch(Iyears, cols$years[1]:cols$years[2])
      cols$Iyears[wIyears, paste("index", iSu, sep = "")] = Iyears
      counter = counter + 1
    }
  }
  
  cols$Imonths = matrix(NA, ncol = nI, nrow = 1, dimnames = list("month", paste("index", 1:nI, sep = "")))
  cols$Imonths[] = na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter = counter + cols$Inum
  cols$Index = matrix(NA, ncol = nI, nrow = nY, dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumyears[iSu] > 0){
      wIyears = rownames(cols$Iyears)[which(is.na(cols$Iyears[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Index[wIyears, paste("index", iSu, sep = "")] = na.omit(.an(res1[[counter]]))
      counter = counter + 1
    }
  }
  
  cols$Indexerr = matrix(NA, ncol = nI, nrow = nY, dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumyears[iSu] > 0){
      wIyears = rownames(cols$Iyears)[which(is.na(cols$Iyears[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Indexerr[wIyears, paste("index", iSu, sep = "")] = na.omit(.an(res1[[counter]]))
      counter = counter + 1
    }
  }
  
  cols$Inumageyears = matrix(NA, ncol = nI, nrow = 1, dimnames = list("years", paste("index", 1:nI, sep = "")))
  cols$Inumageyears[] = na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter = counter + cols$Inum
  cols$Inumlengthyears = matrix(NA, ncol = nI, nrow = 1, dimnames = list("years", paste("index", 1:nI, sep = "")))
  cols$Inumlengthyears[] = na.omit(.an(unlist(res1[counter:(counter + cols$Inum - 1)])))
  counter = counter + cols$Inum
  
  cols$Iyearslength = matrix(NA, ncol = nI, nrow = nY, 
                              dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumlengthyears[iSu] > 0){
      Iyearslength = na.omit(.an(res1[[counter]]))
      wIyearslength = pmatch(Iyearslength, cols$years[1]:cols$years[2])
      cols$Iyearslength[wIyearslength, iSu] = Iyearslength
      counter = counter + 1
    }
  }
  
  cols$Iyearsage = matrix(NA, ncol = nI, nrow = nY, 
                           dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumageyears[iSu] > 0){
      Iyearsage = na.omit(.an(res1[[counter]])); wIyearsage = pmatch(Iyearsage, cols$years[1]:cols$years[2])
      cols$Iyearsage[wIyearsage, iSu] = Iyearsage
      counter = counter + 1
    }
  }
  
  cols$Iagesample = matrix(NA, ncol = nI, nrow = nY, 
                            dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumageyears[iSu] > 0){
      wIyears = rownames(cols$Iyearsage)[which(is.na(cols$Iyearsage[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Iagesample[wIyears,iSu] = na.omit(.an(res1[[counter]]))
      counter = counter + 1
    }
  }
  cols$Ipropage = array(NA, dim = c(nY, nA, nI), 
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumageyears[iSu] > 0){
      wIyears = rownames(cols$Iyearsage)[which(is.na(cols$Iyearsage[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Ipropage[wIyears,,iSu] = matrix(na.omit(.an(unlist(res1[counter:(counter + cols$Inumageyears[iSu]-1)]))),
                                            ncol = nA, nrow = cols$Inumageyears[iSu], byrow = TRUE)
      counter = counter + cols$Inumageyears[iSu]
    }
  }
  
  cols$Ilengthsample = matrix(NA, ncol = nI, nrow = nY, 
                               dimnames = list(years = Ys[1]:Ys[2], paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumlengthyears[iSu] > 0){
      wIyears = rownames(cols$Iyearslength)[which(is.na(cols$Iyearslength[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Ilengthsample[wIyears, iSu] = na.omit(.an(res1[[counter]]))
      counter = counter + 1
    }
  }
  cols$Iproplength = array(NA, dim = c(nY, nL, nI),
                            dimnames = list(years = Ys[1]:Ys[2], lengths = Ls[1]:Ls[length(Ls)], 
                                            paste("index", 1:nI, sep = "")))
  for(iSu in 1:nI){
    if(cols$Inumlengthyears[iSu] > 0){
      wIyears = rownames(cols$Iyearslength)[which(is.na(cols$Iyearslength[,paste("index", iSu, sep = "")]) == FALSE)]
      cols$Iproplength[wIyears,,iSu] = 
        matrix(na.omit(.an(unlist(res1[counter:(counter + cols$Inumlengthyears[iSu] - 1)]))), 
               ncol = nL, nrow = cols$Inumlengthyears[iSu], byrow = TRUE)
      counter = counter + cols$Inumlengthyears[iSu]
    }
  }
  
  cols$Iwtatage = array(NA, dim = c(nY, nA, nI),
                         dimnames = list(years = Ys[1]:Ys[2], age = As[1]:As[2], paste("index", 1:nI, sep = "")))
  
  for(iSu in 1:nI){
    cols$Iwtatage[,,iSu] = matrix(na.omit(.an(unlist(res1[counter:(counter + nY - 1)]))),
                                   ncol = nA, nrow = nY, byrow = TRUE)
    counter = counter + nY
  }
  
  #-Population data
  if(version != "2015MS"){
	  cols$Pwtatage = matrix(NA, ncol = 1, nrow = nA, dimnames = list(age = As[1]:As[2], "weight"))
	  cols$Pwtatage[] = na.omit(.an(res1[[counter]])); counter = counter + 1
	  cols$Pmatatage = matrix(NA, ncol = 1, nrow = nA, dimnames = list(age = As[1]:As[2], "maturity"))
	  cols$Pmatatage[] = na.omit(.an(res1[[counter]])); counter = counter + 1
  }
  cols$Pspwn = numeric()
  cols$Pspwn = na.omit(.an(res1[[counter]])); counter = counter + 1
  cols$Pageerr = matrix(NA, ncol = nA, nrow = nA, dimnames = list(age = As[1]:As[2], age = As[1]:As[2]))
  cols$Pageerr[] = matrix(na.omit(.an(unlist(res1[counter:(counter + nA - 1)]))),
                           ncol = nA, nrow = nA, byrow = TRUE); counter = counter + nA
  
  attr(cols, which = "filename") = dat
  attr(cols, which = "version")  = version
  return(cols)
  
}

.read.datEx = function(filename, version){ 
  
  res1      = scan(file = filename, what = 'numeric', quiet = TRUE, sep = "\n",
                   comment.char = "#", allowEscapes = TRUE)
  res1      = strsplit(res1, "\t")
  
  fVector = NULL
  for(i in seq_along(res1)){
    res1[[i]] = paste(res1[[i]], collapse = " ")
    Vector = strsplit(res1[[i]], " ")[[1]]
    Vector = Vector [! Vector %in% ""]
    fVector = c(fVector, Vector)
  }
  
  
  
  listCtl = list()
  cV = 1
  listCtl$FirstYear  = fVector[cV] ;cV = cV + 1
  listCtl$LastYear = fVector[cV] ;cV = cV + 1
  listCtl$FirstAge   = fVector[cV] ;cV = cV + 1
  listCtl$LastAge = fVector[cV] ;cV = cV + 1
  listCtl$NLenInt = fVector[cV] ;cV = cV + 1
  listCtl$LengthBin = fVector[cV:(cV + as.numeric(listCtl$NLenInt) - 1)] ;cV = cV + as.numeric(listCtl$NLenInt)
  listCtl$NFisheries = fVector[cV] ;cV = cV + 1
  listCtl$nameFish = fVector[cV] ;cV = cV + 1
  
  nYears = as.numeric(listCtl$LastYear) - as.numeric(listCtl$FirstYear) + 1
  nFisheries = as.numeric(listCtl$NFisheries)
  
  VCatch = fVector[cV:(cV + nFisheries*nYears - 1)]
  MCatch = matrix(VCatch, nrow = nFisheries, byrow = TRUE)
  listCtl$Catch   = MCatch
  cV = cV + nFisheries*nYears
  
  VCatchCV = fVector[cV:(cV + nFisheries*nYears - 1)]
  MCatchCV = matrix(VCatchCV, nrow = nFisheries, byrow = TRUE)
  listCtl$CatchCV   = MCatchCV
  cV = cV + nFisheries*nYears
  
  listCtl$nYrsFAge = fVector[cV:(cV + nFisheries - 1)]; cV = cV + nFisheries
  listCtl$nYrsFLength = fVector[cV:(cV + nFisheries - 1)]; cV = cV + nFisheries
  listCtl$nYrsFAge = as.numeric(listCtl$nYrsFAge)
  listCtl$nYrsFLength = as.numeric(listCtl$nYrsFLength)  
  
  for(i in seq_along(listCtl$nYrsFAge)){
    if(listCtl$nYrsFAge[i] > 0) {
      listCtl[[paste0("Fishery_", i, "_Age_Yr")]] = fVector[cV:(cV + listCtl$nYrsFAge[i] - 1)]
      cV = cV + listCtl$nYrsFAge[i]
    }
  }
  
  for(i in seq_along(listCtl$nYrsFLength)){
    if(listCtl$nYrsFLength[i] > 0) {
      listCtl[[paste0("Fishery_", i, "_Length_Yr")]] = fVector[cV:(cV + listCtl$nYrsFLength[i] - 1)]
      cV = cV + listCtl$nYrsFLength[i]
    }
  }
  
  for(i in seq_along(listCtl$nYrsFAge)){
    if(listCtl$nYrsFAge[i] > 0) {
      listCtl[[paste0("Fishery_", i, "_Age_SamSize")]] = fVector[cV:(cV + listCtl$nYrsFAge[i] - 1)]
      cV = cV + listCtl$nYrsFAge[i]
    }
  }
  
  for(i in seq_along(listCtl$nYrsFLength)){
    if(listCtl$nYrsFLength[i] > 0) {
      listCtl[[paste0("Fishery_", i, "_Length_SamSize")]] = fVector[cV:(cV + listCtl$nYrsFLength[i] - 1)]
      cV = cV + listCtl$nYrsFLength[i]
    }
  }
  
  nBinAge = as.numeric(listCtl$LastAge)
  for(i in seq_along(listCtl$nYrsFAge)){
    if(listCtl$nYrsFAge[i] > 0) {
      Vtemp = fVector[cV:(cV + listCtl$nYrsFAge[i]*nBinAge - 1)]
      Mtemp = matrix(Vtemp, nrow = listCtl$nYrsFAge[i], byrow = TRUE)
      listCtl[[paste0("Fishery_", i, "_AgeMatrix")]] = Mtemp
      cV = cV + listCtl$nYrsFAge[i]*nBinAge
    }
  }
  
  nBinLen = as.numeric(listCtl$NLenInt)
  for(i in seq_along(listCtl$nYrsFAge)){
    if(listCtl$nYrsFLength[i] > 0) {
      Vtemp = fVector[cV:(cV + listCtl$nYrsFLength[i]*nBinLen - 1)]
      Mtemp = matrix(Vtemp, nrow = listCtl$nYrsFLength[i], byrow = TRUE)
      listCtl[[paste0("Fishery_", i, "_LengthMatrix")]] = Mtemp
      cV = cV + listCtl$nYrsFLength[i]*nBinLen
    }
  }
  
  nFish = as.numeric(listCtl$NFisheries)
  for(i in 1:nFish){
      Vtemp = fVector[cV:(cV + nYears*nBinAge - 1)]
      Mtemp = matrix(Vtemp, nrow = nYears, byrow = TRUE)
      listCtl[[paste0("Fishery_", i, "_wtatage")]] = Mtemp
      cV = cV + nYears*nBinAge
  }
  
  listCtl$nIndex = fVector[cV]; cV = cV + 1
  listCtl$nameIndex = fVector[cV]; cV = cV + 1
  
  listCtl$nIndex = as.numeric(listCtl$nIndex)
  listCtl$nObsIndex = fVector[cV:(cV + listCtl$nIndex - 1)]; cV = cV + listCtl$nIndex 
  listCtl$nObsIndex = as.numeric(listCtl$nObsIndex)
  
  for(i in seq_along(listCtl$nObsIndex)){
    if(listCtl$nObsIndex[i] > 0) {
      listCtl[[paste0("Index_", i, "_Yr")]] = fVector[cV:(cV + listCtl$nObsIndex[i] - 1)]
      cV = cV + listCtl$nObsIndex[i]
    }
  }
  
  listCtl$monthIndex = fVector[cV:(cV + listCtl$nIndex - 1)]; cV = cV + listCtl$nIndex 
  
  for(i in seq_along(listCtl$nObsIndex)){
    if(listCtl$nObsIndex[i] > 0) {
      listCtl[[paste0("Index_", i, "_biomass")]] = fVector[cV:(cV + listCtl$nObsIndex[i] - 1)]
      cV = cV + listCtl$nObsIndex[i]
    }
  }
  
  for(i in seq_along(listCtl$nObsIndex)){
    if(listCtl$nObsIndex[i] > 0) {
      listCtl[[paste0("Index_", i, "_biomass_sd")]] = fVector[cV:(cV + listCtl$nObsIndex[i] - 1)]
      cV = cV + listCtl$nObsIndex[i]
    }
  }
  
  listCtl$IndexAgeDat = fVector[cV:(cV + listCtl$nIndex - 1)]; cV = cV + listCtl$nIndex
  listCtl$IndexLenDat = fVector[cV:(cV + listCtl$nIndex - 1)]; cV = cV + listCtl$nIndex
  listCtl$IndexAgeDat = as.numeric(listCtl$IndexAgeDat)
  listCtl$IndexLenDat = as.numeric(listCtl$IndexLenDat)
  
  for(i in seq_along(listCtl$IndexAgeDat)){
    if(listCtl$IndexAgeDat[i] > 0) {
      listCtl[[paste0("Index_", i, "_Age_Yrs")]] = fVector[cV:(cV + listCtl$IndexAgeDat[i] - 1)]
      cV = cV + listCtl$IndexAgeDat[i]
    }
  }
  
  for(i in seq_along(listCtl$IndexAgeDat)){
    if(listCtl$IndexAgeDat[i] > 0) {
      listCtl[[paste0("Index_", i, "_Age_SamSize")]] = fVector[cV:(cV + listCtl$IndexAgeDat[i] - 1)]
      cV = cV + listCtl$IndexAgeDat[i]
    }
  }
  
  for(i in seq_along(listCtl$IndexAgeDat)){
    if(listCtl$IndexAgeDat[i] > 0) {
      Vtemp = fVector[cV:(cV + listCtl$IndexAgeDat[i]*nBinAge - 1)]
      Mtemp = matrix(Vtemp, nrow = listCtl$IndexAgeDat[i], byrow = TRUE)
      listCtl[[paste0("Index_", i, "_AgeMatrix")]] = Mtemp
      cV = cV + listCtl$IndexAgeDat[i]*nBinAge
    }
  }
  
  for(i in seq_along(listCtl$IndexLenDat)){
    if(listCtl$IndexLenDat[i] > 0) {
      Vtemp = fVector[cV:(cV + listCtl$IndexLenDat[i]*nBinLen - 1)]
      Mtemp = matrix(Vtemp, nrow = listCtl$IndexLenDat[i], byrow = TRUE)
      listCtl[[paste0("Index_", i, "_LengthMatrix")]] = Mtemp
      cV = cV + listCtl$IndexLenDat[i]*nBinLen
    }
  }
  
  nInd = as.numeric(listCtl$nIndex)
  for(i in 1:nInd){
    Vtemp = fVector[cV:(cV + nYears*nBinAge - 1)]
    Mtemp = matrix(Vtemp, nrow = nYears, byrow = TRUE)
    listCtl[[paste0("Index_", i, "_wtatage")]] = Mtemp
    cV = cV + nYears*nBinAge
  }
  
  if(version != "2015MS"){
	listCtl$popwtatage = fVector[cV:(cV + nBinAge - 1)]; cV = cV + nBinAge
	listCtl$popmtatage = fVector[cV:(cV + nBinAge - 1)]; cV = cV + nBinAge
  }
  
  listCtl$monthSpaw = fVector[cV]; cV = cV + 1

  listCtl$LastAge = as.numeric(listCtl$LastAge)

    Vtemp = fVector[cV:(cV + (listCtl$LastAge^2) - 1)]
    Mtemp = matrix(Vtemp, nrow = listCtl$LastAge, byrow = TRUE)
    listCtl$ageErrorMatrix = Mtemp
  
  posChar = which(names(listCtl) == "nameFish" | names(listCtl) == "nameIndex")
  lenList = length(listCtl)
  finList = 1:lenList
  finList = finList[-c(posChar)]
  
  for(i in finList){
    class(listCtl[[i]]) = "numeric"
  }
  
  return(listCtl)
  
}

.readCtl = function(ctl, info) {
  
  version = .versionJJM(ctl)
  
  control = if(version == "2015MS")
    .read.ctlMS(filename = ctl, info=info$output, infoDat=info$data) else 
      .read.ctl(filename = ctl, info=info$output, infoDat=info$data)
  
  attr(control, which = "filename") = ctl
  attr(control, which = "version")  = version
  
  return(control)
}


.read.ctlMS = function(filename, info, infoDat) {

Fishery = as.vector(info$fisheryNames)
Index = as.vector(info$indexModel)
nFishery = length(Fishery)
nIndex = length(Index)
nAges = infoDat$age[2] - infoDat$age[1] + 1
nStock = info$nStock

res1      = scan(file = filename, what = 'numeric', quiet = TRUE, sep = "\n",
                 comment.char = "#", allowEscapes = TRUE)
res1      = strsplit(res1, "\t")

fVector = NULL
for(i in seq_along(res1)){
  res1[[i]] = paste(res1[[i]], collapse = " ")
  Vector = strsplit(res1[[i]], " ")[[1]]
  Vector = Vector [! Vector %in% ""]
  fVector = c(fVector, Vector)
}

listCtl = list()
cV = 1
listCtl$ControlFile = paste0("#", info$filename)
listCtl$dataFile  = fVector[cV]; cV = cV + 1
listCtl$modelName = fVector[cV]; cV = cV + 1
listCtl$nStocks   = as.numeric(fVector[cV]); cV = cV + 1
listCtl$nameStock = fVector[cV]; cV = cV + 1


fVector = fVector[- c(1,2,3,4)]
fVector = as.numeric(fVector)

cV = 1
VFishery = fVector[cV:(3*(nFishery + nIndex))]
MFishery = matrix(VFishery, ncol = (nFishery + nIndex), byrow = TRUE)
cV = cV + (3*(nFishery + nIndex))

listCtl$SelMatrix = MFishery

listCtl$nregbyStock = fVector[cV:(cV + nStock - 1)]; cV = cV + nStock
listCtl$SrType      = fVector[cV]; cV = cV + 1
listCtl$AgeError    = fVector[cV]; cV = cV + 1
listCtl$Retro       = fVector[cV]; cV = cV + 1

listCtl$RecMatrix   = fVector[cV:(cV + sum(listCtl$nregbyStock) - 1)]; cV = cV + sum(listCtl$nregbyStock)

diffRec = length(unique(listCtl$RecMatrix))

VSteep = fVector[cV:(cV + 3*diffRec - 1)]
MSteep = matrix(VSteep, nrow = 3, byrow = TRUE)
listCtl$Steepness   = MSteep
cV = cV + 3*diffRec

VSigma = fVector[cV:(cV + 3*diffRec - 1)]
MSigma = matrix(VSigma, nrow = 3, byrow = TRUE)
listCtl$SigmaR   = MSigma
cV = cV + 3*diffRec

listCtl$phase_Rzero   = fVector[cV:(cV + sum(listCtl$nregbyStock) - 1)]
cV = cV + sum(listCtl$nregbyStock)

listCtl$Nyrs_sr   = fVector[cV:(cV + sum(listCtl$nregbyStock) - 1)]
cV = cV + sum(listCtl$nregbyStock)

for(i in seq_along(listCtl$Nyrs_sr)){
  listCtl[[paste0("Nyrs_sr_", i)]] = fVector[cV:(cV + listCtl$Nyrs_sr[i] - 1)]
  cV = cV + listCtl$Nyrs_sr[i]
}

nShift = sum(listCtl$nregbyStock - 1)
if(nShift == 0) listCtl$RegShift = NA
if(nShift > 0) {
	listCtl$RegShift = fVector[cV:(cV + nShift - 1)]
}
cV = cV + nShift

listCtl$GrowMatrix   = fVector[cV:(cV + sum(listCtl$nregbyStock) - 1)] 
cV = cV + sum(listCtl$nregbyStock)

diffGrow = length(unique(listCtl$GrowMatrix))

VLinf = fVector[cV:(cV + 3*diffGrow - 1)]
MLinf = matrix(VLinf, nrow = 3, byrow = TRUE)
listCtl$Linf   = MLinf
cV = cV + 3*diffGrow

VK = fVector[cV:(cV + 3*diffGrow - 1)]
MK = matrix(VK, nrow = 3, byrow = TRUE)
listCtl$K   = MK
cV = cV + 3*diffGrow

VLo_len = fVector[cV:(cV + 3*diffGrow - 1)]
MLo_len = matrix(VLo_len, nrow = 3, byrow = TRUE)
listCtl$Lo_len   = MLo_len
cV = cV + 3*diffGrow

VSigma_len = fVector[cV:(cV + 3*diffGrow - 1)]
MSigma_len = matrix(VSigma_len, nrow = 3, byrow = TRUE)
listCtl$Sigma_len   = MSigma_len
cV = cV + 3*diffGrow

listCtl$NMatrix   = fVector[cV:(cV + sum(listCtl$nregbyStock) - 1)] 
cV = cV + sum(listCtl$nregbyStock)

diffN = length(unique(listCtl$NMatrix))

VN_Mort = fVector[cV:(cV + 3*diffN - 1)]
MN_Mort = matrix(VN_Mort, nrow = 3, byrow = TRUE)
listCtl$N_Mort   = MN_Mort
cV = cV + 3*diffN

listCtl$npars_mage  = fVector[cV:(cV + diffN - 1)]
cV = cV + diffN

nparM = sum(listCtl$npars_mage)
if(nparM == 0) { 
listCtl$ages_M_changes = NA
listCtl$Mage_in        = NA
cV = cV } 
if(nparM > 0) {
  listCtl$ages_M_changes = fVector[cV:(cV + nparM - 1)] ; cV = cV + nparM
  listCtl$Mage_in = fVector[cV:(cV + nparM - 1)] ; cV = cV + nparM
}

listCtl$phase_Mage = fVector[cV:(cV + diffN - 1)] ; cV = cV + diffN
listCtl$Phase_Random_walk_M = fVector[cV:(cV + nStock - 1)]; cV = cV + nStock
listCtl$Nyrs_Random_walk_M = fVector[cV:(cV + nStock - 1)]; cV = cV + nStock

nranM = sum(listCtl$Nyrs_Random_walk_M)
if(nranM == 0) { 
listCtl$RW_M_yrs = NA
listCtl$RW_M_sigmas = NA
cV = cV } 
if(nranM > 0) {
  listCtl$RW_M_yrs = fVector[cV:(cV + nranM - 1)] ; cV = cV + nranM
  listCtl$RW_M_sigmas = fVector[cV:(cV + nranM - 1)] ; cV = cV + nranM
}

Vcatch = fVector[cV:(cV + 3*nIndex - 1)]
Mcatch = matrix(Vcatch, ncol = nIndex, byrow = TRUE)
listCtl$qMatrix = Mcatch
cV = cV + (3*nIndex)

Vqpow = fVector[cV:(cV + 3*nIndex - 1)]
Mqpow = matrix(Vqpow, ncol = nIndex, byrow = TRUE)
listCtl$qpowMatrix = Mqpow
cV = cV + (3*nIndex)

listCtl$RW_q_phases = fVector[cV:(cV + nIndex - 1)] ; cV = cV + nIndex
listCtl$RW_nyrs_q   = fVector[cV:(cV + nIndex - 1)] ; cV = cV + nIndex

nWalkq = sum(listCtl$RW_nyrs_q)
if(nWalkq == 0) { 
  listCtl$RW_q_yrs = NA
  listCtl$RW_q_sigmas = NA
  cV = cV
}
if(nWalkq > 0)  {
  listCtl$RW_q_yrs = fVector[cV:(cV + nWalkq - 1)] ; cV = cV + nWalkq
  listCtl$RW_q_sigmas = fVector[cV:(cV + nWalkq - 1)] ; cV = cV + nWalkq
}

listCtl$q_agemin = fVector[cV:(cV + nIndex - 1)]; cV = cV + nIndex
listCtl$q_agemax = fVector[cV:(cV + nIndex - 1)]; cV = cV + nIndex

listCtl$junk = fVector[cV]; cV = cV + 1
listCtl$n_proj_yrs = fVector[cV]; cV = cV + 1

FshInd = c(Fishery, Index)

FshInd <- c(paste0("F", seq(Fishery)), paste0("I", seq(Index)))

  for(i in seq_along(FshInd)){
    listCtl[[paste0(FshInd[i], "_info")]] = fVector[cV:(cV + 5)]
    cV = cV + 6
    if(listCtl[[paste0(FshInd[i], "_info")]][6] == 0) {
      listCtl[[paste0(FshInd[i], "_selchangeYear")]] = NA
      listCtl[[paste0(FshInd[i], "_selchange")]] = NA
      listCtl[[paste0(FshInd[i], "_selbyage")]] = fVector[cV:(cV + nAges - 1)]
      cV = cV + nAges
    } else {
      nChan = listCtl[[paste0(FshInd[i], "_info")]][6]
      listCtl[[paste0(FshInd[i], "_selchangeYear")]] = fVector[cV:(cV + nChan - 1)] ; cV = cV + nChan
      listCtl[[paste0(FshInd[i], "_selchange")]] = fVector[cV:(cV + nChan - 1)] ; cV = cV + nChan
      listCtl[[paste0(FshInd[i], "_selbyage")]] = fVector[cV:(cV + nAges - 1)]
      cV = cV + nAges
    }
  }


wtatage = fVector[cV:(cV + nStock*nAges - 1)]
Mwaa = matrix(wtatage, ncol = nAges, byrow = TRUE)
listCtl$Pwtatage = Mwaa
cV = cV + nStock*nAges

mtatage = fVector[cV:(cV + nStock*nAges - 1)]
Mmaa = matrix(mtatage, ncol = nAges, byrow = TRUE)
listCtl$Pmatatage = Mmaa
cV = cV + nStock*nAges

listCtl$test = fVector[cV]

return(listCtl)

}

.read.ctl = function(filename, info, infoDat){

Fishery = as.vector(info$fisheryNames)
Index = as.vector(info$indexModel)
nFishery = length(Fishery)
nIndex = length(Index)
nAges = infoDat$age[2]
nStock = info$nStock

res1      = scan(file = filename, what = 'numeric', quiet = TRUE, sep = "\n",
                 comment.char = "#", allowEscapes = TRUE)
res1      = strsplit(res1, "\t")

fVector = NULL
for(i in seq_along(res1)){
  res1[[i]] = paste(res1[[i]], collapse = " ")
  Vector = strsplit(res1[[i]], " ")[[1]]
  Vector = Vector [! Vector %in% ""]
  fVector = c(fVector, Vector)
}

listCtl = list()
cV = 1
listCtl$ControlFile = paste0("#", info$filename)
listCtl$dataFile  = fVector[cV] ;cV = cV + 1
listCtl$modelName = fVector[cV] ;cV = cV + 1

fVector = fVector[- c(1,2)]
fVector = as.numeric(fVector)

cV = 1
VFishery = fVector[cV:(2*(nFishery + nIndex))]
MFishery = matrix(VFishery, ncol = (nFishery + nIndex), byrow = TRUE)
cV = cV + (2*(nFishery + nIndex))

listCtl$SelMatrix = MFishery

listCtl$SrType      = fVector[cV]; cV = cV + 1
listCtl$AgeError    = fVector[cV]; cV = cV + 1
listCtl$Retro       = fVector[cV]; cV = cV + 1

listCtl$Steepness   = fVector[cV:(cV + 2)]
cV = cV + 3

listCtl$SigmaR   = fVector[cV:(cV + 2)]
cV = cV + 3

listCtl$yrs_sr   = fVector[cV:(cV + 1)]
cV = cV + 2

listCtl$Linf   = fVector[cV:(cV + 2)]
cV = cV + 3
listCtl$K   = fVector[cV:(cV + 2)]
cV = cV + 3
listCtl$Lo_len   = fVector[cV:(cV + 2)]
cV = cV + 3
listCtl$Sigma_len   = fVector[cV:(cV + 2)]
cV = cV + 3
listCtl$N_Mort   = fVector[cV:(cV + 2)]
cV = cV + 3


listCtl$npars_mage  = fVector[cV]
cV = cV + 1

nparM = listCtl$npars_mage
if(nparM == 0) {
  listCtl$Mage_in = NA
cV = cV} 
if(nparM > 0)  {
  listCtl$Mage_in = fVector[cV:(cV + nparM - 1)] ; cV = cV + nparM
}

listCtl$phase_Mage = fVector[cV] ; cV = cV + 1
listCtl$Phase_Random_walk_M = fVector[cV]; cV = cV + 1
listCtl$Nyrs_Random_walk_M = fVector[cV]; cV = cV + 1

nranM = listCtl$Nyrs_Random_walk_M
if(nranM == 0) {
  listCtl$RW_M_yrs = NA
  listCtl$RW_M_sigmas = NA
cV = cV }
if(nranM > 0)  {
  listCtl$RW_M_yrs = fVector[cV:(cV + nranM - 1)] ; cV = cV + nranM
  listCtl$RW_M_sigmas = fVector[cV:(cV + nranM - 1)] ; cV = cV + nranM
}


Vcatch = fVector[cV:(cV + 3*nIndex - 1)]
Mcatch = matrix(Vcatch, ncol = nIndex, byrow = TRUE)
listCtl$qMatrix = Mcatch
cV = cV + (3*nIndex)

Vqpow = fVector[cV:(cV + 3*nIndex - 1)]
Mqpow = matrix(Vqpow, ncol = nIndex, byrow = TRUE)
listCtl$qpowMatrix = Mqpow
cV = cV + (3*nIndex)

listCtl$RW_q_phases = fVector[cV:(cV + nIndex - 1)] ; cV = cV + nIndex
listCtl$RW_walk_q   = fVector[cV:(cV + nIndex - 1)] ; cV = cV + nIndex

nWalkq = sum(listCtl$RW_walk_q)
if(nWalkq == 0) {
  listCtl$RW_q_yrs = NA
  listCtl$RW_q_sigmas = NA
cV = cV }
if(nWalkq > 0)  {
  listCtl$RW_q_yrs = fVector[cV:(cV + nWalkq - 1)] ; cV = cV + nWalkq
  listCtl$RW_q_sigmas = fVector[cV:(cV + nWalkq - 1)] ; cV = cV + nWalkq
}

listCtl$q_agemin = fVector[cV:(cV + nIndex - 1)]; cV = cV + nIndex
listCtl$q_agemax = fVector[cV:(cV + nIndex - 1)]; cV = cV + nIndex

listCtl$junk = fVector[cV]; cV = cV + 1
listCtl$n_proj_yrs = fVector[cV]; cV = cV + 1

FshInd = c(Fishery, Index)


for(i in seq_along(FshInd)){
  listCtl[[paste0(FshInd[i], "_info")]] = fVector[cV:(cV + 5)]
  cV = cV + 6
  if(listCtl[[paste0(FshInd[i], "_info")]][6] == 0) {
    listCtl[[paste0(FshInd[i], "_selchangeYear")]] = NA
    listCtl[[paste0(FshInd[i], "_selchange")]] = NA
    listCtl[[paste0(FshInd[i], "_selbyage")]] = fVector[cV:(cV + nAges - 1)]
    cV = cV + nAges
  } else {
    nChan = listCtl[[paste0(FshInd[i], "_info")]][6]
    listCtl[[paste0(FshInd[i], "_selchangeYear")]] = fVector[cV:(cV + nChan - 1)] ; cV = cV + nChan
    listCtl[[paste0(FshInd[i], "_selchange")]] = fVector[cV:(cV + nChan - 1)] ; cV = cV + nChan
    listCtl[[paste0(FshInd[i], "_selbyage")]] = fVector[cV:(cV + nAges - 1)]
    cV = cV + nAges
  }
}

listCtl$test = fVector[cV]


return(listCtl)

}

.readPar = function(par, control, info){

  version = attr(control, which = "version")
  
  res1      = scan(file = par, what = 'numeric', quiet = TRUE, sep = "\n",
                   comment.char = "#", allowEscapes = TRUE)
  res1      = strsplit(res1, "\t") 

  fVector = NULL
  for(i in seq_along(res1)) {
    res1[[i]] = paste(res1[[i]], collapse = " ")
    Vector = strsplit(res1[[i]], " ")[[1]]
    Vector = Vector [! Vector %in% ""]
    fVector = c(fVector, Vector)
  }
  
  listCtl = control
  
  if(version == "2015MS"){
	  nReg = listCtl$nregbyStock
	  nStock = info$output$nStock
	  nPeriod = info$data$year[2] - info$data$year[1] + info$data$age[2]
	  diffRec = length(unique(listCtl$RecMatrix))
	  diffGrow = length(unique(listCtl$GrowMatrix))
	  diffN = length(unique(listCtl$NMatrix))
  } else {
	  nReg = 1
	  nStock = 1
	  nPeriod = info$data$year[2] - info$data$year[1] + info$data$age[2]
	  diffRec = 1
	  diffGrow = 1
	  diffN = 1

  }
  
  fVector = as.numeric(fVector)
  listPar = list()
  cV = 1
  listPar$N_Mort = fVector[cV:(cV + diffN - 1)]; cV = cV + diffN
  
  nparM = sum(listCtl$npars_mage)
  if(nparM == 0) cV = cV 
  if(nparM > 0)  {
  #   listPar$Mage_offset = fVector[cV:(cV + nparM - 1)] 
    cV = cV + nparM
  }
  
  nranM = sum(listCtl$Nyrs_Random_walk_M)
  if(nranM == 0) cV = cV 
  if(nranM > 0)  {
  #   listPar$M_rw = fVector[cV:(cV + nranM - 1)] ; 
    cV = cV + nranM
  }
  
  listPar$log_Linf = fVector[cV:(cV + diffGrow - 1)] ; cV = cV + diffGrow
  listPar$log_k    = fVector[cV:(cV + diffGrow - 1)] ; cV = cV + diffGrow
  listPar$log_Lo   = fVector[cV:(cV + diffGrow - 1)] ; cV = cV + diffGrow
  listPar$log_sdage = fVector[cV:(cV + diffGrow - 1)] ; cV = cV + diffGrow
  listPar$mean_log_rec = fVector[cV:(cV + sum(nReg) - 1)] ; cV = cV + sum(nReg)
  listPar$steepness = fVector[cV:(cV + diffRec - 1)] ; cV = cV + diffRec
  listPar$log_Rzero = fVector[cV:(cV + sum(nReg) - 1)] ; cV = cV + sum(nReg)
  cV = cV + nStock * nPeriod
  listPar$log_sigmar = fVector[cV:(cV + diffRec - 1)] ; cV = cV + diffRec

  return(listPar)
  
}
  
.ParTable = function(lstOuts){
  
  Out = list()
  
  for(m in seq_along(lstOuts)){ 

    
    version = lstOuts[[m]]$info$data$version
    listPar = lstOuts[[m]]$parameters
    listCtl = lstOuts[[m]]$control
    nStock = lstOuts[[m]]$info$output$nStock
    
    if(version != "2015MS") { 
      nReg = 1
      countM = 1
      countG = 1
      countR = 1
    } else {
      nReg = lstOuts[[m]]$control$nregbyStock
      countM = listCtl$NMatrix
      countG = listCtl$GrowMatrix
      countR = listCtl$RecMatrix
    }
    
    namesCol = NULL
    
    for(i in 1:nStock){
      for(k in 1:nReg[i]){
        temp = paste0("Stock_", i, "_Reg_", k)
        namesCol = c(namesCol, temp)
      }
    }
    
    
    namesPar = c("Natural Mortality", "log_Linf", "log_K", "log_Lo", "log_sdAge", 
                 "mean_log_Rec", "Steepness", "log_Rzero", "log_SigmaR")
    
    
    outMatrix = matrix(NA, ncol = length(namesCol), nrow = length(namesPar))
    rownames(outMatrix) = namesPar
    colnames(outMatrix) = namesCol
    
    
    for(i in 1:nrow(outMatrix)){
      
      if(i == 1){
        for(k in seq_along(countM)){
          if(k == 1) { outMatrix[i,k] = listPar[[i]][countM[k]] } 
          if(k > 1) {
            if(countM[k] != countM[k-1]) { outMatrix[i,k] = listPar[[i]][countM[k]] 
            } else { outMatrix[i,k] = outMatrix[i,(k-1)] }
          }
        }
      }
      
      if(i > 1 & i < 6){
        for(k in seq_along(countG)){
          if(k == 1) { outMatrix[i,k] = listPar[[i]][countG[k]] } 
          if(k > 1) {
            if(countG[k] != countG[k-1]) { outMatrix[i,k] = listPar[[i]][countG[k]] 
            } else { outMatrix[i,k] = outMatrix[i,(k-1)] }
          }
        }
      }
      
      if(i == 6 | i == 8){
        outMatrix[i,] = listPar[[i]]
      }
      
      if(i == 7 | i == 9){
        for(k in seq_along(countR)){
          if(k == 1) { outMatrix[i,k] = listPar[[i]][countR[k]] } 
          if(k > 1) {
            if(countR[k] != countR[k-1]) { outMatrix[i,k] = listPar[[i]][countR[k]] 
            } else { outMatrix[i,k] = outMatrix[i,(k-1)] }
          }
        }
      }
      
    }
    
    Out[[m]] = outMatrix
    
  }
  
  Name = NULL
  for(i in seq_along(lstOuts)){
    Name[i] = lstOuts[[i]]$info$output$model
  }  
  
  names(Out) = Name
  
  return(Out)
  
}


.LikeTable = function(lstOuts){

  Name = NULL
  Outs = list()
  for(i in seq_along(lstOuts)){
    for(j in seq_along(lstOuts[[i]]$output))
    Name[i] = lstOuts[[i]]$info$output$model
    Outs[[i]] = lstOuts[[i]]$output[[j]]
	}
 
  names(Outs) = Name
  tab = do.call(cbind, lapply(Outs, function(x){round(x$Like_Comp, 2)}))
  row.names(tab) = Outs[[1]]$Like_Comp_names
  
  return(tab)
}


.versionJJM = function(ctl) {

  header = scan(file = ctl, nlines = 10, what = "character", sep = "\n", 
               quiet = TRUE, comment.char = "#")
  header = header[! header %in% ""]
  
  resx = strsplit(header, "\t")  
  
  tVector = NULL
  for(i in seq_along(resx)) {
    resx[[i]] = paste(resx[[i]], collapse = " ")
    Vector = strsplit(resx[[i]], " ")[[1]]
    Vector = Vector [! Vector %in% ""]
    tVector = c(tVector, Vector)
  }
  
  x = tVector[4]
  idx = length(grep("[a-z]", x))
  if(idx > 0) out = "2015MS"
  if(idx == 0) out = "2014"
  
  return(out)

}


.ProjTable = function(lstOuts, Projections, Fmult, BiomProj, CapProj, MRS){


if(Projections){
		
		Fs = Fmult
		#Bp = BiomProj
		#Cp = CapProj
		mrs = MRS
		
		if(is.null(CapProj)) Cp = c(lstOuts[[1]]$info$data$year[2] + 1, lstOuts[[1]]$info$data$year[2] + 2) 
		else Cp = CapProj
		
		if(is.null(BiomProj)) { Bp = c(lstOuts[[1]]$info$data$year[2] + 2, 
		lstOuts[[1]]$info$data$year[2] + 6, lstOuts[[1]]$info$data$year[2] + 10) 
		} else { 
		Bp = BiomProj }
		
		if(is.null(Fmult)) Fs = c(0, 0.5, 0.75, 1, 1.25)
		else Fs = Fmult
		
		##################
		#for(i in seq_along(lstOuts)){
		#  for(j in seq_along(lstOuts[[i]]$output)){
		#    if(is.null(MRS)) mrs = mean(lstOuts[[i]]$output[[j]]$msy_mt[,10])
		#    else mrs = MRS
		#    }
		#}

		mrs = MRS
		
		Name = NULL
		for(i in seq_along(lstOuts)){
			Name[i] = lstOuts[[i]]$info$output$model
		}
		
		
		for(i in seq_along(lstOuts)){
			xa = strsplit(Name[i], split = "_")
			xb = paste0(xa[[1]][2:length(xa[[1]])], collapse = "") 
			Name[i] = paste(xa[[1]][1], xb, sep = "_")
		}
		
		
	tableTot = list()
	
	for(i in seq_along(lstOuts)){
	
		Outs = list()
		for(j in seq_along(lstOuts[[i]]$output)){
			Outs[[j]] = lstOuts[[i]]$output[[j]]
		}
		
	namesStock = paste0("Stock_", 1:length(Outs)) 
				
	tableTot[[i]] = list()	
		
	for(j in seq_along(Outs)){
	
	

		 if(is.null(MRS))  mrs = mean(Outs[[j]]$msy_mt[,10])
		 if(!is.null(MRS)) mrs = MRS
		
	
		
	fut = do.call(rbind,lapply(Outs[[j]][grep("SSB_fut_",names(Outs[[j]]))],
                   function(y){return(y[,1:3])}))
		
	#fut = do.call(rbind, lapply(Outs, function(x){
	#				do.call(rbind, lapply(x[grep("SSB_fut_",names(x))],
    #                      function(y){return(y[,1:3])}))})
  
	  fut = as.data.frame(fut, stringsAsFactors = F)
	  colnames(fut) = c("year", "SSB", "SD")
	  fut$modelscenario = paste(rep(names(Outs)[j], each=nrow(Outs[[j]]$SSB_fut_1) *
									   length(grep("SSB_fut_", names(Outs[[j]])))),
								 paste("Scen",
									   rep(1:length(grep("SSB_fut_", names(Outs[[j]]))), each=nrow(Outs[[j]]$SSB_fut_1)),
									   sep="_"),
								 sep="_")
	  fut$nModel = i
  
  assdato = mrs
  
  partName = NULL
  for(k in seq_along(Bp)){
    part = c(Bp[k], Bp[k])
    partName = c(partName, part)
  }
  
  FsName = lstOuts[[1]]$info$data$year[2]
  namesTabla = c(FsName,partName,Cp)
  
  #tabla   = list()
  
  #for(i in names(lstOuts)){
    
    #tabla[[i]] = matrix(NA, ncol = length(namesTabla[[2]]), nrow = length(Fs), dimnames = namesTabla)
	tabla = matrix(NA, ncol = length(namesTabla), nrow = length(Fs))
    rsktable  = matrix(NA, nrow = 1, ncol = length(grep("SSB_fut_", names(Outs[[j]]))),
                        dimnames = list(names(Outs)[j], 1:length(grep("SSB_fut_",names(Outs[[j]])))))
    
    for(k in seq_along(Bp)){
      
      futdat = fut[fut$year == Bp[k] & fut$nModel == i, ]
      Bs = futdat[,2][c(5:4,2:1,3)]
      rsktable[1,] = (1 - pnorm(mrs, futdat$SSB, futdat$SD))*100
      tabla[,1] = Fs
      tabla[,(k*2)] = round(Bs)
      tabla[,(k*2+1)] = round(rsktable[1,][c(5:4,2:1,3)])
      
    }
  #}
  
  catchPrj = list()
  for(k in seq_along(Cp)){
    catchPrj[[k]] = unlist(lapply(Outs[[j]][grep("Catch_fut_",names(Outs[[j]]))],
                                  function(y){y[y[,1]==Cp[k],2]}))
  }
  
  seqPos = (length(Bp)*2+2):length(namesTabla)

    for(k in seq_along(seqPos)){
      tabla[,seqPos[k]] = round(catchPrj[[k]][c(5:4,2:1,3)])
    }
	
	colnames(tabla) = namesTabla
	tabla = as.data.frame(tabla)
	tableTot[[i]][[j]] = tabla
	
	}
	
	names(tableTot[[i]]) = namesStock
	
	}
	
	names(tableTot) = Name
	
	}
	
	else { tableTot = NULL }

  return(tableTot)
  
}

.writeJJM = function(object, outFile, path = NULL, transpose=TRUE){
  
  outFile = if(is.null(path)) outFile else file.path(path, outFile)
  
  .writeFiles(object = object, outFile = outFile, transpose=transpose)
  
  return(invisible(NULL))
  
}

.writeFiles = function(object, outFile, transpose=TRUE) {

  object$ControlFile = NULL
  object$lengths = NULL
  if(!is.null(object$Fnames)) 
    object$Fnames = paste(object$Fnames, collapse="%")
  if(!is.null(object$Inames)) 
    object$Inames = paste(object$Inames, collapse="%")
  
  idx = names(which(!is.na(object)))
  
  listCtl = list()
  for(i in seq_along(idx)) {
    listCtl[[i]] = object[[idx[i]]]
  }
  names(listCtl) = idx
  
  for(i in seq(listCtl))
    listCtl[[i]] <- toWrite(listCtl[[i]], transpose=transpose)
  
  nams = paste("#", names(listCtl), sep = "") 
  
    for(i in seq_along(listCtl)) { 
      append = if(i==1) FALSE else TRUE
      cat(nams[i], "\n", file = outFile, append = append)
      for(j in 1:nrow(listCtl[[i]])) { 
        toOut = c(na.omit(listCtl[[i]][j,]))
        # toOut = paste(toOut, collapse="\t")
        if(length(toOut)>0)
          cat(toOut, "\n", file = outFile, append = TRUE)
        }
      }
  
}

toWrite = function(x, transpose=TRUE) {
  UseMethod("toWrite")
}

toWrite.default = function(x, transpose) t(as.matrix(x))

toWrite.matrix = function(x, transpose) {
  x = if(isTRUE(transpose)) t(x) else x
  return(x)
}

toWrite.array  = function(x, transpose) {
  y = apply(x, 3, function(x) list(x))
  y = lapply(y, FUN="[[", 1)
  # y = if(isTRUE(transpose)) lapply(y, t) else y
  return(do.call(rbind, y))
}



#.Fut_SSB_SD = function(lstOuts){
#  if(class(lstOuts)[1] == 'jjm.output') {
#    Name = lstOuts$output$info$model
#    lstOuts = list(lstOuts$output$output)
#  }else {
 #   Name = lstOuts$info
#    lstOuts = lstOuts$combined$output
#  }
  
#  names(lstOuts) = Name
#  fut = do.call(rbind, lapply(lstOuts, function(x){do.call(rbind, lapply(x[grep("SSB_fut_", names(x))],
#                                                                          function(y){return(y[,1:3])}))}))
#  fut = as.data.frame(fut, stringsAsFactors = FALSE)
#  colnames(fut) = c("year", "SSB", "SD")
#  
#  fut$modelscenario = paste(rep(names(lstOuts),
#                                 lapply(lstOuts, function(x) {nrow(x$SSB_fut_1)*length(grep("SSB_fut_", names(x)))})),
#                             paste("Scen",
#                                   as.vector(do.call(c, lapply(lstOuts, 
#                                                               function(x){rep(1:length(grep("SSB_fut_", names(x))),
#                                                                               each = nrow(x$SSB_fut_1))}))),
#                                   sep = "_"),
#                             sep = "_")
  
 # return(fut)
#}

#.SSB_SD = function(lstOuts){
#  if(class(lstOuts)[1] == 'jjm.output') {
#    Name = lstOuts$output$info$model
#    lstOuts = list(lstOuts$output$output)
#F  }else {
#    Name = lstOuts$info
#    lstOuts = lstOuts$combined$output
#  }
  
#  names(lstOuts) = Name
#  SSB_SD = do.call(rbind, lapply(lstOuts, function(x){x$SSB}))
  #  SSB_SD=lstOuts[[1]]$SSB
#  SSB_SD = SSB_SD[,1:3]
#  SSB_SD = as.data.frame(SSB_SD, stringsAsFactors = FALSE)
#  colnames(SSB_SD) = c("year", "SSB", "SD")
#  if(length(lstOuts) > 1){
#    SSB_SD$model = rep(names(lstOuts), lapply(lstOuts, function(x){nrow(x$SSB)}))}
#  
#  return(SSB_SD)
#}

.Puntual_SSB_SD = function(lstOuts,year){
  if(class(lstOuts)[1] == 'jjm.output') {
    Name = lstOuts$output$info$model
    lstOuts = list(lstOuts$output$output)
  }else {
    Name = lstOuts$info
    lstOuts = lstOuts$combined$output
  }
  
  names(lstOuts) = Name
  if(year > lstOuts[[1]]$SSB[nrow(lstOuts[[1]]$SSB), 1])
    stop(cat('Year should be lesser than ', lstOuts[[1]]$SSB[nrow(lstOuts[[1]]$SSB), 1]))
  
  ass = do.call(rbind, lapply(lstOuts, function(x){x$SSB[which(x$SSB[,1] == year), 1:3]}))
  ass = as.data.frame(ass, stringsAsFactors = FALSE)
  colnames(ass) = c("year", "SSB", "SD")
  # if(length(lstOuts)>1){ass$modelscenario = names(lstOuts)}
  return(ass)
}

.prepareCombine = function(...){
  
  modelList = deparse(substitute(list(...)))
  modelList = substr(modelList, start = 6, stop = nchar(modelList) - 1)
  modelList = unlist(strsplit(x = modelList, split = ", "))
  
  # Models in a list called 'allModels'
  allModels = list()
  for(i in 1:length(modelList)){
    allModels[[i]] = get(modelList[i])$output$output
  }
  
  result = list(
    modelList = modelList,
    allModels = allModels
  )
  
  return(result)
  
}


.combineSSB = function(models) {
  
  nModels = length(models)
  
  # Correction of SSB, R, TotBiom matrices:
  nFilas = numeric(length(models))
  for(i in seq_along(models)){
    nFilas[i] = nrow(models[[i]]$SSB)
  }
  
  
  minF = which.min(nFilas)[1]
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "SSB")
    FYear = models[[minF]]$SSB[1, 1]
    
    temp = models[[i]]
    temp = temp$SSB[which(temp$SSB[,1] == FYear):nrow(temp$SSB),]
    
    models[[i]]$SSB = temp
  }
  
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "SSB")
    FYear = models[[minF]]$SSB[nrow(models[[minF]]$SSB), 1]
    
    temp = models[[i]]
    temp = temp$SSB[1:which(temp$SSB[,1] == FYear),]
    
    models[[i]]$SSB = temp
  }
  
  
  # Empty matrix
  output = matrix(0, ncol = 5, nrow = nrow(models[[1]]$SSB))
  
  # Analysis
  output[,1] = models[[1]]$SSB[,1]
  for(i in seq(nModels)){
    output[,2] = rowSums(cbind(output[,2], models[[i]]$SSB[,2]))
    output[,3] = rowSums(cbind(output[,3], (models[[i]]$SSB[,3])^2))
  }
  
  output[,3] = sqrt(output[,3])
  
  for(i in seq(nModels)){
    output[,4] = output[,2] - 1.96*output[,3]
    output[,5] = output[,2] + 1.96*output[,3]
  }
  
  
  return(output)
  
}


.combineR = function(models) {
  
  nModels = length(models)
  
  # Correction of SSB, R, TotBiom matrices:
  nFilas = numeric(length(models))
  for(i in seq_along(models)){
    nFilas[i] = nrow(models[[i]]$R)
  }
  
  
  minF = which.min(nFilas)[1]
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "R")
    FYear = models[[minF]]$R[1, 1]
    
    temp = models[[i]]
    temp = temp$R[which(temp$R[,1] == FYear):nrow(temp$R),]
    
    models[[i]]$R = temp
  }
  
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "R")
    FYear = models[[minF]]$R[nrow(models[[minF]]$R), 1]
    
    temp = models[[i]]
    temp = temp$R[1:which(temp$R[,1] == FYear),]
    
    models[[i]]$R = temp
  }
  
  
  # Empty matrix
  output = matrix(0, ncol = 5, nrow = nrow(models[[1]]$R))
  
  # Analysis
  output[,1] = models[[1]]$R[,1]
  for(i in seq(nModels)){
    output[,2] = rowSums(cbind(output[,2], models[[i]]$R[,2]))
    output[,3] = rowSums(cbind(output[,3], (models[[i]]$R[,3])^2))
  }
  
  output[,3] = sqrt(output[,3])
  
  for(i in seq(nModels)){
    output[,4] = output[,2] - 1.96*output[,3]
    output[,5] = output[,2] + 1.96*output[,3]
  }
  
  
  return(output)
  
}


.combineTotBiom = function(models) {
  
  nModels = length(models)
  
  # Correction of SSB, R, TotBiom matrices:
  nFilas = numeric(length(models))
  for(i in seq_along(models)){
    nFilas[i] = nrow(models[[i]]$TotBiom)
  }
  
  
  minF = which.min(nFilas)[1]
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "TotBiom")
    FYear = models[[minF]]$TotBiom[1, 1]
    
    temp = models[[i]]
    temp = temp$TotBiom[which(temp$TotBiom[,1] == FYear):nrow(temp$TotBiom),]
    
    models[[i]]$TotBiom = temp
  }
  
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "TotBiom")
    FYear = models[[minF]]$TotBiom[nrow(models[[minF]]$TotBiom), 1]
    
    temp = models[[i]]
    temp = temp$TotBiom[1:which(temp$TotBiom[,1] == FYear),]
    
    models[[i]]$TotBiom = temp
  }
  
  # Empty matrix
  output = matrix(0, ncol = 5, nrow = nrow(models[[1]]$TotBiom))
  
  # Analysis
  output[,1] = models[[1]]$TotBiom[,1]
  for(i in seq(nModels)){
    output[,2] = rowSums(cbind(output[,2], models[[i]]$TotBiom[,2]))
    output[,3] = rowSums(cbind(output[,3], (models[[i]]$TotBiom[,3])^2))
  }
  
  output[,3] = sqrt(output[,3])
  
  for(i in seq(nModels)){
    output[,4] = output[,2] - 1.96*output[,3]
    output[,5] = output[,2] + 1.96*output[,3]
  }
  
  
  return(output)
  
}



.combineN = function(models) {
  
  nModels = length(models)
  
  # Correction of SSB, R, TotBiom matrices:
  nFilas = numeric(length(models))
  for(i in seq_along(models)){
    nFilas[i] = nrow(models[[i]]$N)
  }
  
  
  minF = which.min(nFilas)[1]
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "N")
    FYear = models[[minF]]$N[1, 1]
    
    temp = models[[i]]
    temp = temp$N[which(temp$N[,1] == FYear):nrow(temp$N),]
    
    models[[i]]$N = temp
  }
  
  for(i in seq_along(models)){
    index = which(names(models[[i]]) == "N")
    FYear = models[[minF]]$N[nrow(models[[minF]]$N), 1]
    
    temp = models[[i]]
    temp = temp$N[1:which(temp$N[,1] == FYear),]
    
    models[[i]]$N = temp
  }
  
  
  # Take in account if all models have the same number of age
  nAges = numeric(length(models))
  for(i in seq_along(models)){
    nAges[i] = ncol(models[[i]]$N)
  }
  
  
  if(length(unique(nAges)) == 1){
    
    # Empty matrix
    output = matrix(0, ncol = ncol(models[[1]]$N), nrow = nrow(models[[1]]$N))
    
    # Analysis
    output[,1] = models[[1]]$N[,1]
    
    for(i in seq(nModels)){
      output[, 2:ncol(output)] = output[, 2:ncol(output)] + models[[i]]$N[, 2:ncol(output)]
    }
    
    
  } else {
    
    output = NULL
  }
  
  return(output)
  
}



.combineCatchFut = function(models){
  
  nModels = length(models)
  
  # Take in account if all models have the same number of scenarios
  nScenarios = numeric(length(models))
  for(i in seq_along(models)){
    nScenarios[i] = length(grep("Catch_fut_", names(models[[i]])))
  }
  
  # Create Slots2
  Slots2 = c(paste0("Catch_fut_", seq(unique(nScenarios))))
  
  
  if(length(unique(nScenarios)) == 1){
    
    #Match the same years projection
    fYears = numeric(nModels)
    for(i in seq(nModels)){
      fYears[i] = models[[i]]$Catch_fut_1[1,1]
    }
    
    lYears = numeric(nModels)
    for(i in seq(nModels)){
      lYears[i] = models[[i]]$Catch_fut_1[nrow(models[[i]]$Catch_fut_1),1]
    }
    
    maxF = max(fYears)
    minL = min(lYears)
    
    for(i in seq(nModels)){
      for(j in Slots2){
        index = which(names(models[[i]]) == j)
        models[[i]][[index]] = models[[i]][[index]][which(models[[i]][[index]][,1] == maxF):which(models[[i]][[index]][,1] == minL), ]
      }
    }
    
    
    LastYear    = min(models[[1]]$Catch_fut_1[,1]) - 1
    NYearP      = nrow(models[[1]]$Catch_fut_1)
    YearsProy   = seq(from = (LastYear + 1), to = (LastYear + NYearP))
    nYearsProy  = length(YearsProy)
    
    # Empty matrix
    output = matrix(0, ncol = 2, nrow = nYearsProy)
    output = replicate(length(Slots2), output, simplify = FALSE)
    
    # Analysis (only sum)
    for(j in seq_along(Slots2)){
      output[[j]][,1] = YearsProy # por el momento se pone de frente
      
      for(i in seq(nModels)){
        output[[j]][,2] = rowSums(cbind(output[[j]][,2],
                                         models[[i]][[Slots2[j]]][,2]))
      }
    }
    
    # name to the list
    names(output) = Slots2
    
  } else {
    
    # the outcome is a NA's matrix
    output = replicate(length(Slots2), NA, simplify = FALSE)
    names(output) = Slots2
    
  }
  
  return(output)
}


.combineSSBFut = function(models){
  
  nModels = length(models)
  
  # Take in account if all models have the same number of scenarios
  nScenarios = numeric(length(models))
  for(i in seq_along(models)){
    nScenarios[i] = length(grep("SSB_fut_", names(models[[i]])))
  }
  
  # Create Slots2
  Slots2 = c(paste0("SSB_fut_", seq(unique(nScenarios))))
  
  
  if(length(unique(nScenarios)) == 1){
    
    fYears = numeric(nModels)
    for(i in seq(nModels)){
      fYears[i] = models[[i]]$SSB_fut_1[1,1]
    }
    
    lYears = numeric(nModels)
    for(i in seq(nModels)){
      lYears[i] = models[[i]]$SSB_fut_1[nrow(models[[i]]$SSB_fut_1),1]
    }
    
    maxF = max(fYears)
    minL = min(lYears)
    
    for(i in seq(nModels)){
      for(j in Slots2){
        index = which(names(models[[i]]) == j)
        models[[i]][[index]] = models[[i]][[index]][which(models[[i]][[index]][,1] == maxF):which(models[[i]][[index]][,1] == minL), ]
      }
    }
    
    LastYear    = min(models[[1]]$SSB_fut_1[,1]) - 1
    NYearP      = nrow(models[[1]]$SSB_fut_1)
    YearsProy   = seq(from = (LastYear + 1), to = (LastYear + NYearP))
    nYearsProy  = length(YearsProy)
    
    # Empty matrix
    output = matrix(0, ncol = 5, nrow = nYearsProy)
    output = replicate(length(Slots2), output, simplify = FALSE)
    
    # Analysis (only sum)
    for(j in seq_along(Slots2)){
      output[[j]][,1] = YearsProy # por el momento se pone de frente
      
      for(i in seq(nModels)){
        output[[j]][,2] = rowSums(cbind(output[[j]][,2],
                                         models[[i]][[Slots2[j]]][,2]))
        output[[j]][,3] = rowSums(cbind(output[[j]][,3],
                                         (models[[i]][[Slots2[j]]][,3])^2))
      }
      
      output[[j]][,3] = sqrt(output[[j]][,3])
      
      for(i in seq(nModels)){
        output[[j]][,4] = output[[j]][,2] - 1.96*output[[j]][,3]
        output[[j]][,5] = output[[j]][,2] + 1.96*output[[j]][,3]
      }
      
    }
    
    # name to the list
    names(output) = Slots2
    
  } else {
    
    # the outcome is a NA's matrix
    output = replicate(length(Slots2), NA, simplify = FALSE)
    names(output) = Slots2
    
  }
  
  return(output)
}


.writeCombinedStocks = function(combinedModel, output = "results", modelName = NULL){
  
  # Final Result
  if(is.null(modelName)) 
    writeList(combinedModel, file.path(output,"Combine_R.rep"), format = "P") 
  else 
    writeList(combinedModel, file.path(output, paste0(modelName, "_R.rep")), format = "P")
  
  return(invisible())
}



.resultCombined = function(..., modelName = modelName, output = "results"){
  
  listModels = .prepareCombine(...)
  
  finalList1 = list(
    SSB     = .combineSSB(models = listModels$allModels),
    R       = .combineR(models = listModels$allModels),
    TotBiom = .combineTotBiom(models = listModels$allModels),
    N       = .combineN(models = listModels$allModels)
  )
  
  finalList = c(
    finalList1,
    .combineCatchFut(models = listModels$allModels),
    .combineSSBFut(models = listModels$allModels)
  )
  
  
  # Length of the final list (to write in _R.rep)
  nNames = length(names(listModels$allModels[[1]]))
  
  # names to the final list
  outcome = replicate(nNames, NA, simplify = FALSE)
  names(outcome) = names(listModels$allModels[[1]])
  
  # Merge final list with output.merge
  for(i in seq_along(names(finalList))){
    index = which(names(finalList)[i] == names(outcome))
    outcome[[index]] = finalList[[i]]
  }
  
  .writeCombinedStocks(combinedModel = outcome, output = output, modelName = modelName)
  
  infoData = list(file = listModels$modelList,
                   variables = sum(!is.na(outcome)),
                   year = c(outcome$TotBiom[1, 1], outcome$TotBiom[nrow(outcome$TotBiom), 1]),
                   age = NULL,
                   length = NULL)
  
  output = list(info = list(model = NULL),
                 output = list(info = NULL, output = outcome, YPR = NULL),
                 data = list(info = infoData, data = NULL))
  
  class(output) = c("jjm.output")
  return(output)
  
}


.compareTime =  function(lstOuts, Slot = "TotBiom", SD = TRUE, Sum = NULL, startYear = NULL, legendPos = "topright",
                          xlim=NULL, ylim = NULL, yFactor = 1e-3, main = NA, ylab = Slot,
                          linesCol = NULL, lwd = 1, lty = 1, ...){
  
  dat = lapply(lstOuts$combined$outputs, function(x){return(x[[Slot]])})
  nms = names(dat)
  
  if(!is.null(Sum)){
    nms = c(nms, paste(Sum[1], "+", Sum[2], sep = ""))}
  
  nD = length(dat)
  if(is.null(startYear)){
    xrange = range(unlist(lapply(dat, function(x){x[,1]})), na.rm = TRUE)
  }else {
    xrange = c(startYear, range(unlist(lapply(dat, function(x) x[,1] )), na.rm = TRUE)[2])
  }
  
  if(is.null(xlim)) xlim = xrange
  
  dat = lapply(dat, function(x) { idx = which(x[,1] %in% xrange[1]:xrange[2]); 
                                   return(x[idx,]) } )
  
  if(is.null(ylim)) 
    ylim = range(pretty(range(unlist(lapply(dat, function(x) yFactor*x[,4:5] )), na.rm = TRUE)))
  
  if(is.null(linesCol))
    linesCol = rainbow(nD) else
      linesCol = rep(linesCol, length.out = nD)
  
  if(is.na(main))
    mar = c(3, 5, 2, 3) else
      mar = c(3, 5, 4, 3)
  
  par(mar = mar, xaxs = "i")
  
  plot(x = dat[[1]][,1], y = dat[[1]][,2]*yFactor, col = linesCol[1], type = "l", main = main,
       ylim = ylim, xlim = xlim, axes = FALSE, lwd = lwd, lty = lty, 
       ylab = ylab, xlab="", ...)
  
  axis(1)
  axis(2, las=2)
  
  for(i in 2:nD)
    lines(x = dat[[i]][,1], y = dat[[i]][,2]*yFactor, col = linesCol[i], lwd = lwd, lty = lty)
  
  if(!is.null(Sum)){
    idx1    = which(nms == Sum[1])
    idx2    = which(nms == Sum[2])
    datsum  = colSums(rbind(dat[[idx1]][,2], dat[[idx2]][,2]))
    
    lines(x = dat[[idx1]][,1], y = datsum*yFactor, col = nD + 1, lwd = lwd, lty = lty)
  }
  
  if(SD){
    for(i in 1:nD){
      polygon(x = c(dat[[i]][,1], rev(dat[[i]][,1])),
              y = c(dat[[i]][,4], rev(dat[[i]][,5]))*yFactor,
              col = adjustcolor(linesCol[i], alpha.f = 0.2), border = 0)
    }
  }
  
  legend(legendPos, legend = nms, col = linesCol, lwd = lwd, lty = lty, box.col = NA)
  box()
  
  return(invisible())
}

.compareMatrix = function(lstOuts, Slot = 'TotF', Sum = NULL, YrInd = FALSE, Apply = "mean", startYear = NULL,
                           legendPos = "topright", lwd = 1, lty = 1, xlab = NULL, ylab = NULL, 
                           linesCol = NULL, ...){
  
  lst = list(...)
  
  Apply = match.fun(Apply)
  
  dat = lapply(lstOuts$combined$outputs, function(x) x[[Slot]])
  nms = names(dat)
  
  if(!is.null(Sum)){
    nms = c(nms,paste(Sum[1], "+", Sum[2], sep = ""))
  }
  
  nD = length(dat)
  if(!YrInd){
    for(i in seq(nD)){
      dat[[i]] = cbind(lstOuts$combined$outputs[[i]]$Yr, dat[[i]])
    }
  }
  
  for(i in 1:nD) 
    dat[[i]] = cbind(dat[[i]][,1], apply(dat[[i]][,-1], 1, FUN=Apply))
  
  if(is.null(startYear)){
    xrange = range(unlist(lapply(dat, function(x) x[,1])), na.rm = TRUE)
  } else{ 
    xrange = c(startYear, range(unlist(lapply(dat, function(x) x[,1])), na.rm = TRUE)[2])}
  
  dat = lapply(dat, function(x){idx = which(x[,1] %in% xrange[1]:xrange[2]); return(x[idx,])})
  
  yrange = range(pretty(range(unlist(lapply(dat,function(x){x[,2]})), na.rm = TRUE)))
  
  if(!is.null(lst$ylim)) 
    yrange = lst$ylim
  
  if(!is.null(lst$xlim)) 
    xrange = lst$xlim
  
  if(is.null(xlab))
    xlab = "Years"
  
  if(is.null(ylab))
    ylab = Slot
  
  if(is.null(linesCol))
    linesCol = rainbow(nD) else
      linesCol = rep(linesCol, length.out = nD)
  
  if(!is.null(Sum)){
    idx1 = which(nms == Sum[1])
    idx2 = which(nms == Sum[2])
    datsum = colSums(rbind(dat[[idx1]][,2], dat[[idx2]][,2]))
    yrange = range(pretty(range(c(unlist(lapply(dat, function(x) x[,2])), datsum))))
  }
  
  plot(x = dat[[1]][,1], y = dat[[1]][,2], type = "l", lwd = lwd, lty = lty, 
       xlab = xlab, ylab = ylab, xlim = xrange, ylim = yrange, col = linesCol[1], ...)
  
  for(i in seq(2, nD))
    lines(x = dat[[i]][,1], y = dat[[i]][,2], col = linesCol[i], lwd = lwd, lty = lty)
  
  if(!is.null(Sum))
    lines(x = dat[[idx1]][,1], y = datsum, col = nD + 1, lwd = lwd, lty = lty)
  
  legend(legendPos,legend = nms, col = linesCol, lwd = lwd, lty = lty, 
         box.lty = 0, bty = "n")
  box()
  
  return(invisible())
}

.getParameters = function(patternList, myList) {
  
  list3 = NULL
  for(i in seq_along(patternList))
    if(names(patternList)[i] %in% names(myList))
      list3[[i]] = myList[[i]] else
        list3[[i]] = patternList[[i]]
  
  return(list3)
}

.getResume = function(typePlot, object) {
  formulaVector = NULL
  for(i in names(object[[typePlot]]))
  {
    if(class(object[[typePlot]][[i]]) == "list")
    {
      result = c(name = i, type = "List of plots")
    }else
    {
      result = c(name = i, type = "Single plot")
    }
    
    formulaVector = rbind(formulaVector, result)
  }
  
  return(formulaVector)
}

.getPath = function(path) {
  firstChar = substr(path, 1, 1)
  firstSecondChar = substr(path, 1, 2)
  if(firstSecondChar != "..")
  {
    if(firstChar == "/" | firstChar == "" | firstChar == ".")
      path = file.path(getwd(), path)
  }
  else
  {
    firstDir = unlist(strsplit(getwd(), split = .Platform$file.sep)[[1]])
    secondDir = unlist(strsplit(path, split = .Platform$file.sep)[[1]])
    m = gregexpr(pattern = paste("..",.Platform$file.sep,sep=""), text = path, fixed = TRUE)
    n = length(unlist(regmatches(path, m)[[1]]))
    firstDir = rev(rev(firstDir)[-(1:n)])
    secondDir = regmatches(path, m, invert = TRUE)[[1]][-(1:n)]
    path = paste(firstDir, sep=.Platform$file.sep, collapse = '/')
    path = file.path(path, secondDir)
  }
  
  return(path)
}

.getPath2 = function(path, pattern, target)
{
  output = list.files(path = path, recursive = TRUE, pattern = pattern)
  output = output[grep(x = output, pattern = target)]
  
  return(output)
}

.getPath3 = function(path, pattern, target, output="arc", ...)
{
  Dir = output
  output = list.files(path = path, recursive = TRUE, pattern = paste0(target, pattern))
  output = output[grep(x = output, pattern = Dir, fixed = TRUE)]
  output = output[grep(x = output, pattern = paste0(target, pattern))]
  
  return(output)
}

.cleanad = function() {
  cat("\n\tCleaning ADMB files...\n")
  file.remove(dir(pattern="tmp_admb"))
  file.remove(dir(pattern="varssave*"))
  file.remove(dir(pattern="cmpdiff*"))
  file.remove(dir(pattern="gradfil2*"))
  file.remove(dir(pattern="variance*"))
  file.remove(dir(pattern="~$"))
  file.remove(dir(pattern="\\.0.$"))
  file.remove(dir(pattern="\\.[r,p,b][0-9]"))
  
  exts = c("tmp", "dep", "log", "obj", ".o", "htp", "hes", "cov", 
           "rpt", "cor", "eva", "td2", "tds","tr2", "rep")
  
  for(ext in exts) {
    pat = sprintf("\\.%s$", ext)
    file.remove(dir(pattern=pat))
  }
}


.to = function(ext, output, model) {
  fmt = paste0("%s", ext)
  out = file.path(output, sprintf(fmt, basename(model)))
  return(out)
}

.runJJM2 = function(model, wait, ...) {
  system(paste("./run", model), wait = wait, ...)
  return(invisible())
}

.checkModels = function(models) {
  models = unique(models)
  check = file.exists(paste0(models, ".ctl"))
  if(any(!check)) {
    noCtl = models[!check]
    msg = paste("Ignoring non existing models:", 
                paste(noCtl, collapse=", "))
    message(msg)
    models = models[check]
  }
  
  if(length(models)<1) stop("No models to be run.")

  return(models)
}

.checkGuess = function(models, guess, output) {
  
  if(is.null(guess)) {
    guess = file.path(output, paste0(models, ".par"))
  } else {
    guess = file.path(output, guess)
  }
  if(length(guess)==1) {
    guess = rep(guess, length(models))
    warning("Using the same initial guess for all models.")
  }
  if(length(guess)!=length(models)) stop("Initial guess files for models do not match model length.")
  
  guess = normalizePath(guess, mustWork = FALSE)
  
  guess[!file.exists(guess)] = NA
  
  return(guess)
}

.setParallelJJM = function(model, input, exec, tmpDir=NULL) {
  
  if(is.null(tmpDir)) tmpDir = tempdir()
  tmpDir = file.path(tmpDir, basename(model))
  if(!file.exists(tmpDir)) dir.create(tmpDir)
  
  ctl = paste0(model, ".ctl") # ctl file
  dat = .getDatFile(ctl, input)

  jjm = if(Sys.info()[["sysname"]]=="Windows") "jjm.exe" else "jjm"
  
  file.copy(from=ctl, to=tmpDir, overwrite=TRUE)
  file.copy(from=dat, to=tmpDir, overwrite=TRUE)
  file.copy(from=exec, to=file.path(tmpDir, jjm), overwrite=TRUE)
  
  return(tmpDir)
  
}

.checkExecutable = function(exec, version) {
  # TO_DO: system.file
  if(is.null(exec)) exec = "jjm"
  
  exec = gsub(pattern="\\.exe$", replacement="", x=exec)
  
  if(Sys.info()[["sysname"]]=="Windows") exec = paste0(exec, ".exe")
  
  exec = normalizePath(exec, mustWork = FALSE)
  
  if(!file.exists(exec)) 
    stop(sprintf("Executable file %s not found.", exec))
  
  return(exec)
  
}

.getCtlFile = function(model, path) {
  ctl = paste0(model, ".ctl")
  ctl = if(is.null(path)) ctl else file.path(path, ctl)
  ctl = normalizePath(ctl, mustWork=FALSE)
  return(ctl)
}

.getDatFile = function(ctl, input=NULL) {
  if(is.null(input)) input = dirname(normalizePath(ctl, mustWork=FALSE))
  dat = scan(ctl, nlines=5, what="character", comment.char = "#", quiet=TRUE)[1]
  dat = normalizePath(file.path(input, dat), mustWork=FALSE)
  return(dat)
}

.getOutFile = function(model, output, ext) {
  out = file.path(output, paste0(model, ext)) # add path
  out = normalizePath(out)
  return(out)
}

.getParFile = function(model, output) .getOutFile(model, output, ext=".par")
.getYldFile = function(model, output) .getOutFile(model, output, ext=".yld")

.getRepFiles = function(model, output) {
  output = normalizePath(output, mustWork=FALSE)
  files = list.files(path = output, pattern = paste0(model, "_[0-9]*_R.rep$"))
  files = file.path(output, files)    
  return(files)
}



.getModelName = function(ctl) {
  modelName = scan(ctl, nlines=4, what="character", comment.char = "#",
                   quiet=TRUE)[2]
  modelName = gsub(x = modelName, pattern = " ", replacement = "")
  return(modelName)
}

.runJJM = function(model, output, input, exec, useGuess, guess, piner, iprint, wait, temp=NULL, ...) {
  
  cat("\nRunning model", model, "|", as.character(Sys.time()), "\n")
  
  tmpDir = .setParallelJJM(model=model, input=input, exec=exec, tmpDir=temp)  
  setwd(tmpDir)
  .cleanad()

  exec = if(Sys.info()[["sysname"]]=="Windows") "jjm" else "./jjm"
  
  jjm = if(isTRUE(useGuess) & !is.na(guess)) {
    sprintf("%s -nox -ind %s.ctl -ainp %s -iprint %d", exec,
            basename(model), guess, iprint)
  } else {
    sprintf("%s -nox -ind %s.ctl -iprint %d", exec, basename(model), iprint)
  }
  
  if(!is.null(piner)) {
    jjm = sprintf("%s -piner %f", jjm, piner)
  }
  
  start   = proc.time()  
  system(jjm, wait = TRUE, ...)
  elapsed = proc.time() - start
  
  cat("\n\tModel run finished. Time elapsed =", elapsed[3],"s.")
  cat("\n\tCopying output files...")
  
  Files = list.files(pattern = "_R_?[0-9]*\\.rep")
  
  # copy outputs to 'output' folder
  
  if(!file.exists(output)) {
    cat("\nCreating output directory...\n")
    dir.create(output, recursive = TRUE)
  }
  
  file.copy(from="jjm.par",   to=.to(".par", output, model), overwrite = TRUE)
  file.copy(from="jjm.rep",   to=.to(".rep", output, model), overwrite = TRUE)
  file.copy(from="jjm.std",   to=.to(".std", output, model), overwrite = TRUE)
  file.copy(from="jjm.cor",   to=.to(".cor", output, model), overwrite = TRUE)
  file.copy(from="Fprof.yld", to=.to(".yld", output, model), overwrite = TRUE)
  
  for(i in seq_along(Files)) {
	file.copy(from = Files[i], 
			  to=.to(paste0("_", i, "_R.rep"), output, model), overwrite = TRUE)
  }
  
  cat("\n\n")
  return(as.numeric(elapsed[3]))
}

toExpress = function(char.expressions){
  return(parse(text=paste(char.expressions,collapse=";")))
}

