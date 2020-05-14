createAndResampleLearner = function(data, name, ps, fw.method) {
  cl = createLearner(name = name, ps = ps, fw.method = fw.method)
  res = resampling(lrn = cl$lrn, data = data)
  result = c(res, list(pars = cl$pars))
  return(result)
}


createLearner = function(name, ps, fw.method) {
  pars = as.list(generateDesign(1, ps, trafo = TRUE))

  classes = sapply(pars, class)
  for (i in which(classes == "factor")) {
    pars[[i]] = varhandle::unfactor(pars[[i]])
  }

  lrn = makeLearner(name)
  lrn = setHyperPars(lrn, par.vals = pars)

  if (fw.method != "none") {
    fw.perc = runif(1)
    pars = c(pars, list(fw.perc = fw.perc))
    lrn = makeFilterWrapper(lrn, fw.method = fw.method, fw.perc = fw.perc)
  }

  return(list(lrn = lrn, pars = pars))
}


assessStability = function(features, p, sim.mat) {
  lsm = listStabilityMeasures()
  values = times = numeric(nrow(lsm))

  for (i in seq_len(nrow(lsm))) {
    fun = get(lsm$Name[i])
    if (lsm$Adjusted[i]) {
      t1 = Sys.time()
      values[i] = fun(features = features, sim.mat = sim.mat, impute.na = 0)
      t2 = Sys.time()
      times[i] = as.numeric(difftime(t2, t1), units = "secs")
    } else {
      t1 = Sys.time()
      values[i] = fun(features = features, p = p, impute.na = 0)
      t2 = Sys.time()
      times[i] = as.numeric(difftime(t2, t1), units = "secs")
    }
  }

  values = as.list(values)
  times = as.list(times)
  names(values) = names(times) = lsm$Name

  result = list(values = values, times = times)
  return(result)
}


resampling = function(lrn, data) {
  source("extractSelectedFeatures.R")
  source("extractSelectedFeaturesInternal.R")

  # resampling on train data set
  r.train = resample(learner = lrn, task = data$rins$task.train, resampling = data$rins$rin.train,
    models = FALSE, extract = extractSelectedFeatures, measures = list(mmce), show.info = FALSE)

  mmce.train = unname(r.train$aggr)
  features.train = r.train$extract
  mean.size.train = mean(lengths(features.train))
  stability.train = assessStability(features.train, p = getTaskNFeats(data$rins$task.train),
    sim.mat = data$sim.mats$sim.mat.train)


  # resampling on test data set
  r.test = resample(learner = lrn, task = data$rins$task.test, resampling = data$rins$rin.test,
    models = FALSE, extract = extractSelectedFeatures, measures = list(mmce), show.info = FALSE)

  mmce.test = unname(r.test$aggr)
  features.test = r.test$extract
  mean.size.test = mean(lengths(features.test))
  stability.test = assessStability(features.test, p = getTaskNFeats(data$rins$task.test),
    sim.mat = data$sim.mats$sim.mat.test)


  res = list(
    mmce.train = mmce.train, mmce.test = mmce.test,
    mean.size.train = mean.size.train, mean.size.test = mean.size.test,
    stability.train = stability.train, stability.test = stability.test,
    features.train = features.train, features.test = features.test)

  return(res)
}

