#' Internal method for extracting the selected features from a model.
#'
#' S3 method for selecting the features which are used in a model, given
#' the \code{\link{WrappedModel}} and the \code{\link{Learner}} which was used to
#' train the model.
#'
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @param learner [\code{\link{Leaner}}]\cr
#'   The learner which was used to train the model. This internal function will
#'   always be called with \code{learner} set to \code{model$learner}.
#' @return [\code{character(n)}]\cr
#'   Names of selected features.
#' @export
#' @note Only exported for extension purposes. For usage see
#' \code{\link{extractSelectedFeatures}}.


#' @export
extractSelectedFeaturesInternal = function(learner, model) {
  UseMethod("extractSelectedFeaturesInternal")
}

#' @export
extractSelectedFeaturesInternal.BaseWrapper = function(learner, model) {
  extractSelectedFeaturesInternal(model$learner.model$next.model$learner, model$learner.model$next.model)
}

#' @export
extractSelectedFeaturesInternal.classif.featureless = function(learner, model) {
  return(character(0L))
}

extractSelectedFeaturesInternal.classif.glmnet = function(learner, model) {
  if (length(model$features) == 0L) {
    return(character(0L))
  } else {
    co = coef(model$learner.model, s = model$learner$par.vals$s)
    feats = rownames(co)[abs(co[, 1]) > 0]
    feats = setdiff(feats, "(Intercept)")
    return(feats)
  }
}

#' @export
extractSelectedFeaturesInternal.classif.glmboost = function(learner, model) {
  if (length(model$features) == 0L) {
    return(character(0L))
  } else {
    coef = unlist(model$learner.model$coef())
    if (is.null(coef)) {
      return(character(0L))
    } else {
      feats = setdiff(names(coef), "(Intercept)")
      return(feats)
    }
  }
}

#' @export
extractSelectedFeaturesInternal.classif.kknn = function(learner, model) {
  return(model$features)
}

#' @export
extractSelectedFeaturesInternal.classif.ksvm = function(learner, model) {
  return(model$features)
}

#' @export
extractSelectedFeaturesInternal.classif.LiblineaR = function(learner, model) {
  w = model$learner.model$W
  if (is.null(w)) {
    return(character(0L))
  } else {
    v = apply(w, 2, function(x) any(abs(x) > 0))
    feats = colnames(w)[v]
    feats = setdiff(feats, "Bias")
    return(feats)
  }
}

#' @export
extractSelectedFeaturesInternal.classif.LiblineaRL1L2SVC = function(learner, model) {
  extractSelectedFeaturesInternal.classif.LiblineaR(learner, model)
}

#' @export
extractSelectedFeaturesInternal.classif.LiblineaRL1LogReg = function(learner, model) {
  extractSelectedFeaturesInternal.classif.LiblineaR(learner, model)
}

#' @export
extractSelectedFeaturesInternal.classif.LiblineaRL2L1SVC = function(learner, model) {
  extractSelectedFeaturesInternal.classif.LiblineaR(learner, model)
}

#' @export
extractSelectedFeaturesInternal.classif.LiblineaRL2LogReg = function(learner, model) {
  extractSelectedFeaturesInternal.classif.LiblineaR(learner, model)
}

#' @export
extractSelectedFeaturesInternal.classif.LiblineaRL2SVC = function(learner, model) {
  extractSelectedFeaturesInternal.classif.LiblineaR(learner, model)
}

#' @export
extractSelectedFeaturesInternal.classif.LiblineaRMultiClassSVC = function(learner, model) {
  extractSelectedFeaturesInternal.classif.LiblineaR(learner, model)
}

#' @export
extractSelectedFeaturesInternal.classif.ranger = function(learner, model) {
  if (length(model$features) == 0) {
    return(character(0L))
  } else{
    v = drop(model$learner.model$variable.importance)
    if (is.null(v)) {
      stop("For extracting the selected features of classif.ranger set 'importance' to 'impurity' or 'permutation'!")
    } else {
      feats = names(v)[which(v > 1e-16)]
      return(feats)
    }
  }
}

#' @export
extractSelectedFeaturesInternal.classif.rda = function(learner, model) {
  return(model$features)
}

#' @export
extractSelectedFeaturesInternal.classif.rpart = function(learner, model) {
  vars = levels(model$learner.model$frame$var)
  if (is.null(vars)) {
    return(character(0L))
  } else {
    vars = setdiff(vars, "<leaf>")
    return(vars)
  }
}

#' @export
extractSelectedFeaturesInternal.classif.xgboost = function(learner, model) {
  all.feats = model$features
  if (length(all.feats) == 0L) {
    return(character(0L))
  } else {
    importance = xgboost::xgb.importance(model = model$learner.model, feature_names = all.feats)
    feats = importance$Feature
    return(feats)
  }
}
