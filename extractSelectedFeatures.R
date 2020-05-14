#' Extract the selected features from a model.
#'
#' Given a \code{\link{WrappedModel}}, selects the features which
#' are used in this model. This is useful, if feature selection is performed in the model
#' fitting process (e.g. by a filter wrapper or by embedded feature selection).
#'
#' @param model [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @return [\code{character(n)}]\cr
#'   Names of selected features.
#' @export
#' @examples
#' lrn = makeLearner("classif.LiblineaRL1LogReg", cost = 0.5, epsilon = 1e-4)
#' mod = train(lrn, sonar.task)
#' extractSelectedFeatures(mod)
#' @note Currently, this function only works for some classification models.
#' To write a feature extraction method for models fitted using any other learner see
#' \code{\link{extractSelectedFeaturesInternal}}.


#' @export
extractSelectedFeatures = function(model) {
  extractSelectedFeaturesInternal(model$learner, model)
}
