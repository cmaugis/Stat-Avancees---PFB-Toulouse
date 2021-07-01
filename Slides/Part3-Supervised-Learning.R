perf <- function(prob, truth, 
                 what = c("ROC", "Precision-Recall", "Sensitivity-Specificity"), 
                 ...) {
    what <- match.arg(what)
    measures <- switch(what,
                       "ROC" = list(x = "fpr", y = "tpr"),
                       "Precision-Recall" = list(x = "rec", y = "prec"),
                       "Sensitivity-Specificity" = list(x = "spec", y = "sens"))
    performance(prediction(prob, truth), 
                measure = measures$y, x.measure = measures$x,
                ...)
}

tidy_perf <- function(prob, truth, method, what = "ROC") {
    roc <- perf(prob, truth, what = what)
    df <- data.frame(x = roc@x.values[[1]], y = roc@y.values[[1]])
    names(df) <- c(roc@x.name, roc@y.name)
    cbind(df, method = method)
}
