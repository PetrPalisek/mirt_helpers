report_M2 <- function(fit_object){
  fit_object <- round(fit_object,3)
  p <- ifelse(fit_object$p == 0, "p < .001", paste0("p = ", fit_object$p, sep = ""))
  paste0("M2", "(", fit_object$df, ")", " ", "=", " ", fit_object$M2,", ",
         p, ", ",
         "RMSEA = ", fit_object$RMSEA, " [", fit_object$RMSEA_5, " ; ", fit_object$RMSEA_95, "]", ", ",
         "TLI = ", fit_object$TLI, ", ",
         "SRMSR = ", fit_object$SRMSR,
         sep = "")
}
