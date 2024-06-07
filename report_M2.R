report_M2 <- function(fit_object) {
  # Round the fit_object values to 3 decimal places
  fit_object <- round(fit_object, 3)
  
  # Format p-value
  p <- ifelse(fit_object$p == 0, "p < .001", paste0("p = ", fit_object$p))
  
  # Construct the main part of the report
  report <- paste0(
    "M2(", fit_object$df, ") = ", fit_object$M2, ", ",
    p, ", ",
    "RMSEA = ", fit_object$RMSEA, " [", fit_object$RMSEA_5, " ; ", fit_object$RMSEA_95, "], ",
    "TLI = ", fit_object$TLI
  )
  
  # Add SRMSR values if they exist in the fit_object
  srmsr_names <- grep("SRMSR", names(fit_object), value = TRUE)
  if (length(srmsr_names) > 0) {
    srmsr_values <- sapply(srmsr_names, function(name) {
      paste0(name, " = ", fit_object[[name]])
    })
    report <- paste(report, paste(srmsr_values, collapse = ", "), sep = ", ")
  }
  
  return(report)
}

