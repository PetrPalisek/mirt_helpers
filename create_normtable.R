# Function to create a normalized table for multiple groups
create_normtable <- function(model, group_ids) {
  # Initialize an empty list to store the group data
  group_data <- list()
  
  # Loop through each group ID
  for (group_id in group_ids) {
    # Extract the GroupPars coefficients for the current group
    group_pars <- coef(model, IRTpars = TRUE)[[as.character(group_id)]][["GroupPars"]]
    
    # Create a data frame for the current group with M and SD
    group_data[[group_id]] <- data.frame(
      M = group_pars[1],
      SD = sqrt(group_pars[1, 2]),
      group = group_id
    )
  }
  
  # Combine all group data into a single data frame
  norm_table <- bind_rows(group_data)
  
  norm_table[1,2] <- 1
  
  return(norm_table)
}
