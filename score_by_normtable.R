# Function to perform scoring
score_by_normtable <- function(data, norm_table, id_col, theta_col, theta_se_col, sum_col, group_col, norm_group_col) {
  # Select relevant columns and rename them for internal consistency
  scores <- data %>%
    select(!!sym(id_col), !!sym(theta_col), !!sym(theta_se_col), !!sym(group_col), !!sym(sum_col)) %>%
    rename(
      id = !!sym(id_col),
      theta = !!sym(theta_col),
      theta_SE = !!sym(theta_se_col),
      group = !!sym(group_col),
      total = !!sym(sum_col)
    )
  
  # Compute W
  scores <- scores %>%
    mutate(
      W = 9.1024 * theta + 500,
      IQ = NA_real_,
      lower = NA_real_,
      higher = NA_real_,
      pct = NA_real_
    )
  
  # Iterate through each row to calculate IQ, lower, higher, and pct
  for (i in 1:nrow(scores)) {
    group <- scores$group[i]
    group_stats <- norm_table %>% 
      filter(!!sym(norm_group_col) == group) %>%
      slice(1)  # Ensure only one row is selected
    
    if (nrow(group_stats) == 1) {  # Check if one row was found
      group_mean <- group_stats$M
      group_sd <- group_stats$SD
      
      theta <- scores$theta[i]
      theta_SE <- scores$theta_SE[i]
      
      # Calculate scores
      IQ <- 100 + ((theta - group_mean) / group_sd) * 15
      lower <- 100 + ((theta - group_mean - 1.96 * theta_SE) / group_sd) * 15
      higher <- 100 + ((theta - group_mean + 1.96 * theta_SE) / group_sd) * 15
      pct <- pnorm(IQ, mean = 100, sd = 15) * 100
      
      # Assign calculated values to the data frame
      scores$IQ[i] <- round(IQ, 1)
      scores$lower[i] <- round(lower, 1)
      scores$higher[i] <- round(higher, 1)
      scores$pct[i] <- round(pct, 2)
    } else {
      # Handle case where no or multiple rows match the group
      scores$IQ[i] <- NA_real_
      scores$lower[i] <- NA_real_
      scores$higher[i] <- NA_real_
      scores$pct[i] <- NA_real_
    }
  }
  
  # Rename columns back to original using !!sym()
  scores <- scores %>%
    rename(
      !!sym(id_col) := id,
      !!sym(theta_col) := theta,
      !!sym(theta_se_col) := theta_SE,
      !!sym(group_col) := group,
      !!sym(sum_col) := total
    )
  
  return(scores)
}
