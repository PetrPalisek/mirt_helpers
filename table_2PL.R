table_2PL <- function(model) {
  require(mirt)
  require(tidyverse)
  coef(model, IRTpars = T, as.data.frame = T, SE = T) %>% 
  round(2) %>%  data.frame() %>% rownames_to_column(var = "rowname") %>%
  filter(grepl("\\.(a|b)$", rowname)) %>%
  rename(`Název parametru` = rowname, `Odhad parametru` = par,
         `Dolní mez 95%CI` = CI_2.5, `Horní mez 95%CI` = CI_97.5) %>%
  separate(`Název parametru`, into = c("Položka", "Parametr"), sep = "\\.") %>%
  mutate(Položka = ifelse(row_number() %% 2 == 0, "", Položka))} 