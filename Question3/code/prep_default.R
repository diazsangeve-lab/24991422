# Turning the raw loans into a clean analysis frame. Following the brief's tip, a loan
# has a final outcome only once it is fully paid or written off as a loss, so I
# keep those and build a 0/1 default flag. Current, late and grace loans are still
# running and are set aside.
prep_default <- function(df){

    resolved <- c("Fully Paid", "Charged Off", "Default") # loans with a final outcome

    df %>%
        filter(loan_status %in% resolved) %>% # drop ongoing loans
        mutate(default  = as.integer(loan_status %in% c("Charged Off", "Default")), # 1 = charged off as a loss
               grade    = factor(grade, levels = LETTERS[1:7]), # A best through G worst
               short    = term == "36 months",  # short-term loan flag
               owner    = home_ownership %in% c("OWN", "MORTGAGE"), # owns property, mortgage or outright
               emp10    = emp_length == "10+ years", # ten or more years employed
               dti      = ifelse(dti >= 0 & dti <= 60, dti, NA_real_),# trim the -1 and 999 codes
               hist_yrs = as.numeric(lubridate::my(issue_d) - # credit-history length in years,
                                     lubridate::my(earliest_cr_line)) / 365.25) # the closest thing to an age proxy
}
