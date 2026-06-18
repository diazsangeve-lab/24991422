# I read the anonymised Lending Club extract and kept only the columns the brief
# calls for.
load_loans <- function(path = "data/Loan_Cred/loan_data.rds"){

    keep <- c("loan_amnt", "term", "int_rate", "grade", "sub_grade", "emp_length",# loan and grade
              "home_ownership", "annual_inc", "verification_status", "issue_d", # borrower basics
              "loan_status", "purpose", "addr_state", "dti", "delinq_2yrs", # outcome and risk
              "earliest_cr_line", "inq_last_6mths", "pub_rec", "revol_util") # credit-file fields

    read_rds(path) %>% # load the one-million-row extract
        select(any_of(keep)) # drop the columns we do not use
}
