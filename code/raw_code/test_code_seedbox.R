#Install and loading R packages
install.packages(c("ggplot2", "tidyr", "reshape2", "dplyr", "janitor", "skimr", "stringr"))
library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)
library(janitor)
library(skimr)
library(stringr)

#Create tables with just the control group and test group entries
control_group_samp <- testSamples %>% filter(test_group = "0")
test_group_samp <- testSamples %>% filter(test_group = "1")

#Join control group sample table with transData table
control_group_joined <- control_group_samp %>% 
  left_join(transData, control_group_samp, by = c("sample_id" = "sample_id"))

#Separate control group joined table into rebills and refund instances
control_group_joined_rebill <- control_group_joined %>% filter(transaction_type == "REBILL")
control_group_refund <- control_group_joined %>% filter(grepl('REFUND|CHARGEBACK', transaction_type))

#Set rebill and refund tables so that there are only unique sample_ids
unique_control_joined <- distinct(control_group_joined, sample_id, .keep_all = TRUE) %>% filter(transaction_type != "NA")
unique_control_rebill <- distinct(control_group_joined_rebill, sample_id, .keep_all = TRUE)
unique_control_refund <- distinct(control_group_refund, sample_id, .keep_all = TRUE)

#Now do the same for the test group
#Join test group sample table with transData table
test_group_joined <- test_group_samp %>% 
  left_join(transData, test_group_samp, by = c("sample_id" = "sample_id"))

#Separate control group joined table into rebills and refund instances
test_group_rebill <- test_group_joined %>% filter(transaction_type == "REBILL")
test_group_refund <- test_group_joined %>% filter(grepl('REFUND|CHARGEBACK', transaction_type))

#Set rebill and refund tables so that there are only unique sample_ids
unique_test_joined <- distinct(test_group_joined, sample_id, .keep_all = TRUE) %>% filter(transaction_type != "NA")
unique_test_rebill <- distinct(test_group_rebill, sample_id, .keep_all = TRUE)
unique_test_refund <- distinct(test_group_refund, sample_id, .keep_all = TRUE)

#Add new columns to control and test data filtered by unique sample_ids, REBILL = 1, REFUND/CHARGEBACK = 0
unique_control_joined <- unique_control_joined %>% mutate(customer_rebill = case_when(transaction_type == "REBILL" ~ "1", transaction_type == "REFUND" | transaction_type == "CHARGEBACK" ~ "0")
                                  )
unique_test_joined <- unique_test_joined %>% mutate(customer_rebill = case_when(transaction_type == "REBILL" ~ "1", transaction_type == "REFUND" | transaction_type == "CHARGEBACK" ~ "0")
)

#Tabulate customers who rebilled vs. refunded
tabulated_rebills <- data.frame(rebill  = c(941, 1556),
                                       refund = c(141, 87))
chisq.test(tabul)

