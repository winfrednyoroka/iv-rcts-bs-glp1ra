##########################################################################################
# Script for identifying studies with multiple arms
# Detects these multiple arms and creates new labels
# Multi-intervention or control, add labels to the tune of A, B, C, D, etc 
# depending on number of arms
# Save as rds object to be used for data driven visualisation and IV estimation
#########################################################################################
#################################################
# Load libraries and custom functions----
#################################################
source("R/shared/setup.R")

####################################################
# Read in the data ----
###################################################
baseline_post <- readRDS('data/bs/processed/post_results_updated.rds')
glimpse(baseline_post)

# Create a author_year column
baseline_post$author_year <- paste0(baseline_post$last_name," (", baseline_post$publication_year, ")")
glimpse(baseline_post)

# Look up treatment groups and create ARMIDs
arm_lookup <- baseline_post |> 
  distinct(ID, ARMID, treatment_group) |> 
  arrange(ID, treatment_group, ARMID) |> 
  group_by(ID, treatment_group) |> 
  mutate(
    ARMID_new = if (n() == 1) {
      ARMID
    } else {
      paste0(ARMID, "_", LETTERS[seq_len(n())])
    }
  ) |> 
  ungroup()

print(arm_lookup, n =51)

# Merge the labels to the data
baseline_post <- baseline_post |> 
  left_join(
    arm_lookup |> 
      select(ID, ARMID, ARMID_new),
    by = c("ID", "ARMID")
  )
glimpse(baseline_post)

print(baseline_post$ARMID_new)
print (baseline_post |> 
  select((ncol(.)-4):ncol(.)), n=110)

# Create a new labels (author, year and the treatment name)
baseline_post <- baseline_post |> 
  mutate(
    study_arm_label = paste0(
      author_year, ' ',
      sub("^\\d+_", "", ARMID_new)
    )
  )
glimpse(baseline_post)
#############################################################
# Save the data to be used in data driven visualisation ----
# Baseline vs post-intervention
# Euclidean distance
# T-test 4 hypothesis testing
############################################################
saveRDS(baseline_post, file = 'data/bs/processed/baseline_post_relabelmultiarms.rds')