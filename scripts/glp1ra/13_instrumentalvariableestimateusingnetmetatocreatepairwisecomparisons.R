#
# Load the libraries and custom functions where necessary
source('R/shared/setup.R')
source('R/glp1ra/waldratio_estimatorforglp1raonly.R')


####################################################
# Read in the data ----
###################################################
baseline_post <- readRDS('data/glp1ra/processed/post_results_updated.rds')
glimpse(baseline_post)
unique(baseline_post$ID)
print(baseline_post, n=15)#####
# Create a author_year column
baseline_post$author_year <- paste0(baseline_post$last_name," (", baseline_post$publication_year, ")")
glimpse(baseline_post)
unique(baseline_post$author_year)

# Make short readable treatment arm names
baseline_post <- baseline_post |> 
  mutate(
    arm_name  = ARMID |> 
      str_remove("^\\d+_") |> 
      str_remove("_(Intervention|Control)$")
  )
glimpse(baseline_post)

# Filter out Wharton
baseline_post <- baseline_post|> 
  filter( !grepl("Wharton", author_year))
#filter(pw, author_year == 'Lam (2022)')
baseline_post <- baseline_post |> 
  filter( !grepl ('Lam', author_year))
baseline_post

# Hand calculate IV estimate for two arms (manual- hands on)
baseline_post |> 
  filter(grepl('Frandsen',author_year)) |> 
  select(Outcome, treatment_group,post_mean, post_lowerbound, post_upperbound, post_samplesize)
# Create unique ID by time in months
baseline_post$study_id <- with(
  baseline_post,
  paste(author_year, Outcome, post_time_months, sep = "_")
)
glimpse(baseline_post)
# BMI subset
bmi <- subset(baseline_post, Outcome == "BMI")
bmi


pw_bmi <- pairwise(
  treat = ARMID,
  mean = post_mean,
  sd = post_sd,
  n = post_samplesize,
  studlab = study_id,
  data = bmi,
  sm = "MD"
)

glimpse(pw_bmi)

#######################
# check data sanity before proceeding
sort(unique(pw_bmi$treat1))
sort(unique(pw_bmi$treat2))
head(pw_bmi[, c("studlab", "treat1", "treat2")])

pw_bmi$treat1 <- sub("^[0-9]+_", "", pw_bmi$treat1)
pw_bmi$treat2 <- sub("^[0-9]+_", "", pw_bmi$treat2)

sort(unique(pw_bmi$treat1))
glimpse(pw_bmi)
#######################
# Network connection
nc <- netconnection( treat1 = pw_bmi$treat1,
                   treat2 = pw_bmi$treat2,
                   studlab = pw_bmi$studlab)
print(nc)
nc <- netconnection(pw_bmi)
nc
print(nc, details = TRUE)

# netgraph
netgraph(netconnection(
           treat1 = pw_bmi$treat1,
         treat2 = pw_bmi$treat2,
        studlab = pw_bmi$studlab) )
netgraph(  netconnection(
    treat1 = pw_bmi$treat1,
    treat2 = pw_bmi$treat2,
    studlab = pw_bmi$studlab ),
   plastic = FALSE,
   number.of.studies = TRUE)

######################Netmeta for BMI only
bmi_net <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = pw_bmi
)

BMI_net <- as.data.frame(bmi_net)
glimpse(BMI_net)


##### SBP subset-----
sbp <- subset(baseline_post, Outcome == "SBP")
sbp

pw_sbp <- pairwise(
  treat = ARMID,
  mean = post_mean,
  sd = post_sd,
  n = post_samplesize,
  studlab = study_id,
  data = sbp,
  sm = "MD"
)

glimpse(pw_sbp)

#######################
# check data sanity before proceeding
sort(unique(pw_sbp$treat1))
sort(unique(pw_sbp$treat2))
head(pw_sbp[, c("studlab", "treat1", "treat2")])

pw_sbp$treat1 <- sub("^[0-9]+_", "", pw_sbp$treat1)
pw_sbp$treat2 <- sub("^[0-9]+_", "", pw_sbp$treat2)

sort(unique(pw_sbp$treat1))
glimpse(pw_sbp)
#######################
# Network connection
nc <- netconnection( treat1 = pw_sbp$treat1,
                     treat2 = pw_sbp$treat2,
                     studlab = pw_sbp$studlab)
print(nc)
nc <- netconnection(pw_sbp)
nc
print(nc, details = TRUE)

# netgraph
netgraph(netconnection(
  treat1 = pw_sbp$treat1,
  treat2 = pw_sbp$treat2,
  studlab = pw_sbp$studlab) )
netgraph(  netconnection(
  treat1 = pw_sbp$treat1,
  treat2 = pw_sbp$treat2,
  studlab = pw_sbp$studlab ),
  plastic = FALSE,
  number.of.studies = TRUE)

######################Netmeta for BMI only
net <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = pw_sbp
)

#
# SBP subset
sbp <- subset(baseline_post, Outcome == "SBP")
sbp
#Create pairwise comparisons
pw_sbp <- pairwise(
  treat = ARMID,
  mean = post_mean,
  sd = post_sd,
  n = post_samplesize,
  studlab = study_id,
  data = sbp,
  sm = "MD")

glimpse(pw_sbp)

#######################
# check data sanity before proceeding
sort(unique(pw_sbp$treat1))
sort(unique(pw_sbp$treat2))
head(pw_sbp[, c("studlab", "treat1", "treat2")])

pw_sbp$treat1 <- sub("^[0-9]+_", "", pw_sbp$treat1)
pw_sbp$treat2 <- sub("^[0-9]+_", "", pw_sbp$treat2)

sort(unique(pw_sbp$treat1))
glimpse(pw_sbp)
#######################
# Network connection
nc <- netconnection( treat1 = pw_sbp$treat1,
                     treat2 = pw_sbp$treat2,
                     studlab = pw_sbp$studlab)
print(nc)
nc <- netconnection(pw_sbp)
nc
print(nc, details = TRUE)

# netgraph
netgraph(netconnection(
  treat1 = pw_sbp$treat1,
  treat2 = pw_sbp$treat2,
  studlab = pw_sbp$studlab) )
netgraph(  netconnection(
  treat1 = pw_sbp$treat1,
  treat2 = pw_sbp$treat2,
  studlab = pw_sbp$studlab ),
  plastic = FALSE,
  number.of.studies = TRUE)

######################Netmeta for SBP only
sbp_net <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = pw_sbp
)
SBP_net <- as.data.frame(sbp_net)
glimpse(SBP_net)

# DBP subset-----
dbp <- subset(baseline_post, Outcome == "DBP")
dbp
# Create pairwise comparisons
pw_dbp <- pairwise(
  treat = ARMID,
  mean = post_mean,
  sd = post_sd,
  n = post_samplesize,
  studlab = study_id,
  data = dbp,
  sm = "MD"
)

glimpse(pw_dbp)

#######################
# Check data sanity before proceeding
sort(unique(pw_dbp$treat1))
sort(unique(pw_dbp$treat2))
head(pw_dbp[, c("studlab", "treat1", "treat2")])

pw_dbp$treat1 <- sub("^[0-9]+_", "", pw_dbp$treat1)
pw_dbp$treat2 <- sub("^[0-9]+_", "", pw_dbp$treat2)

sort(unique(pw_dbp$treat1))
glimpse(pw_dbp)
#######################
# Network connection
nc <- netconnection( treat1 = pw_dbp$treat1,
                     treat2 = pw_dbp$treat2,
                     studlab = pw_dbp$studlab)
print(nc)
nc <- netconnection(pw_dbp)
nc
print(nc, details = TRUE)

# netgraph
netgraph(netconnection(
  treat1 = pw_dbp$treat1,
  treat2 = pw_dbp$treat2,
  studlab = pw_dbp$studlab) )
netgraph(  netconnection(
  treat1 = pw_dbp$treat1,
  treat2 = pw_dbp$treat2,
  studlab = pw_dbp$studlab ),
  plastic = FALSE,
  number.of.studies = TRUE)

######################Netmeta for DBP only
dbp_net <- netmeta(
  TE,
  seTE,
  treat1,
  treat2,
  studlab,
  data = pw_dbp
)

DBP_net <- as.data.frame(dbp_net)

###############################################
glimpse(BMI_net)
glimpse(SBP_net)
glimpse(DBP_net)
BMI_net$n.arms

unique(BMI_net$studlab)

# Calculate approximately adjusted standard errors
# with inflation factor
add_approx_se <- function(dat) {
  dat |> 
    mutate(
      seTE.approx = sqrt(n.arms / 2) * seTE,
      inflation_factor = seTE.adj / seTE)
  }

BMI_net <- add_approx_se(BMI_net)
SBP_net <- add_approx_se(SBP_net)
DBP_net <- add_approx_se(DBP_net)

# Sanity check the data
BMI_net |> 
  select(studlab, treat1, treat2,
         seTE, seTE.approx, seTE.adj,
         inflation_factor)

# Combine BMI, SBP and DBP for Wald ratio estimation -----
# Rename TE and seTE.adj
BMI_net <- BMI_net |>
  dplyr::rename(BMITE = TE,
                BMISEunadj = seTE,
                BMISEexactadj = seTE.adj,
                BMISEapproxadj = seTE.approx
                )

SBP_net <- SBP_net |>
  dplyr::rename(
    SBPTE = TE,
    SBPSEunadj = seTE,
    SBPSEexactadj = seTE.adj,
    SBPSEapproxadj = seTE.approx
  )

DBP_net <- DBP_net |>
  dplyr::rename(
    DBPTE = TE,
    DBPSEunadj = seTE,
    DBPSEexactadj = seTE.adj,
    DBPSEapproxadj = seTE.approx
  )
glimpse(BMI_net)
glimpse(SBP_net)
glimpse(DBP_net)

#############################
# Merge BMI, SBP, DBP------
#############################
head(BMI_net$studlab)
head(SBP_net$studlab)
BMI_net <- BMI_net |>
  mutate(
    study_id = gsub("_BMI_", "_", studlab)
  )

SBP_net <- SBP_net |>
  mutate(
    study_id = gsub("_SBP_", "_", studlab)
  )

DBP_net <- DBP_net |>
  mutate(
    study_id = gsub("_DBP_", "_", studlab)
  )

sbp_bmi <- inner_join(
  SBP_net |> select(study_id,treat1,treat2,SBPTE,SBPSEunadj,SBPSEexactadj,SBPSEapproxadj),
  BMI_net |> select(study_id,treat1,treat2,BMITE,BMISEunadj,BMISEexactadj,BMISEapproxadj),
  by = c("study_id","treat1","treat2")
)
sbp_bmi

dbp_bmi <- inner_join(
  DBP_net |> select(study_id,treat1,treat2,DBPTE,DBPSEunadj,DBPSEexactadj,DBPSEapproxadj),
  BMI_net |> select(study_id,treat1,treat2,BMITE,BMISEunadj,BMISEexactadj,BMISEapproxadj),
  by = c("study_id","treat1","treat2")
)
dbp_bmi

#####################################
# Wald ratio estimator----
# SBP----
sbp_bmi <- wald_ratio(
  sbp_bmi,
  "SBPTE", "SBPSEunadj",
  "BMITE", "BMISEunadj",
  suffix = "_unadj"
)

sbp_bmi <- wald_ratio(
  sbp_bmi,
  "SBPTE", "SBPSEexactadj",
  "BMITE", "BMISEexactadj",
  suffix = "_exact"
)

sbp_bmi <- wald_ratio(
  sbp_bmi,
  "SBPTE", "SBPSEapproxadj",
  "BMITE", "BMISEapproxadj",
  suffix = "_approx"
)
sbp_bmi


# DBP-----
dbp_bmi <- wald_ratio(
  dbp_bmi,
  "DBPTE", "DBPSEunadj",
  "BMITE", "BMISEunadj",
  suffix = "_unadj"
)

dbp_bmi <- wald_ratio(
  dbp_bmi,
  "DBPTE", "DBPSEexactadj",
  "BMITE", "BMISEexactadj",
  suffix = "_exact"
)

dbp_bmi <- wald_ratio(
  dbp_bmi,
  "DBPTE", "DBPSEapproxadj",
  "BMITE", "BMISEapproxadj",
  suffix = "_approx"
)
dbp_bmi


#######################
# Create the months column

sbp_bmi$months <- as.numeric(
  str_extract(sbp_bmi$study_id, "(?<=_)\\d+$")
)
unique(sbp_bmi[, c("study_id", "months")])

dbp_bmi$months <- as.numeric(
  str_extract(dbp_bmi$study_id, "(?<=_)\\d+$")
)
unique(dbp_bmi[, c("study_id", "months")])

sbp_bmi
dbp_bmi

# Filter out the inf WRs
sbp_bmi <- sbp_bmi |>
  dplyr::filter(
    is.finite(WR_exact),
    is.finite(WR_SE_exact)
  )
dbp_bmi <- dbp_bmi |>
  dplyr::filter(
    is.finite(WR_exact),
    is.finite(WR_SE_exact)
  )

##############################
# metagen- meta-analysis
# sbp_bmi-----
sbp_bmi$study_label <- paste(
  sbp_bmi$study_id,
  ":",
  sbp_bmi$treat1,
  "vs",
  sbp_bmi$treat2
)
sbp_bmi$time_group <- dplyr::case_when(
  sbp_bmi$months <= 6~ "≤6 months",
  sbp_bmi$months >= 12 & sbp_bmi$months <= 24 ~ "12-24 months",
  TRUE ~ "Other"
)

m_6_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(sbp_bmi, time_group == "≤6 months")
)

m_12_24_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(sbp_bmi, time_group == "12-24 months")
)

m_6_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(sbp_bmi, time_group == "≤6 months")
)

m_12_24_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(sbp_bmi, time_group == "12-24 months")
)

m_6_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(sbp_bmi, time_group == "≤6 months")
)

m_12_24_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(sbp_bmi, time_group == "12-24 months")
)

# Visualise----
# Unadjusted SE
dev.new(width = 16, height = 16)
pdf(
  "output/glp1ra/figures/sbp_bmi_6months_forestunajd.pdf",
  width = 16,
  height = 16)
forest(
  m_6_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/sbp_bmi_12-24months_forestunajd.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_unadj,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (unadjusted seTE) ",
  leftlabs = c("Study")
)
dev.off()

# Exact adjustment------
pdf(
  "output/glp1ra/figures/sbp_bmi_6months_forestExact.pdf",
  width = 16,
  height = 16)
forest(
  m_6_exact,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/sbp_bmi_12-24months_forestExact.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_exact,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()
# Approximate adjustment------
pdf(
  "output/glp1ra/figures/sbp_bmi_6months_forestapproxajd.pdf",
  width = 16,
  height =16)
forest(
  m_6_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/sbp_bmi_12-24months_forestapproxadj.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_approx,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

#############
# dbp_bmi
dbp_bmi$study_label <- paste(
  dbp_bmi$study_id,
  ":",
  dbp_bmi$treat1,
  "vs",
  dbp_bmi$treat2
)
dbp_bmi$time_group <- dplyr::case_when(
  dbp_bmi$months <= 6~ "≤6 months",
  dbp_bmi$months >= 12 & dbp_bmi$months <= 24 ~ "12-24 months",
  TRUE ~ "Other"
)

m_6_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(dbp_bmi, time_group == "≤6 months")
)

m_12_24_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(dbp_bmi, time_group == "12-24 months")
)

m_6_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(dbp_bmi, time_group == "≤6 months")
)

m_12_24_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(dbp_bmi, time_group == "12-24 months")
)

m_6_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(dbp_bmi, time_group == "≤6 months")
)

m_12_24_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(dbp_bmi, time_group == "12-24 months")
)

# Visualise----
# Unadjusted SE
dev.new(width = 16, height = 16)
pdf(
  "output/glp1ra/figures/dbp_bmi_6months_forestunajd.pdf",
  width = 16,
  height = 16)
forest(
  m_6_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/dbp_bmi_12-24months_forestunajd.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_unadj,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (unadjusted seTE) ",
  leftlabs = c("Study")
)
dev.off()

# Exact adjustment------
pdf(
  "output/glp1ra/figures/dbp_bmi_6months_forestExact.pdf",
  width = 16,
  height = 16)
forest(
  m_6_exact,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/dbp_bmi_12-24months_forestExact.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_exact,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()
# Approximate adjustment------
pdf(
  "output/glp1ra/figures/dbp_bmi_6months_forestapproxajd.pdf",
  width = 16,
  height =16)
forest(
  m_6_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/dbp_bmi_12-24months_forestapproxadj.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_approx,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


