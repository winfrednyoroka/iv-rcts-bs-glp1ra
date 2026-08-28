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

##############################################
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

########################
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
sbp_bmi
dbp_bmi

#####################################################
# metagen and metafor - meta-analysis
# sbp_bmi-----
#####################################################
sbp_bmi$study_label <- paste(
  sbp_bmi$study_id,
  ":",
  gsub("_$", "", gsub("_Control", "", gsub("_Intervention_", "_", sbp_bmi$treat1))),
  "vs",
  gsub("_$", "", gsub("_Control", "", gsub("_Intervention_", "_", sbp_bmi$treat2)))
)
sbp_bmi

###### 3-6 months and 12 to 24 months
sbp_bmi$time_group <- dplyr::case_when(
  sbp_bmi$months <= 6~ "≤6 months",
  sbp_bmi$months >= 12 & sbp_bmi$months <= 24 ~ "12-24 months",
  TRUE ~ "Other"
)
########## 6 months only (metafor and metagen)
sbp_bmi_6m <- sbp_bmi |>
dplyr::filter(months == 6)
sbp_bmi_6m

# 3 to 6 months----
# Calculate the WRs using metagen
m_6_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(sbp_bmi, time_group == "≤6 months")
)
m_6_unadj

# Calculate the WRs using metafor
m_6_unadj_res <- rma(yi = sbp_bmi$WR_unadj,
                     sei = sbp_bmi$WR_SE_unadj,
                     method = 'REML',
                     data = sbp_bmi,
                     subset = time_group == "≤6 months")
m_6_unadj_res

# 6 months only---
# Calculate the WRs using metagen
m_6monly_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = sbp_bmi_6m
)
m_6monly_unadj

# Calculate the WRs using metafor
m_6monly_unadj_res <- rma(yi = sbp_bmi_6m$WR_unadj,
                          sei = sbp_bmi_6m$WR_SE_unadj,
                          method = 'REML')
m_6monly_unadj_res

# 12-24 months
# Calculate the WRs using metagen
m_12_24_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(sbp_bmi, time_group == "12-24 months")
)
m_12_24_unadj

# Calculate the WRs using metafor
m_12_24_unadj_res <- rma(yi = sbp_bmi$WR_unadj,
                     sei = sbp_bmi$WR_SE_unadj,
                     method = 'REML',
                     data = sbp_bmi,
                     subset = time_group == "12-24 months")
m_12_24_unadj_res

# Exact adjusted SE of the treatment effect

# 3-6 months
# Calculate the WRs using metagen
m_6_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(sbp_bmi, time_group == "≤6 months")
)
m_6_exact

# Calculate the WRs using metafor
m_6_exact_res <- rma(yi = sbp_bmi$WR_exact,
                         sei = sbp_bmi$WR_SE_exact,
                         method = 'REML',
                         data = sbp_bmi,
                         subset = time_group == "≤6 months")
m_6_exact_res

# 6 months only
# Calculate the WRs using metagen
m_6monly_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = sbp_bmi_6m
)
m_6monly_exact
# Calculate the WRs using metafor
m_6monly_exact_res <- rma(yi = sbp_bmi_6m$WR_exact,
                     sei = sbp_bmi_6m$WR_SE_exact,
                     method = 'REML',
                     data = sbp_bmi_6m)
m_6monly_exact_res

# 12-24 months
# Calculate the WRs using metagen
m_12_24_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(sbp_bmi, time_group == "12-24 months")
)
m_12_24_exact

# Calculate the WRs using metafor
m_12_24_exact_res <- rma(yi = sbp_bmi$WR_exact,
                         sei = sbp_bmi$WR_SE_exact,
                         method = 'REML',
                         data = sbp_bmi,
                         subset = time_group == "12-24 months")
m_12_24_exact_res

# Approximate adjusted SE of the treatment effect

# 3-6 months
# Calculate the WRs using metagen
m_6_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(sbp_bmi, time_group == "≤6 months")
)
m_6_approx

# Calculate the WRs using metafor
m_6_approx_res <- rma(yi = sbp_bmi$WR_approx,
                     sei = sbp_bmi$WR_SE_approx,
                     method = 'REML',
                     data = sbp_bmi,
                     subset = time_group == "≤6 months")
m_6_approx_res

# 6 months only
# Calculate the WRs using metagen
m_6monly_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = sbp_bmi_6m
)
m_6monly_approx

# Calculate the WRs using metafor
m_6monly_approx_res <- rma(yi = sbp_bmi_6m$WR_approx,
                      sei = sbp_bmi_6m$WR_SE_approx,
                      method = 'REML',
                      data = sbp_bmi_6m)
m_6monly_approx_res

# Calculate the WRs using metagen
m_12_24_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(sbp_bmi, time_group == "12-24 months")
)
m_12_24_approx
# Calculate the WRs using metafor
m_12_24_approx_res <- rma(yi = sbp_bmi$WR_approx,
                           sei = sbp_bmi$WR_SE_approx,
                           method = 'REML',
                           data = sbp_bmi,
                          subset = time_group == "12-24 months")
m_12_24_approx_res

# Visualise the plots----
# Unadjusted SE (metagen and metafor)

# 3-6 months -----
dev.new(width = 16, height = 16)
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestunajd_metagen.pdf",
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

dev.new(width = 16, height = 16)
jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestunajd_metagen.jpeg",
  width = 16,
  height = 16,
  units = "in",
  res = 600)
forest(
  m_6_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestunajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6_unadj_res,
       slab = sbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestunajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res =600)
par(mar = c(5, 2, 2, 2))
forest(m_6_unadj_res,
       slab = sbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()

# 6 months only-----
dev.new(width = 16, height = 14)
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestunajd_metagen.pdf",
  width = 16,
  height = 14)
forest(
  m_6monly_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

dev.new(width = 16, height = 14)
jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestunajd_metagen.jpeg",
  width = 16,
  height = 14,
  units = "in",
  res = 600)
forest(
  m_6monly_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestunajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_unadj_res,
       slab = sbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()

jpeg("output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestunajd_metafor.jpeg",
     width = 18,
     height = 16,
     units = 'in',
     res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_unadj_res,
       slab = sbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()

# 12 - 24 months unadjusted----
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestunajd_metagen.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_unadj,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (unadjusted seTE) ",
  leftlabs = c("Study")
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestunajd_metagen.jpeg",
  width = 14,
  height = 7,
  units = "in",
  res = 600)
forest(
  m_12_24_unadj,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (unadjusted seTE) ",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestunajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_unadj_res,
       slab = sbp_bmi$study_label,
       xlim = c(-10,10),
       alim = c(-4,6),
       cex = 1.2,
       at = seq(-4, 6, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestunajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_unadj_res,
       slab = sbp_bmi$study_label,
       xlim = c(-10,10),
       alim = c(-4,6),
       cex = 1.2,
       at = seq(-4, 6, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()


# Exact adjustment------
# 3-6 months----
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestExact_metagen.pdf",
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

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestExact_metagen.jpeg",
  width = 16,
  height = 16,
  units = "in",
  res = 600)
forest(
  m_6_exact,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestExact_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6_exact_res,
       slab = sbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestExact_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6_exact_res,
       slab = sbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()

# 6 months only----
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestExact_metagen.pdf",
  width = 16,
  height = 14)
forest(
  m_6monly_exact,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestExact_metagen.jpeg",
  width = 16,
  height = 14,
  units = "in",
  res = 600)
forest(
  m_6monly_exact,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestExact_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_exact_res,
       slab = sbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestExact_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_exact_res,
       slab = sbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()


# 12-24 months----
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestExact_metagen.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_exact,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestExact_metagen.jpeg",
  width = 14,
  height = 7,
  units = "in",
  res =600)
forest(
  m_12_24_exact,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestExact_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_exact_res,
       slab = sbp_bmi$study_label,
       xlim = c(-11,10),
       alim = c(-5,7),
       cex = 1.2,
       at = seq(-5, 7, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_exact_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestExact_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_exact_res,
       slab = sbp_bmi$study_label,
       xlim = c(-11,10),
       alim = c(-5,7),
       cex = 1.2,
       at = seq(-5, 7, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_exact_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()


# Approximate adjustment------
# 3-6 months----
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestapproxajd_metagen.pdf",
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

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestapproxajd_metagen.jpeg",
  width = 16,
  height =16,
  units = "in",
  res = 600)
forest(
  m_6_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestapproxajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6_approx_res,
       slab = sbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off() 

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6months_forestapproxajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6_approx_res,
       slab = sbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off()


# 6 months only----
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestapproxajd_metagen.pdf",
  width = 16,
  height =14)
forest(
  m_6monly_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestapproxajd_metagen.jpeg",
  width = 16,
  height =14,
  units = "in",
  res = 600)
forest(
  m_6monly_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestapproxajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_approx_res,
       slab = sbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_6monthsonly_forestapproxajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_approx_res,
       slab = sbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off()


# 12-24 months
pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestapproxadj_metagen.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_approx,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off() 

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestapproxadj_metagen.jpeg",
  width = 14,
  height = 7,
  units = "in",
  res = 600)
forest(
  m_12_24_approx,
  prediction = FALSE,
  xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestapproxadj_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_approx_res,
       slab = sbp_bmi$study_label,
       xlim = c(-11,10),
       alim = c(-5,7),
       cex = 1.2,
       at = seq(-5, 7, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_approx_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/sbp/sbp_bmi_12-24months_forestapproxadj_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_approx_res,
       slab = sbp_bmi$study_label,
       xlim = c(-11,10),
       alim = c(-5,7),
       cex = 1.2,
       at = seq(-5, 7, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_approx_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "SBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off()


#############
# dbp_bmi
dbp_bmi$study_label <- paste(
  dbp_bmi$study_id,
  ":",
  gsub("_$", "", gsub("_Control", "", gsub("_Intervention_", "_", dbp_bmi$treat1))),
  "vs",
  gsub("_$", "", gsub("_Control", "", gsub("_Intervention_", "_", dbp_bmi$treat2)))
)
dbp_bmi
dbp_bmi$time_group <- dplyr::case_when(
  dbp_bmi$months <= 6~ "≤6 months",
  dbp_bmi$months >= 12 & dbp_bmi$months <= 24 ~ "12-24 months",
  TRUE ~ "Other"
)

########## 6 months only
dbp_bmi_6m <- dbp_bmi |>
  dplyr::filter(months == 6)
dbp_bmi_6m

# 3-6 months
# Calculate the WRs using metagen
m_6_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(dbp_bmi, time_group == "≤6 months")
)
m_6_unadj

# Calculate the WRs using metafor
m_6_unadj_res <- rma(yi = dbp_bmi$WR_unadj,
                     sei = dbp_bmi$WR_SE_unadj,
                     method = 'REML',
                     data = dbp_bmi,
                     subset = time_group == "≤6 months")
m_6_unadj_res

# 6 months only
# Calculate the WRs using metagen
m_6monly_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = dbp_bmi_6m
)
m_6monly_unadj

# Calculate the WRs using metafor
m_6monly_unadj_res <- rma(yi = dbp_bmi_6m$WR_unadj,
                     sei = dbp_bmi_6m$WR_SE_unadj,
                     method = 'REML',
                     data = dbp_bmi_6m)
m_6monly_unadj_res

# 12-24 months only
# Calculate the WRs using metagen
m_12_24_unadj <- metagen(
  TE = WR_unadj,
  studlab = study_label,
  seTE = WR_SE_unadj,
  data = subset(dbp_bmi, time_group == "12-24 months")
)
m_12_24_unadj

# Calculate the WRs using metafor
m_12_24_unadj_res <- rma(yi = dbp_bmi$WR_unadj,
                     sei = dbp_bmi$WR_SE_unadj,
                     method = 'REML',
                     data = dbp_bmi,
                     subset = time_group == "12-24 months")
m_12_24_unadj_res


# Exact ajdustment
# 3-6 months only
# Calculate the WRs using metagen
m_6_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(dbp_bmi, time_group == "≤6 months")
)
m_6_exact

# Calculate the WRs using metafor
m_6_exact_res <- rma(yi = dbp_bmi$WR_exact,
                         sei = dbp_bmi$WR_SE_exact,
                         method = 'REML',
                         data = dbp_bmi,
                         subset = time_group == "≤6 months")
m_6_exact_res

# 6 months only
# Calculate the WRs using metagen
m_6monly_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = dbp_bmi_6m
)
m_6monly_exact

# Calculate the WRs using metafor
m_6monly_exact_res <- rma(yi = dbp_bmi_6m$WR_exact,
                     sei = dbp_bmi_6m$WR_SE_exact,
                     method = 'REML',
                     data = dbp_bmi_6m)
m_6monly_exact_res

# 12-24 months only
# Calculate the WRs using metagen
m_12_24_exact <- metagen(
  TE = WR_exact,
  studlab = study_label,
  seTE = WR_SE_exact,
  data = subset(dbp_bmi, time_group == "12-24 months")
)
m_12_24_exact

# Calculate the WRs using metafor
m_12_24_exact_res <- rma(yi = dbp_bmi$WR_exact,
                          sei = dbp_bmi$WR_SE_exact,
                          method = 'REML',
                          data = dbp_bmi,
                         subset = time_group == "12-24 months")
m_12_24_exact_res

# Approximate adjustment
# 3-6 months only
# Calculate the WRs using metagen
m_6_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(dbp_bmi, time_group == "≤6 months")
)
m_6_approx

# Calculate the WRs using metafor
m_6_approx_res <- rma(yi = dbp_bmi$WR_approx,
                         sei = dbp_bmi$WR_SE_approx,
                         method = 'REML',
                         data = dbp_bmi,
                         subset = time_group == "≤6 months")
m_6_approx_res

# 6 months only
# Calculate the WRs using metagen
m_6monly_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = dbp_bmi_6m
  )
m_6monly_approx

# Calculate the WRs using metafor
m_6monly_approx_res <- rma(yi = dbp_bmi_6m$WR_approx,
                      sei = dbp_bmi_6m$WR_SE_approx,
                      method = 'REML',
                      data = dbp_bmi_6m)
m_6monly_approx_res

# 12-24 months only
# Calculate the WRs using metagen
m_12_24_approx <- metagen(
  TE = WR_approx,
  studlab = study_label,
  seTE = WR_SE_approx,
  data = subset(dbp_bmi, time_group == "12-24 months")
)
m_12_24_approx

# Calculate the WRs using metafor
m_12_24_approx_res <- rma(yi = dbp_bmi$WR_approx,
                           sei = dbp_bmi$WR_SE_approx,
                           method = 'REML',
                           data = dbp_bmi,
                          subset = time_group == "12-24 months")
m_12_24_approx_res

# Visualise----
# Unadjusted SE
# 3-6 months only
dev.new(width = 16, height = 16)
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestunajd_metagen.pdf",
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


dev.new(width = 16, height = 16)
jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestunajd_metagen.jpeg",
  width = 16,
  height = 16,
  units = "in",
  res = 600)
forest(
  m_6_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestunajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6_unadj_res,
       slab = dbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off() 


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestunajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6_unadj_res,
       slab = dbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off() 

# 6 months only----
dev.new(width = 16, height = 14)
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestunajd_metagen.pdf",
  width = 16,
  height = 14)
forest(
  m_6monly_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

dev.new(width = 16, height = 14)

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestunajd_metagen.jpeg",
  width = 16,
  height = 14,
  units = "in",
  res = 600)
forest(
  m_6monly_unadj,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
  leftlabs = c("Study")
)
dev.off()



pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestunajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_unadj_res,
       slab = dbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()  


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestunajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_unadj_res,
       slab = dbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()


# 12-24 months
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestunajd_metagen.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_unadj,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (unadjusted seTE) ",
  leftlabs = c("Study")
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestunajd_metagen.jpeg",
  width = 14,
  height = 7,
  units = "in",
  res = 600)
forest(
  m_12_24_unadj,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (unadjusted seTE) ",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestunajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_unadj_res,
       slab = dbp_bmi$study_label,
       xlim = c(-9,10),
       alim = c(-4,8),
       cex = 1.2,
       at = seq(-4, 8, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestunajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_unadj_res,
       slab = dbp_bmi$study_label,
       xlim = c(-9,10),
       alim = c(-4,8),
       cex = 1.2,
       at = seq(-4, 8, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_unadj_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_unadj_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (unadjusted seTE)",
)
dev.off()



# Exact adjustment------
# 3-6 months 
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestExact_metagen.pdf",
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

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestExact_metagen.jpeg",
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
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestExact_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6_exact_res,
       slab = dbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off() 


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestExact_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6_exact_res,
       slab = dbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off() 

# 6 months only-----
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestExact_metagen.pdf",
  width = 16,
  height = 14)
forest(
  m_6monly_exact,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestExact_metagen.jpeg",
  width = 16,
  height = 14,
  units = "in",
  res = 600)
forest(
  m_6monly_exact,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestExact_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_exact_res,
       slab = dbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off() 


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestExact_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_exact_res,
       slab = dbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_exact_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off() 


# 12-24 months-----
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestExact_metagen.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_exact,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestExact_metagen.jpeg",
  width = 14,
  height = 7,
  units = "in",
  res = 600)
forest(
  m_12_24_exact,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestExact_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_exact_res,
       slab = dbp_bmi$study_label,
       xlim = c(-9,10),
       alim = c(-4,8),
       cex = 1.2,
       at = seq(-4, 8, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_exact_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestExact_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_exact_res,
       slab = dbp_bmi$study_label,
       xlim = c(-9,10),
       alim = c(-4,8),
       cex = 1.2,
       at = seq(-4, 8, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_exact_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_exact_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (exact adjusted seTE)",
)
dev.off()

# Approximate adjustment----
# 3-6 months only
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestapproxajd_metagen.pdf",
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

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestapproxajd_metagen.jpeg",
  width = 16,
  height =16,
  units = "in",
  res = 600)
forest(
  m_6_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestapproxajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6_approx_res,
       slab = dbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off() 


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6months_forestapproxajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6_approx_res,
       slab = dbp_bmi$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off() 




# 6 months only----
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestapproxajd_metagen.pdf",
  width = 16,
  height =14)
forest(
  m_6monly_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestapproxajd_metagen.jpeg",
  width = 16,
  height =14,
  units = "in",
  res = 600)
forest(
  m_6monly_approx,
  prediction = FALSE,
  xlim = c(-10, 10),
  xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestapproxajd_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_approx_res,
       slab = dbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off() 


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_6monthsonly_forestapproxajd_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_6monly_approx_res,
       slab = dbp_bmi_6m$study_label,
       xlim = c(-50,40),
       alim = c(-20,25),
       cex = 1.2,
       at = seq(-20, 25, by = 2),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_6monly_approx_res$QE, 2),
         "; p = ",
         format.pval(m_6monly_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off() 
# 12-24 months -----
pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestapproxadj_metagen.pdf",
  width = 14,
  height = 7)
forest(
  m_12_24_approx,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()

jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestapproxadj_metagen.jpeg",
  width = 14,
  height = 7,
  units = "in",
  res = 600)
forest(
  m_12_24_approx,
  prediction = FALSE,
  xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
  leftlabs = c("Study")
)
dev.off()


pdf(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestapproxadj_metafor.pdf",
  width = 18,
  height = 16)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_approx_res,
       slab = dbp_bmi$study_label,
       xlim = c(-10,11),
       alim = c(-5,9),
       cex = 1.2,
       at = seq(-5, 9, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_approx_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off() 


jpeg(
  "output/glp1ra/figures/ivanalysis/dbp/dbp_bmi_12-24months_forestapproxadj_metafor.jpeg",
  width = 18,
  height = 16,
  units = "in",
  res = 600)
par(mar = c(5, 2, 2, 2))
forest(m_12_24_approx_res,
       slab = dbp_bmi$study_label,
       xlim = c(-10,11),
       alim = c(-5,9),
       cex = 1.2,
       at = seq(-5, 9, by = 1),
       mlab = paste0(
         "Random-effect model (Q = ",
         round(m_12_24_approx_res$QE, 2),
         "; p = ",
         format.pval(m_12_24_approx_res$QEp, digits = 2, eps = 0.001),
         ")"
       ),
       xlab = "DBP/BMI Wald ratio (approximate adjusted seTE)",
)
dev.off() 