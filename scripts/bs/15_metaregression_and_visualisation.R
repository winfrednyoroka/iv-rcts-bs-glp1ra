# Study level meta-regression at each follow-up point, regressing the BP difference
# on the corresponding BMI difference
# BMI diff and BP diff at 12 months
# BMI diff and BP diff at 24 months
# Firstly Fit a linear model
# Secondly then explore non-linearity using a quadratic term
source('R/shared/setup.R')
source('R/shared/metaregression_viz.R')

# Load the libraries and shared custom functions

# Read in the data
# # Temporality (bmi @12 months and bp @24 months)----
bmi12_sbp24 <- readRDS('data/bs/processed/wr_bmi12_sbp24.rds')
bmi12_sbp24
# 
bmi12_dbp24 <- readRDS('data/bs/processed/wr_bmi12_dbp24.rds')
bmi12_dbp24

# bmi and bp @12 months----
bmi12_sbp12 <- readRDS('data/bs/processed/wr_bmi12_sbp12.rds')
bmi12_sbp12

bmi12_dbp12 <- readRDS('data/bs/processed/wr_bmi12_dbp12.rds')
bmi12_dbp12

# bmi and bp @24 months----
bmi24_sbp24 <- readRDS('data/bs/processed/wr_bmi24_sbp24.rds')
bmi24_sbp24

bmi24_dbp24 <- readRDS('data/bs/processed/wr_bmi24_dbp24.rds')
bmi24_dbp24

# BMI, DBP and SBP at 12 months ----
# 1. Calculate variance from the standard error
glimpse(bmi12_sbp12)
glimpse(bmi12_dbp12)
bmi12_sbp12$vi = bmi12_sbp12$se_SBP_12^2
bmi12_dbp12$vi = bmi12_dbp12$se_DBP_12^2
# 2. Calculate the quadratic term, BMI difference squared
bmi12_sbp12$beta_BMI_12_sq <- bmi12_sbp12$beta_BMI_12^2
bmi12_dbp12$beta_BMI_12_sq <- bmi12_dbp12$beta_BMI_12^2
glimpse(bmi12_sbp12)
glimpse(bmi12_dbp12)

# BMI, DBP and SBP at 24 months ----
# 1. Calculate variance from the standard error
glimpse(bmi24_sbp24)
glimpse(bmi24_dbp24)
bmi24_sbp24$vi = bmi24_sbp24$se_SBP_24^2
bmi24_dbp24$vi = bmi24_dbp24$se_DBP_24^2
# 2. Calculate the quadratic term, BMI difference squared
bmi24_sbp24$beta_BMI_24_sq <- bmi24_sbp24$beta_BMI_24^2
bmi24_dbp24$beta_BMI_24_sq <- bmi24_dbp24$beta_BMI_24^2
glimpse(bmi24_sbp24)
glimpse(bmi24_dbp24)

# BMI at 12 months, DBP and SBP at 24 months ----
# 1. Calculate variance from the standard error
glimpse(bmi12_sbp24)
glimpse(bmi12_dbp24)
bmi12_sbp24$vi = bmi12_sbp24$se_SBP_24^2
bmi12_dbp24$vi = bmi12_dbp24$se_DBP_24^2
# 2. Calculate the quadratic term, BMI difference squared
bmi12_sbp24$beta_BMI_12_sq <- bmi12_sbp24$beta_BMI_12^2
bmi12_dbp24$beta_BMI_12_sq <- bmi12_dbp24$beta_BMI_12^2
glimpse(bmi12_sbp24)
glimpse(bmi12_dbp24)

# Perform linear and non-linear meta-regression
####### Linear regression

# bmi and bp at 12 months
bmi_sbp_12_res <- rma(yi = beta_SBP_12 , vi = vi, mods = ~ beta_BMI_12,
  data = bmi12_sbp12, method = "FE" )
summary(bmi_sbp_12_res)

bmi_dbp_12_res <- rma(yi = beta_DBP_12 , vi = vi, mods = ~ beta_BMI_12,
  data = bmi12_dbp12, method = "FE")
summary(bmi_dbp_12_res)

# bmi and bp at 24 months
bmi_sbp_24_res <- rma(yi = beta_SBP_24 , vi = vi, mods = ~ beta_BMI_24,
                      data = bmi24_sbp24, method = "FE" )
summary(bmi_sbp_24_res)

bmi_dbp_24_res <- rma(yi = beta_DBP_24 , vi = vi, mods = ~ beta_BMI_24,
                      data = bmi24_dbp24, method = "FE")
summary(bmi_dbp_24_res)

# bmi at 12 months and bp at 24 months
bmi12_sbp_24_res <- rma(yi = beta_SBP_24 , vi = vi, mods = ~ beta_BMI_12,
                        data = bmi12_sbp24, method = "FE" )
summary(bmi12_sbp_24_res)

bmi12_dbp_24_res <- rma(yi = beta_DBP_24 , vi = vi, mods = ~ beta_BMI_12,
                        data = bmi12_dbp24, method = "FE")
summary(bmi12_dbp_24_res)

####### Nonlinear meta-regression----
# bmi and bp at 12 months
bmi_sbp_12_res_quad <- rma(yi = beta_SBP_12 , vi = vi, mods = ~ beta_BMI_12 + beta_BMI_12_sq,
                           data = bmi12_sbp12, method = "FE" )
summary(bmi_sbp_12_res_quad)

bmi_dbp_12_res_quad <- rma(yi = beta_DBP_12 , vi = vi, mods = ~ beta_BMI_12 + beta_BMI_12_sq,
                           data = bmi12_dbp12, method = "FE")
summary(bmi_dbp_12_res_quad)

# bmi and bp at 24 months
bmi_sbp_24_res_quad <- rma(yi = beta_SBP_24 , vi = vi, mods = ~ beta_BMI_24 + beta_BMI_24_sq,
                           data = bmi24_sbp24, method = "FE" )
summary(bmi_sbp_24_res_quad)

bmi_dbp_24_res_quad <- rma(yi = beta_DBP_24 , vi = vi, mods = ~ beta_BMI_24 + beta_BMI_24_sq,
                           data = bmi24_dbp24, method = "FE")
summary(bmi_dbp_24_res_quad)

# bmi at 12 months and bp at 24 months
bmi12_sbp_24_res_quad <- rma(yi = beta_SBP_24 , vi = vi, mods = ~ beta_BMI_12 + beta_BMI_12_sq,
                             data = bmi12_sbp24, method = "FE" )
summary(bmi12_sbp_24_res_quad)

bmi12_dbp_24_res_quad <- rma(yi = beta_DBP_24 , vi = vi, mods = ~ beta_BMI_12 + beta_BMI_12_sq,
                             data = bmi12_dbp24, method = "FE")
summary(bmi12_dbp_24_res_quad)

# Visualise the linear regression

jpeg(filename = 'output/bs/figures/meta-regression/sbp/bmi12_sbp12_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_sbp12, xvar = "beta_BMI_12", yvar = "beta_SBP_12",
  linear_model = bmi_sbp_12_res, study_var = "study_id",
  xlab = "BMI difference at 12 months (kg/m²)", ylab = "SBP difference at 12 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/sbp/bmi24_sbp24_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_sbp24, xvar = "beta_BMI_24", yvar = "beta_SBP_24",
  linear_model = bmi_sbp_24_res, study_var = "study_id", 
  xlab = "BMI difference at 24 months (kg/m²)", ylab = "SBP difference at 24 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/dbp/bmi12_dbp12_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_dbp12, xvar = "beta_BMI_12", yvar = "beta_DBP_12",
                     linear_model = bmi_dbp_12_res, study_var = "study_id",
                     xlab = "BMI difference at 12 months (kg/m²)", ylab = "DBP difference at 12 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/dbp/bmi24_dbp24_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_dbp24, xvar = "beta_BMI_24", yvar = "beta_DBP_24",
                     linear_model = bmi_dbp_24_res, study_var = "study_id", 
                     xlab = "BMI difference at 24 months (kg/m²)", ylab = "DBP difference at 24 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/sbp/bmi12_sbp24_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_sbp24, xvar = "beta_BMI_12", yvar = "beta_SBP_24",
                     linear_model = bmi12_sbp_24_res, study_var = "study_id", 
                     xlab = "BMI difference at 12 months (kg/m²)", ylab = "SBP difference at 24 months (mmHg)"
)
dev.off()


jpeg(filename = 'output/bs/figures/meta-regression/dbp/bmi12_dbp24_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_dbp24, xvar = "beta_BMI_12", yvar = "beta_DBP_24",
                     linear_model = bmi12_dbp_24_res, study_var = "study_id",
                     xlab = "BMI difference at 12 months (kg/m²)", ylab = "DBP difference at 24 months (mmHg)")

dev.off()

# Visualise the linear and quad regression

jpeg(filename = 'output/bs/figures/meta-regression/sbp/bmi12_sbp12_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_sbp12, xvar = "beta_BMI_12", yvar = "beta_SBP_12",
                     linear_model = bmi_sbp_12_res,quadratic_model = bmi_sbp_12_res_quad, study_var = "study_id",
                     xlab = "BMI difference at 12 months (kg/m²)", ylab = "SBP difference at 12 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/sbp/bmi24_sbp24_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_sbp24, xvar = "beta_BMI_24", yvar = "beta_SBP_24",
                     linear_model = bmi_sbp_24_res,quadratic_model = bmi_sbp_24_res_quad, study_var = "study_id", 
                     xlab = "BMI difference at 24 months (kg/m²)", ylab = "SBP difference at 24 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/dbp/bmi12_dbp12_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_dbp12, xvar = "beta_BMI_12", yvar = "beta_DBP_12",
                     linear_model = bmi_dbp_12_res, quadratic_model = bmi_dbp_12_res_quad, study_var = "study_id",
                     xlab = "BMI difference at 12 months (kg/m²)", ylab = "DBP difference at 12 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/dbp/bmi24_dbp24_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_dbp24, xvar = "beta_BMI_24", yvar = "beta_DBP_24",
                     linear_model = bmi_dbp_24_res, quadratic_model = bmi_dbp_24_res_quad, study_var = "study_id", 
                     xlab = "BMI difference at 24 months (kg/m²)", ylab = "DBP difference at 24 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/bs/figures/meta-regression/sbp/bmi12_sbp24_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_sbp24, xvar = "beta_BMI_12", yvar = "beta_SBP_24",
                     linear_model = bmi12_sbp_24_res, quadratic_model = bmi12_sbp_24_res_quad, study_var = "study_id", 
                     xlab = "BMI difference at 12 months (kg/m²)", ylab = "SBP difference at 24 months (mmHg)"
)
dev.off()


jpeg(filename = 'output/bs/figures/meta-regression/dbp/bmi12_dbp24_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi12_dbp24, xvar = "beta_BMI_12", yvar = "beta_DBP_24",
                     linear_model = bmi12_dbp_24_res, quadratic_model = bmi12_dbp_24_res_quad, study_var = "study_id",
                     xlab = "BMI difference at 12 months (kg/m²)", ylab = "DBP difference at 24 months (mmHg)")

dev.off()

