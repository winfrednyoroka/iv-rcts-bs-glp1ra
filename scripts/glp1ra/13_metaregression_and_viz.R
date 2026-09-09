# Study level meta-regression at each follow-up point, regressing the BP difference
# on the corresponding BMI difference
# BMI diff and BP diff at 6 months
# BMI diff and BP diff at 12-24 months
# Firstly Fit a linear model
# Secondly then explore non-linearity using a quadratic term
source('R/shared/setup.R')
source('R/shared/metaregression_viz.R')

# Load the libraries and shared custom functions

# Read in the data

# bmi and bp @6 months----
bmi6_sbp6 <- readRDS('data/glp1ra/processed/sbp_bmi_6m.rds')
bmi6_sbp6

bmi6_dbp6 <- readRDS('data/glp1ra/processed/dbp_bmi_6m.rds')
bmi6_dbp6

# bmi and bp @12-24 months----
bmi24_sbp24 <- readRDS('data/glp1ra/processed/sbp_bmi_12_24m.rds')
bmi24_sbp24

bmi24_dbp24 <- readRDS('data/glp1ra/processed/dbp_bmi_12_24m.rds')
bmi24_dbp24

# BMI, DBP and SBP at 6 months ----
# 1. Calculate variance from the standard error
glimpse(bmi6_sbp6)
glimpse(bmi6_dbp6)
bmi6_sbp6$vi = bmi6_sbp6$SBPSEexactadj^2
bmi6_dbp6$vi = bmi6_dbp6$DBPSEexactadj^2
# 2. Calculate the quadratic term, BMI difference squared
bmi6_sbp6$BMITE_sq <- bmi6_sbp6$BMITE^2
bmi6_dbp6$BMITE_sq <- bmi6_dbp6$BMITE^2
glimpse(bmi6_sbp6)
glimpse(bmi6_dbp6)

# BMI, DBP and SBP at 12-24 months ----
# 1. Calculate variance from the standard error
glimpse(bmi24_sbp24)
glimpse(bmi24_dbp24)
bmi24_sbp24$vi = bmi24_sbp24$SBPSEexactadj^2
bmi24_dbp24$vi = bmi24_dbp24$DBPSEexactadj^2
# 2. Calculate the quadratic term, BMI difference squared
bmi24_sbp24$BMITE_sq <- bmi24_sbp24$BMITE^2
bmi24_dbp24$BMITE_sq <- bmi24_dbp24$BMITE^2
glimpse(bmi24_sbp24)
glimpse(bmi24_dbp24)


# Perform linear and non-linear meta-regression
####### Linear regression

# bmi and bp at 6 months
bmi_sbp_6_res <- rma(yi = SBPTE , vi = vi, mods = ~ BMITE,
                      data = bmi6_sbp6, method = "FE" )
summary(bmi_sbp_6_res)

bmi_dbp_6_res <- rma(yi = DBPTE , vi = vi, mods = ~ BMITE,
                      data = bmi6_dbp6, method = "FE")
summary(bmi_dbp_6_res)

# bmi and bp at 24 months
bmi_sbp_24_res <- rma(yi = SBPTE , vi = vi, mods = ~ BMITE,
                      data = bmi24_sbp24, method = "FE" )
summary(bmi_sbp_24_res)

bmi_dbp_24_res <- rma(yi = DBPTE , vi = vi, mods = ~ BMITE,
                      data = bmi24_dbp24, method = "FE")
summary(bmi_dbp_24_res)

####### Nonlinear meta-regression----
# bmi and bp at 6 months
bmi_sbp_6_res_quad <- rma(yi = SBPTE , vi = vi, mods = ~ BMITE + BMITE_sq,
                     data = bmi6_sbp6, method = "FE" )
summary(bmi_sbp_6_res_quad)

bmi_dbp_6_res_quad <- rma(yi = DBPTE , vi = vi, mods = ~ BMITE + BMITE_sq,
                     data = bmi6_dbp6, method = "FE")
summary(bmi_dbp_6_res_quad)

# bmi and bp at 12-24 months
bmi_sbp_24_res_quad <- rma(yi = SBPTE , vi = vi, mods = ~ BMITE + BMITE_sq,
                      data = bmi24_sbp24, method = "FE" )
summary(bmi_sbp_24_res_quad)

bmi_dbp_24_res_quad <- rma(yi = DBPTE , vi = vi, mods = ~ BMITE + BMITE_sq,
                      data = bmi24_dbp24, method = "FE")
summary(bmi_dbp_24_res_quad)

# Visualise the linear regression----
jpeg(filename = 'output/glp1ra/figures/meta-regression/sbp/bmi6_sbp6_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi6_sbp6, xvar = "BMITE", yvar = "SBPTE",
                     linear_model = bmi_sbp_6_res, study_var = "study",
                     xlab = "BMI difference at 6 months (kg/m²)", ylab = "SBP difference at 6 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/glp1ra/figures/meta-regression/sbp/bmi_sbp12-24_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_sbp24, xvar = "BMITE", yvar = "SBPTE",
                     linear_model = bmi_sbp_24_res, study_var = "study_id", 
                     xlab = "BMI difference at 12-24 months (kg/m²)", ylab = "SBP difference at 12-24 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/glp1ra/figures/meta-regression/dbp/bmi6_dbp6_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi6_dbp6, xvar = "BMITE", yvar = "DBPTE",
                     linear_model = bmi_dbp_6_res, study_var = "study",
                     xlab = "BMI difference at 6 months (kg/m²)", ylab = "DBP difference at 6 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/glp1ra/figures/meta-regression/dbp/bmi_dbp12-24_linreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_dbp24, xvar = "BMITE", yvar = "DBPTE",
                     linear_model = bmi_dbp_24_res, study_var = "study_id", 
                     xlab = "BMI difference at 12-24 months (kg/m²)", ylab = "DBP difference at 12-24 months (mmHg)"
)
dev.off()

# Visualise the linear and quad regression

jpeg(filename = 'output/glp1ra/figures/meta-regression/sbp/bmi6_sbp6_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi6_sbp6, xvar = "BMITE", yvar = "SBPTE",
                     linear_model = bmi_sbp_6_res,quadratic_model = bmi_sbp_6_res_quad, study_var = "study",
                     xlab = "BMI difference at 6 months (kg/m²)", ylab = "SBP difference at 6 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/glp1ra/figures/meta-regression/sbp/bmi_sbp12-24_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_sbp24, xvar = "BMITE", yvar = "SBPTE",
                     linear_model = bmi_sbp_24_res,quadratic_model = bmi_sbp_24_res_quad, study_var = "study_id", 
                     xlab = "BMI difference at 12-24 months (kg/m²)", ylab = "SBP difference at 12-24 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/glp1ra/figures/meta-regression/dbp/bmi6_dbp6_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi6_dbp6, xvar = "BMITE", yvar = "DBPTE",
                     linear_model = bmi_dbp_6_res, quadratic_model = bmi_dbp_6_res_quad, study_var = "study",
                     xlab = "BMI difference at 6 months (kg/m²)", ylab = "DBP difference at 6 months (mmHg)"
)
dev.off()

jpeg(filename = 'output/glp1ra/figures/meta-regression/dbp/bmi_dbp12-24_lin_quadreg.jpeg', 
     width = 6000,    
     height = 3000,     
     res = 600,
     quality = 100)
plot_meta_regression(data = bmi24_dbp24, xvar = "BMITE", yvar = "DBPTE",
                     linear_model = bmi_dbp_24_res, quadratic_model = bmi_dbp_24_res_quad, study_var = "study_id", 
                     xlab = "BMI difference at 12-24 months (kg/m²)", ylab = "DBP difference at 12-24 months (mmHg)"
)
dev.off()
