# Instrumental variable analysis applied on randomised controlled trials of glucagon-like-peptide-1 receptor agonists and bariatric surgery

## Project overview

This repository contains the code, documentation and reproducible analytical workflows for the project:
> Instrumental Variable Analysis applied to Randomised Controlled Trials of Glucagon-like-peptide-1 receptor Agonsist and Bariatric Surgery.

This project evaluates the causal effect of body mass index change on blood pressure using instrumental variable (IV) analysis applied to randomised controlled trials of:
- Bariatric surgery (BS)
- Glucagon-like-peptide-1 receptor agonists (GLP1RAs)

## Repository Structure

```text
.
├── data/           # Raw and processed datasets
├── docs/           # SAPs, decision logs
├── output/         # Generated tables and figures
├── R/              # Reusable functions
├── scripts/        # Analysis pipelines
├── renv.lock       # Reproducible package environment
└── README.md
```
## Analysis workflow
### Run scripts in the following order
1.`01_import_data.R`  
2.`02_study_outcome_visualisation.R`  
3.`03_trialarms_updatefemalenumber_proportion.R`   
4.`04_baseline_updateCIsSDsSEs.R`   
5.`05_results_updatemissingvalues.R`    
6.`06_separatemultiplearmsintoAsandBs.R`   
7.`07_baseline_post_visualisation.R`   
8.`08_euclidean_distance.R`   
9.`09_hypothesistest_usingt-test.R`   
10.`10_combinemultiarm_trials.R`    
11.`11_instrumentstrength_Fstat.R`   
12.`12_instrumentalvariableestimation.R`   
13.`13_metanalysis_visualisation.R`   
14.`14_bmi_hypertension.R`    
15.`15_metaregression_and_visualisation.R`    
### 1. Data preparation
- Import RCT datasets from BS and GLP1RAs studies separately.
- Read in different sheets of data and harmonise the variable names across the datasets
- Create analysis ready datasets
- - Save RDS object and csv files for the data to be used by other scripts.
### 2. Outcome evidence mapping post randomisation.
- Visualise the outcomes (blood pressure traits) where there is data using a dot plot.
- X-axis - time in months and Y-axis study labels (first author last name and year of publication)
### 3. Update data
- Read in the RDS files for BS and GLP1RAs separately
- Update number and proportion of females where they are missing
- Update baseline features missing values such as confidence intervals (CI) of mean BMI and BP
- Update results, follow up mean,SDs,SEs and CIs for BMI and BP post randomisation at different time points where possible.
- Quality control - manually check completeness of the data and update when necessary
- Manually verify the calculations by randomly picking on some output and calculating by hand.
- Save the data files for data-driven visualisation and IV analysis
### 4. Data-driven visualisation
- Read in the data from previous step
- Where there are multiple treatment arms, use alphabets to annotate different intervention and controls
- Plot baseline and post randomisation plots of mean and CIs for BMI (x-axis) versus mean and CIs for BP (y-axis)
- Calculate the Euclidean distance at baseline and visualise. aiming to answer the question of whether the treated and control started with the same BMI and BP or not. Standardise the mean differences of BMI and BP, set mean to 0 and sd to 1
- Perform an hypothesis test, evaluating whether indeed the treatment groups had different BMI and BP, two sample independent t-test
- write and save the t-test results in a csv file for reporting
- Use standard formula recommended in Cochrane Handbook to combine the multiarms for BS (avoids unit of error through double counting)
- Use exact adjustment method (Ruckers et al 2017) to handle multiple treatment groups for GLP1RAs. Use netmeta library to generate adjusted se of treatment effects. (avoids unit of error through double counting)
- Save the data (RDS object) to be used in IV analysis
- Estimate the F-statistic, a square of t-statistic to assess first IV assumption (relevance)
- Visualise the F-statistic in a plot
### 5. IV analysis
- Read in the data from above on combined arms
- Calculate the Wald ratio, blood pressure difference / BMI difference where there is sufficient data.
- Meta-analyse and visualise in a forest plot
- Calculate the IV estimate for hypertension prevalence where possible; for BS
- Meta-analyse and visualise the effect of BMI difference on hypertension in a forest plot where possible
### 6. Metaregression
- Run linear and quad random effects metaregression on BMI and BP differences
- Visualiseo on the same scatter plot
### 7. Results
- Create tables and figures
- Export all figures and tables to respective subdirectories in output

## Reproducibility
This project uses `renv` for package management.     
Restore the project environment using:        
`renv::restore()`   
Rerun the scripts in sequence to reprodue all analyses and outputs.


## Authors
Winfred N Gatua
