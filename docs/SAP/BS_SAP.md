# Data synthesis and visualisation
## Assess extracted data for completeness   
a.	Review all extracted data for (Mean values, SDs, SEs, 95% CIs, Sample sizes, Change from baseline estimates, Follow up means, Prevalence/number of antihypertensives (hypertension))    
b.	Standardise all continuous outcomes (BMI, SBP, DBP) where possible    
c.	Create a derived data sheet that automatically calculates: (SE form CI, SD from SE, CI from SD and sample size, follow up mean from baseline mean and change from baseline)    
## Missing data  
a.	Extract numerical data from figures using plotdigitizer (https://plotdigitizer.com)    
b.	Contact study authors requesting (outcome means, SDs, SEs, Follow up values)    
c.	Check clinicaltrials.gov if study authors do not respond    
## Characterise evidence base.   
a.	Summarise the studies by number of study arms 1) two treatment groups 2) multi-treatment groups. Present as a table with design and number of studies.   
b.	Multi-arm studies     
  i.	Apply Cochrane methods to create a single pairwise comparison (for example, combine control groups, combine surgical groups).  Expected comparison will be intervention vs control. Axon et al and Rucker et al 2017 (Pairwise comparisons – a) Combine groups b) exact adjustment which preserves the dosages)    
  ii.	Identify which papers are problematic for pairwise comparison to be discussed by all, Kate and may be David Phillipo    
c.	Follow up duration.   
  i.	Explore which follow-up periods are available?    
  ii.	Which periods contain BMI and blood pressure jointly and temporal?     
  iii.	Which duration is evidence most abundant?     
    Summarise and present a table of study ID, follow up duration with the outcome at each timepoint. Use Heat map and bar plot to visualise the outcomes and duration with most studies.      
d.	Outcome availability    
  i.	Summarise the studies and all possible outcome to guide prioritising studies for BMI-SBP, BMI-DBP and BMI-Hypertension analyses    
## Identify IV-ready studies
a.	Exposure  is BMI difference between intervention and control.   
b.	Outcomes SBP, DBP and hypertension     
a.	To be included there must be BMI measured before outcomes (blood pressure) or jointly with outcomes     
## Data-driven visualisation
a.	Visualise baseline and post intervention BMI, DBP, SBP for all studies timepoints.     
b.	Calculate the Euclidean distance at baseline     
  a.	Standardise the data, since 10 units difference in SBP contributes more to the distance than a 1-unit BMI difference    
c.	Perform t-test and present the results to 2 decimal places (A table)     
d.	First-stage assessment (F-statistic calculator)     
  a.	How strongly does randomisation affect BMI?      
  b.	For each time point whenever BMI is reported post randomisation; calculate BMI difference and the SE. Finally calculate the F-stat by squaring the t-statistic. Interpretation; F>10 is a strong instrument, F<10 is a weak instrument. Use bubble plot to visualise the instrument strength for the studies by time points where possible.   
## Conduct instrumental variable analyses
a.	Estimating the effect of BMI reduction on SBP, DBP, hypertension (Temporality and with the same time point).  
  i.	First-stage assessment – calculate BMI difference    
  ii.	Second-stage assessment – calculate BP difference.    
  iii.	Calculate the Wald Ratio estimate – BP difference/BMI difference    
b.	For hypertension use an alternative formula to calculate the Wald Ratio where possible (2 * 2 table , IV estimator on risk difference).   
## Risk of bias assessment 
a.	Use a co-developed risk of bias tool to assess risk of bias for all the results – dual independent assessment with discussion     
b.	Use robVIS to visualise the risk of bias    
## Conventional meta-analysis
a.	Conduct a pairwise random-effects meta-analysis using the metafor package for SBP, DBP and hypertension (prevalence, remission, antihypertensive medication use where possible).   
b.	Conduct narrative synthesis of IV estimates where quantitative meta-analysis is not possible     
c.	Assess consistency of results using I2 metric     
d.	Explore between study heterogeneity using Cochran’s Q stat, consider study duration, instrument strength as potential sources of heterogeneity     
e.	Where possible perform meta-regression and subgroup analysis     
