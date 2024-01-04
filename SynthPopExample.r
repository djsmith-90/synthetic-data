### Example script for creating synthesised datasets using the freely-available ALSPAC training dataset (https://osf.io/8sgze)
### Created by Dan Major-Smith
### R version 4.0.4


###########################################################################################
#### Clear workspace, install/load packages, and set working directory
rm(list = ls())

setwd("FILEPATH")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("synthpop")
library(synthpop)

#install.packages("readstata13")
library(readstata13)


###########################################################################################
#### Read in the example Stata dataset. For this example, say that our outcome is depression at age 17 and out exposure is post-natal maternal depression (approx. 8 months after delivery), adjusting for a range of plausible sociodemographic confounders

data_raw <- read.dta13("Master_MSc_Data.dta")

# Take a working copy and just a subset of variables relevant for analysis
dat <- data_raw[, c("gender", "bwt", "gest", "ethnic", "matage", "mated", "pated", "msoc", "psoc", 
                     "housing", "marital", "parity", "pregSize", "mat_dep", "depression_17")]
summary(dat)


## Check each variable and recode if necessary

# Child gender
table(dat$gender, useNA = "ifany")

dat <- dat %>%
  mutate(gender = ifelse(gender == 1, "Boy", "Girl")) %>%
  mutate(gender = factor(gender, levels = c("Boy", "Girl")))

table(dat$gender, useNA = "ifany")

# Child birthweight (grams)
summary(dat$bwt)

# Child gestation (weeks)
summary(dat$gest)

# Child ethnicity
table(dat$ethnic, useNA = "ifany")

dat <- dat %>%
  mutate(ethnic = ifelse(ethnic == 1, "White", "Other than White")) %>%
  mutate(ethnic = factor(ethnic, levels = c("White", "Other than White")))

table(dat$ethnic, useNA = "ifany")

# Maternal age at birth
summary(dat$matage)

# Highest maternal educational qualification
table(dat$mated, useNA = "ifany")

dat <- dat %>%
  mutate(mated = ifelse(mated == 1, "CSE/None", 
                        ifelse(mated == 2, "Vocational", 
                               ifelse(mated == 3, "O level",
                                      ifelse(mated == 4, "A level", "Degree"))))) %>%
  mutate(mated = factor(mated, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree")))

table(dat$mated, useNA = "ifany")

# Highest paternal educational qualification
table(dat$pated, useNA = "ifany")

dat <- dat %>%
  mutate(pated = ifelse(pated == 1, "CSE/None", 
                        ifelse(pated == 2, "Vocational", 
                               ifelse(pated == 3, "O level",
                                      ifelse(pated == 4, "A level", "Degree"))))) %>%
  mutate(pated = factor(pated, levels = c("CSE/None", "Vocational", "O level", "A level", "Degree")))

table(dat$pated, useNA = "ifany")

# Maternal occupational social class (will combine categories IV and V together, as small N)
table(dat$msoc, useNA = "ifany")

dat <- dat %>%
  mutate(msoc = ifelse(msoc == 1, "I",
                       ifelse(msoc == 2, "II",
                              ifelse(msoc == 3, "III (non-manual)",
                                     ifelse(msoc == 4, "III (manual)", "IV/V"))))) %>%
  mutate(msoc = factor(msoc, levels = c("I", "II", "III (non-manual)", "III (manual)", "IV/V")))

table(dat$msoc, useNA = "ifany")

# Paternal occupational social class (will combine categories IV and V together, as small N)
table(dat$psoc, useNA = "ifany")

dat <- dat %>%
  mutate(psoc = ifelse(psoc == 1, "I",
                       ifelse(psoc == 2, "II",
                              ifelse(psoc == 3, "III (non-manual)",
                                     ifelse(psoc == 4, "III (manual)", "IV/V"))))) %>%
  mutate(psoc = factor(psoc, levels = c("I", "II", "III (non-manual)", "III (manual)", "IV/V")))

table(dat$psoc, useNA = "ifany")

# Housing status (will combine some categories together)
table(dat$housing, useNA = "ifany")

dat <- dat %>%
  mutate(housing = ifelse(housing == 0 | housing == 1, "Owned/Mortgaged",
                          ifelse(housing == 2 | housing == 5, "Council/HA",
                                 ifelse(housing == 3 | housing == 4, "Rented", "Other")))) %>%
  mutate(housing = factor(housing, levels = c("Owned/Mortgaged", "Council/HA", "Rented", "Other")))

table(dat$housing, useNA = "ifany")

# Mother's marital status (will combine some categories together)
table(dat$marital, useNA = "ifany")

dat <- dat %>%
  mutate(marital = ifelse(marital == 1, "Never married", 
                          ifelse(marital == 5 | marital == 6, "Married", "Widowed/Divorced/Separated"))) %>%
  mutate(marital = factor(marital, levels = c("Married", "Never married", "Widowed/Divorced/Separated")))

table(dat$marital, useNA = "ifany")

# Mother's parity
table(dat$parity, useNA = "ifany")

dat <- dat %>%
  mutate(parity = ifelse(parity == 0, "0",
                         ifelse(parity == 1, "1", "2 or more"))) %>%
  mutate(parity = factor(parity, levels = c("0", "1", "2 or more")))

table(dat$parity, useNA = "ifany")

# Size of pregnancy
table(dat$pregSize, useNA = "ifany")

dat <- dat %>%
  mutate(pregSize = ifelse(pregSize == 1, "Singleton", "Twin")) %>%
  mutate(pregSize = factor(pregSize, levels = c("Singleton", "Twin")))

table(dat$pregSize, useNA = "ifany")

# Maternal post-natal depression (assessed using EPDS)
table(dat$mat_dep, useNA = "ifany")
summary(dat$mat_dep)

# Child depression at age 17 (assessed using CIS-R)
table(dat$depression_17, useNA = "ifany")

dat <- dat %>%
  mutate(depression_17 = ifelse(depression_17 == 0, "No", "Yes")) %>%
  mutate(depression_17 = factor(depression_17, levels = c("No", "Yes")))

table(dat$depression_17, useNA = "ifany")


# Keep just complete cases in multivariable analysis
cca_marker <- complete.cases(dat[, c("gender", "ethnic", "matage", "mated", "housing", "mat_dep", "depression_17")])
table(cca_marker)
dat <- dat[cca_marker == TRUE, ]
summary(dat)


################################################################################################################
#### Now start the synthpop process

# Get information about variables in the dataset
codebook.syn(dat)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
dat_syn <- syn(dat, seed = 13327)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, 4 observations have been dropped (0.11% of data)
replicated.uniques(dat_syn, dat)
dat_syn <- sdc(dat_syn, dat, rm.replicated.uniques = TRUE)

# Explore this synthetic dataset - The first command gives a useful summary of the synthetic dataset, including: the number of synthesised datasets (here, just 1), the first few rows of the synthesised data, the method used to synthesise each variable, and a predictor matrix of the variables used to predict each synthesised variable.
dat_syn
summary(dat_syn)


## Take a few unique true observations, and make sure not fully-replicated in synthetic dataset (based on the 'replicated.uniques' command from the 'synthpop' package)

# Make a dataset just of unique individuals using the observed data (as if two or more participants share exactly the same data, then it's impossible to link back to a unique individual)
sum(!(duplicated(dat) | duplicated(dat, fromLast = TRUE)))
dat_unique <- dat[!(duplicated(dat) | duplicated(dat, fromLast = TRUE)), ]

# Make a dataset just of unique individuals from the synthetic dataset
sum(!(duplicated(dat_syn$syn) | duplicated(dat_syn$syn, fromLast = TRUE)))
syn_unique <- dat_syn$syn[!(duplicated(dat_syn$syn) | duplicated(dat_syn$syn, fromLast = TRUE)), ]

# Select a random row from the observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 1), ])

# Combine observed row with the synthetic data, and see if any duplicates
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))

# Repeat for a few more rows of observed data
(row_unique <- dat_unique[sample(nrow(dat_unique), 10), ])
sum(duplicated(rbind.data.frame(syn_unique, row_unique)))


## Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is very good). Save this as a PDF
compare(dat_syn, dat, stat = "counts", nrow = 3, ncol = 5)

pdf("ComparingDescStats.pdf", height = 10, width = 16)
compare(dat_syn, dat, stat = "counts", nrow = 3, ncol = 5)
dev.off()

setEPS()
postscript("ComparingDescStats.eps", height = 10, width = 16)
compare(dat_syn, dat, stat = "counts", nrow = 3, ncol = 5)
dev.off()


## Simple analysis of child depression as outcome and maternal depression scaore as exposure to check that get similar results in both datasets (i.e., that the structures of the dataset are preserved)

# Now run the univariable model
model.real <- glm(depression_17 ~ mat_dep, family = "binomial", data = dat)
summary(model.real)

model.syn <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn)
summary(model.syn)

# Get comparable pattern of results (and store as PDF)
compare(model.syn, dat)

pdf("ComparingUnadjustedModel.pdf", height = 4, width = 6)
compare(model.syn, dat)
dev.off()

setEPS()
postscript("ComparingUnadjustedModel.eps", height = 4, width = 6)
compare(model.syn, dat)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2 <- glm(depression_17 ~ mat_dep + matage + ethnic + gender + mated + housing, 
                   family = "binomial", data = dat)
summary(model.real2)

model.syn2 <- glm.synds(depression_17 ~ mat_dep + matage + ethnic + gender + mated + housing, 
                        family = "binomial", data = dat_syn)
summary(model.syn2)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2, dat)

pdf("ComparingAdjustedModel.pdf", height = 6, width = 8)
compare(model.syn2, dat)
dev.off()

setEPS()
postscript("ComparingAdjustedModel.eps", height = 6, width = 8)
compare(model.syn2, dat)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
dat_syn$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(dat_syn$syn)), dat_syn$syn)
summary(dat_syn)


### Store the synthetic dataset for others to use - Save as RData, CSV and Stata formats
write.syn(dat_syn, file = "syntheticData", filetype = "RData", save.complete = FALSE)
write.syn(dat_syn, file = "syntheticData", filetype = "csv", save.complete = FALSE)
write.syn(dat_syn, file = "syntheticData", filetype = "Stata", convert.factors = "labels", save.complete = FALSE)



##############################################################################################################
#### Exploring some different synthesis methods to see if/how this alters synthesis

## 1) Using parametric, rather than tree-based, methods for synthesis

# Synthesis is slower
dat_syn_para <- syn(dat, seed = 674417, method = "parametric")

# Fewer unique replicates here (0, rather than 4)
replicated.uniques(dat_syn_para, dat)
dat_syn_para <- sdc(dat_syn_para, dat, rm.replicated.uniques = TRUE)

# Explore data (esp. method of synthesis)
dat_syn_para

# Compare results (get comparable pattern of results)
model.syn_para <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn_para)
summary(model.syn_para)
compare(model.syn_para, dat)


## 2) Altering visit sequence, with exposure and outcome synthesised first
dat2 <- dat %>%
  relocate(mat_dep, depression_17, .before = gender)
head(dat2)

# Tree-based synthesis
dat_syn2 <- syn(dat2, seed = 675253)

# Check and remove unique replicates
replicated.uniques(dat_syn2, dat2)
dat_syn2 <- sdc(dat_syn2, dat2, rm.replicated.uniques = TRUE)

# Explore data
dat_syn2

# Compare results (get very different results here, as now the association in the synthetic data is essential null, with little-to-no overlap in the 95% confidence intervals)
model.syn2 <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn2)
summary(model.syn2)
compare(model.syn2, dat2)


## 3) Altering the random seed for synthesis (as sometimes may get a poor correspondence between observed and synthetic data just by chance) - Will use the 'dat2' dataset above.

# Tree-based synthesis, with a different seed
dat_syn2b <- syn(dat2, seed = 77742)

# Check and remove unique replicates
replicated.uniques(dat_syn2b, dat2)
dat_syn2b <- sdc(dat_syn2b, dat2, rm.replicated.uniques = TRUE)

# Explore data
dat_syn2b

# Compare results (get very similar results here to the model above when using a different seed, potentially suggesting that the lack of correspondence is due to the synthesising model, rather than random fluctuations). For this example, it would be better to use the original results with the exposure and outcome synthesised last, as that appeared to better reproduce the observed association (although, at other times, simply altering the random seed can improve correspondence).
model.syn2b <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn2b)
summary(model.syn2b)
compare(model.syn2b, dat2)


## 3b) Same visit sequence as above (i.e., exposure and outcome synthesised first), but this time using a parametric model, to see whether this improves correspondence between synthesised and observed data.

# Parametric synthesis
dat_syn2b_para <- syn(dat2, seed = 236732, method = "parametric")

# Check and remove unique replicates
replicated.uniques(dat_syn2b_para, dat2)
dat_syn2b_para <- sdc(dat_syn2b_para, dat2, rm.replicated.uniques = TRUE)

# Explore data
dat_syn2b_para

# Compare results (this time we get more similar results for both the synthesised and observed data)
model.syn2b_para <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn2b_para)
summary(model.syn2b_para)
compare(model.syn2b_para, dat2)



## 4) Using 'smoothing' to alter continuous variables so they differ from the observed values and lowers the disclosure risk even more. Will use tree-based methods here. Note that smoothing works best for numeric variables that are either truly continuous (i.e., can take decimal values) or are discrete/integer but with a larger number of possible values (e.g., birthweight in grams); for integer variables with a low number of values (e.g., gestation in weeks, maternal age in years, or depression score), smoothing works less well as it just adds small amounts of noise to values, but means they are still potentially disclosive (e.g., an age of 30 might be smoothed to between 29.95 and 30.05, but it's still obvious that the 'true' value should be 30).

# Tree-based synthesis, with smoothing for the birthweight variable
dat_syn_smooth <- syn(dat, seed = 135704,
                      smoothing = list(bwt = "density"))

# Fewer unique replicates here (were 4 without smoothing, and now only 2 with smoothing)
replicated.uniques(dat_syn_smooth, dat)
dat_syn_smooth <- sdc(dat_syn_smooth, dat, rm.replicated.uniques = TRUE)

# Explore data
dat_syn_smooth
summary(dat_syn_smooth)

# Compare results (get comparable pattern of results)
model.syn_smooth <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn_smooth)
summary(model.syn_smooth)
compare(model.syn_smooth, dat)


## 5) If we were worried about potentially-disclosive outliers, we could also recode via top and/or bottom coding using the 'sdc' command

# Tree-based synthesis, without smoothing
dat_syn_recode <- syn(dat, seed = 881748)

# Check distributions of continuous variables
hist(dat_syn_recode$syn$bwt) ## Recode very low (<1000) and very high (>5000) outliers
hist(dat_syn_recode$syn$gest) ## Recode very low (<30) and very high (>45) outliers
hist(dat_syn_recode$syn$mat_dep) ## Recode very high (>25) outliers

# Recode these variables
dat_syn_recode <- sdc(dat_syn_recode, dat, recode.vars = c("bwt", "gest", "mat_dep"),
                      bottom.top.coding = list(c(1000, 5000), c(30, 45), c(0, 25)))

# Check these recoded variables
summary(dat_syn_recode)

# Number of unique replicates and remove them
replicated.uniques(dat_syn_recode, dat)
dat_syn_smooth <- sdc(dat_syn_recode, dat, rm.replicated.uniques = TRUE)

# Explore data
dat_syn_recode

# Compare results (get comparable pattern of results)
model.syn_recode <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn_recode)
summary(model.syn_recode)
compare(model.syn_recode, dat)



####################################################################################################
##### Repeat the synthesis process above, this time removing unused variables - This is to show that, when using a small number of variables, especially if categorical and some responses have low numbers of responses, synthetic data can result in a larger number of unique replicates between observed and synthetic data

# Remove unused variables
dat_red <- dat[, c("gender", "ethnic", "matage", "mated", "housing", "mat_dep", "depression_17")]
summary(dat_red)

# Get information about variables in the dataset
codebook.syn(dat_red)$tab

# Create a synthetic dataset using default options (which are non-parametric/CART [classification and regression trees])
dat_syn_red <- syn(dat_red, seed = 82691)

# Use the 'sdc' command (statistical disclosure control) to identify and remove any cases that are unique in both synthetic and observed data (i.e., cases which may be disclosive) - Here, because there are a relatively small number of variables (most of which are categorical), there are 374 observations which would have been dropped (10.03% of data)
replicated.uniques(dat_syn_red, dat_red)


### This is quite a considerable amount of data loss, so will try an alternative method to make the data less disclosive, starting with parametric synthesis
dat_syn_red2 <- syn(dat_red, seed = 909242, method = "parametric")

# A little better, but still 259 unique replicates (6.95%)
replicated.uniques(dat_syn_red2, dat_red)


### Next, will combine parametric synthesis with smoothing by adding a 'smoothing' parameter to the continuous variables 'matage' and 'mat_dep' (predominantly to show that it is a bad idea to add smoothing to integer variables with low numbers of values, as discussed above)
dat_syn_red3 <- syn(dat_red, seed = 450313, method = "parametric", 
                    smoothing = list(matage = "density", mat_dep = "density"))

# Now there are no unique replicates, although if we look at the data it looks like just a bit of jitter has been added to these variables (e.g., a maternal age of 30 has become an age of 29.95), which could still be potentially disclosive (the 'smoothing' option works much better for truly continuous variables, or discrete variables with a large number of potential values (e.g., birthweight in grams), rather than discrete/whole integer numbers, especially if the number of integer values is quite low, as here, with only ~30 values for maternal age and depression). So, while this synthesis appears to result in no unique replicates, this is somewhat illusory.
replicated.uniques(dat_syn_red3, dat_red)

# Maternal age
head(dat_syn_red3$syn$matage)

# Maternal depression
head(dat_syn_red3$syn$mat_dep)


### Next, will keep the parametric synthesis, but drop the smoothing and replace with top and bottom coding some outliers for maternal age and maternal depression
dat_syn_red4 <- syn(dat_red, seed = 895541, method = "parametric")

# Check distributions of continuous variables
hist(dat_syn_red4$syn$matage) ## Recode very low (<18) and very high (>40) outliers
hist(dat_syn_red4$syn$mat_dep) ## Recode very high (>20) outliers

# Recode these variables
dat_syn_red4 <- sdc(dat_syn_red4, dat_red, recode.vars = c("matage", "mat_dep"),
                      bottom.top.coding = list(c(18, 40), c(0, 20)))

# There are still 258 unique replicates here (6.92%), so top and bottom coding did not alter much
replicated.uniques(dat_syn_red4, dat_red)


### Will return to original CART-based synthesis to see which variables are over-represented in these unique replicates
dat_syn_red <- syn(dat_red, seed = 82691)

# 374 unique replicates (10.03% of data)
replicated.uniques(dat_syn_red, dat_red)

# Append this unique replicates marker to the synthesised dataset
dat_red_check <- dat_syn_red$syn
dat_red_check$rep <- replicated.uniques(dat_syn_red, dat_red)$replications
summary(dat_red_check)
head(dat_red_check)

# Unique replicates by gender - No real difference
table(dat_red_check$gender, dat_red_check$rep)
round(prop.table(table(dat_red_check$gender, dat_red_check$rep), margin = 1) * 100, 2)
chisq.test(table(dat_red_check$gender, dat_red_check$rep), correct = FALSE)

# Unique replicates by ethnicity - 'Other than white' participants slightly more likely to be unique replicates, but little difference according to chi-square test
table(dat_red_check$ethnic, dat_red_check$rep)
round(prop.table(table(dat_red_check$ethnic, dat_red_check$rep), margin = 1) * 100, 2)
chisq.test(table(dat_red_check$ethnic, dat_red_check$rep), correct = FALSE)

# Unique replicates by maternal age - Harder to read pattern (as small cell counts per age), but looks as though younger and older mums more likely to be unique replicates - Confirmed by chi-square test (although should be treated with caution, as small cell counts)
table(dat_red_check$matage, dat_red_check$rep)
round(prop.table(table(dat_red_check$matage, dat_red_check$rep), margin = 1) * 100, 2)
chisq.test(table(dat_red_check$matage, dat_red_check$rep), correct = FALSE)

# Unique replicates by maternal education - Much more likely to be a unique replicate if CSE/None or vocational qualification - Confirmed by chi-square test
table(dat_red_check$mated, dat_red_check$rep)
round(prop.table(table(dat_red_check$mated, dat_red_check$rep), margin = 1) * 100, 2)
chisq.test(table(dat_red_check$mated, dat_red_check$rep), correct = FALSE)

# Unique replicates by housing - More likely to be a unique replicate if rent or council/HA - Confirmed by chi-square test
table(dat_red_check$housing, dat_red_check$rep)
round(prop.table(table(dat_red_check$housing, dat_red_check$rep), margin = 1) * 100, 2)
chisq.test(table(dat_red_check$housing, dat_red_check$rep), correct = FALSE)

# Unique replicates by maternal depression - Harder to read pattern (as small cell counts per score), but looks as though higher depression scores more likely to be unique replicates - Confirmed by chi-square test (although should be treated with caution, as small cell counts)
table(dat_red_check$mat_dep, dat_red_check$rep)
round(prop.table(table(dat_red_check$mat_dep, dat_red_check$rep), margin = 1) * 100, 2)
chisq.test(table(dat_red_check$mat_dep, dat_red_check$rep), correct = FALSE)

# Unique replicates by offspring depression - More likely to be a unique replicate if depressed - Confirmed by chi-square test
table(dat_red_check$depression_17, dat_red_check$rep)
round(prop.table(table(dat_red_check$depression_17, dat_red_check$rep), margin = 1) * 100, 2)
chisq.test(table(dat_red_check$depression_17, dat_red_check$rep), correct = FALSE)


### Given this, there does seem to be a few variables driving the high number of unique replicates, mainly maternal age, education, housing status, maternal depression and offspring depression. Because most of the variables are categorical, or are continuous with a relatively small number of possible values (~30 here), this appears to inflate the number of unique replicates. This is in contrast to the original synthesis above, using the same number of observations but a larger number of variables (15, compared to 7 here); shows that sometimes fewer variables can lead to more unique replicates, especially if these variables are categorical. 

## If we increase the number of variables synthesised, this could reduce the number of unique replicates removed, especially if they are continuous variables. Will demonstrate this by adding 'birthweight' to the synthesis model, which is a continuous variable taking on a large number of possible values.
dat_red5 <- dat[, c("gender", "bwt", "ethnic", "matage", "mated", "housing", "mat_dep", "depression_17")]
summary(dat_red5)

# Now synthesise
dat_syn_red5 <- syn(dat_red5, seed = 793757)

# Down to 259 unique replicates (6.95% of data), down from 374 (10.03% of data)
replicated.uniques(dat_syn_red5, dat_red5)

# If run parametric model, this will decrease even further - Now down to 52 unique replicates (1.40% of data)
dat_syn_red5b <- syn(dat_red5, seed = 654363, method = "parametric")
replicated.uniques(dat_syn_red5b, dat_red5)

# And if add smoothing to bwt (which works because there are enough values to smooth over, unlike maternal age and depression above) - Now only 1 unique replicate (0.03% of data)
dat_syn_red5c <- syn(dat_red5, seed = 891548, method = "parametric", 
                     smoothing = list(bwt = "density"))
replicated.uniques(dat_syn_red5c, dat_red5)

head(dat_syn_red5c$syn$bwt)


### The take-home message is that, while we recommend to limit the number of variables synthesised, this can sometimes result in a larger number of unique replicates if there are a small number of variables, especially if they are categorical.


### An alternative approach may be to recode some of the categories over-represented as unique replicates (e.g., age, education, housing, etc. in the example above) and re-run synthesis with these larger groupings. One issue with this, however, is that now the models from the paper will not exactly correspond to those from the synthetic datasets.

## Let's explore this quickly anyway, by combining education into 'low' (CSE/vocational/O-level) vs 'high' (A-level/degree), combining housing into 'owned/mortgaged' vs 'rent/council/HA/other', top and bottom coding age to be between 20 and 40, and top coding maternal depression to be maximum of 20.

# Create dataset
dat_red6 <- dat_red
summary(dat_red6)

# Recode maternal education
dat_red6 <- dat_red6 %>%
  mutate(mated = ifelse(mated == "CSE/None" | mated == "Vocational" | mated == "O level", "Low", "High")) %>%
  mutate(mated = factor(mated, levels = c("Low", "High")))
table(dat_red6$mated)

# Recode housing
dat_red6 <- dat_red6 %>%
  mutate(housing = ifelse(housing == "Owned/Mortgaged", "Owned/Mortgaged", "Rent/Council/Other")) %>%
  mutate(housing = factor(housing, levels = c("Owned/Mortgaged", "Rent/Council/Other")))
table(dat_red6$housing)
  

## Now synthesise
dat_syn_red6 <- syn(dat_red6, seed = 710355)

# Recode maternal age and depression variables
dat_syn_red6 <- sdc(dat_syn_red6, dat_red6, recode.vars = c("matage", "mat_dep"),
                    bottom.top.coding = list(c(20, 40), c(0, 20)))

# Has reduced down from 374 (10.03% of data) to 261 (7.00% of data), but still quite a large amount
replicated.uniques(dat_syn_red6, dat_red6)



#### Circling back to the original synthetic dataset, let's remove the 10% unique replicates and see how results compare against the observed data - Even if the sample size differs, as long as the results are similar this doesn't really matter too much.

dat_syn_red <- syn(dat_red, seed = 82691)
replicated.uniques(dat_syn_red, dat_red)
dat_syn_red <- sdc(dat_syn_red, dat_red, rm.replicated.uniques = TRUE)

# Explore this synthetic dataset - The first command gives a useful summary of the synthetic dataset, including: the number of synthesised datasets (here, just 1), the first few rows of the synthesised data, the method used to synthesise each variable, and a predictor matrix of the variables used to predict each synthesised variable.
dat_syn_red
summary(dat_syn_red)


## Compare between actual and synthetic datasets - This provides tables and plots comparing distribution of variables between the two datasets (correspondence is still quite good, but as lost ~10% of the data the frequencies for the synthetic data are slightly lower than the observed, although still mostly proportional). Save this as a PDF
compare(dat_syn_red, dat_red, stat = "counts", nrow = 2, ncol = 4)

pdf("ComparingDescStats_red.pdf", height = 8, width = 14)
compare(dat_syn_red, dat_red, stat = "counts", nrow = 2, ncol = 4)
dev.off()

# If plot by percent, rather than count, correspondence is clearer.
compare(dat_syn_red, dat_red, nrow = 2, ncol = 4)

pdf("ComparingDescStats_red_percent.pdf", height = 8, width = 14)
compare(dat_syn_red, dat_red, nrow = 2, ncol = 4)
dev.off()


## Simple analysis of child depression as outcome and maternal depression scaore as exposure to check that get similar results in both datasets (i.e., that the structures of the dataset are preserved)

# Now run the univariable model
model.real_red <- glm(depression_17 ~ mat_dep, family = "binomial", data = dat_red)
summary(model.real_red)

model.syn_red <- glm.synds(depression_17 ~ mat_dep, family = "binomial", data = dat_syn_red)
summary(model.syn_red)

# Get comparable pattern of results (and store as PDF)
compare(model.syn_red, dat_red)

pdf("ComparingUnadjustedModel_red.pdf", height = 4, width = 6)
compare(model.syn_red, dat_red)
dev.off()


## Test this with a more complex model, with additional covariates
model.real2_red <- glm(depression_17 ~ mat_dep + matage + ethnic + gender + mated + housing, 
                   family = "binomial", data = dat_red)
summary(model.real2_red)

model.syn2_red <- glm.synds(depression_17 ~ mat_dep + matage + ethnic + gender + mated + housing, 
                        family = "binomial", data = dat_syn_red)
summary(model.syn2_red)

# Again, get comparable pattern of results, this time for all of the additional coefficients in the model as well (again, store as PDF)
compare(model.syn2_red, dat_red)

pdf("ComparingAdjustedModel_red.pdf", height = 6, width = 8)
compare(model.syn2_red, dat_red)
dev.off()


### Adding in a variable called 'FALSE_DATA', with the value 'FALSE_DATA' for all observations, as an additional safety check to users know the dataset is synthetic
dat_syn_red$syn <- cbind(FALSE_DATA = rep("FALSE_DATA", nrow(dat_syn_red$syn)), dat_syn_red$syn)
summary(dat_syn_red)


### Store the synthetic dataset for others to use - Save as RData, CSV and Stata formats
write.syn(dat_syn_red, file = "syntheticData_red", filetype = "RData", save.complete = FALSE)
write.syn(dat_syn_red, file = "syntheticData_red", filetype = "csv", save.complete = FALSE)
write.syn(dat_syn_red, file = "syntheticData_red", filetype = "Stata", convert.factors = "labels", save.complete = FALSE)

