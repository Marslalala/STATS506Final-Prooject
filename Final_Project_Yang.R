#######################################################################
#######################################################################
######                                                           ######
######                STATS 506 Final Project                    ######
######                        Yang Han                           ######
######                                                           ######
#######################################################################
#######################################################################



## Preparation

#  Load necessary packages.
library(dplyr)
library(ggplot2)
library(survey)
library(spatstat)



## Data Extraction

#  Load the data and set up the final weights.
cbecs <- read.csv("cbecs2018_final_public.csv")
samp_wts <- cbecs$FINALWT
rep_wts <- cbecs[, grepl("^FINALWT", names(cbecs))]
rep_wts$FINALWT <- NULL

#  Extract the variables we need.
cbecs_response <- cbecs %>%
  select(MFBTU, MFEXP, 
         ELBTU, ELCNS, 
         ELEXP, NGBTU, 
         NGCNS, NGEXP, 
         FKBTU, FKCNS, 
         FKEXP, DHBTU, 
         DHEXP, PUBID)
cbecs_building <- cbecs %>%
  select(DAYLTP, SQFT, 
         VACANT, RENOV, 
         YRCONC, PUBID)
cbecs_solar <- cbecs %>%
  select(SOUSED, SOPANEL, 
         SOTHERM, SOHT1, 
         SOHT2, SOWATR, 
         SOOTH, PUBID)
cbecs_operation <- cbecs %>%
  select(OPEN24, WKHRS, 
         NOCC, OWNOCC, 
         OWNOPR, PBA, 
         NWKER, PUBID)
cbecs_climate <- cbecs %>%
  select(CENDIV, PUBCLIM,
         HDD65, CDD65,
         PUBID)

#  The raw data contains missing values, and some of them need to be combined to form new variables which
#  may explain the research better. I just separated the potential responses and 4 sets of predictors for
#  next steps.



## Data Cleaning

#  Starting from the response, since we are considering the total consumption of other energy sources, we 
#  should add up all the energy usage for each observation.

#  Extract all energy consumption and calculate the total consumption in kBtu.
cbecs_response <- cbecs %>%
  select(elec = ELBTU,
         gas = NGBTU,
         oil = FKBTU,
         space = SQFT,
         PUBID) %>%
  mutate(elec = ifelse(is.na(elec), 0, elec),    # Replace missing values with 0
         gas = ifelse(is.na(gas), 0, gas),
         oil = ifelse(is.na(oil), 0, oil))
#  After the summation, we also need to standardize it with respect to the total square footage of all the space.
cbecs_response <- cbecs_response %>%
  mutate(total = elec + gas + oil,
         TEUI = total / space) %>%
  select(-elec, -gas, -oil, -space, -total)
View(cbecs_response)
#  Now we have our response variable TEUI: Total Annual Energy Usage Intensity with unit kBtu/ft^2*year. Notice that
#  in this variable solar energy is not included. It only considers three main energy type, which are electricity,
#  natural gas and oil.

#  Now let's consider the building set predictors.
#  Replace missing values with 0 for numerical variables.
cbecs_building <- cbecs_building %>%
  mutate(DAYLTP = ifelse(is.na(DAYLTP), 0, DAYLTP))

#  Dealing missing values for some variables.
#  We can see for variable VACANT most values are NAs, so I decide to remove this variable. The not applicable category
#  is 0 after the replacing of NAs.
cbecs_building <- cbecs_building %>%
  mutate(RENOV = as.factor(ifelse(is.na(RENOV), 0, RENOV)),
         YRCONC = ifelse(is.na(YRCONC), 0, YRCONC)) %>%
  select(-VACANT)
View(cbecs_building)
#  After cleaning the missing values, we need to decide the polarity of the variables with Likert scale to make sure it 
#  coincides with the response variable. We will save this before we remove all missing values in 4 sets of predictors.

#  Cleaning the solar set. (SOTH1 and SOOTH are excluded since all observations have the same category for these two)
cbecs_solar <- cbecs_solar %>%
  mutate(SOPANEL = as.factor(ifelse(is.na(SOPANEL), 0, SOPANEL)),
         SOTHERM = as.factor(ifelse(is.na(SOTHERM), 0, SOTHERM)),
         SOUSED = as.factor(SOUSED),
         SOHT2 = as.factor(SOHT2),
         SOWATR = as.factor(SOWATR)) %>%
  select(-SOHT1,
         -SOOTH)
View(cbecs_solar)
#  Cleaning the operational set.
cbecs_operation <- cbecs_operation %>%
  mutate(OWNOCC = as.factor(ifelse(is.na(OWNOCC), 0, OWNOCC)),
         NOCC = ifelse(NOCC == 996, 0, NOCC),
         OPEN24 = as.factor(OPEN24),
         OWNOPR = as.factor(OWNOPR),
         PBA = as.factor(PBA))
View(cbecs_operation)
#  Cleaning the climate set.
cbecs_climate <- cbecs_climate %>%
  mutate(PUBCLIM = ifelse(PUBCLIM == 5, 1, ifelse(PUBCLIM == 4, 2, ifelse(PUBCLIM == 7, 0, PUBCLIM))),
         CENDIV = as.factor(CENDIV))
View(cbecs_climate)

#  We changed the PUBCLIM to a numerical variable in Likert scale, with 1 being extreme climate, 2 being cool/warm,
#  3 being mixed mild and 0 being those not applicable.
#  Now consider the polarity of the numerical data with Likert scale.
#  Let us say the objective attitude is the larger the energy usage intensity, the smaller the number in Likert scale.
#  Treat the year of construction category as a numerical variable in Likert scale, we expect older buildings consume
#  energy more inefficient, so variable YRCONC has correct polarity. About the variable PUBCLIM, we expect more extreme
#  the climate is, more energy needed for heating/cooling, so this variable also has correct polarity.
#  Now the data is ready for statistical analysis.



## Exploratory Data Analysis

#  Beginning with generating some basic descriptive statistics and graphs to see which specific statistical method
#  should be used.
#  Combine all data first.
cbecs_full <- inner_join(cbecs_response, cbecs_building, by = "PUBID")
cbecs_full <- inner_join(cbecs_full, cbecs_climate, by = "PUBID")
cbecs_full <- inner_join(cbecs_full, cbecs_solar, by = "PUBID")
cbecs_full <- inner_join(cbecs_full, cbecs_operation, by = "PUBID")
cbecs_full <- cbecs_full %>%
  select(-PUBID)
View(cbecs_full)

#  Check quantile statistics and the mean of the response, TEUI.
summary(cbecs_full$TEUI)
#  The minimum value for energy usage intensity equals zero, which is quite impossible. Even if the building is no
#  longer in use, it would consume energy unless it is abandoned. The only possible explanation is that they are 
#  missing values that would be treated as outliers. Also, we can see that more than 75% of the observations have
#  ETUI less than or equal to 98.97 kBtu/ft2 per year. The maximum usage intensity is 1710.84 kBtu/ft2, which is 
#  quite large.

#  Since we are investigating the relationship between energy usage other than solar power and usage of solar power,
#  we can check the relationship between TEUI and solar-related predictors. SOUSED is a categorical variable with 1
#  indicating solar energy was used in the building in 2018 and 2 indicating was not used. We can form a one-factor
#  model to check if the means are different between two groups or not.
plot(TEUI ~ SOUSED, cbecs_full)
stripchart(TEUI ~ SOUSED, cbecs_full, vertical = TRUE, 
           method = "stack", xlab = "solar Energy Usage 2018", ylab = "Total Energy Usage Intensity")
lmod <- lm(TEUI ~ SOUSED, cbecs_full)
anova(lmod)
summary(lmod)

#  From the result of anova test and the box-plot we can see that there is no difference between two groups.

#  Check some relationships with other variables.
#  NWKER represents the working hour per week of employees present in the building. This represents the scale of
#  the building in some extent. We expect larger energy usage intensity with larger scale buildings.
plot(cbecs_full$WKHRS, cbecs_full$TEUI)
#  The plot shows that most building's energy usage intensities are indifferent to the number of working hours in the 
#  building, but there is a slight positive relationship between these two variables. More buildings would use
#  more energy resources as the weekly working hours getting higher.
#  Although we did not find any particular relationships between the response and some other variables, we can fit
#  a generalized linear regression model to begin, and carry out the analysis by adjust the first fitted model.



## Model Selection

#  Set up the sample design.
samp_design <- svrepdesign(weights = samp_wts, repweights = rep_wts,
                           type = "JK2", mse = TRUE, data = cbecs_full)

#  With the consideration of complex design of the survey, fit a ordinary least square regression model.
glmod <- svyglm(TEUI ~ DAYLTP + SQFT + RENOV + YRCONC + CENDIV + PUBCLIM + HDD65 + CDD65 + 
                SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                OPEN24 + WKHRS + NOCC + OWNOCC + OWNOPR + PBA + NWKER, 
                design = samp_design, family = gaussian)
summary(glmod)

#  Before selecting the best model step-by-step, I believe it is better to check for collinearities among
#  numerical variables to ensure the validity of the t-tests and numerical stability.
#  Compute the correlation matrix for numerical predictors.
num_var <- cbecs_full %>%
  select(DAYLTP, SQFT,
         YRCONC, PUBCLIM,
         HDD65, CDD65, 
         WKHRS, NOCC, 
         NWKER)
round(cor(num_var), 2)
#  We can see there are not significant correlations among these variables, implying there is no severe 
#  collinearity issue for the predictors.

#  Based on the summary of the initial model, use backward elimination to select our final generalized linear
#  model. We start from the building set of predictors.
#  RENOV represents if the building had undergone any renovations. We can see the two levels are all insignificant
#  which means neither undergone any renovations nor never undergo are likely to result the same as those buildings
#  not applicable. Remove this first.
glmod1 <- svyglm(TEUI ~ DAYLTP + SQFT + YRCONC + CENDIV + PUBCLIM + HDD65 + CDD65 + 
                  SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                  OPEN24 + WKHRS + NOCC + OWNOCC + OWNOPR + PBA + NWKER, 
                design = samp_design, family = gaussian)
summary(glmod1)
#  We assumed older building potentially has a low energy usage efficiency, which drives TEUI high, however, the 
#  insignificant p-value indicates that this is not true.
glmod2 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + HDD65 + CDD65 + 
                   SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                   OPEN24 + WKHRS + NOCC + OWNOCC + OWNOPR + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod2)
#  Then focus on climate set of predictors. CDD65 is statistically insignificant, remove this.
glmod3 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + HDD65 + 
                   SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                   OPEN24 + WKHRS + NOCC + OWNOCC + OWNOPR + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod3)
#  Eliminate HDD65.
glmod3 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                   OPEN24 + WKHRS + NOCC + OWNOCC + OWNOPR + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod3)
#  CENDIV represents census division. There are some levels giving insignificant p-value, but some of them, e.g.
#  2nd and 3rd division (Middle Atlantic and East North Central) are significant, implying there are differences
#  among different places, thus we keep this factor for now. Consider the operational set of predictors.
#  NOCC representing number of businesses in the building has an insignificant p-value, remove this.
glmod4 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                   OPEN24 + WKHRS + OWNOCC + OWNOPR + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod4)
#  Eliminate the factor OWNOCC. The insignificant p-value shows that there is no difference whether the building
#  is occupied by the owner or leasing tenant.
glmod5 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                   OPEN24 + WKHRS + OWNOPR + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod5)
#  Eliminate OWNOPE representing if the building is owner operating.
glmod6 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL + SOTHERM + SOHT2 + SOWATR + 
                   OPEN24 + WKHRS + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod6)
#  PBA indicates principle building activity. From the summary we can tell the TEUI is heavily dependent on the
#  activities in the building, which makes sense as different activities require different amount of energy consumption.
#  So the energy usage intensity would be different.
#  Most of the predictors from solar set give insignificant p-value, implying that the use of solar energy may not
#  necessarily reduce the consumption of energy in other forms. We can try to eliminate predictors from this set
#  except SOUSED, which represents whether the building is using any kinds of solar energy, by their insignificance
#  level to examine if there are any other potential issues hiding the significance of some predictors.
glmod7 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL + SOTHERM + SOWATR + 
                   OPEN24 + WKHRS + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod7)

glmod8 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL + SOTHERM  + 
                   OPEN24 + WKHRS + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod8)

glmod9 <- svyglm(TEUI ~ DAYLTP + SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL  + 
                   OPEN24 + WKHRS + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod9)
#  Remove DAYLTP at last.
glmod10 <- svyglm(TEUI ~ SQFT + CENDIV + PUBCLIM + 
                   SOUSED + SOPANEL  + 
                   OPEN24 + WKHRS + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod10)
#  Remove PUBCLIM.
glmod0 <- svyglm(TEUI ~ SQFT + CENDIV + 
                    SOUSED + SOPANEL  + 
                    OPEN24 + WKHRS + PBA + NWKER, 
                  design = samp_design, family = gaussian)
summary(glmod0)
#  This is going to be our final model. Next, carry out some diagnoses.



## Diagnoses

#  First, check the goodness-of-fit of the final model.
nullmod <- svyglm(TEUI ~ 1, design = samp_design)
summary(nullmod)
rsqed <- 1 - 26796636 / 52154886
print(c("R-squared" = rsqed))
#  The R-squared is 0.486, which indicates that nearly half of the variability observed is explained by this regression
#  model, which is enough to be a good model for data from a complex survey.
#  Then check the assumptions of the model.

#  Check for constant variance.
plot(glmod0$fitted.values, glmod0$residuals, 
     xlab = "Fitted", ylab = "Residuals")
#  The majority of observation errors have constant variance, some of the points show heteroscedasticity. However, in 
#  general I would say the error term has constant variance, following a linear trend.

#  Check for error normality.
qqnorm(glmod0$residuals, ylab = "Residuals")
qqline(glmod0$residuals)
#  Seems there is a heavy-tail problem. Bootstrapping could be used to generate robust estimates of standard errors 
#  and confidence intervals. We could do this if we need to do any inference. Since here we just need to interpret
#  the model in order to draw a conclusion, this process can be saved.

#  Check for outliers.
#  Use Bonferroni Correction to test if there are any outliers in the observations, and locate these points by 
#  checking the Cook's distance.
ti <- rstudent(glmod0)
which(ti == max(abs(ti)))
2 * (1 - pt(max(abs(ti)), df = 6436 - 35))
#  The p-value is 0, which means there definitely are outliers.
cook <- cooks.distance(glmod0)
par(mfrow = c(1, 1))
halfnorm(cook, nlab = 3, ylab = "Cook's Distance")
#  remove the 2434th, 5019th and 192nd observations and fit the model again.
cbecs_full <- cbecs_full %>%
  filter(cook < cook[192])
samp_design <- svrepdesign(weights = samp_wts[-c(2434, 5019, 192)], repweights = rep_wts[-c(2434, 5019, 192), ], 
                           type = "JK2", mse = TRUE, data = cbecs_full)

glmod0 <- svyglm(TEUI ~ SQFT + CENDIV + 
                   SOUSED + SOPANEL  + 
                   OPEN24 + WKHRS + PBA + NWKER, 
                 design = samp_design, family = gaussian)
summary(glmod0)



## Conclusion

#  We have done the investigation of how the use of solar energy reduce the energy consumption of other sources of 
#  a commercial building with some other controlled variables. In a simple linear regression model, there are no
#  large differences of total energy usage intensity between the buildings which use or do not use solar energy.
#  However, when the complex survey design is considered and with other controlled variables joining in, the ordinary
#  least squares regression model gave a very significant p-value for those who use and do not use solar energy(SOUSED),
#  implying significant difference between these two groups. The second group of SOUSED represents those who do not use
#  solar energy, and the positive estimate indicates this level would raise the the TEUI. Hence we can conclude that the
#  usage of solar energy can help a commercial building to reduce the usage of energy of other forms. As this conclusion 
#  is reflected by TEUI, it does not mean that a building would consume less energy comparing with other buildings if
#  it is using solar energy. The point is the change in consumption of other energies before and after the use of 
#  solar energy. Interestingly, those buildings who use solar panels for solar thermal or electricity would actually 
#  result in an increase in TEUI. There is one possibility: those buildings need solar panels for thermal energy or
#  electricity require enormous energy amount, which is costly, so they choose to use solar panels to save the energy
#  costs, but actually their TEUI is high.

#  There are some other predictors influence TEUI significantly. One more worker in the building would result an increase
#  of TEUI by 3.491 * 10^-2 and one more hour people work in the building per week would result in an increase of 
#  TEUI by 0.591. TEUI is also heavily influenced by the principle activities in the building. We can see that PBA6,
#  PBA15 and PBA16 (representing Food sales, Food service and Impatient health care) have extremely significant p-values
#  and large estimates need to be added on the intercept. This indicates that these three activities require more
#  energy consumption than any other activities (more than 10 units of TEUI). This might because of refrigerating 
#  is needed for the storage of food and medicines. 



















