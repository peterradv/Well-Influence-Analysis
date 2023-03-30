# Well Influence Analysis

### This simulation study is intended as a proof of concept for well influence analysis (WIA), a computationally efficient method for ranking the wells of groundwater monitoring networks by their influence on the fitted contaminant concentration surface of regression-based statistical models.

WIA is used to order the list of monitoring wells in the 'well redundancy analysis' feature of the open-source software Groundwater Spatiotemporal Data Analysis Tool (GWSDAT; https://github.com/WayneGitShell/GWSDAT).

## GWSDAT spatiotemporal smoother

Statistical modelling is commonly used on groundwater quality monitoring data to analyse the behaviour of contaminants in space and time. The conventional approach to groundwater contamination data is to use spatial statistical models for individual sampling events. However, McLean et al. (2019; https://doi.org/10.1016/j.scitotenv.2018.10.231) demonstrated that a spatiotemporal model, which borrows strength across time, is more efficient and can achieve the same performance with much smaller sample sizes. 

The spatiotemporal solute concentration smoother in GWSDAT uses a non-parametric regression technique known as Penalised Splines (P-splines), developed by Eilers & Marx (1992; https://doi.org/10.1214/ss/1038425655). For a detailed description of the GWSDAt spatiotemporal solute concentration smoother see https://github.com/WayneGitShell/GWSDAT_User_Manual and https://doi.org/10.1002/env.2347. A common feature of regression models is the use of the projection or hat matrix, which maps the vector of response values to the vector of fitted values. The diagonal elements of the hat matrix describe the influence of each response on the corresponding fitted value.

## Well redundancy analysis

Collecting and analysing samples from groundwater monitoring wells is costly, time-intensive and incures health and safety risks. Therefore, minimising the number of sampling locations whilst maintaining adequate spatial monitoring coverage is of great importance. Fewer wells are often sufficient for supporting robust models. A natural approach to reducing the number of monitoring wells in a network is comparing a model fitted to a complete data set to a model fitted to a reduced data set from which all observations of a single well have been removed. If the ommission of the well does not cause significant changes to the solute concentration estimates, the well can be ignored, since it provides redundant data. The process can be repeated for all wells and their influence on solute concentrations can be quantified and recorded. This approach is a form of leave-one-out cross validation, and it allows for a ranking of the wells based on their influence. However, it also requires fitting a large number of models (equal to the number of wells in the network). Moreover, only one well can be dropped at a time based on the ranking. For each subsequent well removal the whole process has to be repeated, since the influences of wells are expected to change. This means that the computational cost increases linearly with the number of monitoring wells in the network and with the number of dropped wells. This makes this approach computationally infeasible.

## Statistical influence analysis

Influence analysis refers to a collection of techniques in statistics, originating in regression analysis, that aim to measure the individual influences of observations on the outcome of a statistical procedure. Influential observations can often be outliers with respect to other response and/or explanatory variables, thus identifying them can help create more representative statistical models. Some well known influence metrics are Cook's distance (https://doi.org/10.2307/1268249), DFBETAS and DFFITS (https://doi.org/10.1002/0471725153). Influence metrics can be calculated using the observations' leverages and residuals. The leverages are the diagonal elements of the hat matrix. As opposed to cross validation, these techniqes estimate influence using information from a single model fit. 

## Well Influence Analysis

WIA aims to adapt influence statistics techniques in the context of well redundancy analysis to provide a much more computationally efficient, albeit more approximate, alternative approach to leave-one-out cross validation. After model fitting, an influence statistic is calculated for each observation in the groundwater monitoring data set. The influence statistic values are then grouped by which well their corresponding observation originates from. The wells are then ranked by numerically ordering their median influence statistic values. Hence, **a well's overall influence on the regression model is determined by the average influence of its observations.**

## Simulation study

The simulation study was designed to empirically demonstrate that WIA is a reasonable estimator of well influence when compared to the cross validation based approach. To identify the best estimator, six different influence metrics were tested for 100 realizations each in different scenarios. The scenarios were designed with three hypothetical groundwater contamination plumes of increasing geometric complexity (see image) and three well placement strategies (random, grid and expert) for 6, 12 and 24 wells on a domain with area 35 x 100 units (see image). The effect of adding additive or multiplicative measurement noise to the raw data was also investigated on well influence estimates, although groundwater monitoring data is commonly assumed to have multiplicative noise. Multiplicative noise means that the measurement error scales with the magnitude of the solute concentration i.e. low concentration measurements will have smaller errors than high ones. On the other hand, additive noise means that regardless of the solute concentration, the measurement errors will be of similar magnitude. 

three hypothetical contaminant plumes:

![plumes](https://user-images.githubusercontent.com/85235934/228821955-ca0e73ea-7904-42e6-8850-0ffc01b94650.png)

well placement scenarios:

![wells](https://user-images.githubusercontent.com/85235934/228855062-5cc937ca-951d-4f34-a39b-e10f58e27e4b.png)

The tested influence metrics were **leverages, studentized residuals, Cook's distance, DFFITS, COVRATIO and Hadi's influence measure**. The accuracy of each metric in estimating the baseline well influence ranking (computed by the cross validation based approach) was evaluated by calculating a difference score, which quantified placement differences in the well rankings. The difference score falls between 0 and 1 with 0 meaning the ranking is identical to the baseline and 1 meaning it is vastly different. The study was conducted using two different sets of model settings. The first of which is the default setting for the spatiotemporal solute concentration smoother in GWSDAT, and the second is a setting with an increased number of cubic P-spline basis functions, resulting in a less smooth model.

## Results

WIA was a good estimator of well influence if the groundwater contamination data had multiplicative measurement noise, which is commonly assumed to be the case. The results showed that in most scenarios, Cook's distance was the most reliable estimator with a mean standardised difference score of 0.23, which corresponds to 77% accuracy, and a standard deviation in the mean difference scores of 0.098 among the different scenarios (see image).

![results](https://user-images.githubusercontent.com/85235934/228843841-c816e1f7-b5cf-4f67-8255-a53303660bc1.png)

The breakdown of the results for Cook's distance by the design features of the simulation study can be seen on the figure below.

![resultsv2](https://user-images.githubusercontent.com/85235934/228842456-1b7dbfcf-8365-4ba0-983a-a912862c865b.png)

## Shiny web application 

A shiny webb application was also developed to allow for running a single iteration of the simulation study on selected design features. The model parameters nseg 1-3 and bdeg refer to the spatiotemporal solute concentration smoother from GWSDAT. They control the degree of smoothness in the 3 dimensions and the power of the splines respectively. The default settings are nseg(6,6,6) and bdeg(2) corresponding to quadratic splines. Increasing these parameters decreases the smoothness of the model but comes with the burden of increasing computational cost. The output shows the normalised difference scores of the six influence metrics. A lower score means closer well influence estimates to well-based cross validation. The output also shows the rankings of the wells by cross validation and the influence metrics. This allows for direct comparison of the orders.

The shiny app can be found at: https://peterradv.shinyapps.io/well-influence-analysis/


