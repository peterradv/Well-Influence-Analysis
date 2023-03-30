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

[CE_comp.pdf](https://github.com/peterradv/Well-Influence-Analysis/files/11110622/CE_comp.pdf)

## Well Influence Analysis

WIA aims to adapt influence statistics techniques in the context of well redundancy analysis to provide a much more computationally efficient, albeit more approximate, alternative approach to leave-one-out cross validation. After model fitting, an influence statistic is calculated for each observation in the groundwater monitoring data set. The influence statistic values are then grouped by which well their corresponding observation originates from. The wells are then ranked by numerically ordering their median influence statistic values. Hence, **a well's overall influence on the regression model is determined by the average influence of its observations.**

## Simulation study

The simulation study was designed to empirically demonstrate that WIA is a reasonable estimator of well influence when compared to the cross validation based approach. 

## Shiny web application 

Shiny app to run simulations with selected model parameters: https://peterradv.shinyapps.io/well-influence-analysis/


