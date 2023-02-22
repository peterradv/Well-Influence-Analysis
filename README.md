# Well Influence Analysis

### This simulation study is intended as a proof of concept for well influence analysis (WIA), a computationally efficient method based on influence diagnostics for ranking the wells of groundwater monitoring networks by importance.

WIA is used to order the list of monitoring wells in the 'well redundancy analysis' feature of GWSDAT (https://github.com/WayneGitShell/GWSDAT).

## The problem

Collecting and analysing samples from groundwater monitoring wells is costly, time-consuming and incurs health and safety risks. Therefore, minimising fieldwork whilst maximising data value is crucial for developing a sustainable and efficient monitoring strategy. Fewer wells could be sufficient for supporting robust statistical models, provided they appropriately capture the spatiotemporal heterogeneity in contaminant concentrations. A possible approach for optimising monitoring networks is omitting wells whose observations do not substantially alter the model fit when deleted, i.e. they are not influential. 

The influences of wells can be quantified using cross-validation, where each well (meaning its observations) is deleted sequentially and used as the test set for a model trained on the remainder of the data. The wells can then be ranked by their corresponding prediction errors. However, the computation time for this approach, depending on the number of wells, observations and model complexity, can be very long due to the number of times the model has to be re-fitted.

WIA is an approximate alternative approach, which takes adventage of influence diagnostics commonly used in regression analysis. It aims to provide a guideline for omitting wells, by ranking them based on the average influence metric values of their observations.

Shiny app to run simulations with selected model parameters: https://peterradv.shinyapps.io/well-influence-analysis/


