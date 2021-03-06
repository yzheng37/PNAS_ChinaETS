# PNAS_ChinaETS

This repository contains the Stata code used in the study: Cui J, Wang C, Zhang J, Zheng Y "The effectiveness of China's regional carbon market pilots in reducing firm emissions". Proceedings of the National Academy of Sciences (2021). The code show the process of obtaining empirical results in the main text (contained in the folder MainText) and supplementary information appendix (contained in the folder SI).

This repository contains the following code scripts:

1. MainText(Mah).do -- the code that obtain the results in the main text, and summary statistics and balancing test in the supplementary information appendix.
2. DynamicEffect.do -- the code that plot the figures of dynamic effects on total emissions and emission intensity.
3. Robustness(AltMeas&Confound).do -- the code that run the robustness checks on alternative emission measurements and confounding factors.
4. Robustness(AltMatchNum).do -- the code that run the robustness checks on alternative matching numbers.
5. Robustness(AltCovSet).do -- the code that run the robustness checks on alternative sets of covariates in the matching procedure.
6. Robustness(PSM).do -- the code that employ propensity score matching (PSM) as an alternative method.
7. Robustness(IPTW).do -- the code that employ inverse probability of treatment weighting (IPTW) as an alternative method.
8. Robustness(CEM).do --  the code that employ coarsened exact matching (CEM) as an alternative method.
9. Robustness(AltRateBase).do -- the code that run the robustness checks on alternative classifications for rate-based allowance allocation.
10. Robustness(HeteroSector).do -- the code that examine the heterogeneous effects by different sectors.
11. Robustness(HeteroPolicy).do -- the code that examine the policy heterogeneous effects (based upon carbon market performance) by diffferent alowance allocation methods.

Key variables:

* regu -- the binary indicator that denotes whether a firm is regulated by China's ETS pilots.
* post_annc -- the binary indicator that denotes the announcement periods of China's ETS pilots.
* post_implt -- the binary indicator that denotes the trading periods of China's ETS pilots.
* region -- the binary indicator that denotes whether a firm is located in an ETS pilot region.
* price -- carbon price of ETS pilots where ETS firms are located in (zero for non-ETS firms).
* turnover -- turnover rate of ETS pilots where ETS firms are located in (zero for non-ETS firms).
* RateBaseGroup -- the binary indicator that denotes if a firm is under the rate-based allocation system (and its matched control firm).
* Emiss -- firm-level emissions calculated based on the CNTSD, measured in metric tons of carbon dioxide.
* output -- firm-level output values from the CNTSD (10k yuan).
* CE -- firm-level total energy consupmtion from the CNTSD, measured in metric tons of standard coal equivalent.
* EmissOutput -- firm-level emission intensity (emission/output) calculated based on the CNTSD.
* CEOutput -- firm-level energy intensity (energy consumption/output) calculated based on the CNTSD.
* EmissCE -- firm-level carbon emissions per unit of energy consumption.
* GasCERatio -- firm-level the ratio of natural gas to total energy.
* employ -- firm-level number of employees from the CNTSD.
* capital -- firm-level amount of capital from the CNTSD (10k yuan).
* wage -- firm-level total amount of wage from the CNTSD (10k yuan).
* invest -- firm-level total amount of investment from the CNTSD (10k yuan).
* valueadd -- firm-level value added from the CNTSD (10k yuan).
* export -- firm-level export value from the CNTSD (10k yuan).
* TFP-OP -- firm-level total factor productivity measured by the approach in Olley and Pakes (1996).
* TFP-LP -- firm-level total factor productivity measured by the approach in Levinsohn and Petrin (2003).
