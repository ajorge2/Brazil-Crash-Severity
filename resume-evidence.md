# Brazil crash-severity resume evidence

- Source rows: 463,152
- Modeling rows: 404,508
- Excluded rows: 58,644 with road delineation recorded as `Not Reported`
- Fatal crashes in modeling sample: 26,648
- Selected specification: `model_5` (minimum AIC 177765.3)
- Controls: cause of accident, time of day, road delineation, and weather condition
- Reference categories: alcohol consumption, night, curve, and rainy weather
- Lead association: pedestrian-walking-in-road crashes had 11.52x the adjusted odds of fatality versus alcohol-related crashes (95% Wald CI 10.23-12.98; p < 2e-16)
- Sensitivity to adjustment: the odds ratio shrank from 12.12 unadjusted (95% CI 10.78-13.64) to 11.52 adjusted, a 4.9% change

## Resume-ready claim

Analyzed 463,152 Brazilian crash records with multivariable logistic regression; found crashes attributed to pedestrians walking in the road had 11.52x the adjusted odds of fatality versus alcohol-related crashes (95% CI 10.23-12.98). The estimate shrank 4.9% from its unadjusted value after controlling for time of day, road delineation, and weather.

## Interpretation boundary

This is an adjusted association in observational crash records, not a causal effect. The interval is model-based and assumes independent records and a correctly specified logistic model.

## Reproduction

Input file: local `accidents.csv` (not redistributed)
Input MD5: `8b2672d2451e806cd7a71231ecad4cd7`

```sh
Rscript run_analysis.R /absolute/path/to/accidents.csv .
```
