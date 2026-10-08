# Brazil Crash Severity Analysis

**[Evidence overview](https://ajorge2.github.io/Brazil-Crash-Severity/)** · **[Original analysis](paper.pdf)**

This repository exposes the model-selection table, exact coefficient output, analysis script, input checksum, and R session information behind the resume claim. The source dataset is not redistributed; its MD5 digest is retained so a local copy can be verified before reproduction.

## Reproduce

```sh
Rscript run_analysis.R /absolute/path/to/accidents.csv .
```

The analysis selects the candidate logistic-regression specification with minimum AIC and reports both unadjusted and adjusted odds ratios. The result is an observational association, not a causal effect.
