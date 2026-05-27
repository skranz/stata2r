# stata2r

R package that can translate a subset of Stata commands used for data preparation to R. This will not be usable to convert complete Stata analyses to R (use AI for full translation of some Stata code you have). It only translates selected data preparation steps.

It will be used as part of the repbox / metareg pipeline that shall facilitate systematic methodological meta studies based on actual reproduction package. 

A pilot example for such a meta study is my paper:

[From Replications to Revelations: Heteroskedasticity-Robust Inference](https://arxiv.org/abs/2411.14763)

But eveything is work in progress.

# Architectural Scope: What `stata2r` Does (and Doesn't) Do

### `stata2r` is strictly a Data Manipulation Translator. 

- It translates commands that structurally or mathematically alter the data set (e.g., `generate`, `replace`, `collapse`, `merge`, `keep`, `drop`, `xi`, `egen`).

### Generat no `e()` or `r()` values

- It does not translate commands for side effects beyond data manipulation, i.e. to generate `e()` or `r()` variables.

**How it is handled instead:** `metaregBase` natively executes the Stata script and writes any needed `e()` or `r()` values directly to disk (e.g., as `.dta` or `.txt` files). `repboxDRF` injects R code to load these exact values into `stata2r_env` immediately before they are needed. `stata2r` simply maps the expression `e(sample)` to `stata2r_env$e_sample`, blindly trusting that the environment has been correctly populated by the orchestrator.


**Why:** If the pipeline initiates a reproduction from a cached intermediate data file (Safe Caching), prior regression commands are skipped. If `stata2r` relies on tracking its own running state, it will crash when asked to recall an `e(sample)` from a skipped command.

## NO Regression Translation

- `stata2r` should **never** attempt to simulate Stata's `regress` or similar command.

- This will be done elsewhere in the repbox pipeline

- Results like `e(sample)` from a regression are pre-populated by repboxDRF.

- The `predict` command, which modifies the data set but relies on a previous regression will not be translated by `stata2r`.
