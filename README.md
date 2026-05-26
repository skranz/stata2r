# stata2r

R package that can translate a subset of Stata commands used for data preparation to R. This will not be usable to convert complete Stata analyses to R (use AI for full translation of some Stata code you have). It only translates selected data preparation steps.

It will be used as part of the repbox / metareg pipeline that shall facilitate systematic methodological meta studies based on actual reproduction package. 

A pilot example for such a meta study is my paper:

[From Replications to Revelations: Heteroskedasticity-Robust Inference](https://arxiv.org/abs/2411.14763)

But eveything is work in progress.

I try to write this package to a super large extend with AI. The idea is to have test Stata code (see e.g. `aicoder_work/tests`) run it, try to translate with the software and check if generated data set in R is same as in Stata. If not tell the AI to correct the package. 

So basically the [Ralph Wiggum Loop](https://ghuntley.com/loop/) is:


1. Run the tests
2. If some tests fail correct the code and go back to 1.
3. Make new tests and go back to 1

I am still using a traditional coding style of amateur coders from the 2024/25s, i.e. I am still in the loop and do copy & paste with an AI Chatbot, initiate the tests myself, etc.

Letting a fully automatic agent do the work for some hours would be the modern thing to do and probably work fine. But then setting up the secure agent environment also takes time...


Here is a draft for the `stata2r` `README.md` that clearly defines its boundaries and explains *why* the architecture is designed this way. This should serve as a strong guardrail against future regressions.

***

## ⚠️ Architectural Scope: What `stata2r` Does (and Doesn't) Do

**`stata2r` is strictly a Data Manipulation Translator.** 

It translates commands that structurally or mathematically alter the data frame (e.g., `generate`, `replace`, `collapse`, `merge`, `keep`, `drop`, `xi`, `egen`). 

To maintain "Safe Caching" capabilities in the broader `repbox` / `metaregBase` pipeline, **`stata2r` explicitly does NOT act as a statistical engine or a state tracker.** 

If you are developing or refactoring `stata2r`, you must adhere to the following architectural boundaries:

### 1. NO Statistical Simulation
`stata2r` should **never** attempt to simulate Stata's `regress`, `summarize`, `tabulate`, or other analytical commands via R equivalents (like `lm()` or `mean()`) for the purpose of mimicking Stata's internal state.
* **Why:** Replicating exact Stata sample masks (`e(sample)`) or summary statistics (`r(max)`) in R is fragile due to minor numerical differences and complex Stata-specific omission rules.
* **How it is handled instead:** Estimation and summary commands in `stata2r` are evaluated as **no-ops** (unless they generate variables, like `xi:` prefixes or `tabulate, gen()`).

### 2. NO `e()` or `r()` State Tracking
`stata2r` should **never** attempt to generate, store, or track Stata `e()` (estimation) or `r()` (return) macros internally. 
* **Why:** If the pipeline initiates a reproduction from a cached intermediate data file (Safe Caching), prior regression commands are skipped. If `stata2r` relies on tracking its own running state, it will crash when asked to recall an `e(sample)` from a skipped command.
* **How it is handled instead:** `metaregBase` natively executes the Stata script and writes any needed `e()` or `r()` values directly to disk (e.g., as `.dta` or `.txt` files). `repboxDRF` injects R code to load these exact values into `stata2r_env` immediately before they are needed. `stata2r` simply maps the expression `e(sample)` to `stata2r_env$e_sample`, blindly trusting that the environment has been correctly populated by the orchestrator.

### 3. NO Backward-Scanning Dependency Checks
`stata2r` translates commands line-by-line in isolation. It should **never** scan backward through the code history to figure out which command generated a specific `r(mean)` or to determine if a downstream line requires an `xi` dummy.
* **Why:** Dependency graph resolution is the explicit responsibility of `repboxDRF` (`drf_deps.R`). 
* **How it is handled instead:** `repboxDRF` analyzes the Stata script, maps all macro dependencies and required side-effects to their source `runid`, and passes flags (like `need_xi`) down to `stata2r`. `stata2r` only looks at the current command and the flags provided to it.

**Summary for AI / Future Developers:** If you find yourself writing code in `stata2r` to run an `lm()` model, calculate a `mean()`, or look up previous commands to resolve an `r()` value—**STOP**. You are breaking the pipeline architecture. That logic belongs in `repboxDRF` or the Stata side of `metaregBase`.
