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
