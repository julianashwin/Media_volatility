"""
Implementation of BTR with Gibbs sampling for FT dataset
"""

cd("/Users/julianashwin/Documents/GitHub/Media_volatility/")

"""
To install the package is run, enter pkg mode by running "]" then run
pkg> dev path_to_folder/BTR.jl
"""

"""
Load data and necessary packages
"""
## Packages
using BTR
using TextAnalysis, DataFrames, CSV, Random, GLM, Distributions
using Plots, StatsPlots, StatsBase, Plots.PlotMeasures, TableView


## Load data
df = CSV.read("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/BTR_FT_data.csv",
    DataFrame, threaded = false)

## Keep those where all values of highlow are available
numeric_cols = [:highlow, :highlow_1lag, :highlow_2lag, :highlow_3lag,
    :highlow_4lag, :highlow_5lag, :abs_intra_day]
df = df[completecases(df[:,numeric_cols]),:]
## Clean up and compute loughran sentiment
df.text_clean[ismissing.(df.text_clean)] .= ""
df.text[ismissing.(df.text)] .= ""
df.text = join.(split.(lowercase.(string.(df.text))), " ")
df.sentiment = sentimentscore(df.text, LM_dicts)
df.sentiment[isnan.(df.sentiment)] .= 0.

## Add some id columns
df.Code_id = convert_to_ids(string.(df.Code))
df.Date_id = convert_to_ids(string.(df.Date))

for col in numeric_cols
    ex1 = Meta.parse("df."*string(col)*" =  Array{Float64,1}(df."*string(col)*")")
    eval(ex1)
end

df.highlow_firmav = group_mean(df.highlow, df.Code_id, same_length = true)
df.highlow_dateav = group_mean(df.highlow, df.Date_id, same_length = true)


showtable(df)


# Check that the variables look sensible
plot(df.highlow[1:200], label = ["highlow volatility"], legend = :bottomleft,xguidefontsize=8)
plot!(df.abs_intra_day[1:200], label = ["abs return"])

fm = @formula(highlow ~ mention + abs_intra_day + highlow_1lag +
    highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag +
    highlow_dateav + highlow_firmav + sentiment)
display(lm(fm, df))

fm = @formula(highlow ~ mention + abs_intra_day + highlow_1lag +
    highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag +
    highlow_dateav + highlow_firmav + sentiment + VI_put + VI_call)
display(lm(fm, df))

df_VIadj = deepcopy(df)
df_VIadj.VI_put[ismissing.(df_VIadj.VI_put)] .= 0.
df_VIadj.VI_call[ismissing.(df_VIadj.VI_call)] .= 0.

df_VIadj.VI_put = Float64.(df_VIadj.VI_put)
df_VIadj.VI_call = Float64.(df_VIadj.VI_call)
fm = @formula(highlow ~ mention + abs_intra_day + highlow_1lag +
    highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag +
    highlow_dateav + highlow_firmav + sentiment + VI_put + VI_call)
display(lm(fm, df_VIadj))




## Toggle whether to save the various figures output throughout
save_files = false


"""
Create document ids from either review id or business id
"""
## The ids should be in Int64 format
df.doc_idx = 1:nrow(df)
#df[!,:doc_idx] = convert_to_ids(df.review_id)
#sort!(df, [:doc_idx])
#showtable(df)


"""
Prepare data for estimation
"""
## Create labels and covariates
x = hcat(df_VIadj.mention, df_VIadj.sentiment, df_VIadj.abs_intra_day,
    df_VIadj.highlow_dateav, df_VIadj.highlow_firmav, df_VIadj.highlow_1lag,
    df_VIadj.highlow_2lag, df_VIadj.highlow_3lag, df_VIadj.highlow_4lag,
    df_VIadj.highlow_5lag, df_VIadj.VI_put, df_VIadj.VI_call)
y = df_VIadj.highlow
docidx_vars = df.doc_idx
docidx_dtm = df.doc_idx
D = length(unique(docidx_dtm))

## Use TextAnalysis package to create a Corpus
docs = StringDocument.(df.text_clean)
crps = Corpus(docs)
update_lexicon!(crps)

## Remove very rare terms
lex_df = DataFrame(term = String.(keys(lexicon(crps))),
    count = Int.(values(lexicon(crps))))
showtable(sort(lex_df, :count, rev = true))
vocab = lex_df.term[(lex_df.count .> 10)]
dtm_sparse = DocumentTermMatrix(crps, vocab)
@assert dtm_sparse.terms == vocab "Vocab and DTM do not match"
dtm_in = dtm_sparse.dtm


"""
Split into training and test sets (by doc_idx) and convert to BTRRawData structure
"""
## Extract sparse matrix and create BTRRawData structure(s)

train_data, test_data = btr_traintestsplit(dtm_in, docidx_dtm, docidx_vars, y, vocab, x = x,
    train_split = 0.75, shuffle_obs = true)
# Alternatively, can convert the entier set to BTRRawData with
all_data = DocStructs.BTRRawData(dtm_in, docidx_dtm, docidx_vars, y, x, vocab)
## Visualise the training-test split
histogram(train_data.docidx_dtm, bins = 1:D, label = "training set",
    xlab = "Observation", ylab= "Paragraphs", c=1, lc=nothing)
histogram!(test_data.docidx_dtm, bins = 1:D, label = "test set", c=2, lc=nothing)
if save_files; savefig("figures/FT_BTR/FT_trainsplit.pdf"); end;


## OLS regression
regressors = hcat(ones(size(all_data.x,1)),all_data.x)
ols_coeffs = inv(transpose(regressors)*regressors)*(transpose(regressors)*all_data.y)
mse_ols = mean((all_data.y .- regressors*ols_coeffs).^2)



"""
Standardise using only the training data
"""
## Use mean and std from training data to normalise both sets
#y_mean_tr = mean(train_data.y)
#y_std_tr = std(train_data.y)
#x_mean_tr = mean(train_data.x,dims=1)
#x_std_tr = std(train_data.x,dims=1)
#train_data.y = (train_data.y .- y_mean_tr)#./y_std_tr
#train_data.x = (train_data.x .- x_mean_tr)./x_std_tr
#test_data.y = (test_data.y .- y_mean_tr)#./y_std_tr
#test_data.x = (test_data.x .- x_mean_tr)./x_std_tr



"""
Set priors and estimation optioncs here to be consistent across models
"""
## Initialiase estimation options
btropts = BTROptions()
## Number of topics
btropts.ntopics = 10
## LDA priors
btropts.α=0.5
btropts.η=0.1
## BLR priors
btropts.μ_ω = 0. # coefficient mean
btropts.σ_ω = 2. # coefficient variance
btropts.a_0 = 2. # residual shape: higher moves mean closer to zero
btropts.b_0 = 2. # residual scale: higher is more spread out
# Plot the prior distribution for residual variance (in case unfamiliar with InverseGamma distributions)
# mean will be b_0/(a_0 - 1)
plot(InverseGamma(btropts.a_0, btropts.b_0), xlim = (0,3.), title = "Residual variance prior",
    label = "Prior on residual variance")
scatter!([mse_ols],[0.],label = "OLS residual variance")
if save_files; savefig("figures/FT_BTR/FT_IGprior.pdf"); end;

## Number of iterations and cross validation
btropts.E_iters = 100 # E-step iterations (sampling topic assignments, z)
btropts.M_iters = 2500 # M-step iterations (sampling regression coefficients residual variance)
btropts.EM_iters = 25 # Maximum possible EM iterations (will stop here if no convergence)
btropts.burnin = 10 # Burnin for Gibbs samplers
btropts.CVEM = :obs # Split for separate E and M step batches (if batch = true)
btropts.CVEM_split = 0.5 # Split for separate E and M step batches (if batch = true)

## Comvergence options
btropts.mse_conv = 2
btropts.ω_tol = 0.015 # Convergence tolerance for regression coefficients ω
btropts.rel_tol = true # Whether to use a relative convergence criteria rather than just absolute


"""
Run some text-free regressions for benchmarking
"""
## Define regressors
regressors = hcat(ones(size(all_data.x,1)),all_data.x)
## Bayesian linear regression
blr_coeffs, blr_σ2, blr_coeffs_post, σ2_post = BLR_Gibbs(all_data.y, regressors, iteration = btropts.M_iters,
    m_0 = btropts.μ_ω, σ_ω = btropts.σ_ω, a_0 = btropts.a_0, b_0 = btropts.b_0)
## Out-of-sample
predict_blr = regressors*blr_coeffs
mse_blr = mean((all_data.y .- predict_blr).^2)



"""
Estimate BTR
"""
## Include x regressors by changing the options
btropts.xregs = [1,2,3,4,5,6,7,8,9,10,11,12]
btropts.interactions = Array{Int64}([2])
## Initialise BTRModel object
btrcrps_all = create_btrcrps(all_data, btropts.ntopics)
btrmodel = BTRModel(crps = btrcrps_all, options = btropts)
## Estimate BTR with EM-Gibbs algorithm
btrmodel = BTRemGibbs(btrmodel)
## Plot results
BTR_plot(btrmodel.β, btrmodel.ω_post, btrmodel.crps.vocab,
    plt_title = "Yelp BTR", fontsize = 5, nwords = 10, title_size = 10,
    interactions = btropts.interactions)
if save_files; savefig("figures/Yelp_BTR/Yelp_BTR.pdf"); end;

## Estimate of treatment effect
sort(btrmodel.ω_post[(btropts.ntopics+length(btropts.interactions)*btropts.ntopics+1),:])
btrmodel.ω[(btropts.ntopics+length(btropts.interactions)*btropts.ntopics+1)]


## Out of sample prediction in test set
mse_btr = btrmodel.mse
pplxy_btr = btrmodel.pplxy




"""
Estimate BTR without CVEM
"""

TE_Krobustness_df = DataFrame(NoText_reg = sort(blr_coeffs_post[2,:]))
Ks = [2,5,10,15,20,25,30,40,50]
for K in Ks
    display("Estimating with "*string(K)*" topics")
    btropts_noCVEM = deepcopy(btropts)
    btropts_noCVEM.CVEM = :none
    btropts_noCVEM.ntopics = K
    btropts_noCVEM.mse_conv = 2

    btrcrps_tr = create_btrcrps(all_data, btropts_noCVEM.ntopics)
    btrmodel_noCVEM = BTRModel(crps = btrcrps_tr, options = btropts_noCVEM)
    ## Estimate BTR with EM-Gibbs algorithm
    btrmodel_noCVEM = BTRemGibbs(btrmodel_noCVEM)

    TE_Krobustness_df[:,"BTR_noCVEM_K"*string(K)] =
        sort(btrmodel_noCVEM.ω_post[(K+length(btropts_noCVEM.interactions)*K+1),:])
end





"""
Estimate 2 stage LDA then Bayesian Linear Regression (BLR)
    In the synthetic data this does about as well as BTR because the
    text is generated from an LDA model.
"""

Ks = [2,5,10,15,20,25,30,40,50]
for K in Ks
    display("Estimating with "*string(K)*" topics")

    ## Use the same options as the BTR, but might want to tweak the number of iterations as there's only one step
    ldaopts = deepcopy(btropts)
    ldaopts.fullGibbs_iters = 1000
    ldaopts.fullGibbs_thinning = 2
    ldaopts.burnin = 50
    ldaopts.ntopics = K
    ## Initialise model (re-initialise the corpora to start with randomised assignments)
    ldacrps_all = create_btrcrps(all_data, ldaopts.ntopics)
    ldamodel = BTRModel(crps = ldacrps_all, options = ldaopts)
    ## Estimate LDA model on full training set
    ldamodel  = LDAGibbs(ldamodel)

    ## Bayesian linear regression on training set
    blr_ω, blr_σ2, blr_ω_post, blr_σ2_post = BLR_Gibbs(ldamodel.y, ldamodel.regressors,
        m_0 = ldaopts.μ_ω, σ_ω = ldaopts.σ_ω, a_0 = ldaopts.a_0, b_0 = ldaopts.b_0,
        iteration = ldaopts.M_iters)
        ldamodel.ω = blr_ω
        ldamodel.ω_post = blr_ω_post

    TE_Krobustness_df[:,"LDA_K"*string(ldamodel.options.ntopics)] = sort(ldamodel.ω_post[
        (ldamodel.options.ntopics+length(ldamodel.options.interactions)*ldamodel.options.ntopics+1),:])
end


"""
Estimate 2 stage slDA then BLR on residuals
"""
## Repeat for many K
for K in Ks
    display("Estimating sLDA with "*string(K)*" topics")
    ## Set options sLDA on residuals
    slda2opts = deepcopy(btropts)
    slda2opts.CVEM = :none
    slda2opts.xregs = Array{Int64,1}([])
    slda2opts.interactions = Array{Int64,1}([])
    slda2opts.ntopics = K

    ## Initialise BTRModel object
    slda2crps_tr = create_btrcrps(all_data, slda2opts.ntopics)
    slda2model = BTRModel(crps = slda2crps_tr, options = slda2opts)

    ## Estimate sLDA on residuals
    slda2model = BTRemGibbs(slda2model)

    ## Identify residuals to train second stage regression
    residuals_slda = all_data.y .- slda2model.regressors*slda2model.ω

    ## Bayesian linear regression on training set
    regressors_slda = hcat(ones(size(all_data.x,1)),all_data.x)
    # No fixed effect, no batch
    blr_ω, blr_σ2, blr_ω_post, blr_σ2_post = BLR_Gibbs(residuals_slda, regressors_slda,
        m_0 = slda2opts.μ_ω, σ_ω = slda2opts.σ_ω, a_0 = slda2opts.a_0, b_0 = slda2opts.b_0,
        iteration = slda2opts.M_iters)

    slda2_TE = blr_ω[2]

    TE_Krobustness_df[:,"sLDA_K"*string(slda2opts.ntopics)] = sort(blr_ω_post[2,:])

end

## Set options sLDA on residuals
slda2opts = deepcopy(btropts)
slda2opts.xregs = []
slda2opts.interactions = []

## Initialise BTRModel object
slda2crps_tr = create_btrcrps(train_data, slda2opts.ntopics)
slda2crps_ts = create_btrcrps(test_data, slda2opts.ntopics)
slda2model = BTRModel(crps = slda2crps_tr, options = slda2opts)

## Estimate sLDA on residuals
slda2model = BTRemGibbs(slda2model)

## Plot results
BTR_plot(slda2model.β, slda2model.ω_post, slda2model.crps.vocab,
    plt_title = "Yelp sLDA + BLR", fontsize = 10, nwords = 10, title_size = 10)
if save_files; savefig("figures/Yelp_BTR/Yelp_sLDA_LR.pdf"); end;

## Identify residuals to train second stage regression
residuals_slda = train_data.y .- slda2model.regressors*slda2model.ω

## Bayesian linear regression on training set
# Create regressors
regressors_slda = hcat(ones(size(train_data.x,1)),train_data.x)
# No fixed effect, no batch
blr_ω, blr_σ2, blr_ω_post, blr_σ2_post = BLR_Gibbs(residuals_slda, regressors_slda,
    m_0 = slda2opts.μ_ω, σ_ω = slda2opts.σ_ω, a_0 = slda2opts.a_0, b_0 = slda2opts.b_0)

## Out of sample
slda2_predicts = BTRpredict(slda2crps_ts, slda2model)
# Second stage regressors
regressors_test = hcat(ones(size(test_data.x,1)),test_data.x)
# Add y prediction from slda to portion of residual explained by BLR
y_pred_slda_blr = slda2_predicts.y_pred + regressors_test*blr_ω
# Compute MSE
mse_slda_blr = mean((test_data.y .- y_pred_slda_blr).^2)



"""
Export/Import results
"""

if save_files
    #CSV.write("BTR_results/TE_Krobustness.csv",TE_Krobustness_df)
end



"""
Plot treatment effects
"""
## Identify columns for each model
NoText_cols = occursin.("NoText",names(TE_Krobustness_df))
BTR_cols = occursin.("BTR_noCVEM",names(TE_Krobustness_df))
BTR_CVEM_cols = occursin.("BTR_CVEM",names(TE_Krobustness_df))
LDA_cols = occursin.("LDA_",names(TE_Krobustness_df)) .& .!(occursin.("s",names(TE_Krobustness_df)))
sLDA_cols = occursin.("sLDA_",names(TE_Krobustness_df))

## Function to plot estimate with ccredible intervals
est_df = TE_Krobustness_df
cols = NoText_cols
label = "No Text LR"
est_color = :grey
function plot_estimates(est_df, Ks, label, cols, est_color)
    med_row = Int(nrow(est_df)/2)
    low_row = Int(round(nrow(est_df)*0.025,digits = 0))
    high_row = Int(round(nrow(est_df)*0.975, digits = 0))

    if sum(cols) == 1
        med_ests = repeat(Array(est_df[med_row, cols]), length(Ks))
        upper_ests = repeat(Array(est_df[high_row, cols]), length(Ks))
        lower_ests = repeat(Array(est_df[low_row, cols]), length(Ks))
    else
        med_ests = Array(est_df[med_row, cols])
        upper_ests = Array(est_df[high_row, cols])
        lower_ests = Array(est_df[low_row, cols])
    end

    plot!(Ks, med_ests, color = est_color, label = label)
    scatter!(Ks, med_ests, color = est_color, label = "")
    plot!(Ks, med_ests, ribbon=(upper_ests.- med_ests, med_ests.- lower_ests),
        color = est_color, label = "", fillalpha = 0.5)

end


model_names = ["BTR (no CVEM)","BTR (CVEM)", "LDA", "sLDA", "NoText BLR"]
nmodels = length(model_names)
plt1 = plot(legend = false, xlim = (0,maximum(Ks)+2), ylim = (-0.5, 0.5),
    xlabel = "Number of Topics", ylabel = "Estimate Treatment Effect",
    title = "FT media coverage effect")
plot!([0.,(Float64(maximum(Ks))+2.0)],[-0.,-0.], linestyle = :dash,color =:red,
    label = "Ground truth", legend = :topright)
# Add various model estimates
plot_estimates(TE_Krobustness_df, Ks, "No Text LR", NoText_cols, :grey)
plot_estimates(TE_Krobustness_df, Ks, "BTR (sentiment interaction)", BTR_cols, :blue)
plot_estimates(TE_Krobustness_df, Ks, "BTR", BTR_CVEM_cols, :lightblue)
plot_estimates(TE_Krobustness_df, Ks, "LDA", LDA_cols, :green)
plot_estimates(TE_Krobustness_df, Ks, "sLDA", sLDA_cols, :orange)
# Add scholar results
plot!([2,5,10,15,20,25,30,40,50], scholar_df.w1_median, color = :pink, label = "SCHOLAR")
scatter!([2,5,10,15,20,25,30,40,50], scholar_df.w1_median, color = :pink, label = "")
plot!([2,5,10,15,20,25,30,40,50], scholar_df.w1_median, ribbon=(scholar_df.w1_upper.-
    scholar_df.w1_median, scholar_df.w1_median.- scholar_df.w1_lower),
    color = :pink, label = "", fillalpha = 0.5)

plot!([2,5,10,15,20,25,30,40,50], scholar_CVEM_df.w1_median, color = :purple, label = "SCHOLAR (CV)")
scatter!([2,5,10,15,20,25,30,40,50], scholar_CVEM_df.w1_median, color = :purple, label = "")
plot!([2,5,10,15,20,25,30,40,50], scholar_CVEM_df.w1_median, ribbon=(scholar_CVEM_df.w1_upper.-
    scholar_CVEM_df.w1_median, scholar_CVEM_df.w1_median.- scholar_CVEM_df.w1_lower),
    color = :purple, label = "", fillalpha = 0.5)

plot!(size = (500,400))











"""
End of script
"""
