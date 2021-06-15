import os
# set directory to the clean by year folder
os.chdir('/Users/julianashwin/Documents/GitHub/Firm_level_news_analysis')


import matplotlib
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import topicmodels



source_location = "/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched"
import_filename = source_location + "/companyarticles_matched_residuals.csv"
ftdata = pd.read_csv(import_filename)

# ftheadlines = ftheadlines[ftheadlines.headline.map(lambda x: len(x) > 25)]
# ftheadlines = ftheadlines[1:5000] # for debugging

print "Text data imported."
print "Number of documents is %d" % (len(ftdata))

ftdata = ftdata.dropna(subset = ['main_text'])
ftdata = ftdata.reset_index()

print "Number of non-empty documents is %d" % (len(ftdata))

# Remove all non utf-8 characters
def decode_function(line):
    if all(ord(char) < 128 for char in line):
        line = line
    else:
        # print "%d is NOT fine, so doing the decoding thing " % i
        line=line.decode('utf-8','replace')
    return line


ftdata.main_text = ftdata.main_text.apply(decode_function)


print(ftdata.headline[8067])


# ftdata.dates = pd.to_datetime(ftdata.date_num)

#clean and stem data using topicmodels
doc = topicmodels.RawDocs(ftdata.main_text, "long")
doc.token_clean(1)
doc.stopword_remove("tokens")
print "Stemming words..."
doc.stem()
doc.stopword_remove("stems")
doc.term_rank("stems")

# removing words that only appear twice
doc.rank_remove("df", "stems",2)
doc.rank_remove("tfidf", "stems",5)

all_stems = [s for d in doc.stems for s in d]
print("number of unique stems = %d" % len(set(all_stems)))
print("number of total stems = %d" % len(all_stems))

# Remove words that only appear in 2 or fewer documents
#doc.rank_remove("df", "stems",doc.df_ranking[4][1])


#replace text data with relevant stems
ftdata['stemmed'] = [' '.join(s) for s in doc.stems]
# Drop any empty documents
ftdata['stemmed'].replace('', np.nan, inplace=True)

#ftdata = ftdata.dropna(subset = ['stemmed'])
#ftdata = ftdata.reset_index()

# Check that there are no empty documents
assert all(doc.stems)


# Pure LDA attempt
ldaobj = topicmodels.LDA.LDAGibbs(doc.stems, 30)

burnin = 500
thinning = 20
samples = 300
total = burnin + (thinning*samples)
print "Sample with a burn in of %d, thinning interval %d and %d samples." % (
burnin, thinning, samples)
print "Total number of iterations will be %d" % total

ldaobj.sample(burnin,thinning,samples)
lda_dt = ldaobj.dt_avg()
lda_tt = ldaobj.tt_avg()

source_location = "/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched"
export_filename = source_location + "/company_lda_30topic_description.csv"
ldaobj.topic_content(20, output_file = export_filename)

lda_aggdata = pd.DataFrame(lda_dt,index = ftdata.Date, columns=['T' + str(i) for i in xrange(ldaobj.K)])
lda_aggdata['article_id'] = np.asarray(ftdata.article_id)
lda_aggdata['Code'] = np.asarray(ftdata.Code)
lda_aggdata['Date'] = np.asarray(ftdata.Date)

# Export the topic proportions
source_location = "/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched"
export_filename = source_location + "/company_lda_30proportions.csv"
lda_aggdata.to_csv(export_filename, encoding = "utf-8")





#### Split the sample for the six different sorts of residual
ftdata_absboth = ftdata.dropna(subset = [''])
ftdata_abslags = ftdata.dropna(subset = ['main_text'])
ftdata_highlowboth = ftdata.dropna(subset = ['main_text'])
ftdata_highlowlags = ftdata.dropna(subset = ['highlow_lags_residual'])
ftdata_intraboth = ftdata.dropna(subset = ['main_text'])
ftdata_intralags = ftdata.dropna(subset = ['main_text'])



# Swap the temp doc here
tempdoc = topicmodels.RawDocs(ftdata_highlowlags.stemmed, "long")
# Estimate the topicmodel

topicnumber = 30
K = topicnumber
D = len(tempdoc.tokens)
y = np.asarray(ftdata_highlowlags.highlow_lags_residual)
#X = np.asarray(df.X).reshape((D,1))
X = np.zeros(D).reshape((D,1))
# X = np.random.normal(size=X_size*D).reshape((D, 2))
phi = np.random.randn(K)
gamma = np.zeros(X.shape[1])
sigma = 0.1

sldaobj = topicmodels.LDA.sLDAGibbs(tempdoc.tokens, K, y, X, phi, gamma, sigma)


# Initialise the phi and gamma coefficients using the topic seed
sldaobj.initialise_coeffs()


#### Run the EM algorithm ####

maxEMiter = 80
phi_estimates = np.zeros(maxEMiter*K).reshape(maxEMiter, K)

sldaobj.y = sldaobj.y - np.mean(sldaobj.y)

for i in range(45,50):
    print "EM iteration %d out of a maximum possible %d." % (i, maxEMiter)
    sldaobj.E_step(200,20,100)
    sldaobj.M_step()
    print(sldaobj.phi)
    print(sldaobj.sigma)
    # store phi estimates to examine convergence
    phi_estimates[i] = sldaobj.phi
    phi_estimatesdf = pd.DataFrame(phi_estimates)
    phi_filename = source_location + "/company_phi_temp.csv"
    phi_estimatesdf.to_csv(phi_filename)
    topics_filename = source_location + "/company_topic_description_temp.csv"
    sldaobj.topic_content(20, output_file=topics_filename)


# Now export the slda output so that it can be compared with the

slda_dt = sldaobj.dt_avg()
slda_tt = sldaobj.tt_avg()
content_filename = source_location + "/company_highlow_slda_30topic_description.csv"
sldaobj.topic_content(20, output_file=content_filename)


slda_aggdata = pd.DataFrame(slda_dt,index = ftdata_highlowlags.Date, columns=['T' + str(i) for i in xrange(sldaobj.K)])
slda_aggdata['article_id'] = np.asarray(ftdata_highlowlags.article_id)
slda_aggdata['Code'] = np.asarray(ftdata_highlowlags.Code)
slda_aggdata['Date'] = np.asarray(ftdata_highlowlags.Date)


# Export the sLDA topic proportions
source_location = "/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched"
export_filename = source_location + "/company_highlow_slda_30proportions.csv"
slda_aggdata.to_csv(export_filename, encoding = "utf-8")



# Export the phi estimates
phi_estimatesdf = pd.DataFrame(phi_estimates)

phi_filename = source_location + "/company_highlow_slda_30phi.csv"
phi_estimatesdf.to_csv(phi_filename)



print "-" * 10, "This is the end", "-" * 10
