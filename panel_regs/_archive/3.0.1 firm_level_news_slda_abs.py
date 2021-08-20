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


### Split into a training set (exlcuding outliers etc)
train = ftdata[ftdata.abs_lags_include != 0]

# ftdata.dates = pd.to_datetime(ftdata.date_num)

#clean and stem data using topicmodels
doc = topicmodels.RawDocs(train.main_text, "long")
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
#ftdata['stemmed'] = [' '.join(s) for s in doc.stems]
# Drop any empty documents
#ftdata['stemmed'].replace('', np.nan, inplace=True)

#ftdata = ftdata.dropna(subset = ['stemmed'])
#ftdata = ftdata.reset_index()

# Check that there are no empty documents
assert all(doc.stems)


# Estimate the topicmodel
topicnumber = 30
K = topicnumber
D = len(doc.stems)
y = np.asarray(train.abs_lags_residuals)
#X = np.asarray(df.X).reshape((D,1))
X = np.zeros(D).reshape((D,1))
# X = np.random.normal(size=X_size*D).reshape((D, 2))
phi = np.random.randn(K)
gamma = np.zeros(X.shape[1])
sigma = 0.1

sldaobj = topicmodels.LDA.sLDAGibbs(doc.stems, K, y, X, phi, gamma, sigma)


# Initialise the phi and gamma coefficients using the topic seed
sldaobj.initialise_coeffs()


#### Run the EM algorithm ####

maxEMiter = 80
phi_estimates = np.zeros(maxEMiter*K).reshape(maxEMiter, K)

sldaobj.y = sldaobj.y - np.mean(sldaobj.y)

for i in range(21,22):
    print "EM iteration %d out of a maximum possible %d." % (i, maxEMiter)
    sldaobj.E_step(1000,20,500)
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
content_filename = source_location + "/company_abs_slda_30topic_description.csv"
sldaobj.topic_content(20, output_file=content_filename)


slda_aggdata = pd.DataFrame(slda_dt,index = train.Date, columns=['T' + str(i) for i in xrange(sldaobj.K)])
slda_aggdata['article_id'] = np.asarray(train.article_id)
slda_aggdata['Code'] = np.asarray(train.Code)
slda_aggdata['Date'] = np.asarray(train.Date)


# Export the sLDA topic proportions
source_location = "/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched"
export_filename = source_location + "/company_abs_slda_30proportions.csv"
slda_aggdata.to_csv(export_filename, encoding = "utf-8")



# Export the phi estimates
phi_estimatesdf = pd.DataFrame(phi_estimates)

phi_filename = source_location + "/company_abs_slda_30phi.csv"
phi_estimatesdf.to_csv(phi_filename)




### Now query on the full set

test_doc = topicmodels.RawDocs(ftdata.main_text, "long")
test_doc.token_clean(1)
test_doc.stopword_remove("tokens")
print "Stemming words in test set..."
test_doc.stem()
test_doc.stopword_remove("stems")

# Query each article in turn
test_query = topicmodels.LDA.QueryGibbs(test_doc.stems,sldaobj.token_key,sldaobj.tt)
test_query.query(25)
test_dt = test_query.dt_avg()

test_output = pd.DataFrame(test_dt, index = ftdata.article_id, columns=['T' + str(i) for i in xrange(sldaobj.K)])
test_output['Code'] = np.asarray(ftdata.Code)
test_output['Date'] = np.asarray(ftdata.Date)
test_output['abs_lags_residuals'] = np.asarray(ftdata.abs_lags_residual)


export_filename = source_location + "/company_abs_slda_30proportions_full.csv"
test_output.to_csv(export_filename, encoding = "utf-8")




print "-" * 10, "This is the end", "-" * 10
