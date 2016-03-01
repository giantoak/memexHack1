import pandas as pd

# Add LEMAS and UCR data
ucr = pd.read_csv('ucr_crime_msayearlevel.csv')
ucr['census_msa_code'] = ucr['CBSACode'].apply(lambda x: "31000US%s" % str(x))
ucr=ucr[ucr['rape'].notnull()]
ucr=ucr[ucr['property'].notnull()]
ucr=ucr[ucr['violent'].notnull()]
ucr[['census_msa_code','year','violent','property','rape']].to_csv('ucr.csv', index=False)
