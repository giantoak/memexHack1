#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
This script takes ad price extractions and msa extractions to understand balance
"""
import pandas as pd
# import datetime
import ipdb
# import json
import numpy as np
nrows = None

data = pd.read_csv('data/cdr/rates-text-and-ist-cleaned.tsv', sep='\t', header=None, nrows=nrows)


def fraction_true(df):
    try:
        true=df[df['has_price']==1]['count']
        if not len(true):
            true=0
    except:
        true=0

    total=df['count'].sum()
    return(np.float(true)/total)

def all_call_merge(df, merge_dir):
    """
    Merge in incall, outcall, incalloutcall
    :param df: Dataframe that needs calls merged in
    :param str merge_dir: 'right' or 'left'
    :return: DataFrame with merged calls
    """
    for call_type in ['incall', 'outcall', 'incalloutcall']:
        fpath = 'data/forGiantOak6/{}-new.tsv'.format(call_type)
        call_input = '{}_input'.format(call_type)
        no_call_type = 'no_{}'.format(call_type)

        df = df.merge(pd.read_csv(fpath, sep='\t', header=None, names=['ad_id', call_input], nrows=nrows),
                      how=merge_dir)
        df[call_type] = df[call_input] == 1
        df[no_call_type] = df[call_input] == -1
        del df[call_input]

    return df


print('There are %s observations' % data.shape[0])  # about 2.1M
data.rename(columns={0: 'ad_id', 1: 'price', 2:'minutes'}, inplace=True)
#data['time_str'] = data['rate'].apply(lambda x: x.split(',')[1])
#data['price'] = data['rate'].apply(lambda x: x.split(',')[0])
#data['unit'] = data['time_str'].apply(lambda x: x.split(' ')[1])
#data = data[data['unit'] != 'DURATION']  # about 1.7M
print('There are %s observations after dropping no duration prices' % data.shape[0])


# Begin using MSA data
msa = pd.read_csv('data/cdr/cbsa-text-and-dom-and-url.ver3.tsv',
                      sep='\t', header=None, names=['ad_id', 'census_msa_code_short','msa_name','macro_micro'], nrows=nrows)
msa['census_msa_code'] = msa['census_msa_code_short'].apply(lambda x: '31000US%s' % x)  # 310000 is the MSA code
del msa['census_msa_code_short']

msa_price_extraction_rates = pd.merge(data, msa, how='right')
msa_price_extraction_rates['has_price'] = msa_price_extraction_rates['price'].notnull()
msa_price_extractions = pd.DataFrame(msa_price_extraction_rates.groupby(['has_price','msa_name']).size(), columns=['count'])
msa_price_extractions = msa_price_extractions.reset_index()
msa_price_extractions['has_price'] = msa_price_extractions['has_price'].astype('int')
msa_price_extractions.to_csv('msa_price.csv', index=False)
fraction=msa_price_extractions.groupby('msa_name').apply(fraction_true)
total=msa_price_extractions.groupby('msa_name')['count'].sum()
fraction_df= pd.DataFrame(fraction, columns=['fraction'])
total_df= pd.DataFrame(total)
out = pd.merge(fraction_df, total_df, left_index=True, right_index=True)
out=out.reset_index()
#aggs=msa_price_extractions.groupby('msa_name')['count'].agg(['sum','mean'])
#aggs['frac']=aggs['mean']/aggs['sum']
#aggs=aggs.reset_index()
out.to_csv('msa_price.csv', index=False)

# merge in date
dates = pd.read_csv('data/dates_cdr.tsv', sep='\t', nrows=nrows, header=None, names=['ad_id','date_str'])
dates['ym_str'] = dates['date_str'].apply(lambda x: x[0:7])
dates = msa_price_extraction_rates.merge(dates, how='left')
date_price_extractions = pd.DataFrame(dates.groupby(['has_price','ym_str']).size(), columns=['count'])
date_price_extractions=date_price_extractions.reset_index()
date_price_extractions['has_price'] = date_price_extractions['has_price'].as_type('int')
date_price_extractions.to_csv('date_price.csv', index=False)
fraction=date_price_extractions.groupby('ym_str').apply(fraction_true)
total=date_price_extractions.groupby('ym_str')['count'].sum()
fraction_df= pd.DataFrame(fraction, columns=['fraction'])
total_df= pd.DataFrame(total)
out = pd.merge(fraction_df, total_df, left_index=True, right_index=True)
out=out.reset_index()
out['year'] = out['ym_str'].apply(lambda x: int(x[0:4]))
out['month'] = out['ym_str'].apply(lambda x: int(x[5:7]))
out.to_csv('date_price.csv', index=False)
#aggs=msa_price_extractions.groupby('msa_name')['count'].agg(['sum','mean'])
#aggs['frac']=aggs['mean']/aggs['sum']
#aggs=aggs.reset_index()
out.to_csv('msa_price.csv', index=False)

del msa_price_extraction_rates
del data
del msa
np.random.seed(2)
with_price = dates[dates['has_price']]
no_price = dates[~dates['has_price']]
price_sample = with_price.loc[np.random.choice(with_price.index, 20000)]
no_price_sample = no_price.loc[np.random.choice(no_price.index, 20000)] 
text_length = pd.read_csv('data/text_length.tsv', sep='\t', header=None, columns=['ad_id','length'])
sample = pd.concat([price_sample, no_price_sample])
del dates
sample=pd.merge(sample, text_length, how='left')
sample[['has_price','msa_name','length']].to_csv('text_length_sample.csv', index=False)
