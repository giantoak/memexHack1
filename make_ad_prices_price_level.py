# -*- coding: utf-8 -*-
from AdPriceHelper import basic_ad_id_loader
from AdPriceHelper import basic_ad_id_merger
from AdPriceHelper import all_call_merge
import pandas as pd
import numpy as np

__author__ = 'Pete[r] M. Landwehr, adapted from work by Jeff Borowitz'


def main():
    print('==== Beginning make_ad_prices_price_level.py ====')

    nrows = None

    data = basic_ad_id_loader(['data/forGiantOak3/rates2.tsv', 'data/forGiantOak3/rates.tsv.gz'],
                              ['ad_id', 'rate'],
                              nrows)

    print('There are %s observations' % data.shape[0])  # about 2.1M

    data['time_str'] = data['rate'].apply(lambda x: x.split(',')[1])
    data['price'] = data['rate'].apply(lambda x: x.split(',')[0])
    data['unit'] = data['time_str'].apply(lambda x: x.split(' ')[1])

    data = data.ix[data['unit'] != 'DURATION', :]  # about 1.7M
    print('There are %s observations after dropping no duration prices' % data.shape[0])

    data['timeValue'] = data['time_str'].apply(lambda x: x.split(' ')[0]).astype(np.int_)
    data.ix[data['unit'] == 'HOURS', 'unit'] = 'HOUR'

    # Convert times to minutes
    data.ix[data['unit'] == 'MINS', 'minutes'] = data.ix[data['unit'] == 'MINS', 'timeValue']
    data.ix[data['unit'] == 'HOUR', 'minutes'] = 60 * data.ix[data['unit'] == 'MINS', 'timeValue']
    data.drop(['unit', 'timeValue'], axis=1, inplace=True)

    # Clean up currencies
    dollar_synonyms = ['$', 'roses', 'rose', 'bucks', 'kisses', 'kiss', 'dollars', 'dollar', 'dlr']
    for d_s in dollar_synonyms:
        data.ix[:, 'price'] = data['price'].apply(lambda x: x.replace(d_s, ''))

    other_currencies = ['euro', 'eur', 'eu', 's', '¥', '\xef\xbc\x90', 'aud']
    for o_c in other_currencies:
        data = data.ix[data['price'].apply(lambda x: o_c not in x)]

    print('There are %s prices after dropping foreign prices' % data.shape[0])

    # This code is useful for dealing with the 'price' string problem in
    # sam's rates_locs file from 12/29
    data.ix[:, 'price'] = data['price'].astype('int')

    # Begin merging information from census
    data = basic_ad_id_merger(data,
                              ['data/forGiantOak3/isssexad.tsv', 'data/forGiantOak3/isssexad.tsv.gz'],
                              'left',
                              ['ad_id', 'sex_ad'],
                              nrows)

    # data = data[data['sex_ad'] == 1] # remove non- sex ads
    # print('There are %s prices after dropping Non-sex ad prices' % data.shape[0])

    # Merge in massage parlor information
    data = basic_ad_id_merger(data,
                              ['data/forGiantOak3/ismassageparlorad.tsv'],
                              'left',
                              ['ad_id', 'massage_ad'],
                              nrows)

    counts = pd.DataFrame(data.groupby('ad_id')['ad_id'].count())
    print('The %s extracted prices pertain to %s observations' % (data.shape[0], counts.shape[0]))
    counts.rename(columns={'ad_id': 'prices_from_ad'}, inplace=True)
    out = pd.merge(data, counts, left_on='ad_id', right_index=True)
    del counts

    # Merge in MSA data
    out = basic_ad_id_merger(out,
                             ['data/forGiantOak3/msa_locations.tsv', 'data/forGiantOak3/msa_locations.tsv.gz'],
                             'left',
                             ['ad_id', 'census_msa_code'],
                             nrows)

    # Merge in cluster ID
    out = basic_ad_id_merger(out,
                             ['data/forGiantOak3/doc-provider-timestamp.tsv',
                              'data/forGiantOak3/doc-provider-timestamp.tsv.gz'],
                             'left',
                             ['ad_id', 'cluster_id', 'date_str'],
                             nrows)

    out[out['cluster_id'] == '\N'] = np.nan
    out[out['date_str'] == '\N'] = np.nan

    # Merge in massage parlor flag
    out = basic_ad_id_merger(out,
                             ['data/forGiantOak3/ismassageparlorad.tsv'],
                             'left',
                             ['ad_id', 'is_massage_parlor_ad'],
                             nrows)

    # Merge in call data
    out = all_call_merge(out, 'left', nrows)

    # Write results
    out.to_csv('ad_prices_price_level.csv', index=False)

if __name__ == "__main__":
    main()
