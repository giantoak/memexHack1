# -*- coding: utf-8 -*-
# TODO Update docstring to match reality
"""
This script takes ad price extraction and creates ad_prices_ad_level.csv (ad level clean price data)

It then creates a data set of only ads with two prices "doubles" with 
the implied fixed cost
"""
from AdPriceHelper import basic_ad_id_merger
from AdPriceHelper import all_call_merge
import pandas as pd


def main():
    print('==== Beginning make_ad_prices.py ====')

    nrows = None

    out = pd.read_csv('ad_price_ad_level.csv')

    # Now begin rebuilding DF by merging in original raw files, so we can
    # see how much stuff is missing...

    # Drop columns to be re-added
    out.drop(['cluster_id', 'date_str',
              'census_msa_code',
              'is_massage_parlor_ad',
              'incall', 'outcall', 'incalloutcall'],
             axis=1, inplace=True)

    # Merge in Timestamp data
    out = basic_ad_id_merger(out,
                             ['data/forGiantOak3/doc-provider-timestamp.tsv',
                              'data/forGiantOak3/doc-provider-timestamp.tsv.gz'],
                             'outer',
                             ['ad_id', 'cluster_id', 'date_str'],
                             nrows)

    # Merge in MSA data
    out = basic_ad_id_merger(out,
                             ['data/forGiantOak3/msa_locations.tsv', 'data/forGiantOak3/msa_locations.tsv.gz'],
                             'outer',
                             ['ad_id', 'census_msa_code'],
                             nrows)

    # Merge in massage parlor flag
    out = basic_ad_id_merger(out,
                             ['data/forGiantOak3/ismassageparlorad.tsv'],
                             'outer',
                             ['ad_id', 'is_massage_parlor_ad'],
                             nrows)

    # Merge in incall / outcall data
    out = all_call_merge(out, 'right', nrows)

    out.to_csv('ad_price_ad_level_all.csv', index=False)

if __name__ == "__main__":
    main()
