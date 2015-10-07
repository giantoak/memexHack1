#!/usr/bin/python
# -*- coding: utf-8 -*-
# TODO Update docstring to match reality
"""
This script takes ad price extraction and creates ad_prices_ad_level.csv (ad level clean price data)

It then creates a data set of only ads with two prices "doubles" with 
the implied fixed cost
"""
from AdPriceHelper import basic_ad_id_loader
from AdPriceHelper import all_call_merge
import pandas as pd


def main():
    print('==== Beginning make_ad_prices.py ====')

    nrows = None

    out = pd.read_csv('ad_price_ad_level.csv')

    # Now begin rebuilding DF by merging in original raw files, so we can
    # see how much stuff is missing...
    del out['cluster_id']
    del out['date_str']
    ts = basic_ad_id_loader(
        ['data/forGiantOak3/doc-provider-timestamp.tsv', 'data/forGiantOak3/doc-provider-timestamp.tsv.gz'],
        ['ad_id', 'cluster_id', 'date_str'],
        nrows)
    out = ts.merge(out, how='outer')
    del ts

    del out['census_msa_code']
    msa = basic_ad_id_loader(
        ['data/forGiantOak3/msa_locations.tsv', 'data/forGiantOak3/msa_locations.tsv.gz'],
        ['ad_id', 'census_msa_code'],
        nrows)
    out = msa.merge(out, how='outer')
    del msa

    # Merge in massage parlor flag
    del out['is_massage_parlor_ad']
    massage = basic_ad_id_loader(
        ['data/forGiantOak3/ismassageparlorad.tsv'],
        ['ad_id', 'is_massage_parlor_ad'],
        nrows)
    out = out.merge(massage, how='right')
    del massage

    del out['incall']
    del out['outcall']
    del out['incalloutcall']
    out = all_call_merge(out, 'right', nrows)

    out.to_csv('ad_price_ad_level_all.csv', index=False)

if __name__ == "__main__":
    main()
