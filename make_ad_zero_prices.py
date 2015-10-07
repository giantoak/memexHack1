# -*- coding: utf-8 -*-
import pandas as pd

__author__ = 'Pete[r] M. Landwehr, adapted from work by Jeff Borowitz'


def main():
    print('==== Beginning make_ad_zero_prices.py ====')

    out = pd.read_csv('ad_prices_price_level.csv')

    # Begin work on fixed prices
    out = out.ix[out['prices_from_ad'] == 2, :]
    print('There are %s ads after restricting to ads with 2 prices' % out.shape[0])

    calcs = out.groupby('ad_id').agg({'price': ['min', 'max'], 'minutes': ['min', 'max']})
    out = pd.merge(out, pd.DataFrame(calcs['price']['min']), left_on='ad_id', right_index=True)
    out.rename(columns={'min': 'p1'}, inplace=True)
    out = pd.merge(out, pd.DataFrame(calcs['price']['max']), left_on='ad_id', right_index=True)
    out.rename(columns={'max': 'p2'}, inplace=True)
    out = pd.merge(out, pd.DataFrame(calcs['minutes']['min']), left_on='ad_id', right_index=True)
    out.rename(columns={'min': 'm1'}, inplace=True)
    out = pd.merge(out, pd.DataFrame(calcs['minutes']['max']), left_on='ad_id', right_index=True)
    out.rename(columns={'max': 'm2'}, inplace=True)
    out['zero_price'] = (out['p1'] * out['m2'] - out['m1'] * out['p2']) / (out['m2'] - out['m1'])
    out = out[~out['ad_id'].duplicated()]  # remove duplicates
    print('There are %s ads after dropping duplicates' % out.shape[0])
    out = out[out['m1'] != out['m2']]  # remove those with two prices for the same time...
    out['marginal_price'] = (out['p2'] - out['p1']) / (out['m2'] - out['m1']) * 60

    out.to_csv('ad_zero_prices.csv', index=False)

if __name__ == "__main__":
    main()
