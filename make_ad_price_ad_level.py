# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np

__author__ = 'Pete[r] M. Landwehr, adapted from work by Jeff Borowitz'


def main():
    print('==== Beginning make_ad_price_ad_level.py ====')

    nrows = None

    # Re-read ad_prices_price_level.csv to aggregate from file
    data = pd.read_csv('ad_prices_price_level.csv')
    print(data.shape)
    # Begin computing price per hour:
    # If we have a 1 hour price, that's it. Otherwise, multiply all the
    # quoted prices by a 'multiplier' which represents the average ratio of
    # hourly price to the given time period price

    # The below blocks of code compute 'price_ratios' which is the ratio of
    # average prices for 1 hour for other ads that also posted the same
    # price

    minute_values = pd.Series((data['minutes'].value_counts()/data.shape[0] > .0001)).index.map(np.int_)
    if 60 not in minute_values:
        minute_values = np.append(minute_values, 60)
    minute_values.sort()
    minute_string_series = pd.Series(['price_{}_mins'.format(x) for x in minute_values],
                                     index=minute_values)

    # def get_prices(x):
    #     temp = pd.Series(np.nan, index=minute_values)
    #     for mins in minute_values:
    #         matching_prices = x.ix[x['minutes'] == mins, 'price']
    #         if len(matching_prices):
    #             temp[mins] = matching_prices.mean()
    #     return temp

    def get_prices_five(df):
        return pd.pivot_table(df.ix[df['minutes'].apply(lambda x: x in minute_values),
                                    ['ad_id', 'minutes', 'price']].groupby(['ad_id', 'minutes']).mean().reset_index(),
                              index='ad_id',
                              columns='minutes')

    # me = data.groupby('ad_id').apply(get_prices)  # This is REALLLLY slow
    me = get_prices_five(data)  # This is WAAAY faster!
    me.columns = me.columns.levels[1]
    for m_v in minute_values:
        if m_v not in me.columns:
            me[m_v] = np.nan

    def get_price_ratio(minute_slice):
        hour_price = me.ix[(~me[60].isnull() & ~minute_slice.isnull()), 60].mean()
        return hour_price/minute_slice.mean()

    price_ratios = me.apply(get_price_ratio, axis=0)
    price_ratios_counts = me.apply(lambda x: x.dropna().shape[0])

    print('Computed price ratios for acts of given length to acts of 1 hour')
    print('price_ratios: {}'.format(price_ratios))
    print('price_ratios_counts: {}'.format(price_ratios_counts))

    # Now split the data by whether there's a posted price of 1 hr
    data['1hr'] = data['time_str'] == '1 HOUR'
    a = data.groupby('ad_id')['1hr'].sum()
    a = a > 0
    del data['1hr']
    a = pd.DataFrame(a)
    data = pd.merge(data, a, left_on='ad_id', right_index=True)
    price_level_hourly = pd.DataFrame(data[data['1hr']])
    price_level_hourly['price_per_hour'] = price_level_hourly['price']  # If there's an hourly price, use it
    price_level_no_hourly = pd.DataFrame(data[~data['1hr']])
    price_level_no_hourly.index = price_level_no_hourly['ad_id']
    # Otherwise use the multiplier
    price_level_no_hourly['multiplier'] = price_level_no_hourly['minutes'].apply(lambda x: price_ratios[x])
    price_level_no_hourly['price_per_hour'] = price_level_no_hourly['price'] * price_level_no_hourly['multiplier']
    price_level_no_hourly_prices = pd.DataFrame(price_level_no_hourly.groupby('ad_id')['price_per_hour'].mean())
    price_level_no_hourly['price_per_hour'] = price_level_no_hourly_prices
    price_level = pd.concat([price_level_hourly, price_level_no_hourly], axis=0)
    price_level.sort('1hr', ascending=False, inplace=True)
    ad_level_prices = pd.DataFrame(price_level.groupby('ad_id')['price_per_hour'].mean(), columns=['price_per_hour'])
    ad_level = price_level.drop_duplicates('ad_id')[['ad_id', 'sex_ad', 'census_msa_code', 'cluster_id', 'date_str',
                                                     'is_massage_parlor_ad', '1hr', 'incall', 'no_incall', 'outcall',
                                                     'no_outcall', 'incalloutcall', 'no_incalloutcall']]
    out = pd.merge(ad_level_prices, ad_level, left_index=True, right_on='ad_id', how='left')
    # Clean up some unused data...
    print('cleaning up old data...')
    del data
    del price_level
    del ad_level
    del ad_level_prices

    # Filter out spam guys with > 200 ads in our sample period and save
    spam = pd.DataFrame(out.groupby('cluster_id').apply(lambda x: x.shape[0] > 200), columns=['spam'])
    spam.reset_index(inplace=True)
    out = out.merge(spam)

    # Add site filter
    out['site'] = out['ad_id'].apply(lambda x: x.split(':')[0])

    # Compute the cluster size
    out = out.merge(out.groupby('cluster_id').size().to_frame('cluster_count').reset_index())
    out.to_csv('ad_price_ad_level.csv', index=False)

if __name__ == "__main__":
    main()
