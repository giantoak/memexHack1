import pandas
import ipdb
import numpy as np
import phonenumbers
import datetime

start = datetime.datetime.now()
prices = pandas.read_csv('data/TGG/provider_prices.csv')
stats = pandas.read_csv('data/TGG/provider_stats.csv')
del stats['source']
del stats['service']
out = prices.merge(stats, how='left')
#outcall = prices[(prices.service== 'escort outcall') & (prices.length==1)]
#incall = prices[(prices.service== 'escort incall') & (prices.length==1)]
#escort = prices[(prices.service== 'escort') & (prices.length==1)]
ad_prices = pandas.read_csv('ad_price_ad_level.csv')
#ad_prices = ad_prices.groupby('cluster_id').filter(lambda x: x.shape[0] <= 200)
ad_prices = ad_prices[ad_prices['1hr']==True]
ad_phones=pandas.read_csv('data/forGiantOak3/phone_numbers.tsv', sep='\t', header=None, names=['ad_id','phone'])
loaded_data =datetime.datetime.now()
print(' loaded datat in %s' % (loaded_data-start))
def parse_phone(x):
    if not x:
        return('')
    if x.__class__ == np.float64:
        to_parse = str(int(x))
    else:
        to_parse = x
    try:
        parsed=phonenumbers.parse(to_parse, 'US')
        out_str = phonenumbers.format_number(parsed,phonenumbers.PhoneNumberFormat.NATIONAL)  
        return(out_str)
    except ValueError as e:
        ipdb.set_trace()
        return('')
    except Exception as e:
        ipdb.set_trace()

    #stripped_phone = x.replace(' ','').replace('(','').replace(')','').replace('+','').replace('-','').replace('.','')
    #return(np.float(x.replace(' ','').replace('(','').replace(')','').replace('+','').replace('-','')))
out = out[out['phone_1'].notnull()]
out = out[out['length'] == 1]
out['new_phone'] = out['phone_1'].apply(parse_phone)
ter_review_level = out[['new_phone','price','service']]
ter_review_level = ter_review_level.rename(columns={'price':'review_price'})
ad_price_level = ad_prices.merge(ad_phones)
ad_price_level = ad_price_level[ad_price_level['phone'].notnull()]
ad_price_level['new_phone'] = ad_price_level['phone'].apply(parse_phone)
ad_phone_level = ad_price_level.groupby('new_phone')['price_per_hour'].mean().reset_index()
ad_phone_level = ad_phone_level.rename(columns={'price_per_hour':'ad_price'})
ad_price_level = ad_price_level.rename(columns={'price_per_hour':'ad_price'})

# do all
merged_review_level_all=pandas.merge(ter_review_level[['new_phone','review_price','service']], ad_price_level[['new_phone','ad_price']])
ter_phone_level_all= ter_review_level.groupby(['new_phone','service'])['review_price'].mean().reset_index()
merged_phone_level_all=pandas.merge(ter_phone_level_all, ad_phone_level)
print(merged_review_level_all.describe())
print(merged_phone_level_all.describe())
merged_review_level_all.to_csv('merged_review_level_all.csv', index=False)
merged_phone_level_all.to_csv('merged_phone_level_all.csv', index=False)
## Do incalls
#merged_review_level_incall=pandas.merge(ter_review_level.loc[ter_review_level['service'] == 'escort incall', ['new_phone','review_price']], ad_price_level[['new_phone','ad_price']])
#ter_phone_level = ter_review_level[ter_review_level['service'] == 'escort incall'].groupby('new_phone')['review_price'].mean().reset_index()
#merged_phone_level_incall=pandas.merge(ter_phone_level, ad_phone_level)
#print(merged_review_level_incall.describe())
#print(merged_phone_level_incall.describe())
#merged_review_level_incall.to_csv('merged_review_level_incall.csv', index=False)
#merged_phone_level_incall.to_csv('merged_phone_level_incall.csv', index=False)

## Do outcall
#merged_review_level_outcall=pandas.merge(ter_review_level.loc[ter_review_level['service'] == 'escort outcall', ['new_phone','review_price']], ad_price_level[['new_phone','ad_price']])
#ter_phone_level = ter_review_level[ter_review_level['service'] == 'escort outcall'].groupby('new_phone')['review_price'].mean().reset_index()
#merged_phone_level_outcall=pandas.merge(ter_phone_level, ad_phone_level)
#print(merged_review_level_outcall.describe())
#print(merged_phone_level_outcall.describe())
#merged_review_level_outcall.to_csv('merged_review_level_outcall.csv', index=False)
#merged_phone_level_outcall.to_csv('merged_phone_level_outcall.csv', index=False)
#print(' merged datat in %s' % (datetime.datetime.now()-start))

#ad_phone_set = set(ad_price_level['new_phone'].unique())
#out_phone_set = set(out['new_phone'].unique())

#print('Overlapping phones: %s' % len(ad_phone_set.intersection(out_phone_set)))
#print('Missed phones from TER: %s' % len(out_phone_set.difference(ad_phone_set)))
#c.to_csv('phone_level_review_vs_ad_comparison.csv', index=False)
#b.to_csv('ad_level_review_vs_ad_comparison.csv', index=False)
