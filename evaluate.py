#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
This script does a basic analysis of extractions pre- and post-CDR integration

"""
import pandas as pd
# import datetime
import ipdb
# import json
import numpy as np
nrows = None

cdr_rates = pd.read_csv('data/cdr/rates-text.tsv', sep='\t', header=None, nrows=nrows, names=['extraction_info','ad_id','price_duration'])
ist_rates = pd.read_csv('data/cdr/rates-from-ist.tsv', sep='\t', header=None, nrows=nrows, names=['ad_id','price','duration'])

all_rates = pd.merge(cdr_rates, ist_rates, how='outer')
dd_num_rates = cdr_rates['ad_id'].unique().shape[0]
ist_num_rates = ist_rates['ad_id'].unique().shape[0]
all_num_rates = all_rates['ad_id'].unique().shape[0]

website_source = pd.read_csv('data/websites.tsv', sep='\t', header=None) # note: websites.tsv comes from pulling the site info from cdr/content.tsv
num_sites = website_source.shape[0]

print('Analysis for rate extractions for new data:')
print('There were prices from %s unique ads with deep dive extractions, or %s' % (dd_num_rates, dd_num_rates/float(num_sites)))
print('There were prices from %s unique ads with ist extractions, or %s' % (ist_num_rates, ist_num_rates/float(num_sites)))
print('There were prices from %s unique ads with both extractions, or %s' % (all_num_rates, all_num_rates/float(num_sites)))


print('______')
print('Analysis for rate extractions for old data:')
old_rates = pd.read_csv("data/forGiantOak3/rates2.tsv", sep="\t", header=None, names=['ad_id','rate'])
num_old_rates = old_rates['ad_id'].unique().shape[0]
old_sites = pd.read_csv('data/forGiantOak3/doc-provider-timestamp.tsv', sep='\t', header=None, names=['ad_id','cluster_id','date'])
num_old_sites = old_sites['ad_id'].unique().shape[0]
print('There were prices from %s unique ads out of %s, or %s' % (num_old_rates, num_old_sites, num_old_rates/float(num_old_sites)))

old_locations = pd.read_csv('data/forGiantOak3/msa_locations.tsv', sep='\t', header=None, names=['ad_id','msa'])
num_old_locations = old_locations['ad_id'].unique().shape[0]
new_locations=pd.read_csv('data/cdr/cbsa-text-and-dom-and-url.ver3.tsv', sep='\t', header=None, names=['ad_id', 'code','name','type'])
num_new_locations = new_locations['ad_id'].unique().shape[0]

print('______')
print('Analysis for MSA/CBSA extractions:')
print('There were locations from %s unique ads in the old data set, or %s' % (num_old_locations, num_old_locations/float(num_old_sites)))
print('There were locations from %s unique ads in the new data set, or %s' % (num_new_locations, num_new_locations/float(num_sites)))

print('_____')
print('Analysis of both MSA and price ettractions')
num_new_ads_with_price_and_msa = len(set(all_rates['ad_id'].tolist()).intersection(set(new_locations['ad_id'].tolist())))
num_old_ads_with_price_and_msa = len(set(old_rates['ad_id'].tolist()).intersection(set(old_locations['ad_id'].tolist())))
num_new_ads_with_price_and_msa_dd = len(set(cdr_rates['ad_id'].tolist()).intersection(set(new_locations['ad_id'].tolist())))
print('There were %s ads with both price and MSA in the new data set, or %s' % (num_new_ads_with_price_and_msa, num_new_ads_with_price_and_msa/float(num_sites)))
print('There were %s ads with both price and MSA in the new data set if we only count the DD extractions, or %s' % (num_new_ads_with_price_and_msa_dd, num_new_ads_with_price_and_msa_dd/float(num_sites)))
print('There were %s ads with both price and MSA in the old data set, or %s' % (num_old_ads_with_price_and_msa, num_old_ads_with_price_and_msa/float(num_old_sites)))
