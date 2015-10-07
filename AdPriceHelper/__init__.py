# -*- coding: utf-8 -*-
import os
import pandas as pd

__author__ = 'Pete[r] M. Landwehr'


def basic_ad_id_loader(fpaths, names, nrows):
    """
    Helper func for loading data frames from ad files. Loads the first extant path in fpaths
    If no paths exist, raises an exception
    :param list|tupefpaths:
    :param list names:
    :param int nrows:
    :return pandas.DataFrame:
    """
    for fpath in fpaths:
        if os.path.exists(fpath):
            return pd.read_csv(fpath, sep='\t', header=None, names=names, nrows=nrows)

    raise FileNotFoundError("Could not find fpaths: {}".format(fpaths))


def basic_ad_id_merger(df, fpaths, how, names, nrows):
    """
    Helper func for merging files on the ad_id column. Merges on the first extant path in fpaths
    If no paths exist, raises an exception
    :param pandas.DataFrame df:
    :param list|tuple|set fpaths:
    :param str how: 'right' or 'left'
    :param list names:
    :param int nrows:
    :return pandas.DataFrame:
    """
    return df.merge(basic_ad_id_loader(fpaths, names, nrows), how=how)


def all_call_merge(df, how, nrows):
    """
    Helper func to merge in incall, outcall, incalloutcall
    :param df: Dataframe that needs calls merged in
    :param str how: 'right' or 'left'
    :return: DataFrame with merged calls
    """
    for call_type in ['incall', 'outcall', 'incalloutcall']:
        fpath = 'data/forGiantOak6/{}-new.tsv'.format(call_type)
        call_input = '{}_input'.format(call_type)
        no_call_type = 'no_{}'.format(call_type)

        df = basic_ad_id_merger(df,
                                [fpath],
                                how,
                                ['ad_id', call_input],
                                nrows)

        df = df.merge(pd.read_csv(fpath, sep='\t', header=None, names=['ad_id', call_input], nrows=nrows),
                      how=how)
        df[call_type] = df[call_input] == 1
        df[no_call_type] = df[call_input] == -1
        del df[call_input]

    return df
