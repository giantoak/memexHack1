#!/bin/python
import json
import ipdb
with open('data/cdr/content.tsv') as f:
    with open('data/dates_cdr.tsv','w') as out_file:
        with open('data/websites.tsv','w') as out_sites:
            with open('data/text_length.tsv','w') as text_length_out:
                for line in f:
                    tokens=line.split('\t')
                    data = json.loads(tokens[5])
                    if 'post_date' in data.keys():
                        out_file.write('%s\t%s\n' % (tokens[0], data['post_date']))
                    out_sites.write('%s\t%s\n' % (tokens[0], tokens[1]))
                    text_length_out.write('%s\t%s\n' % (tokens[0], len(tokens[4])))

