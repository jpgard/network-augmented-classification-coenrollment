# Copyright (C) 2017  The Regents of the University of Michigan
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see [http://www.gnu.org/licenses/].
# ========================================================================
'''
script to extract data from a specific term for analysis; this was used to process raw data from umich.

usage:
    
    python extract_data_subset.py -t TERMCODE -o /path/to/outdir

on JG local:
    
    python extract_data_subset.py -t 2010 -o /Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/data/LARC
    
'''

import argparse
import pandas as pd

STUDENT = '/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/data/LARC/LARC-QA_20160801_STDNT_INFO.csv'
STUDENT_TERM = '/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/data/LARC/LARC-QA_20160801_STDNT_TERM_INFO.csv'
STUDENT_TERM_COURSE = '/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/data/LARC/LARC-QA_20160801_STDNT_TERM_CLASS_INFO.csv'


def create_subset(fp, outdir, term, csvname, write = True):
    print("Reading data from {0} for term {1}".format(fp, term))
    # read data uing iterator; CSV data files are too big to read directly into memory
    iter_csv = pd.read_csv(fp, iterator=True, chunksize=1000, error_bad_lines = False)
    df = pd.concat([chunk[chunk.TERM_CD == int(term)] for chunk in iter_csv])
    if write:
        fp_out = outdir + '/' + csvname
        print("Writing output to {0}".format(fp_out))
        df.to_csv(fp_out, index = False)
    return df


def merge_subsets(s, s_t, s_t_c, outdir, csvname, write = True):
    print("Merging data.")
    df_merged = pd.merge(s_t_c, s_t, how = 'inner', on = ['STDNT_ID', 'TERM_CD'], sort = False)
    df_out = pd.merge(df_merged, s, how = 'inner', on = ['STDNT_ID'], sort = False)
    if write:
        print("Writing merged data.")
        fp_out = outdir + '/' + csvname
        df_out.to_csv(fp_out, index = False)
    return None


def main():
    # build parser
    parser = argparse.ArgumentParser(description='Create coenrollment network features from student-term-course CSV file.')
    parser.add_argument('-t', metavar="Term code",
                        nargs=1, type=str, required=True, dest='term_cd_filter')
    parser.add_argument('-o', metavar="Output directory; script will create subdirectory for term code",
                        nargs=1, type=str, required=True, dest='outdir')
    # collect input from parser
    args = parser.parse_args()
    outdir = args.outdir[0]
    term_cd_filter = args.term_cd_filter[0]
    # read data, create subsets and write to out path
    term_outdir = outdir + '/TERM_{0}'.format(term_cd_filter)
    s = pd.read_csv(STUDENT, error_bad_lines = False)
    s_t = create_subset(fp = STUDENT_TERM, outdir = term_outdir, term = term_cd_filter, csvname = 'LARC_STUDENT_TERM_{0}.csv'.format(term_cd_filter))
    s_t_c = create_subset(fp=STUDENT_TERM_COURSE, outdir=term_outdir, term=term_cd_filter, csvname='LARC_STUDENT_TERM_COURSE_{0}.csv'.format(term_cd_filter))
    # read and merge subsets
    merge_subsets(s, s_t, s_t_c, outdir = term_outdir, csvname = 'LARC_{0}_MERGED.csv'.format(term_cd_filter))

if __name__ == "__main__":
    main()