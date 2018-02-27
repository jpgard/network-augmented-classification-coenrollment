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
script to extract coenrollment features from a student-term-course dataset
this is a wrapper for using or testing the functions in coenrollment_feat_utils.py

usage:

python extract_coenrollment_feats.py /path/to/data.csv /path/to/outfile.csv

on JG local:

    python extract_coenrollment_feats.py -i /Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/data/LARC/TERM_2070/LARC_2070_MERGED.csv -o ./nw_feats_2070.csv

for new features w/updated immediate neighbor features:

    python extract_coenrollment_feats.py -i /Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/data/LARC/TERM_2020/LARC_2020_MERGED.csv -o ./nw_v2/nw_2020.csv
    
'''
import argparse
from coenrollment_feat_utils import generate_course_column, extract_link_feats, extract_link_feats_immediate_neighbors
import pandas as pd


def extract_coenrollment_feats(fp, sbjct_cd = None):
    print("Reading data from {0}".format(fp))
    df = pd.read_csv(fp)
    df = generate_course_column(df)
    df_out = extract_link_feats(df, "PREV_TERM_CUM_GPA")
    df_out = extract_link_feats_immediate_neighbors(df_out, "PREV_TERM_CUM_GPA")
    return df_out


def main():
    # build parser
    parser = argparse.ArgumentParser(description='Create coenrollment network features from student-term-course CSV file.')
    parser.add_argument('-i', metavar="Input file path",
                        nargs=1, type=str, required=True, dest='infile')
    parser.add_argument('-o', metavar="Output file path",
                        nargs=1, type=str, required=True, dest='outfile')
    # collect input from parser
    args = parser.parse_args()
    infile = args.infile[0]
    outfile = args.outfile[0]
    # build features
    print("Extracting features...")
    nf_df = extract_coenrollment_feats(fp = infile)
    # write output
    print("Writing output...")
    nf_df.to_csv(outfile, index = False)
    print("Output written to ./{0}".format(outfile))


if __name__ == '__main__':
    main()