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
# contains utility functions for extracting network feats
# these feats are based on features described in [Lu and Getoor 2003], [Getoor 2005], and [Gardner and Brooks 2017].

import pandas as pd
import math
import numpy as np
from multiprocessing import Pool
from functools import partial
from timeit import default_timer as timer


def generate_course_column(df, new_course_col = "COURSE", subject_col = "SBJCT_CD", course_number_col = "CATLG_NBR", section_number_col = "CLASS_SCTN_CD"):
    """
    Create single unique course identifier column by concatenating subject_col, course_number_col, and section_number_col.
    :param df: pd.DataFrame containing columns matching subject_col, course_number_col, and section_number_col
    :param subject_col: name of column in df indicating course subject/department (i.e., MATH) (string). 
    :param course_number_col: name of column in df indicating course number (i.e., 101) (string).
    :param section_number_col: name of column in df indicating course section number (i.e., 005) (string).
    :return: df with new new_course_col appended.
    """
    print("Generating new course identifier column: {0}".format(new_course_col))
    df[new_course_col] = df.apply(lambda row: "{0}_{1}_{2}".format(row[subject_col], row[course_number_col], row[section_number_col]), axis = 1)
    return df


def get_neighbors(sid, df, sid_col = "STDNT_ID", course_col = "COURSE"):
    """
    Utility function to fetch dataframe with only the "neighbors" -- coenrolled students -- of student sid.
    :param sid: student id (string).
    :param df: pd.DataFrame of student-term-course info.
    :param sid_col: name of column in df that represents student identifier (string).
    :return: pd.DataFrame of all records in df for sid's neighbors.
    """
    sid_courses = df[df[sid_col] == sid][course_col]
    sid_neighbor_df = df[df[course_col].isin(sid_courses)]
    return sid_neighbor_df[sid_neighbor_df[sid_col] != sid]


def get_count_link(df, sid_col, feat, bins):
    """
    Use bins to generate counts of students in each bin in df; this creates the count-link features for a given students' neighborhood.
    :param df: pd.DataFrame returned by get_neighbors.
    :param sid_col: name of column in df that represents student identifier (string).
    :param feat: feature to generate count-link feats for using bins.
    :param bins: list of cut points for bins; note that each bin is closed on left but open on right, i.e., (0, 1]
    :return: pd.Series of counts for values of feat in df, indexed by bin.
    """
    # NOTE: this feature excludes any students who don't have prev_term_gpa s (can add handling here if desired)
    df["CL_BIN"] = pd.cut(df[feat], bins)
    vc = pd.value_counts(df["CL_BIN"]).reindex(df["CL_BIN"].cat.categories) # to handle/keep na values, use dropna = False; also need to handle case of no NA observations
    return vc


def generate_binary_link_df(cl_df, bl_cols):
    """
    Create binarized dataframe from count-link dataframe and assign column names bl_cols.
    :param cl_df: count-link dataframe generated in extract_link_feats; note that this function assumes the index of cl_df is sid_col. 
    :param bl_cols: column names for bl_cols; these should be prepended by 'BL' or some other identifier.
    :return: pd.DataFrame with binarized (0 or 1) values for BL features, with index set to sid_col.
    """
    df_out = cl_df.applymap(lambda x: 1 if x > 0 else 0)
    df_out.columns = bl_cols
    return df_out


def generate_proportion_link_df(cl_df, p_l_cols):
    """
    Create binarized dataframe from count-link dataframe and assign column names bl_cols.
    :param cl_df: count-link dataframe generated in extract_link_feats; note that this function assumes the index of cl_df is sid_col.
    :param bl_cols: column names for bl_cols; these should be prepended by 'BL' or some other identifier.
    :return: pd.DataFrame with proportion [0, 1] values for PL features, with index set to sid_col.
    """
    pl_df = cl_df.select_dtypes(exclude=['object']).div(cl_df.sum(axis=1), axis=0)
    pl_df.columns = p_l_cols
    df_out = pd.concat([pl_df, cl_df.select_dtypes(include=['object'])], axis = 1)
    return df_out


def generate_student_ml_cl_row(s, df, sid_col, course_col, feat, feat_bins):
    n_df = get_neighbors(s, df, sid_col, course_col)
    # mean-link
    m_l_row = [s] + [n_df[feat].mean()]
    # count-link
    sid_cl = get_count_link(n_df, sid_col, feat, bins=feat_bins)
    c_l_row = [s] + sid_cl.tolist()
    return (m_l_row, c_l_row)


def extract_link_feats(df, feat, ml_col = "ML_GPA", sid_col = "STDNT_ID", course_col = "COURSE", feat_bins = [0.0, 1.0, 2.0, 3.0, 4.0, 100.0]):
    """
    Create mean-link features for each record using PREV_TERM_CUM_GPA feat.
    :param df: pd.DataFrame; should match format of joined LARC tables.
    :param feat: name of column in df to generate mean_link features for (string). 
    :param ml_col: name of new mean-link column to append to df.
    :param sid_col: name of column in df that represents student identifier (string).
    :param course_col: name of column in df that represents course identifier (string).
    :return: pd.DataFrame of original input with additional column appended.
    """
    print("Extracting link features for {0}".format(feat))
    # initialize data structures and column names
    students = df[sid_col].unique()
    # m_l = []
    # c_l = []
    col_stems = pd.cut(df[feat], feat_bins).cat.categories.tolist()
    c_l_cols = ['CL_BIN_' + x.replace(' ', '') for x in col_stems]
    b_l_cols = ['BL_BIN_' + x.replace(' ', '') for x in col_stems]
    p_l_cols = ['PL_BIN_' + x.replace(' ', '') for x in col_stems]
    # for s in students:
    #     item = generate_student_ml_cl_row(s, df, sid_col, course_col, feat, feat_bins)
    #     m_l.append(item[0])
    #     c_l.append(item[1])
    # parallel version of loop above
    with Pool() as pool:
        map_rows = pool.map_async(partial(generate_student_ml_cl_row, df=df, sid_col=sid_col, course_col=course_col, feat=feat, feat_bins=feat_bins), students)
        pool.close()
        pool.join()
    m_l_par = [x[0] for x in map_rows.get()]
    c_l_par = [x[1] for x in map_rows.get()]
    # code to check equivalence of methods
    # np.allclose(m_l, m_l_par, equal_nan=True)
    # np.allclose(c_l, c_l_par, equal_nan=True)
    # convert lists to dataframes
    ml_df = pd.DataFrame.from_records(m_l_par, columns = [sid_col, ml_col]).set_index(sid_col)
    cl_df = pd.DataFrame.from_records(c_l_par, columns=[sid_col] + c_l_cols).set_index(sid_col)
    # binary link features; these are just binarized count-link features
    bl_df = generate_binary_link_df(cl_df, b_l_cols)
    # proportion link features
    pl_df = generate_proportion_link_df(cl_df, p_l_cols)
    # merge all features
    df_out = df.set_index([course_col, sid_col]).join(ml_df).join(cl_df).join(bl_df).join(pl_df).reset_index()
    return df_out


def extract_link_feats_immediate_neighbors(df, feat, sid_col = "STDNT_ID", course_col = "COURSE", feat_bins = [0.0, 1.0, 2.0, 3.0, 4.0, 100.0]):
    col_stems = pd.cut(df[feat], feat_bins).cat.categories.tolist()
    c_l_nbr_cols = ['CL_BIN_NBR' + x.replace(' ', '') for x in col_stems]
    b_l_nbr_cols = ['BL_BIN_NBR' + x.replace(' ', '') for x in col_stems]
    p_l_nbr_cols = ['PL_BIN_NBR' + x.replace(' ', '') for x in col_stems]
    # get count link feats for immediate neighbors; first get count link feat for each individual using bins
    df.set_index([course_col, sid_col], inplace = True)
    cl_bins = pd.get_dummies(pd.cut(df[feat], feat_bins))
    cl_bins.columns = c_l_nbr_cols
    # sum count link by class, then merge onto original df
    df_cl = df.join(cl_bins).reset_index().groupby(course_col)[c_l_nbr_cols].sum().reset_index()
    df_out = df.reset_index().merge(df_cl, how = 'left', on = course_col)
    # proportion link feats
    import ipdb;ipdb.set_trace()
    df_pl = generate_proportion_link_df(df_cl, p_l_nbr_cols) #note: this produces NaN for any course with only one student
    df_out = df_out.reset_index().merge(df_pl, how='left', on=course_col)
    # get binary link feats
    bl_bins = df_cl.set_index(course_col).applymap(lambda x: 1 if x > 0 else 0)
    bl_bins.columns = b_l_nbr_cols
    df_out = df_out.merge(bl_bins.reset_index(), how = 'left', on = course_col)
    # get mean-link; this is just average gpa within course
    df_ml = df.reset_index().groupby(course_col)[feat].mean().reset_index().rename(columns = {'PREV_TERM_CUM_GPA' : 'ML_NBR_GPA'})
    df_out = df_out.merge(df_ml, how = 'left', on = course_col)
    return df_out