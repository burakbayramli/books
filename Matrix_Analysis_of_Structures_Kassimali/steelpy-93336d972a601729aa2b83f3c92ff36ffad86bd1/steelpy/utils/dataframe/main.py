# Copyright (c) 2023 steelpy
from __future__ import annotations
#
# Python stdlib imports

#
# package imports
#from .dfsql import DataFrameSQL
#from .dfmem import DataFrameBasic
import numpy as np
#
#
class DBframework:
    __slots__ = ['_df', '_db_name', '_pd']

    def __init__(self):
        """
        """
        # FIXME : allow for more modules
        try:
            import pandas as pd
            self._df = pd
        except ModuleNotFoundError:
            #self._df = DataFrameBasic
            raise ModuleNotFoundError("install Pandas")
    #
    #def __call__(self, data: list | dict,
    #             columns: None | list = None,
    #             index: None | list = None):
    #    """
    #    :param data:
    #    :param columns:
    #    :param index:
    #    """
    #    #if self._db_name:
    #    #    return self._df(data, columns, index,
    #    #                    db_name=self._db_name)
    #    #else:
    #    self._df = self._pd(data, columns=columns, index=index)
    #    return self._df
    #    #return self._pd(data, columns, index)
    #
    #
    @property
    def DataFrame(self):
        """ return selected module dataframe"""
        return self._df.DataFrame
    #
    @property
    def nan(self):
        """ """
        return np.nan
    #
    @property
    def concat(self):
        return self._df.concat
    #
    @property
    def read_sql_query(self):
        """ """
        return self._df.read_sql_query
    #
    #
    @property
    def ExcelWriter(self):
        """ """
        return self._df.ExcelWriter
    #
    #
    #@property
    #def df(self):
    #    """ """
    #    return self._df
    #
    #@df.setter
    #def df(self, data: list | dict,
    #       columns: None | list = None,
    #       index: None | list = None):
    #    """ """
    #    self._df = self.__call__(data, columns, index)
    #
    #
    def tabulate(self, df=None, headers=None, tablefmt: str = 'psql'):
        """
        df : The source dataframe
        headers : To denote that the keys of the dataframe needs to be used as table headrs
        tablefmt : To denote the dataframe needs to be printed as psql format
        """
        if not isinstance(df, DataFrame):
            df = self._df
        #
        df_columns = df.columns.tolist()
        max_len_in_lst = lambda lst: len(sorted(lst, reverse=True, key=len)[0])
        align_center = lambda st, sz: "{0}{1}{0}".format(" " * (1 + (sz - len(st)) // 2), st)[:sz] if len(st) < sz else st
        align_right = lambda st, sz: "{0}{1} ".format(" " * (sz - len(st) - 1), st) if len(st) < sz else st
        max_col_len = max_len_in_lst(df_columns)
        max_val_len_for_col = dict([(col, max_len_in_lst(df.iloc[:, idx].astype('str')))
                                    for idx, col in enumerate(df_columns)])
        col_sizes = dict([(col, 2 + max(max_val_len_for_col.get(col, 0), max_col_len))
                          for col in df_columns])
        build_hline = lambda row: '+'.join(['-' * col_sizes[col]
                                            for col in row]).join(['+', '+'])
        build_data = lambda row, align: "|".join([align(str(val), col_sizes[df_columns[idx]])
                                                  for idx, val in enumerate(row)]).join(['|', '|'])
        hline = build_hline(df_columns)
        out = [hline, build_data(df_columns, align_center), hline]
        for _, row in df.iterrows():
            out.append(build_data(row.tolist(), align_right))
        out.append(hline)
        return "\n".join(out)

    #
#
