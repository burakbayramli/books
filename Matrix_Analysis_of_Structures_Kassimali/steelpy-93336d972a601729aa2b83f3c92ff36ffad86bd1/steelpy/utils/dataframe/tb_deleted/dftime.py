# Copyright (c) 2022 steelpy
from __future__ import annotations

# Python stdlib imports
from datetime import datetime as dt, timedelta, date
from math import ceil, nan, isnan
from dataclasses import dataclass
#
# package imports
#from dateutil.parser import parse
#from dateutil import relativedelta as rdelta


#
def today_floor(freq:str="D"):
    """ """
    try:
        from pandas import Timestamp
        today = Timestamp.today()
        return today.floor(freq)
    except ModuleNotFoundError:
        today = date.today()
        return date(today.year, today.month, today.day)
        #return dt(today.year, today.month, today.day)
#
def date_range(start, end, freq:str|None=None):
    """
    """
    try:
        from pandas import date_range
        return date_range(start=start, end=end, freq=freq)
                          #inclusive='left')
    except ModuleNotFoundError:
        from dateutil import rrule
        return list(rrule.rrule(freq=rrule.MONTHLY, dtstart=start, until=end))
#
def date_offset(months:int):
    """ """
    try:
        from pandas.tseries.offsets import DateOffset
        return DateOffset(months=months)
    except ModuleNotFoundError:
        from dateutil.relativedelta import relativedelta
        return relativedelta(months=months)
#
def get_next_monday(year, month, day):
    """ """
    date0 = date(year, month, day)
    next_monday = date0 + timedelta(7 - date0.weekday() or 7)    
    try:
        from pandas import Timestamp
        return Timestamp(next_monday)
    except ModuleNotFoundError:
        return dt(next_monday.year, next_monday.month, next_monday.day)
    #return next_monday
#
def get_past_monday(year, month, day):
    """ """
    date0 = date(year, month, day)
    past_monday = date0 + rdelta.relativedelta(days=-1, weekday=rdelta.MO(-1))
    try:
        from pandas import Timestamp
        return Timestamp(past_monday)
    except ModuleNotFoundError:
        return dt(past_monday.year, past_monday.month, past_monday.day)
#
def monday_of_calenderweek(year, week):
    first = date(year, 1, 1)
    base = 1 if first.isocalendar()[1] == 1 else 8
    return first + timedelta(days=base - first.isocalendar()[2] + 7 * (week - 1))
#
def first_dow(year, month, dow:int=0):
    """
    dow: day of the week (Monday is 0, Sunday is 6)
    """
    day = ((8 + dow) - date(year, month, 1).weekday()) % 7
    return date(year, month, day)
#
def get_date_week(week):
    """ """
    r = dt.strptime(week + '-1', "%Y-W%W-%w")
    return r
#
#
def get_week_number(year:int):
    """ """
    # we add 1 to get to the next year !
    next_year_date = dt(year+1, 1, 1)
    # we subtract 4 days only to go to the last day of previous/your_year 
    # this is because of [ISO spec][1]  
    last_day = next_year_date - timedelta(days=4)
    return last_day.isocalendar()[1]
#
def get_week_year(years:list) -> list[str]:
    """get weeks per year"""
    #year_list = [2022, 2023]
    wnumber = [get_week_number(year) for year in years]
    wnumber = [list(range(year, 0, -1)) for year in wnumber]
    wnumber = [f"W{item}_{year}"
               for x, year in enumerate(years) 
               for item in reversed(wnumber[x])]
    return wnumber
#
#
def is_date(string, fuzzy=False) -> bool:
    """
    Return whether the string can be interpreted as a date.

    :param string: str, string to check for date
    :param fuzzy: bool, ignore unknown tokens in string if True
    """
    try: 
        parse(string, fuzzy=fuzzy)
        return True
    except ValueError:
        return False
    except TypeError:
        if isinstance(string, (dt, timedelta)):
            return True
        return False
#
#
def string2number(string: any):
    try:
        if string.isnumeric():
            return int(string)
    except AttributeError:
        pass

    try:
        val = float(string)
        return int(val) if val == int(val) else val
    except (TypeError, ValueError):
        return string
#
#
def get_date(string, fuzzy=False) -> bool:
    """
    Return whether the string can be interpreted as a date.

    :param string: str, string to check for date
    :param fuzzy: bool, ignore unknown tokens in string if True
    """
    try: 
        return parse(string, fuzzy=fuzzy)
    except ValueError:
        if not string:
            return nan
        1/0
    except TypeError:
        if isinstance(string, (dt, timedelta)):
            return string
        #elif not string:
        #    return nan
        elif isnan(string):
            return string
        else:
            1/0
# 
def get_timedelta(string):
    """ """
    if isinstance(string, timedelta):
        return string
    elif string == None:
        return nan
    elif isinstance(string, str):
        return timedelta(weeks=float(string))    
    elif isnan(string):
        return string
    elif isinstance(string, (int, float)):
        return timedelta(weeks=string)
    else:
        1/0    
#
@dataclass
class dtModule:
    __slots__ = [ '_serie', '_name', '_dtype' ]
     
    def __init__(self, data: list[ float ], 
                 name: int|float|str = "",
                 index: None|list = None, 
                 dtype=None) -> None:
        """Construct a new list object"""
        self._serie = data
    
    #def __getitem__(self, index:int|str|list) -> float|int|str:
    #    """Return the ith index of self."""
    #    1/0
    def to_period(self, freq:str):
        """ Converts DatetimeArray/Index to PeriodArray/Index"""
        1/0
    #
    @property
    def start_time(self):
        """ """
        1/0
#