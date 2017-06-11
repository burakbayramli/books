#!/usr/bin/env python
# encoding: utf-8
"""
geocoder.py

Created by Maksim Tsvetovat on 2011-12-12.
Copyright (c) 2011 __MyCompanyName__. All rights reserved.
"""

#!/usr/bin/env python
# encoding: utf-8
"""
geocoder2.py

Created by Maksim Tsvetovat on 2011-08-11.
Copyright (c) 2011 __MyCompanyName__. All rights reserved.
"""

import sys
import os
import Yahoo as yy_module
import geopy
import bsddb
import json
import unicodedata
import urllib
import logging
l=logging.getLogger("GEOCODER")


class geocoder(object):
    
    def __init__(self):
        yahoo_api_key='dj0yJmk9Qm9UQVljSEVMUlpRJmQ9WVdrOWNscEVPRGcwTldrbWNHbzlNVGMzTVRBeE56QTJNZy0tJnM9Y29uc3VtZXJzZWNyZXQmeD03MQ--'
        yahoo_secret='eeae1d9c40a5a5eec7aea5505c90c0115b799e9f'
        self.yy=yy_module.Yahoo(yahoo_api_key)
        self.db = bsddb.btopen('/tmp/location_cache.db', 'c')
        l.info('Geocoder initialized')

    def geocode(self,js):
        location=""
        
        ## first try to get the coordinates from a mobile device
        try:        
            geo=str(js['geo'])
            if geo and geo != 'None':
                l.debug(">>>> Geo "+geo)
                return(self._parse_geo(geo))
        except KeyError:
            pass
        
        try:
            location=js['user']['location']
            if location != None and location != '':
                return (self._geocode_loc(location))
        except KeyError:
            return None


    def _parse_geo(self,geo):
        """parse the Twitter geo string
        {u'type': u'Point', u'coordinates': [40.14117668, -74.8490068]}
        """
        l.debug(geo + str(type(geo)))
        try :
            if type(geo)==str: 
                geo=json.loads(geo)
            
            if geo['type']=='Point':
                lat=float(geo['coordinates'][0])
                lat=float(geo['coordinates'][1])
                return self._reverse_geocode(lat,lon)
        except:
            return None

    def _parse_point(self, str_pt):
        lat,lon=str_pt.split()
        return float(lat), float(lon)

    def _parse_ut(self, loc):
        """Parse UberTwitter location Strings -- e.g. ÜT: 40.708612,-73.92678"""
        lat, lon = loc.split()[1].split(',')
        return self._reverse_geocode(lat,lon)

    def _parse_iphone(self, loc):
        """Parse iPhone location strings"""
        lat, lon = loc.split()[1].split(',')
        return self._reverse_geocode(lat,lon)         
            

    def _reverse_geocode(self,lat,lon):
        try :
            place=yy.reverse(float(lat),float(lon))
            return place
        except:
            l.debug('reverse geo FAIL')
            return None

    def _geocode_loc(self,loc):
        
        loc_str= loc.encode('utf-8') #self.toAscii(loc)
        
        if loc_str == None or loc_str is '': 
            return None
        elif loc_str.startswith('ÜT:'):
            return self._parse_ut(loc_str)
        elif loc_str.startswith('iPhone'):
            return self._parse_iphone(loc_str)
        
        ## generate a hash-key for caching
        key=str(hash(loc_str))    
        
        ## check if we have already cached this data
        if self.db.has_key(key):
            l.debug("@<<<< read"+loc+" "+key)
            return json.loads(self.db[key])
        
        ## GEOCODE!!!    
        place=''
        if loc != '':
            try :
                place=self.yy.geocode(loc)
            except UnicodeEncodeError:
                return None
            
        ## Check if geocoding went OK, otherwise return None
        if place != '':
            l.debug("@ write >>>>"+loc+" "+key)
            self.db[key]=json.dumps(place)
            return place
        else:
            return None
        

        
