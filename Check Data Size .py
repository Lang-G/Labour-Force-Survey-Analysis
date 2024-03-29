# -*- coding: utf-8 -*-
"""
Created on Thu Mar 21 23:22:30 2024

@author: wutau
"""

import requests
BASE_URL = "https://opendata.nhsbsa.net"

INITIAL_URL = "/api/3/action/datastore_search?resource_id=PCA_"

LIMIT=100

def get_all(Period) -> list:
    result = []
    resp = requests.get(f"{BASE_URL}{INITIAL_URL}{Period}&limit={LIMIT}")
    js = resp.json()["result"]
    result.extend(js["records"])
    while "_links" in js and "next" in js["_links"]:
        resp = requests.get(BASE_URL + js["_links"]["next"])
        js = resp.json()["result"]
        result.extend(js["records"])
        print(js["_links"]["next"]) # just so you know it's actually doing stuff
        if len(js["records"]) < LIMIT:
            # if it returned less records than the limit, the end has been reached
            break
    return result

print(len(get_all("202401")))

from pprint import pprint
pprint(requests.get(BASE_URL+INITIAL_URL+Period+"&limit=1").json()["result"])