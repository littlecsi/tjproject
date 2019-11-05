# This example uses Python Requests library http://docs.python-requests.org/en/master/
import requests
import json

# Request Parameters
store = "android"       # Could be either "android" or "itunes".
country_code = "US"     # Two letter country code.
date = "2019-11-02"     # Date in YYYY-MM-DD format.

req_params = {"date": date,
              "country": country_code}

# Auth Parameters
username = "189dd47396443c58738b17eb7e2e079ccc00a580"  # Replace {API_KEY} with your own API key.
password = "admin"          # Password can be anything.

# Request URL
request_url = "https://api.appmonsta.com/v1/stores/%s/rankings.json" % store

# This header turns on compression to reduce the bandwidth usage and transfer time.
headers = {'Accept-Encoding': 'deflate, gzip'}

# Python Main Code Sample
response = requests.get(request_url,
                        auth=(username, password),
                        params=req_params,
                        headers=headers,
                        stream=True)

print(response.status_code)
print(json.loads(response.iter_lines(1)))

# for line in response.iter_lines():
#   # Load json object and print it out
#   json_record = json.loads(line)
#   print(json_record)
