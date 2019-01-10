import requests, json, time
deposit_data = {  "address": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69",
                   "amount": "0x1000000000" }

url = 'http://ec2-52-203-232-2.compute-1.amazonaws.com:8081/api/'
r = requests.post(url + 'deposit', json=deposit_data)

delay = 0.001
while True:
    time.sleep(0.1)
    s = requests.get(url + 'thread?id=%s' % r.json()['result']['thread'])
    delay *= 2
    if s.json() != {'result': 'The operation is pending'}:
        break
    print('still pending')
print(s.json())
