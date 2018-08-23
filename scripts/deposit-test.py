import requests, json, time
deposit_data = {  "address": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce",
                   "amount": 1000 }
r = requests.post('http://localhost:8081/api/deposit', json=deposit_data)

delay = 0.001
while True:
    time.sleep(0.1)
    s = requests.get('http://localhost:8081/' + r.json()['result'])
    delay *= 2
    if s.json() != {'result': 'The operation is pending'}:
        break
    print('still pending')
print(s.json())
