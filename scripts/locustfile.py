# Trivial script to use locust -*- Python -*-

from locust import HttpLocust, TaskSet

def alice_pays_bob(self):
  self.client.post('/api/payment',None,{"sender": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69","recipient": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce","amount": "0x1"})


def bob_pays_alice(self):
  self.client.post('/api/payment',None,{"sender": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce","recipient": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69","amount": "0x1"})


def sleep_request(self):
  self.client.post('/api/sleep',None,{})


class AlicePaysBobTask(TaskSet):
  tasks = {alice_pays_bob : 5000}


class BobPaysAliceTask(TaskSet):
  tasks = {bob_pays_alice : 5000}


class SleepRequest(TaskSet):
  tasks = {sleep_request : 5000}


class AliceUser(HttpLocust):
  task_set = AlicePaysBobTask
  min_wait = 50
  max_wait = 500


class BobUser(HttpLocust):
  task_set = BobPaysAliceTask
  min_wait = 50
  max_wait = 500


#class Sleep(HttpLocust):
#  task_set = SleepRequest
#  min_wait = 50
#  max_wait = 1000
