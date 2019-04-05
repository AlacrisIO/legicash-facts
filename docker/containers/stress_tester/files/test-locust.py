from locust import HttpLocust, TaskSet

def alicepaysbob(self):
    self.client.post('/api/payment',None,{"sender": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69","recipient": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce","amount": "0x100"})

def bobpaysalice(self):
    self.client.post('/api/payment',None,{"sender": "0xc54e86dffb87b9736e2e35dd85c775358f1c31ce","recipient": "0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69","amount": "0x100"})


class AlicePaysBobTask(TaskSet):
    tasks = {alicepaysbob : 1}

class BobPaysAliceTask(TaskSet):
    tasks = {bobpaysalice : 1}

class AliceUser(HttpLocust):
    task_set = AlicePaysBobTask
    min_wait = 50
    max_wait = 500

class BobUser(HttpLocust):
    task_set = BobPaysAliceTask
    min_wait = 50
    max_wait = 500
