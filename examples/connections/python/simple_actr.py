import json
import socket
import os

# function to send a string to ACT-R with the end of message character

def send(socket,message):
    m = message + chr(4)
    socket.sendall(m.encode('utf-8'))

# Read from the stream until the end of message character and then return
# the value decoded from the JSON string

def receive(socket):
    buffer= ''
    while not chr(4) in buffer:
        data = socket.recv(1)
        buffer += data.decode('utf-8')
    
    return json.loads(buffer[0:-1])


# Read the configuration files for the connection details

with open(os.path.expanduser("~/act-r-port-num.txt"), 'r') as f:
    port = int(f.readline())
    
with open(os.path.expanduser("~/act-r-address.txt"), 'r') as f:
    host = f.readline()

# Open the socket

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect((host, port))

# Set the name for this ACT-R connection

send(sock,'{"method":"set-name","params":["Simple Python Example"],"id":null}')

# Evaluate the "act-r-version" command

send(sock,'{"method":"evaluate","params":["act-r-version"],"id":1}')

# Get the response and parse it make sure it
# is a valid response.  The JSON object decodes
# into a dictionary.

message = receive(sock)

if 'id' in message.keys():
  if message['id'] == 1 :
    if 'result' in message.keys():
      if message['result']:
        print("ACT-R Version is")
        print(message['result'][0])
      elif 'error' in message.keys():
        e = message['error']
        print("error result: "+e['message'])
      else:
        print("Invalid result")
    else:
      print("Invalid result")
  else:
    print("Result with wrong id:")
    print(message['id'])

# Add the command "python-sum" to ACT-R  using "sum" as the name provided.

send(sock,'{"method":"add","params":["python-sum","sum","Sum the values in the provided list. Params: list"],"id":null}')

# Loop forever reading messages from ACT-R and returning
# the sum of items when asked to evaluate the "sum" 
# command.

while True :

  message = receive(sock)

  if 'method' in message.keys() and 'params' in message.keys() and 'id' in message.keys() :
    if message['method'] == 'evaluate':
      if len(message['params']) == 3 and message['params'][0] == "sum":
        result = None
        error = None
        try:
          result = sum(message['params'][2])
        except:
          error = "Error trying to sum values."
      
        if message['id'] :
          if result:
            send(sock,'{"result":[%d],"error":null,"id":%s}'%(result,json.dumps(message['id'])))
          else:
            send(sock,'{"result":null,"error":{"message": "%s"},"id":%s}'%(error,json.dumps(message['id'])))
  else :
    print ("Unexpected message from ACT-R")
