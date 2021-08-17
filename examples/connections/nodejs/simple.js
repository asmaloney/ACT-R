var net = require('net')
var fs = require('fs')

// Get the user's home directory which hopefully is in one of these
// and matches the one ACT-R uses.

var home = process.env.HOME || process.env.USERPROFILE || process.env.HOMEPATH 

var address = fs.readFileSync(home + "/act-r-address.txt",'utf8')
var port = fs.readFileSync(home + "/act-r-port-num.txt",'utf8')

var client = new net.Socket()
client.connect(port,address)

// Handle all the received communication in the data
// function.  Probably not the best way.  Also, I'm sure
// there's some simple object based way to create the
// return results to write, but I don't really know
// javascript.  Since it works and this is only for
// example purposes I'm calling it good enough. :)

client.on('data',function(data) {
  let end = -1;
  let start = 0;
          
  // Assume we get whole messages for the example, but
  // something with large data values may be split and
  // need some way to handle that.

  while ((end = data.indexOf("\u0004", start)) != -1) {
    let m = JSON.parse(data.slice(start,end));
          
    if (m.result) { // When there's a result it must be in response to an evaluate request
      if (m.id == 1) { // Does it have the id we sent? 
        console.log('ACT-R version: ' + m.result[0]);
      } else {
        console.log('Result for wrong id returned');
      }
    }
    if (m.error) { // If it has an error that was in response to an evaluate request
      console.log('Error reported in message: ' + data.slice(start,end));
    }
    if (m.method && m.params) { // The only messages clients get with a method and params
      if (m.method == "evaluate") { // are to evaluate but check to be sure.
        let e = "";
        let r = "";
        if (m.params[0] == "add") { // is it the add command we created?
          if (m.params.length == 4) { 

            // should be some error handling around this...
            r = m.params[2] + m.params[3];
          } else {
            e = "Wrong number of parameters";
          }
        } else {
          e = "Wrong method to evaluate";
        }
        if (m.id) { // if there was an id return the result
          if (e == "") { // no error means send the result
            client.write("{\"result\":" + JSON.stringify(r) + ",\"error\":null,\"id\":" + JSON.stringify(m.id) + "}\u0004");
          } else { // send the error
            client.write("{\result\":null,\"error\":" + e + ",\"id\":" + JSON.stringify(m.id) + "}\u0004");
          }
        }
      } else { // shouldn't happen 
        console.log('Invalid message: ' + data.slice(start,end));
      }
    }
    start = end + 1;
  }
});

// Set the name for this client

client.write("{\"method\":\"set-name\",\"params\":[\"Node.js simple connect\"],\"id\":null}\u0004")

// Evaluate the ACT-R "act-r-version" command, the result will be printed by the function above.

client.write("{\"method\":\"evaluate\",\"params\":[\"act-r-version\"],\"id\":1}\u0004")

// Add a new command called "node-js-add" which will be referred to as "add" locally.

client.write("{\"method\":\"add\",\"params\":\[\"node-js-add\",\"add\",\"Add the two numbers provided. Params: num1 num2\"\],\"id\":null}\u0004")


