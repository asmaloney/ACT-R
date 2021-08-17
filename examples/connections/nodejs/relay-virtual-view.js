// Create a webserver on port 3000 of the machine
// and provide two pages as examples using websockets
// provided by socket.io to update the information.

// The default page is a viewer for the visible virtual
// windows and this code will relay the info to that
// window to display on a canvas item.

// The page /goal.html will display the contents
// of the chunk in the goal buffer everytime the
// update button is pressed.

// For simplicity, this sends all the updates to
// every connected window regardless of what it
// is displaying.

var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

// the visible virtual viewer

app.get('/', function(req, res) {
  res.sendFile(__dirname + '/index.html');
});


// the goal buffer viewer

app.get('/goal.html', function(req,res) {
  res.sendFile(__dirname + '/goal.html');
});

// Start the http server on port 3000.

http.listen(3000);


// Connect to ACT-R

var net = require('net')
var fs = require('fs')

// Get the user's home directory which hopefully is in one of these
// and matches the one ACT-R uses.

var home = process.env.HOME || process.env.USERPROFILE || process.env.HOMEPATH 

var address = fs.readFileSync(home + "/act-r-address.txt",'utf8')
var port = fs.readFileSync(home + "/act-r-port-num.txt",'utf8')

var client = new net.Socket()
client.connect(port,address)


// Record of the window information for the visible virtual windows

windows = {}


// Just handle all the received communication in the data
// function.  Probably not the best way...  

client.on('data',function(data) {
  let end = -1;
  let start = 0;
          
  // Assume we get whole messages.

  while ((end = data.indexOf("\u0004", start)) != -1) {
    let m = JSON.parse(data.slice(start,end));
    
    if (m.result) { // Only response we get is to our printed-buffer-chunk request not bothering to check the id

      // send the result to all connected clients

      io.emit("updategoal",m.result[0]);
    }
    if (m.error) { // shouldn't get any errors, but just incase

      // send the error message as the result to all connected clients

      io.emit("updategoal",m.error.message);
    }

    if (m.method && m.params) { // A request to evaluate our command
      if (m.method == "evaluate") { // to be safe check and make sure 
        if (m.params[0] == "vv") { // it's the vv command we created
          
          // ignore the model parameter which would be params[1]
          // and just use the list of virtual-view info which is
          // the only other parameter.

          p = m.params[2];

          // Virtual view data not actually documented yet other than
          // in the comments of the tools/visible-virtual.lisp file.
          // So you'll need to check there to know what these are.

          switch (p[0]) {
            case "open" :

            // just record the details, will need the main position
            // to add to the item coords since they're window local 
            // and the other corner for determining if a mouse click
            // is within the window.

            windows[p[1]] = {"x1" : p[2], "x2" : p[2] + p[4], "y1" : p[3], "y2" : p[3] + p[5], "items": {}};
            break;

            case "text":

              w = windows[p[1]];
            
              if (w != undefined) { // safety check because we might have missed
                                    // the open if we connected after the window was opened
    
                // create a name for the item which is a combo of the window and the item

                name = p[1] + "_name_" + p[2];

                // record the name for use when the window is closed
 
                w["items"][name] = true;

                // send the details to all the connected pages
     
                io.emit("add",[name,"text",p[3]+w["x1"],p[4]+w["y1"],p[5],p[6],p[7]]);
              } 
            break;

            case "line":

              w = windows[p[1]];
              if (w != undefined) {

                name = p[1] + "_name_" + p[2];
                w["items"][name] = true;
                io.emit("add",[name,"line",p[3]+w["x1"],p[4]+w["y1"],p[5]+w["x1"],p[6]+w["y1"],p[7]]);
              } 
            break;

            case "button":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_name_" + p[2];
                w["items"][name] = true;
                io.emit("add",[name,"button",p[3]+w["x1"],p[4]+w["y1"],p[5],p[6],p[7],p[8]]);
              } 
            break;

            case "attention":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_attn_" + p[2];
                w["items"][name] = true;
                // attention and cursor use a different notification
                // from the normal items so that they can be drawn
                // last and overlap the normal items
                io.emit("attend",[name,"attend",p[3],p[4],p[5]]);
              } 
            break;

            case "cursor":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_cursor_" + p[2];
                w["items"][name] = true;
                io.emit("attend",[name,"cursor",p[3]+w["x1"],p[4]+w["y1"],p[5]]);
              } 
            break;

            case "remove" :

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_name_" + p[2];
                delete w["items"][name];
                io.emit("remove",name);
              }
            break;

            case "clearcursor":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_cursor_" + p[2];
                delete w["items"][name];
                io.emit("remove",name);
              }
            break;

            case "clearattention":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_attn_" + p[2];
                delete w["items"][name];
                io.emit("remove",name);
              }
            break;


            case "close" :

              w = windows[p[1]];
              if (w != undefined) {
                // send the remove for everything 
                for (var i in w["items"]) {
                  io.emit("remove",i);
                }
                w["items"] = {};
              }
      
              // get rid of the window 
              delete windows[p[1]]
            
            break;
          }        
        }

        if (m.id) { // if there was an id just return a true result
            client.write("{\"result\": true,\"error\":null,\"id\":" + JSON.stringify(m.id) + "}\u0004");
        }

      } else { // shouldn't happen 
        console.log('Invalid message: ' + data.slice(start,end));
      }
    }
    start = end + 1;
  }
});

// Set the name for this client

client.write("{\"method\":\"set-name\",\"params\":[\"Node.js AGI virtual viewer and goal chunk server\"],\"id\":null}\u0004")

// Add a new command called "node-js-vv-relay" which will be referred to as "vv" locally.

client.write("{\"method\":\"add\",\"params\":\[\"node-js-vv-relay\",\"vv\",\"Virtual window handler for browser based display. Do not call.\"\],\"id\":null}\u0004")

// Evaluate the ACT-R "add-virtual-window-handler" command so that node-js-vv-relay gets called to display visible virtual window items.

client.write("{\"method\":\"evaluate\",\"params\":[\"add-virtual-window-handler\",false,\"node-js-vv-relay\"],\"id\":null}\u0004")

// When something connects create two handlers that will
// relay the keypresses and mouse clicks back.
// The mouse click is interpreted assuming the windows do
// not overlap to pick which one and translate the global to local
// coordinates.

io.on('connection',function(socket){
  console.log('connected: ' + socket.conn["remoteAddress"]);

  socket.emit('clear',"");

  // when we get a down event that is a keypress
  // and we just create the output-key event with
  // the given key name and a false (nil) model name.
  // Should verify that it's a valid key name for ACT-R, 
  // but not for a simple demo.

  socket.on('down',function(msg){
    client.write("{\"method\":\"evaluate\",\"params\":[\"output-key\",false,false,\""+msg+"\"],\"id\":null}\u0004")
  });

  // when we get a mouse event that's a record of
  // a button click with an x and y position.
  // Loop through the windows that have been opened to
  // find which one it's in (if any) and then send 
  // that to the virtual mouse handler function so it
  // can perform any necessary actions (button clicks, etc)
  // keeping in mind that (at least currently) that handler
  // requirs a window relative position.

  socket.on('mouse',function(msg){
  
    x = msg[0];
    y = msg[1];
    w = -1;

    for (var win in windows) {
      if ( x >= windows[win].x1 && x <= windows[win].x2 && y >= windows[win].y1 && y <= windows[win].y2 ) {
        w = win;
      }
    }

    if (w != -1) {

      x = x - windows[w].x1;
      y = y - windows[w].y1;

      client.write("{\"method\":\"evaluate\",\"params\":[\"visible-virtual-window-mouse-click\",false,\""+win+"\","+x+","+y+"],\"id\":null}\u0004")

    }

  });

  // When we get a getgoal event that means we need to
  // request the goal buffer's chunk to send back.
  // Assumes there's only one model defined because it
  // provides a false (nil) current model value.

  socket.on('getgoal',function(){
    client.write("{\"method\":\"evaluate\",\"params\":[\"printed-buffer-chunk\",false,\"goal\"],\"id\":1}\u0004")
  });


});

