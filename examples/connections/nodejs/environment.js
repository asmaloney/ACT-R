// Create a webserver on port 4000 of the machine
// and provide two pages as examples using websockets
// provided by socket.io to update the information.

// The default page is a set of tools similar to the
// ACT-R Environment.  This server is basically just
// a relay for information between the js code on
// that page and ACT-R.

// The page /expwindow.html will display the contents
// of the ACT-R experiment windows.  All the experiment
// window updates are handled by this code and then sent
// to all of the expwindow connections, and any action 
// on any connected page is passed along to ACT-R.


var app = require('express')();
var http = require('http').Server(app);
var io = require('socket.io')(http);

// This seems to be specific to some version(s) of socket.io
// and probably doesn't help with anything anyway so not 
// doing it now to help with getting things working through
// mybinder and docker containers
// io.set('origins', '*:*');

var expio = io.of('/expwindow');
var envio = io.of('/environment');

// the main page

app.get('/', function(req, res) {
  res.sendFile(__dirname + '/environment.html');
});


// the visible virtual viewer

app.get('/expwindow.html', function(req,res) {
  res.sendFile(__dirname + '/expwindow.html');
});

// Start the http server on port 4000.

http.listen(4000);


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


console.log("ACT-R HTML Environment and Experiment Window viewer ready.");

// Record of the window information for the visible virtual windows

windows = {}

// count how many exp window viewers there are and return false
// for display if there aren't any

var expcount = 0;



// Map of id values to client connections

var idtoclientMap = new Map;

// Next id number to use in a request

var idnum = 0;

// count the environment connections so that
// they can be passed unique names that they
// can use for creating commands and such.

var envcount = 0;

// record the actions sent for reference on
// an error

var sentactionMap = new Map;

// record the added commands so we know who
// to send the evaluates to

var actionMap = new Map;


// record the pending evaluates so that error
// results can be returned if the socket disconnects

var pendingMap = new Map;


// keep track of the internal id (unique) to external id (may not be unique)
// mapping for actions

var idtoidMap = new Map;

// record the actions by sender

var sendertoactionsMap = new Map;

// keep a socekt to number mapping and handle all unique instance name creation
// server side

var socketnumMap = new Map;


var cmdMap = new Map;

// Just handle all the received communication in the data
// function.  Probably not the best way...  

client.on('error',function(error) {
  console.log(error.message);
  throw new Error("lost contact with ACT-R so must exit.");
})


client.on('end',function() {
  throw new Error("lost contact with ACT-R so must exit.");
})

var partial_data = "";

client.on('data',function(data) {
  let end = -1;
  let start = 0;

  // do we have a message to parse yet
  if (data.indexOf("\u0004", start) == -1) {
    partial_data = partial_data + data;
  } else {

    data = partial_data + data;

    while ((end = data.indexOf("\u0004", start)) != -1) {
    
      var m;

      try {
         m = JSON.parse(data.slice(start,end));
      }
      catch(err) {
        console.log(err.message);
        console.log("while processing incoming data data =" + data);
      }
    
      if (m && m.result) { 

        // send the result to the originator
        var sender = idtoclientMap.get(m.id);

      if (sender) {

        idtoidMap.get(m.id)(true,m.result);
        idtoclientMap.delete(m.id);
      }
    
      idtoidMap.delete(m.id);
      sentactionMap.delete(m.id);

    }
    if (m.error) { 

      // send the error message to the originator

      var sender = idtoclientMap.get(m.id);

      if (sender) {
        idtoidMap.get(m.id)(false,m.error.message);

        idtoclientMap.delete(m.id);
      }
    
      console.log("Error result: " + m.error.message + "for sending action: " + sentactionMap.get(m.id));

      idtoidMap.delete(m.id);
      sentactionMap.delete(m.id);
    }

    var response = "true";

    if (m.method && m.params) { // A request to evaluate a command
      if (m.method == "evaluate") { // to be safe check and make sure 
        if (m.params[0] == "vv") { // it's the exp window command handled here
          
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

                // send the details to all the connected viewers
     
                expio.emit("add",[name,"text",p[3]+w["x1"],p[4]+w["y1"],p[5],p[6],p[7]]);
              } 
            break;

            case "line":

              w = windows[p[1]];
              if (w != undefined) {

                name = p[1] + "_name_" + p[2];
                w["items"][name] = true;
                expio.emit("add",[name,"line",p[3]+w["x1"],p[4]+w["y1"],p[5]+w["x1"],p[6]+w["y1"],p[7]]);
              } 
            break;

            case "button":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_name_" + p[2];
                w["items"][name] = true;
                expio.emit("add",[name,"button",p[3]+w["x1"],p[4]+w["y1"],p[5],p[6],p[7],p[8]]);
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
                expio.emit("attend",[name,"attend",p[3],p[4],p[5]]);
              } 
            break;

            case "cursor":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_cursor_" + p[2];
                w["items"][name] = true;
                expio.emit("attend",[name,"cursor",p[3]+w["x1"],p[4]+w["y1"],p[5]]);
              } 
            break;

            case "remove" :

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_name_" + p[2];
                delete w["items"][name];
                expio.emit("remove",name);
              }
            break;

            case "clearcursor":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_cursor_" + p[2];
                delete w["items"][name];
                expio.emit("remove",name);
              }
            break;

            case "clearattention":

              w = windows[p[1]];
              if (w != undefined) {
                name = p[1] + "_attn_" + p[2];
                delete w["items"][name];
                expio.emit("remove",name);
              }
            break;

            case "close" :

              w = windows[p[1]];
              if (w != undefined) {
                // send the remove for everything 
                for (var i in w["items"]) {
                  expio.emit("remove",i);
                }
                w["items"] = {};
              }
      
              // get rid of the window 
              delete windows[p[1]]
            
            break;

            case "check" :
            
              // If there aren't any open connections
              // report that we aren't displaying things.

              if (expcount == 0) {
                response = "false";
              }
            break;
          }

          if (m.id) { // if there was an id just return the current response

            client.write("{\"result\": [" + response + "],\"error\":null,\"id\":" + JSON.stringify(m.id) + "}\u0004");
          }
        
        } else { // it's someone else's so figure out who and send it along


          var sender = actionMap.get(m.params[0]);

          if (sender) {

            if (m.id) { // record 
              
            idtoidMap.set(idnum,m.id);

              if (pendingMap.get(sender)) {
                pendingMap.get(sender).set(idnum, true);
              } else {
                var nm = new Map;
                nm.set(idnum,true);
                pendingMap.set(sender,nm);
              }
            }  

            cmd = m.params.shift();
            
            client_cmd = cmdMap.get(cmd);
            
            m.params.unshift(client_cmd);

            sender.emit("evaluate",m.params,idnum);
            idnum++;

          } else { // sender is gone so return error
            if (m.id) {
              client.write("{\"result\": null,\"error\":{\"message\": \"Error with evaluating " + JSON.stringify(m.params) + "\"},\"id\":" + JSON.stringify(m.id) + "}\u0004");
            }
          }
        }
      } else { // shouldn't happen 
        console.log('Invalid message: ' + data.slice(start,end));
      }
    }
    start = end + 1;
  }

  if (start < data.length) {
    partial_data = data.slice(start);
  } else {
    partial_data = "";
  }
  }
});

// Set the name for this client

client.write("{\"method\":\"set-name\",\"params\":[\"HTML Environment and Exp. Window viewer\"],\"id\":null}\u0004")

// Add a new command called "node-js-vv-relay" which will be referred to as "vv" locally.

client.write("{\"method\":\"add\",\"params\":\[\"node-js-vv-relay\",\"vv\",\"Virtual window handler for browser based display. Do not call.\"\],\"id\":null}\u0004")

// Evaluate the ACT-R "add-virtual-window-handler" command so that node-js-vv-relay gets called to display visible virtual window items.

client.write("{\"method\":\"evaluate\",\"params\":[\"add-virtual-window-handler\",false,\"node-js-vv-relay\"],\"id\":null}\u0004")



// When something connects to the envwindow namespace create two handlers 
// that will relay the keypresses and mouse clicks back, and count how many
// are connected to determine if there should be a visible display.
// The mouse click is interpreted assuming the windows do
// not overlap to pick which one and translate the global to local
// coordinates.


expio.on('connection',function(socket){
 // console.log('Expwindow connected: ' + socket.conn["remoteAddress"]);

  expcount++;

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

  socket.on('disconnect',function(){

    expcount--;

   // console.log('Expwindow disconnected: ' + socket.conn["remoteAddress"]);
  });


});





function cmd_name(socket,cmd) {
  return "env_client" + socketnumMap.get(socket) + "_" + cmd;
}

envio.on('connection',function(socket){


 // console.log('Environment connected: ' + socket.conn["remoteAddress"]);

  envcount++;

  socketnumMap.set(socket,envcount);

  socket.on('disconnect',function(){
  
    socketnumMap.delete(socket);


    if (sendertoactionsMap.get(socket)) {
   
      for(var a of sendertoactionsMap.get(socket).keys()) {
        actionMap.delete(a);
        client.write("{\"method\": \"remove\", \"params\":[\"" + a + "\"],\"id\": 123 }\u0004");
      }
      sendertoactionsMap.delete(socket);
    }

    if (pendingMap.get(socket)) {
      for (var id of pendingMap.get(socket).keys()) {
        var realid = idtoidMap.get(id);

        if (realid) {

          var act = sentactionMap.get(id);
          sentactionMap.delete(id);

          client.write("{\"result\": null,\"error\":{\"message\": \"Error with evaluating " + JSON.stringify(act) + "\"},\"id\":" + JSON.stringify(realid) + "}\u0004");
        }
      }
      pendingMap.delete(socket);
    }
 
   // console.log('Environment disconnected: ' + socket.conn["remoteAddress"]);
  });

  // pass along an add and record the owner

  socket.on('add',function( client_cmd, docs, callback){

    var cmd = cmd_name(socket,client_cmd);

    cmdMap.set(cmd,client_cmd);

    actionMap.set(cmd, socket);

    if (sendertoactionsMap.get(socket)) {
      sendertoactionsMap.get(socket).set(cmd,true);
    } else {
      var cmd_map = new Map;
      cmd_map.set(cmd,true);
      sendertoactionsMap.set(socket,cmd_map);
    }

    if (callback) {

      idtoclientMap.set(idnum,socket);

      idtoidMap.set(idnum,callback);

      sentactionMap.set(idnum,"add "+client_cmd);

      client.write("{\"method\":\"add\",\"params\":[\"" + cmd + "\", \"" + cmd + "\", \"" + docs + "\"],\"id\":" + idnum + "}\u0004");

      idnum++;

    } else {
   
      client.write("{\"method\":\"add\",\"params\":[\"" + cmd + "\", \"" + cmd + "\", \"" + docs + "\"],\"id\":null}\u0004");
    }
    
    
  });

  socket.on('remove',function(client_cmd){

    var cmd = cmd_name(socket,client_cmd);

    cmdMap.delete(cmd);

    if (actionMap.get(cmd)) {
      actionMap.delete(cmd);
      sendertoactionsMap(socket).delete(cmd);
   
      client.write("{\"method\":\"remove\",\"params\":[\"" + cmd + "\"],\"id\":null}\u0004");
    }
  });

  socket.on('monitor',function(remote,client_cmd){

    var cmd = cmd_name(socket,client_cmd);

    client.write("{\"method\":\"monitor\",\"params\":[\"" + remote + "\", \"" + cmd + "\"],\"id\":null}\u0004");

  });

  socket.on('remove-monitor',function(remote,client_cmd){

    var cmd = cmd_name(socket,client_cmd);

    client.write("{\"method\":\"remove-monitor\",\"params\":[\"" + remote + "\", \"" + cmd + "\"],\"id\":null}\u0004");

  });

  socket.on('result',function(result,id){

    realid = idtoidMap.get(id);

    idtoidMap.delete(id);

    if (realid) {
      client.write("{\"result\": [" + JSON.stringify(result) + " ],\"error\":null,\"id\":" + JSON.stringify(realid) +"}\u0004");
    }

    if (pendingMap.get(socket)) {
    
      pendingMap.get(socket).delete(id);
    }  

    sentactionMap.delete(id);
  });

  socket.on('evaluate',function(params,callback){

    if (callback) {

      idtoclientMap.set(idnum,socket);

      idtoidMap.set(idnum,callback);

      sentactionMap.set(idnum,params);

      client.write("{\"method\":\"evaluate\",\"params\":" + JSON.stringify(params) + ",\"id\":" + idnum + "}\u0004");

      idnum++;

    } else {
      client.write("{\"method\":\"evaluate\",\"params\":" + JSON.stringify(params) + ",\"id\":null}\u0004");
    }

  });

  socket.on('check',function(cmd,callback){

    if (callback) {

      idtoclientMap.set(idnum,socket);

      idtoidMap.set(idnum,callback);

      sentactionMap.set(idnum,"check " + cmd);

      client.write("{\"method\":\"check\",\"params\": [\"" + cmd + "\"],\"id\":" + idnum + "}\u0004");

      idnum++;

    } else {
      client.write("{\"method\":\"check\",\"params\": [\"" + cmd + "\"],\"id\":null}\u0004");
    }

  });

  socket.on('cmds',function(callback){

    if (callback) {

      idtoclientMap.set(idnum,socket);

      idtoidMap.set(idnum,callback);

      sentactionMap.set(idnum,"list-commands");

      client.write("{\"method\":\"list-commands\",\"params\": false ,\"id\":" + idnum + "}\u0004");

      idnum++;

    } else {
      client.write("{\"method\":\"list-commands\",\"params\": false ,\"id\":null}\u0004");
    }

  });
  
  socket.on('connections',function(callback){

    if (callback) {

      idtoclientMap.set(idnum,socket);

      idtoidMap.set(idnum,callback);

      sentactionMap.set(idnum,"list-connections");

      client.write("{\"method\":\"list-connections\",\"params\": false ,\"id\":" + idnum + "}\u0004");

      idnum++;

    } else {
      client.write("{\"method\":\"list-connections\",\"params\": false ,\"id\":null}\u0004");
    }
  });  

});

