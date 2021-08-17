package require json

# Save the end of message marker in a variable

set end_of_cmd [format "%c" 4]

# Procedure to send the string to ACT-R with the end of message character.

proc send_string {s string} {

  global end_of_cmd
  puts $s "$string$end_of_cmd"
} 

# Procedure to read a message from the socket and return the parsed JSON result
# (which will be a dictionary for the object).

proc read_string {s} {
 
  global end_of_cmd

  set d [read $s 1]
  set m ""

  set end [string first $end_of_cmd $d]

  while { $d != $end_of_cmd } {

    set m "$m$d"
    set d [read $s 1]
  }

  return [::json::json2dict $m]
}


# Read the ACT-R address and port from the default configuration files.

if {[file readable "~/act-r-address.txt"]} {
  set fileid [open "~/act-r-address.txt" "r"]
  set address [read $fileid]
  close $fileid
}

if {[file readable "~/act-r-port-num.txt"]} {
  set fileid [open "~/act-r-port-num.txt" "r"]
  set port [read $fileid]
  close $fileid
}

# Open the socket.

set actr_socket [socket $address $port]
fconfigure $actr_socket -blocking no -buffering none

# Send the command to set the name for this connection.

send_string $actr_socket "{\"method\":\"set-name\",\"params\":\[\"Simple Tcl/Tk Example\"\],\"id\":null}"

# Evaluate the ACT-R "act-r-version" command.

send_string $actr_socket "{\"method\":\"evaluate\",\"params\":\[\"act-r-version\"\],\"id\":1}"

# Read the response and parse the result out of it.

set m [read_string $actr_socket]

if {[dict exists $m "result"] && [dict exists $m "id"] && [dict exists $m "error"]} {
  set id [dict get $m "id"]
  set error [dict get $m "error"]
  set result [dict get $m "result"]

  if {$id == 1} {
    if {$error == "null"} {
      puts "ACT-R version: $result"
      set response "ACT-R version: $result"
    } else {
      puts "Error returned: $error"
    }
  } else {
    puts "Id $id is not expected value 1"
  }
} else {
  puts "Unexpected message $m"
}

flush stdout

# Using puts and flush aren't actually showing in the console 
# when sourced on my machine, so using a dialog box as well.
# The error and unexpected messages above won't happen so just
# show the valid case.

tk_messageBox -message $response -type ok

# Add a command called "tcl-add" which will be referenced by "add" locally.

send_string $actr_socket "{\"method\":\"add\",\"params\":\[\"tcl-add\",\"add\",\"Add the two numbers provided. Params: num1 num2\"\],\"id\":null}"

# Loop forever reading messages and responding to the
# evaluate requests for "add".

while 1 {
  set m [read_string $actr_socket]
 
  if {[dict exists $m "method"] && [dict exists $m "params"] && [dict exists $m "id"]} {
    set params [dict get $m "params"]
    if {[dict get $m "method"] == "evaluate" && [lindex $params 0] == "add"} {
      set err ""

      if { [llength $params] == 4 } {
        if [catch {set sum [expr [lindex $params 2] + [lindex $params 3]]}] {
          set err "Error when trying to add [lindex $params 2] and [lindex $params 3]"
        }
      } else {
        set err "Did not provide exactly 2 values to add"
      }

      # Generally would need additional parsing to convert id back to json but
      # for simplicity just using it directly because ACT-R currently sends numeric ids

      set id [dict get $m "id"]
      if { $id != "null"}  {
        if {$err != ""} {
          send_string $actr_socket "{\"result\":null,\"error\":{\"message\": \"$err\"},\"id\":$id}"
        } else {
          send_string $actr_socket "{\"result\":$sum,\"error\":null,\"id\":$id}"
        }
      } 
    } else {
      puts "Got invalid request $m which wasn't evaluate add"
    }
  } else {
    puts "Got invalid request $m"
  }
}
        
    

  