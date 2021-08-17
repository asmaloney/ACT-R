
source "json.tcl"

set environment_socket ""
set incomplete_cmd ""
set end_of_cmd [format "%c" 4]
set id_num 0
set cmd_table [dict create]

proc replace_quotes {i} {
  set start 0
  set index [string first "\"" $i $start]
  while {$index != -1} {
    set i [string replace $i $index $index "'"]
    set start $index
    set index [string first "\"" $i $start]
  }

  return $i
}
      

proc item2json {i} {
  if {[string is double -strict $i]} {
    if {[string index [string trimleft $i] 0] == "."} {
      return "0[string trimleft $i]"
    } else {
      return $i
    }
  } elseif {[regexp {^(?:true|false|null)$} $i]} {
    return "$i"
  } else {
    return "\"[replace_quotes $i]\""
  }
}

proc list2json {l} {

  set json ""

  if {$l == "null"} {
    return "null"
  } else {

    foreach i $l  {
      if {[string is double -strict $i]} {
        if {[string index [string trimleft $i] 0] == "."} {
          append json "0[string trimleft $i],"
        } else {
          append json "$i,"
        }
      } elseif {[regexp {^(?:true|false|null)$} $i]} {
        append json "$i,"
      } else {
        append json "\"[replace_quotes $i]\","
      }
    }

    if {$json == ""} {
      return "null" 
    } else {
      return [format "\[%s\]" [string trimright $json ","]]
    }
  }
}

proc list2json_strings {l} {

  set json ""

  if {$l == "null"} {
    return "null"
  } else {

    foreach i $l  {
      if {[regexp {^(?:true|false|null)$} $i]} {
        append json "$i,"
      } else {
        append json "\"[replace_quotes $i]\","
      }
    }

    if {$json == ""} {
      return "null" 
    } else {
      return [format "\[%s\]" [string trimright $json ","]]
    }
  }
}


proc collect_incoming {s} {
 
  global incomplete_cmd
  global end_of_cmd
  global up_and_running

  if [eof $s] {
    close $s
    
    global environment_socket
    set environment_socket ""
    
    if {$up_and_running == 1} {
      tk_messageBox -title "ACT-R connection lost" -message "The environment has lost contact with ACT-R and will quit." -icon warning -type ok
    } else {
      tk_messageBox -title "ACT-R connection error" -message "There was a problem starting the environment and it must exit." -icon warning -type ok
    }
    clean_exit
  } else {

  set d [read $s]

  set end [string first $end_of_cmd $d]

  while { $end != -1 } {

    set previous [string range $d 0 [expr $end - 1]]
    set d [string range $d [expr $end + 1] end]
    
    set complete "$incomplete_cmd$previous"

    set incomplete_cmd ""

    process_cmd $complete

    set end [string first $end_of_cmd $d]
  }

  set incomplete_cmd "$incomplete_cmd$d"
}
}

proc process_cmd {cmd} {

  set d [::json::json2dict $cmd]
  if [dict exists $d "result"] {
    set id [dict get $d "id"]
    set error [dict get $d "error"]
    set result [dict get $d "result"]
    
    global $id
   
    if {$error == "" || $error == "null" || $error == "false"} {
        set $id [list 1 $result]
    } else {
        set $id [list 0 [dict get $error "message"]]
    }
  } elseif {[dict exists $d "method"] && [dict get $d "method"] == "evaluate"} {
  
    # assume that it's a valid cmd on this side since it had to be added...

    set params [dict get $d "params"]

    set res [catch $params ret]

    if {$res == 0} {
      transmit_success $ret [dict get $d "id"]
    } else {
      transmit_error $ret [dict get $d "id"]
    }
    
  } else {
    puts "Invalid message received from server: $cmd"
  }
}


proc add_cmd {cmd_name proc_name {documentation "null"} {single "null"}} {

  global cmd_table

  if [dict exists $cmd_table $cmd_name] {
    send_cmd "remove" [list $cmd_name]
  }

  dict set cmd_table $cmd_name $proc_name
  send_cmd "add" [list $cmd_name $proc_name $documentation $single]
}


proc add_new_cmd {base_name proc_name {documentation "null"} {single "null"}} {

  set n [new_variable_name $base_name]

  while {[send_cmd "check" $n] != "null"} {
    set n [new_variable_name $base_name]
  }

  add_cmd $n $proc_name $documentation $single

  return $n
}



proc remove_cmd {cmd_name} {

  global cmd_table

  if [dict exists $cmd_table $cmd_name] {
   send_cmd "remove" [list $cmd_name]
   dict unset cmd_table $cmd_name
  }
}


proc transmit_success {value id} {

  global environment_socket
  global end_of_cmd

  if {[string trimleft $value] == ""} {
    puts $environment_socket [format "\{\"result\": \[true\], \"error\": null, \"id\": %s\}$end_of_cmd" [item2json $id]]
  } else {
    puts $environment_socket [format "\{\"result\": %s, \"error\": null, \"id\": %s\}$end_of_cmd" [list2json $value] [item2json $id]]
  }
}


proc transmit_error {error id} {

  global environment_socket
  global end_of_cmd

  if {$error == ""} {

    puts $environment_socket [format "\{\"result\": null, \"error\": \{\"message\": \"Unknown error.\"\}, \"id\": %s\}$end_of_cmd" [item2json $id]]
  } else {
    puts $environment_socket [format "\{\"result\": null, \"error\": \{\"message\": %s\}, \"id\": %s\}$end_of_cmd" [item2json $error] [item2json $id]]
  }
}




proc transmit_cmd {cmd params var} {

  global environment_socket
  global end_of_cmd

  set sent [catch {puts $environment_socket [format "\{\"method\": \"$cmd\", \"params\": %s, \"id\": \"$var\"\}$end_of_cmd" [list2json $params]]}]
  if {$sent != 0} {
    global $var
    set $var "null"
  }
}

proc transmit_cmd_strings {cmd params var} {

  global environment_socket
  global end_of_cmd

  set sent [catch {puts $environment_socket [format "\{\"method\": \"$cmd\", \"params\": %s, \"id\": \"$var\"\}$end_of_cmd" [list2json_strings $params]]}]
  if {$sent != 0} {
    global $var
    set $var "null"
  }
}

proc send_cmd {cmd params {only 0}} {

  global id_num

  set var "var_$id_num"

  global $var

  set $var ""

  incr id_num

  if {$only == 0} {
    after 1 "transmit_cmd $cmd \"$params\" $var"
  } else {
    after 1 "transmit_cmd_strings $cmd \"$params\" $var"
  }
 
  tkwait variable $var

  set res [subst $$var]

  if {[lindex $res 0] == 1} {
    return [lindex $res 1]
  } else {
    report_status "communication error result: [lindex $res 1]"
    return ""
  }
}


proc call_act_r_command {cmd {model "nil"} {params ""}} {
  set result [send_cmd {evaluate} [concat [list $cmd $model] $params]]
  if {$result == "null"} {
    return ""
  } else {
    return $result
  }
}

proc call_act_r_command_string_only {cmd {model "nil"} {params ""}} {
  set result [send_cmd {evaluate} [concat [list $cmd $model] $params] 1]
  if {$result == "null"} {
    return ""
  } else {
    return $result
  }
}


proc send_cmd_with_result {cmd params} {

  global id_num

  set var "var_$id_num"

  global $var

  set $var ""

  incr id_num

  after 1 "transmit_cmd $cmd \"$params\" $var"
 
  tkwait variable $var

  set res [subst $$var]

  return $res
}


proc call_act_r_command_with_error_messages {cmd {model "nil"} {params ""}} {
  return [send_cmd_with_result {evaluate} [concat [list $cmd $model] $params]]
}


# code from wiki.tcl.tk/3015

proc ip:address {} {
    # find out localhost's IP address
    # courtesy David Gravereaux, Heribert Dahms
    set TheServer [socket -server none -myaddr [info hostname] 0]
    set MyIP [lindex [fconfigure $TheServer -sockname] 0]
    close $TheServer
    return $MyIP
}


proc ip:address_local {} {
    # find out localhost's IP address
    # courtesy David Gravereaux, Heribert Dahms
    catch {
      set TheServer [socket -server none -myaddr "localhost" 0]
      set MyIP [lindex [fconfigure $TheServer -sockname] 0]
      close $TheServer
    }
    return $MyIP
}

proc start_connection {{address ""} {port ""}} {


  global environment_socket
  global local_connection
  global actr_address
  global actr_port
  global options_array

  if {$address == ""} {
    if {[file readable "~/act-r-address.txt"]} {
      set fileid [open "~/act-r-address.txt" "r"]
      set address [read $fileid]
      close $fileid
    } else {
    set address $actr_address
    }
  }
  if {$port == ""} {
    if {[file readable "~/act-r-port-num.txt"]} {
      set fileid [open "~/act-r-port-num.txt" "r"]
      set port [read $fileid]
      close $fileid
    } else {
      set port $actr_port
    }
  }

  if {$options_array(use_localhost) == 1 && $address == "127.0.0.1"} {
    set address "localhost"
  }

  set actr_address $address
  set actr_port $port
  
  set environment_socket [socket $address $port]
  fconfigure $environment_socket -blocking no -buffering none
  fileevent $environment_socket readable "collect_incoming $environment_socket"

  send_cmd {set-name} [list "ACT-R Environment"]

  if {$address == "localhost" || $address == [ip:address] || $address == [ip:address_local]} {
    set local_connection 1
  } else {
    set local_connection 0
  }
}

proc update_text_pane {pane text} {
  $pane configure -state normal
  $pane delete 1.0 end  
  if {[string trimleft $text] == ""} {
    $pane configure -state disabled
  } else {
    $pane insert 1.0 $text
  }
}

# assume that the list var is $box.var

proc update_list_box {box vals {select 1} {generate 0}} {
  global $box.var

  if {[string trimleft $vals] == ""} {
    set $box.var ""
    $box selection set 0
  } else {
    set $box.var $vals
    $box selection clear 0 end
  
    if {$select == 1} {
      $box selection set 0
    }
  }
  if {$generate == 1} {
    event generate $box <<ListboxSelect>>
  }
}

# want things to update when 'selected' which
# seems to be a tricky thing to do efficiently
# because a focusin on the toplevel window triggers
# for all the subwindows and setting it for a subwindow
# might not result in it getting called if a different
# subwindow is how it comes into focus.
# My current approach is to add a special tag to the
# toplevel window and call the event during that.
# There's still a gotcha however because at least
# on windows one can close a window that doesn't
# currently have the focus which creates a race
# condition between the focus event and the destroy
# event.  If the destroy completes before all of
# the focus happens it can often lead to errors so
# I'm just going to wrap a catch around it and
# hope it all works out for now.

proc set_update_script {win script} {

  set new_tags [concat [list [new_variable_name "focus_tag"]] [bindtags $win]]
  bindtags $win $new_tags

  bind [lindex $new_tags 0] <FocusIn> "catch {eval {$script}}"
}
    

