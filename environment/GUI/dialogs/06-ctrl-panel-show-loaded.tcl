label [control_panel_name].current_load -text "Current Model" -font label_font

tk_optionMenu [control_panel_name].current_model_name global_current_model_name "No Current Model"

[control_panel_name].current_model_name configure -font label_font

[[control_panel_name].current_model_name cget -menu] configure -font button_font


pack [control_panel_name].current_load
pack [control_panel_name].current_model_name


proc create_current_model {ignore_model name} {
  global global_current_model_name

  set menu [[control_panel_name].current_model_name cget -menu]
  set top [$menu entrycget 0 -value]

  if {$top == "No Current Model"} {
    $menu delete "No Current Model"
  }

  $menu add radiobutton -label $name -variable global_current_model_name
  set global_current_model_name $name
}

proc delete_current_model {ignore_model name} {
  global global_current_model_name
  global during_reload
  global options_array

  set menu [[control_panel_name].current_model_name cget -menu]
  set top [$menu entrycget 0 -value]
  
  $menu delete $name 
      
  if {[$menu entrycget 0 -value] == ""} {
    $menu add radiobutton -label "No Current Model" -variable global_current_model_name
  }      

  if {$global_current_model_name == $name} {
    set global_current_model_name [$menu entrycget 0 -value]
  }

  if {$during_reload == 0} {
      
    if {$options_array(kill_model_windows) == 1} {
      kill_all_model_windows $name
    } 
  }
}
  
set env_current_add_monitor [add_new_cmd env_current_add_monitor "create_current_model" "Environment command for setting the current model menu when models created."]

set env_current_delete_monitor [add_new_cmd env_current_delete_monitor "delete_current_model" "Environment command for setting the current model menu when models deleted."]


proc check_current_models {} {
  global global_current_model_name

  set menu [[control_panel_name].current_model_name cget -menu]
  set top [$menu entrycget 0 -value]

  set models [lindex [call_act_r_command "mp-models"] 0]

  if {$top == "No Current Model" && [llength $models] != 0} {
    $menu delete "No Current Model"
  }

  foreach model $models {
   $menu add radiobutton -label $model -variable global_current_model_name
   set global_current_model_name $model
  }
}     
  
check_current_models

send_cmd "monitor" [list "creating-model" $env_current_add_monitor]
send_cmd "monitor" [list "deleting-model" $env_current_delete_monitor]

# Can be lazy about this because the dispatcher will clean up automatically
# when the connection drops.

#bind [control_panel_name].current_load <Destroy> {
#  global env_current_add_monitor
#  global env_current_delete_monitor

#  send_cmd "remove-monitor" [list $env_current_add_monitor "creating-model"]
#  send_cmd "remove-monitor" [list $env_current_delete_monitor "deleting-model"]
#  send_cmd "remove" $env_current_add_monitor
#  send_cmd "remove" $env_current_delete_monitor
#}

proc currently_selected_model {} {
  global global_current_model_name
  
  if {$global_current_model_name == "No Current Model" } {
    return "nil"
  } else {
    return $global_current_model_name
  }
}

proc model_in_current_list {model} {

  set menu [[control_panel_name].current_model_name cget -menu]
  set last [$menu index last]
  
  for {set i 0} {$i <= $last} {incr i} {
    if {$model == [$menu entrycget $i -value]} {
      return 1
    }
  }
  return 0
}
 
proc set_currently_selected_model {model} {
  global global_current_model_name

  if [model_in_current_list $model] {
    set global_current_model_name $model
    return 1
  } else {
    return 0
  }
}

    
global window_to_model
global model_to_windows


proc kill_all_model_windows {name} {

  global model_to_windows

  if {[array names model_to_windows -exact $name] != ""} {

    set windows $model_to_windows($name) 

    foreach x $windows {
      catch {
        destroy $x
      }
    }
    array unset model_to_windows $name
  }
}

proc remove_model_window {win} {
  global window_to_model
  global model_to_windows

  if {[array names window_to_model -exact $win] != ""} {

    set model $window_to_model($win)
    array unset window_to_model $win

    set old $model_to_windows($model) 
  
    set index [lsearch -exact $old $model]

    if {$index != -1} {
      set model_to_windows($model) [lreplace $old $index $index]
    }
  }
}


proc record_new_window {win_name title {model ""}} {
 
  if {$model == ""} {
    record_window_with_model $win_name [currently_selected_model] $title
  } else {
    record_window_with_model $win_name $model $title
  }
}

proc record_window_with_model {win_name model {title ""}} {
  
  global window_to_model
  global model_to_windows

  set window_to_model($win_name) $model

  if {$title == ""} {
    wm title $win_name "$win_name ($model)"
  } else {
    wm title $win_name "$title ($model)"
  }

  if {[array names model_to_windows -exact $model] == ""} {
    set model_to_windows($model) [list $win_name]
  } else {
    set model_to_windows($model) [concat $model_to_windows($model) $win_name]
  }

  bind $win_name <Destroy> {remove_model_window %W}
}

proc model_for_window {win} {
  global window_to_model

  if {[array names window_to_model -exact $win] != ""} {
    return $window_to_model($win)
  } else {
    return "nil"
  }
}


proc update_registered_windows {} {
  global window_to_model

  set names [array names window_to_model]
  
  foreach n $names {
    set tag [lindex [bindtags $n] 0]  

    if [regexp {^focus_tag[0-9]+$} $tag] {
      eval [bind $tag <FocusIn>]
    }
  }
}
      
