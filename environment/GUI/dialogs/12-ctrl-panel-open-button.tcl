# Combine the Open and save buttons into one file (close no longer useful since multiple files may be opened)

# Open

frame [control_panel_name].o_frame

button [control_panel_name].o_frame.open -text "Open File" \
       -font button_font \
       -command {
         global local_connection
         global currently_open_files
         global model_type
         global top_dir

         if {$local_connection == 0} {
           tk_messageBox -icon warning -type ok -title "Open warning" \
                         -message "You cannot use the Open File button if the\
                                   environment is not running on the same machine\
                                   as ACT-R."  
         } else {
           set fname [tk_getOpenFile -title "File to open" -initialdir $top_dir] 
           if {$fname != ""} {
             edit_model_file $fname
           }
         }
       }

button [control_panel_name].o_frame.create -text "New File" \
       -font button_font \
       -command {
         global local_connection
         global currently_open_files
         global model_type
         global top_dir

         if {$local_connection == 0} {
           tk_messageBox -icon warning -type ok -title "Open warning" \
                         -message "You cannot use the Open File button if the\
                                   environment is not running on the same machine\
                                   as ACT-R."  
         } else {
           set fname [tk_getSaveFile -title "Save new file as" -initialdir $top_dir]
           if {$fname != ""} {
             write_data "" $fname
             edit_model_file $fname
           }
         }
       }


 
pack [control_panel_name].o_frame.open -side left
pack [control_panel_name].o_frame.create -side left
pack [control_panel_name].o_frame


set pmatch_id ""

proc edit_model_file {file_name} {
  global current_file_window
  global file_changed
  global currently_open_files   
            
  set win_name [toplevel [new_variable_name .edit_window]]

  wm withdraw $win_name
     
  wm geometry $win_name [get_configuration .model $win_name]
   
  wm title $win_name [file tail $file_name]

  # create a text_box to hold the output and put it in the window
  # also need a frame to hold it and the scroll bar

  frame $win_name.frame -borderwidth 0  
    
  set text_box [text $win_name.frame.text -yscrollcommand "$win_name.frame.vscrl set" \
                     -xscrollcommand "$win_name.frame.hscrl set" -font text_font -wrap none]
          
  set v_scrl_bar [scrollbar $win_name.frame.vscrl -command "$win_name.frame.text yview"]
   
  set h_scrl_bar [scrollbar $win_name.frame.hscrl -command "$win_name.frame.text xview" -orient horizontal]
   
  place $win_name.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
  pack $v_scrl_bar -side right -fill y 
  pack $h_scrl_bar -side bottom -fill x 
  pack $text_box -side left -expand 1 -fill both

  set data [read_file $file_name]

  if {$data != ""} {
    $text_box delete 1.0 end
    $text_box insert end $data
  }

  set currently_open_files($win_name) $file_name
  set file_changed($win_name) 0
  set current_file_window $win_name

  bind $text_box <KeyPress-parenright> "edit_right_paren $win_name %W"

  bind $text_box <KeyPress-parenleft> "edit_left_paren $win_name %W"
   
  bind $text_box <KeyPress> " 
    global file_changed 
    set file_changed($win_name) 1 
  "
        
  set_update_script $win_name "
    global current_file_window 
    set current_file_window $win_name 
  "
  wm protocol $win_name WM_DELETE_WINDOW "prompt_close_text $win_name"

  wm deiconify $win_name
  raise $win_name
  focus $win_name
}

proc edit_left_paren {win text} {
  global file_changed
        
  set file_changed($win) 1
        
  set start [$text index insert]
        
  set paren_count 1
  set index_count -1
  set hit_end 0

  while {
    $paren_count != 0 && $hit_end == 0} {
         
    incr index_count 1

    set cur_char [$text get "$start + $index_count chars"]
          
    if {[$text index "$start + $index_count chars"] == [$text index end]} {
      set hit_end 1
    }

    if { $cur_char == "(" } {
      incr paren_count 1
    } elseif {$cur_char == ")" } {
      incr paren_count -1
    }
  }

  global pmatch_id       

  if {$paren_count == 0} {
    if {[lsearch -exact [$text tag names] pmatch] != -1} {
      after cancel $pmatch_id
      $text tag delete pmatch
    }

    $text tag add pmatch "$start + $index_count chars"
    $text tag configure pmatch -underline 1
          
    set pmatch_id [after 300 "$text tag delete pmatch"]
  }
}

proc edit_right_paren {win text} {
  global file_changed
        
  set file_changed($win) 1
        
  set start [$text index insert]
        
  set paren_count 1
  set index_count 0
  set hit_start 0

  while {$paren_count != 0 && $hit_start == 0} {
        
    incr index_count 1

    set cur_char [$text get "$start - $index_count chars"]
          
    if {[$text index "$start - $index_count chars"] == [$text index 1.0]} {
      set hit_start 1
    }

    if { $cur_char == ")" } {
      incr paren_count 1
    } elseif {$cur_char == "(" } {
      incr paren_count -1
    }
  }

  global pmatch_id       

  if {$paren_count == 0} {
    if {[lsearch -exact [$text tag names] pmatch] != -1} {
      after cancel $pmatch_id
      $text tag delete pmatch
    }

    $text tag add pmatch "$start - $index_count chars"
    $text tag configure pmatch -underline 1
          
    set pmatch_id [after 300 "$text tag delete pmatch"]
  }
}
 

proc prompt_close_text {win} {
  global currently_open_files
  global file_changed
  global current_file_window

  set remaining 0

  if {$file_changed($win) == 1} {
    set answer [tk_messageBox -icon warning -title "Closing $currently_open_files($win)]" \
                              -message "Save changes before closing?" -type yesnocancel]
    switch -- $answer {
      yes {
        save_model $win
        destroy $win
      }
      no {
        destroy $win 
      }
      cancel {
        set remaining 1
      }
    }
  } else {
    destroy $win
  }

  if {$remaining == 0} {
     
    set current_file_window ""
    array unset currently_open_files $win
    array unset file_changed $win
  }
}

# Save

button [control_panel_name].save \
       -command {
         global current_file_window

         [control_panel_name].save configure -state disabled 
         save_model $current_file_window
         [control_panel_name].save configure -state normal 
       } \
       -text "Save File" -font button_font

pack [control_panel_name].save

proc save_model {win} {
  
  global file_changed
  global currently_open_files

  if {$win == ""} {
    tk_messageBox -icon warning -type ok -title "Saving file" \
                  -message "You must select an open file window before pressing Save File."
  } else {
    save_backup $currently_open_files($win)

    save_text $win.frame.text $currently_open_files($win)
    set file_changed($win) 0
  }
}


proc save_backup {fname} {
  global options_array

  if $options_array(save_backups) {
 
    set save_index 0
   
    while {[file exists "$fname-$save_index"] == 1} {
      incr save_index
    }
       
    write_data [read_file $fname] "$fname-$save_index"
  }
}

