proc make_declarative_viewer {} {

  set model [currently_selected_model]

  if {$model == "nil"} {
    tk_messageBox -icon info -type ok -title "Declarative" -message "Declarative memory viewer requires a current model."
  } else {
    set win [toplevel [new_variable_name .declarative]]
    wm withdraw $win

    record_new_window $win $win $model

    wm geometry $win [get_configuration .declarative $win]
  
    set filter_label [label $win.filter_label -text "Filter:" -justify left -font label_font]
    
    set list_frame [frame $win.list_frame -borderwidth 2]  
    
    set list_box [listbox $list_frame.list_box -listvar \
                          $list_frame.list_box.var \
                          -yscrollcommand "$list_frame.list_scrl set" \
                          -selectmode single \
                          -exportselection 0 -font list_font -bd 0]
  
    set list_scroll_bar [scrollbar $list_frame.list_scrl -command "$list_box yview"]


    # here's the frame for the chunk display

    set text_frame [frame $win.text_frame -borderwidth 0]  
 
    set text_box [text $text_frame.text -yscrollcommand \
                       "$text_frame.text_scrl set" -state disabled \
                       -font text_font]
    

    set text_scroll_bar [scrollbar $text_frame.text_scrl -command "$text_box yview"]



    bind $list_box <<ListboxSelect>> "select_chunk $list_box $text_box $model"


    set why_not [button $win.whynot_button -text "Why not?" -font button_font \
                            -command "show_why_not_dm $list_box $model"]


    # Here is where I create my own drop-down (combobox, whatever you want to 
    # call it) widget for the filter.  This one is actually going to be a button
    # that when pressed displays the list of items to choose from.  I guess 
    # that's slightly outside of the style guidelines, but it doesn't seem too 
    # bad to me but if people don't like it maybe I'll fix it then.


    set filter_button [button $win.filter_button -text "_none_" -borderwidth 2 \
                              -relief sunken -font button_font\
                              -command "declarative_drop_list \
                                                    $win \
                                                    $win.filter_button \
                                                    $list_box \
                                                    $text_box \
                                                    $model"]

    # here the items are placed into the window using the relative
    # options for those items that need to resize if the window gets resized 
    # could get this from the Visual Tcl/tk GUI builder, or just work it out
    # sizes are based on the default fonts, so if you change that it could
    # look a little "off"

    place $filter_label -x 80 -y 5 -width 40 -height 20
    place $filter_button -x 124 -y 5 -relwidth 1.0 -width -130 -height 24

    place $why_not  -x 2 -y 5 -width 75 -height 24

    place $list_frame -relx 0.0 -y 35 -relheight 1.0 -height -35 -relwidth .4
    place $text_frame -relx .4 -y 35 -relheight 1.0 -height -35 -relwidth .6
     
    pack $list_scroll_bar -side right -fill y 
    pack $list_box -side left -expand 1 -fill both
    pack $text_scroll_bar -side right -fill y
    pack $text_box -side left -expand 1 -fill both

    set_update_script $win "update_dm_chunk_list $list_box $text_box $model $filter_button"

    # now show the window 

    wm deiconify $win
    focus $win
  }
}


proc update_dm_chunk_list {list text_box model filter_button} {

#  get the new list, and if something is selected output that in the window

  set filter [$filter_button cget -text]
 
  set chunks [lindex [call_act_r_command "filter-dm-chunks" $model [list $filter]] 0]

  global options_array

  if {$options_array(sort_lists) == 1} {
    set chunks [lsort -dictionary $chunks]
  }

  global $list.var

  set selection [$list curselection]

  if {$selection != ""} {
    set name [$list get $selection]
  }

  set $list.var $chunks

  if {$selection != ""} { 
  # check if the selection is still available

    $list selection clear 0 end
    set newpos [lsearch -exact $chunks $name]

    if {$newpos != -1} {
      $list selection set $newpos
    }
  }

  select_chunk $list $text_box $model
}




proc select_chunk {list_box text_box model} {

  if {[$list_box curselection] != ""} {
    update_text_pane $text_box [lindex [call_act_r_command "dm-chunk-details" $model [list [$list_box get [$list_box curselection]]]] 0]
  } else {
    update_text_pane $text_box ""
  }
}

# declarative_drop_list
# This is the procedure that implements the interaction of a drop-down
# widget triggered by pressing a button.  Its parameters are that button (but)
# the window (top) that holds a frame (drop_frame) which contains 
# the listbox (drop_box) and the scrollbar (drop_scroll_bar), and the chunk 
# list widget (list_box).  
# It displays the selection listbox below the button and waits for either a 
# selection or the dismissal of the selection box. If there is a selection 
# different from the current one it updates the button to reflect the new 
# choice and issues an update request for the chunk list for chunks with the
# specified slots.

proc pick_drop_selection {var box} {
  global $var

  if {[$box curselection] != ""} {
    set $var [$box get [$box curselection]] 
  } else {
    set $var -1
  }
}

proc quit_drop_selection {var} {
  global $var
  set $var -1
}


proc declarative_drop_list {win but list_box text_box model} {

  # first create a toplevel window that will hold the selection list
  # it's got to be a separate window so that things like leaving it's focus
  # can easily be trapped

  if [winfo exists $win.top] {
    destroy $win.top
  }

  set top [toplevel $win.top]
  wm overrideredirect $top 1
  wm withdraw $top 


  # now create a listbox to hold the list that drops down

  set drop_frame [frame $top.frame -borderwidth 0] 

  set drop_box [listbox $drop_frame.drop_box \
                        -listvar $drop_frame.drop_box.var \
                        -yscrollcommand "$drop_frame.drop_scrl set" \
                        -exportselection 0 -font list_font -bd 0] 

  set drop_scroll_bar [scrollbar $drop_frame.drop_scrl -command "$drop_box yview"]

  # create a variable for recording the response

  global $but.choice

  set $but.choice ""

  # Only take the selection when the button is released or return pressed
  
  bind $drop_box <ButtonRelease-1> "pick_drop_selection $but.choice $drop_box"
  
  bind $drop_box <Key-Return> "pick_drop_selection $but.choice $drop_box"

  # have the esc key close down the interaction without a choise and moving outside the window
  
  bind $drop_box <Key-Escape> "quit_drop_selection $but.choice"

  # if the user moves the focus outside of this window then destroy it

  bind $top <FocusOut> "quit_drop_selection $but.choice"

  # record the current filter

  set current_filter [$but cget -text]

  # get the new one

  global $drop_frame.drop_box.var

  set $drop_frame.drop_box.var [lindex [call_act_r_command "dm-slot-filters" $model] 0]

  # get a pointer to the list of chunk_types

  upvar $drop_frame.drop_box.var cur_list

  # figure out where the current selection is in the list
  # so that it can be made visible

  set choice [lsearch -exact $cur_list $current_filter]

  # make the selection window as wide as the button and 5 lines high

  set f_height [font metrics list_font -linespace]

  $top configure -width [winfo width $but] -height [expr 5*$f_height]

  # put the selection components into the top level window  

  place $drop_frame -relx 0.0 -rely 0.0 -relheight 1.0  -relwidth 1.0
     
  pack $drop_scroll_bar -side right -fill y 
  pack $drop_box -side left -expand 1 -fill both

  # move the top level window so that it's directly below the button

  set x [winfo rootx $but]
  set y [winfo rooty $but]
  set y [expr $y + [winfo height $but]]

  wm geometry $top "+$x+$y"

  # make it visible and ensure it's on top

  wm deiconify $top
  raise $top

  # set the selected item in the list to the current filter choice and
  # make sure it's visible in the list

  $drop_box activate $choice
  $drop_box see $choice

  # make the selection box the current focus

  focus $drop_box 

  # wait for the user to pick something or dismiss the window

  tkwait variable $but.choice

  upvar $but.choice new_choice

  destroy $top

  # if the user picked something that was different from
  # what was chosen before then change the button's text

  if {$new_choice != -1 && $new_choice != $current_filter} {
      $but configure -text $new_choice
  }
  # make sure to focus on the main window again to
  # have the update occur automatically because 
  # if it gets called directly it could happen twice

  focus -force $win

}


proc show_why_not_dm {chunks model} {
  # get the current selection - knowing it's a single selection list box
  set selection [$chunks curselection]

  if {$selection != "" } {
      
    # get the chunk name
    set chunk [$chunks get $selection]
    
    # make a new window 
    set top [toplevel [new_variable_name ".whynotdm"]]

    wm geometry $top [get_configuration .whynotdm $top]

    record_new_window $top "Whynot-dm $chunk" $model
    
    # create a text_box to hold the output and put it in the window
    # also need a frame to hold it and the scroll bar

    frame $top.frame -borderwidth 0  
    
    set text_box [text $top.frame.text -font text_font -state disabled \
                                       -yscrollcommand "$top.frame.scrl set" ]

    set scrl_bar [scrollbar $top.frame.scrl -command "$top.frame.text yview"]
   
    place $top.frame -x 0 -y 0 -relwidth 1.0 -relheight 1.0
        
    pack $scrl_bar -side right -fill y 
    pack $text_box -side left -expand 1 -fill both
 
    # get the text for the window

    update_text_pane $text_box [lindex [call_act_r_command "dm-whynot-text" $model [list $chunk]] 0]
  }
}


# Make a button for the control panel that will open a new declarative viewer

button [control_panel_name].declarative -command {make_declarative_viewer} \
       -text "Declarative" -font button_font

# put that button on the control panel

pack [control_panel_name].declarative
