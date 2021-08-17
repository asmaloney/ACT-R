global current_options

proc select_options {} {
  global options_array
  global current_options

  array set current_options [array get options_array]

  if {[winfo exists .options] == 1} {
    wm deiconify .options
    raise .options
  } else {

    toplevel .options
    wm withdraw .options
    wm title .options "Options"

    wm geometry .options [get_configuration .options]

    frame .options.but_frame

    button .options.but_frame.save -text "Save" -font button_font -command {
      save_options
      destroy .options
    }

    button .options.but_frame.apply -text "Apply" -font button_font -command {
      apply_options
    }

    button .options.but_frame.cancel -text "Cancel" -font button_font -command {
      destroy .options
    }

    button .options.but_frame.revert -text "Revert" -font button_font -command {
      array set current_options [array get options_array]
    }

    pack .options.but_frame.save -side left
    pack .options.but_frame.apply -side left
    pack .options.but_frame.cancel -side left
    pack .options.but_frame.revert -side left

    checkbutton .options.use_env_window \
                -text "Use an Environment window for visible experiment windows" \
                -font checkbox_font -variable current_options(use_env_window) \
                -onvalue true -offvalue false

    checkbutton .options.use_smart_load \
                -text "Compile an ACT-R file when loaded or reloaded" \
                -font checkbox_font -variable current_options(use_smart_load) \
                -onvalue true -offvalue false

    checkbutton .options.save_before_reload \
                -text "Automatically save all open files when reload pressed" \
                -font checkbox_font \
                -variable current_options(save_before_reload) \
                -onvalue 1 -offvalue 0

    checkbutton .options.show_copyrights \
                -text "Show the ACT-R copyrights screen" \
                -font checkbox_font \
                -variable current_options(show_copyrights) \
                -onvalue 1 -offvalue 0

    checkbutton .options.save_backups \
                -text "Automatically save a backup file when saving opened files" \
                -font checkbox_font \
                -variable current_options(save_backups) \
                -onvalue 1 -offvalue 0

    checkbutton .options.multiple_models_close \
                -text "Close inspector windows for deleted models" \
                -font checkbox_font \
                -variable current_options(kill_model_windows) \
                -onvalue 1 -offvalue 0

    checkbutton .options.update_when_stepped \
                -text "Update inspector windows automatically when stepping" \
                -font checkbox_font \
                -variable current_options(update_when_stepped) \
                -onvalue 1 -offvalue 0

    checkbutton .options.use_localhost \
                -text "Use localhost instead of 127.0.0.1" \
                -font checkbox_font \
                -variable current_options(use_localhost) \
                -onvalue 1 -offvalue 0

    checkbutton .options.sort \
                -text "Sort declarative, procedural, and buffers viewers' item lists" \
                -font checkbox_font \
                -variable current_options(sort_lists) \
                -onvalue 1 -offvalue 0

    pack .options.use_env_window -anchor w -expand 1 -fill x
    pack .options.use_smart_load -anchor w -expand 1 -fill x
    pack .options.save_before_reload -anchor w -expand 1 -fill x
    pack .options.show_copyrights -anchor w -expand 1 -fill x
    pack .options.save_backups -anchor w -expand 1 -fill x
    pack .options.multiple_models_close -anchor w -expand 1 -fill x
    pack .options.update_when_stepped -anchor w -expand 1 -fill x
    pack .options.use_localhost -anchor w -expand 1 -fill x
    pack .options.sort -anchor w -expand 1 -fill x

    pack .options.but_frame
    wm deiconify .options
  }
}

proc apply_options {} {
  global options_array
  global current_options

  array set options_array [array get current_options]
  
  # take care of any special updating necessary

#  catch {
#    send_environment_cmd \
#      "update [get_handler_name .env_window] \
#         (lambda (x) \
#            (declare (ignore x)) \
#            (setf (environment-control-use-env-windows *environment-control*) $options_array(use_env_window) ) \
#            (list 'ignore))"
#  }
}

proc save_options {} {
  global options_array
  global tcl_env_dir

  apply_options

  set file [file join $tcl_env_dir init "99-userconfig.tcl"]

  write_data "# this file generated from saving the options\n" $file
 
  foreach name [array names options_array] {
    append_data "set options_array($name) $options_array($name)\n" $file
  }
}



button [control_panel_name].options -text "Options" -font button_font -command {select_options}  
pack [control_panel_name].options
