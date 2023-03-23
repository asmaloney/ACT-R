# Simple module that will run the Environment
# using the Tck/Tk built into Python from the 
# tutorial/python directory for use in a Jupyter
# notebook that has this:
# 
# import jenvstarter
# import multiprocessing
# multiprocessing.Process(target=jenvstarter.run_env).start() 


import tkinter

x = tkinter.Tk()

def run_env():
    x.tk.eval('cd ..')
    x.tk.eval('cd ..')
    x.tk.eval('cd environment')
    x.tk.eval('cd gui')
    x.tk.eval('source pstarter.tcl')
    x.mainloop()
