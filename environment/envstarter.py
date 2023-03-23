# Very simple script that will run the Environment
# using the Tck/Tk built into Python


import tkinter

x = tkinter.Tk()
x.tk.eval('cd gui')
x.tk.eval('source pstarter.tcl')
x.mainloop()
