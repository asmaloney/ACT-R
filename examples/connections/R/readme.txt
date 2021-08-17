Tested with R version 3.5.1.

Need to install the rjson package if not already installed to use it:

install.packages("rjson")


Source the actr-plotter.R script to add a command called "plot-with-r" to 
ACT-R which will draw a scatter plot to a pdf.  The parameters to "plot-with-r"
are: 

 - a string nameing the pdf file to create
 - a string with the title for the plot
 - a string with the name of the x axis
 - a string with the name of the y axis
 - the minimum x value for the graph
 - the maximum x value for the graph
 - the minimum y value for the graph
 - the maximum y value for the graph
 - a list of the x values for the points to plot
 - a list of the corresponding y values

Here's a sample call:

(evaluate-act-r-command "plot-with-r" (namestring (translate-logical-pathname "ACT-R:test.pdf")) "Test" "x" "y" 0 10 0 10 (list 2 3 3 4 5 6 7 7 8) (list 3 2.5 7 2 1.5 2 2.5 7 3))


Notes:

Under Windows, ACT-R running in most Lisps considers the ~
directory to be /Users/<name> and that's where it writes
the files with the configuration information.  However,
R considers ~ to be /Users/<name>/Documents and thus
there's a little extra code at the top to find the right
files.