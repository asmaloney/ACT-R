
# load the rjson library to parse the incoming data

library(rjson)


# create a function to read from the socket until the
# ACT-R end of message character \004 is read.  Return
# the parsed data from that message.

read_actr_message <- function(socket)
{
  incoming <- ""
  repeat {
    c <- read.socket(socket,1,TRUE)
    if (c == "\004") break
    incoming <- paste(incoming,c, sep = "")
  }
  fromJSON(incoming)
}

# Read the ACT-R configuration files to determine the
# address and port for the connection.  Do a special
# check for Windows because most Lisps consider ~
# to be /Users/<name> but R uses /Users/<name>/Documents.

if (.Platform[["OS.type"]] == "windows") {
  a <- (scan("~/../act-r-address.txt","character"))
  p <- (scan("~/../act-r-port-num.txt"))
} else {
  a <- (scan("~/act-r-address.txt","character"))
  p <- (scan("~/act-r-port-num.txt"))
}

# Open a socket connection to that location

s <- make.socket(a,p)

# Give ACT-R a name for the connection

write.socket(s,"{\"method\":\"set-name\",\"params\":[\"R grapher\"],\"id\":null}\004")

# Evaluate the ACT-R act-r-version command

write.socket(s,"{\"method\":\"evaluate\",\"params\":[\"act-r-version\"],\"id\":1}\004")

# Read the response

m <- read_actr_message(s)

# Check the response to make sure it is a 
# response to the evaluate request which 
# completed successfully.

if (is.null(m[["result"]])) {
  print("Unexpected message from ACT-R")
  print(m)
} else if (is.null(m[["result"]])) {
  print("Error returned from ACT-R")
  print(m[["error"]])
} else if (m[["id"]][[c(1)]] != 1) {
  print("Unexpected id on the result.")
  print(m[["id"]][[c(1)]])
} else {
  print("ACT-R version is")
  print(m[["result"]][[c(1)]])
}

# force the output for the R GUI applications

flush.console()

# Add the "plot-with-r" command to ACT-R, using the name "graph"
# for the evaluate calls it receives.

write.socket(s,"{\"method\":\"add\",\"params\":[\"plot-with-r\",\"graph\",\"Draw a scatter plot in a pdf. Params: file-name title x-label y-label min-x max-x min-y max-y list-of-x-values list-of-y-values\"],\"id\":null}\004")

# Loop forever reading messages from ACT-R and
# responding to the requests as appropriate (they
# should all be evaluate requests for "graph" but
# check for completeness).

repeat {
  
  j <- read_actr_message(s)

  message <- ""

  if (j[["method"]] != "evaluate") {
    message <- "Not an evaluate method."
  } else if (length(j[["params"]]) != 12) {
    message <- "Not enough parameters"
  } else if (j[["params"]][[c(1)]] != "graph") {
    message <- "Only evaluates graph"
  } else {
    params <- j[["params"]]
    model <- params[[c(2)]] # don't need the model name
    file <- params[[c(3)]]
    title <- params[[c(4)]]
    xaxis <- params[[c(5)]]
    yaxis <- params[[c(6)]]
    xmin <- params[[c(7)]]
    xmax <- params[[c(8)]]
    ymin <- params[[c(9)]]
    ymax <- params[[c(10)]]
    xvals <- params[[c(11)]]
    yvals <- params[[c(12)]]

    # Try to open the pdf indicated and draw a graph using plot.

    tryCatch({
      pdf(file)
      plot(x=xvals,y=yvals,main=title,xlab=xaxis,ylab=yaxis,xlim=c(xmin,xmax),ylim=c(ymin,ymax))
      dev.off()
    }, warning = function(w) {
      message <- paste("WARNING: ",w)
    }, error = function(e) {
      message <- paste("ERROR: ",e)
    })

  }

  # If there was an id in the evaluate request indicating
  # a response was required send a true result if there
  # was no error otherwise send an error response with
  # the details of the errror.

  if (!is.null(j[["id"]][[c(1)]])) {
    if (message == "") {
      write.socket(s,paste("{\"result\":[true],\"error\":null,\"id\":",toJSON(j[["id"]][[c(1)]]),"}\004"))
    } else {
      write.socket(s,paste("{\"result\":null,\"error\":{\"message\": \"",message,"\"},\"id\":",toJSON(j[["id"]][[c(1)]]),"}\004"))
    }
  }
}

