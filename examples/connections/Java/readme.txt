Tested using Windows 10 with Java version:

>java -version
java version "1.8.0_191"
Java(TM) SE Runtime Environment (build 1.8.0_191-b12)
Java HotSpot(TM) 64-Bit Server VM (build 25.191-b12, mixed mode)

Uses the JSON parser from https://github.com/stleary/JSON-java.
The jar file json-20180813.jar from:
https://mvnrepository.com/artifact/org.json/json/20180813 
was used.  It was renamed to json.jar and placed into the same 
directory as the simpleACTRtest.java file.

This was used to compile the file:

>javac -cp .;json.jar simpleACTRtest.java

Then this to run the test:

>java -cp .;json.jar simpleACTRtest


It adds a command called "java-add" to ACT-R which can be passed 2
numbers and it returns their sum.


Here is the sample call from ACT-R:

(evaluate-act-r-command "java-add" 1 2)
