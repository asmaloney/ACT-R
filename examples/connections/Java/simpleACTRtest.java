
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.InputStream;
import org.json.*;

public class simpleACTRtest {

    private static char eom = 4; // End of message character.
    private static PrintWriter out;
    private static InputStream in;

    // Send a string down the socket stream followed by the
    // end of message character.

    private static void print (String s) {
        out.print(s);
        out.print(eom);
        out.flush();
    }

    // Read and collect characters from the socket until the
    // end of message character is received and return the
    // String that was collected.

    private static String receive() throws IOException {
        StringBuffer buff = new StringBuffer();
        
        for(int c = in.read(); (c != -1) && (c != eom) ;c = in.read()) {
            buff.append((char)c);
        }

        return(buff.toString());
    }

    public static void main(String[] args) throws UnknownHostException, IOException, ClassNotFoundException, InterruptedException{

        // Read the address and port from the ACT-R files in the
        // user's home directory.

        Path addressPath = Paths.get(System.getProperty("user.home"),"act-r-address.txt");
        Path portPath = Paths.get(System.getProperty("user.home"),"act-r-port-num.txt");
        
        BufferedReader ar = Files.newBufferedReader(addressPath);
        String address = ar.readLine();
        ar.close();

        BufferedReader pr = Files.newBufferedReader(portPath);
        String portString = pr.readLine();
        pr.close();

        // Connect to ACT-R.
        Socket socket = new Socket(address,Integer.parseInt(portString));

        // Save the streams used for writing and reading
        // from the socket.

        out = new PrintWriter(socket.getOutputStream());
        in = socket.getInputStream();

        // Send the message to indicate a name for this connection.

        print("{\"method\":\"set-name\",\"params\":[\"Simple Java Example\"],\"id\":null}");

        // Evaluate the ACT-R "act-r-version" command.

        print("{\"method\":\"evaluate\",\"params\":[\"act-r-version\"],\"id\":1}");

        // Read the result and parse it to get the returned value.
        // Has more error checking that really necessary given we
        // know there are no other results that could be received.

        String result = receive();

        JSONObject obj = new JSONObject(result);

        if (obj.has("result") && obj.has("error") && obj.has("id")) {
            if (obj.getInt("id") == 1) { // matches id sent
                if (obj.isNull("error")) { // no error reported
                    JSONArray arr = obj.getJSONArray("result");
                    System.out.println("ACT-R Version: "+arr.getString(0));
                } else { // Print the error returned.
                    JSONObject e = obj.getJSONObject("error");
                    System.out.println("Error: "+e.getString("message"));
                }
            } else {
               System.out.println("Wrong id received.");
            }
        } else {
            System.out.println("Invalid message received.");
        }

        // Send the method to add a new command called "java-add" and have 
        // it referred to as "add" in the evaluate requests that are sent.

        print("{\"method\":\"add\",\"params\":[\"java-add\",\"add\",\"Add the two values provided. Params: a1 a2\"],\"id\":null}");

        // Loop forever reading messages and responding to requests
        // to evaluate "add".

        for(;;) {

            result = receive();
            obj = new JSONObject(result);
      
            // Parse the object to make sure it is a valid request
            // to evaluate the add command with two numbers.
        
            if (obj.has("method") && obj.has("params") && obj.has("id")) {
                if (!obj.isNull("id")) { // Null id means no return should be sent
                    if (obj.getString("method").equals("evaluate")) {
                        JSONArray arr = obj.getJSONArray("params");
                        if (arr.length() == 4 && arr.getString(0).equals("add")) {
                            long sum = arr.getLong(2) + arr.getLong(3);
                            print("{\"result\":["+sum+"],\"error\":null,\"id\":"+obj.valueToString(obj.get("id"))+"}");
                        } else {
                            print("{\"result\":null,\"error\":{\"message\": \"Bad parameters\"},\"id\":"+obj.valueToString(obj.get("id"))+"}");
                        }
                    } else {
                        System.out.println("Not an evaluate method.");
                    }
                } else {
                    System.out.println("Without id no return needed so do nothing.");
                }
            } else {
                System.out.println("Invalid message received.");
            }
       }
   }
}
