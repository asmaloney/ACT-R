#include <stdio.h>
#include <string.h>
#include <jansson.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <netdb.h> 
#include <stdlib.h>
#include <pwd.h>


// Read characters from a socket into a string until 
// the ACT-R end of message character is found.

void read_message(int sockfd,char* buffer) {
  
  int index;
  int last;
  
  for(index = 0,last=0;last != 4;index++) {
    read(sockfd,&(buffer[index]),1);
    last = buffer[index];
  }
  buffer[index-1] = 0;
}

  
void main (){

  // Get the user's home directory.
  
  const char *homedir;
  
  if ((homedir = getenv("HOME")) == NULL) {
    homedir = getpwuid(getuid())->pw_dir;
  }
  
  // Create the directories for the ACT-R
  // config files and read in the results.
  
  char addr_file[128];
  char port_file[128];
  
  strcpy(addr_file,homedir);
  strcat(addr_file,"/act-r-address.txt");
  strcpy(port_file,homedir);
  strcat(port_file,"/act-r-port-num.txt");
  
  FILE *fp;
  
  fp = fopen(addr_file,"r");
  char addr[128];
  fscanf(fp,"%s",addr);
  fclose (fp);

  fp = fopen(port_file,"r");
  int port_num;
  fscanf(fp,"%d",&port_num);
  fclose (fp);

  // Create and connect the socket to ACT-R.
  
  int sockfd;

  sockfd = socket(AF_INET,SOCK_STREAM,0);
  int flag = 1;
  
  setsockopt(sockfd, IPPROTO_TCP, TCP_NODELAY, (void *) &flag, sizeof(int)); 
  struct sockaddr_in dest;
  
  bzero(&dest, sizeof(dest));
  dest.sin_family = AF_INET;
  dest.sin_port = htons(port_num);
  inet_aton(addr, &dest.sin_addr.s_addr);
  
  connect(sockfd,(struct sockaddr*)&dest, sizeof(dest));
  
  // Send the messages to set the name for the connection and
  // evaluate the "act-r-version" command.   
  
  char  set_name[] ="{\"method\":\"set-name\",\"params\":[\"Simple c Example\"],\"id\":null}\04";
  char  evaluate[] ="{\"method\":\"evaluate\",\"params\":[\"act-r-version\"],\"id\":1}\04";
  
  write(sockfd,set_name,strlen(set_name));
  write(sockfd,evaluate,strlen(evaluate));
  
  // Read the response from the socket, parse the JSON
  // code to get the result (with some unnecessary tests
  // tests for example purposes since ACT-R will always 
  // respond correctly), and then print out the result returned.
  
  char buffer[256];
  
  read_message(sockfd,buffer);
  
  json_t *root;
  json_error_t error;
  
  root = json_loads(buffer,0,&error);

  if(!root) {
    printf("error: on line %d: %s\n", error.line, error.text);
    exit(0);
  }
  
  if (!json_is_object(root)) {
    printf("Not an object\n");
    exit(0);
  }

  json_t *r;
  json_t *e;
  json_t *id;
  
  r = json_object_get(root,"result");
  e = json_object_get(root,"error");
  id = json_object_get(root,"id");
  
  // Check that it matches the id sent.
  if (json_is_integer(id) && json_integer_value(id) == 1) {
    
    // has an array result
    if (json_is_array(r)) {
      json_t *v;
      v = json_array_get(r,0);
      
      // and the first result is a string.
      if (json_is_string(v)) {
	printf("ACT-R version: %s\n",json_string_value(v));
	json_decref(v);
      }
      // Otherwise there should be an error message.
    } else if (json_is_object(e)) {
      json_t *em;
      
      em = json_object_get(e,"message");
      if (json_is_string(em)) {
	printf("ACT-R returned error result: %s\n",json_string_value(em));
	json_decref(em);
      }
    }
    else {
      printf("Invalid response\n");
      exit(0);
    }
  } else {
    printf("Got wrong id in response\n");
    exit(0);
  }
  
  // Release the JSON objects.
  json_decref(id);
  json_decref(e);
  json_decref(r);
  json_decref(root);

  // Send the command to add our "c-add" command which will just 
  // be called "add" when an evaluate request is made.
  char  add[] ="{\"method\":\"add\",\"params\":[\"c-add\",\"add\",\"Command to add two numbers. Params: num1 num2\"],\"id\":null}\04";
  write(sockfd,add,strlen(add));
  
  json_t *m;
  json_t *p;
  json_t *c;
  json_t *n1;
  json_t *n2;
  json_t *o;
  double answer;
  char out[256];
  
  // Loop forever responding to the message sent
  // (which will only be evaluate for the add command
  // but parse the result to verify for example purposes).
  
  for (;;) {
    read_message(sockfd,buffer);
    
    root = json_loads(buffer,0,&error);

    if(!root) {
      printf("error: on line %d: %s\n", error.line, error.text);
      exit(0);
    }
    
    if (!json_is_object(root))  {
      printf("Not an object\n");
      exit(0);
    }
    
    m = json_object_get(root,"method");
    p = json_object_get(root,"params");
    id = json_object_get(root,"id");
    
    // Check that it's an evaluate method.
    
    if (json_is_string(m) && strcmp(json_string_value(m),"evaluate") == 0) {
      
      // make sure there are 4 parameters.
      if (json_is_array(p) && json_array_size(p) == 4) {
	c = json_array_get(p,0);
	n1 = json_array_get(p,2);
	n2 = json_array_get(p,3);
	
	// If the first is "add" then add the third and fourth values.
	if (json_is_string(c) && strcmp(json_string_value(c),"add") == 0) {
	  answer = json_number_value(n1) + json_number_value(n2);
	  
	  // If there was an id return the result.
	  
	  if(!json_is_null(id)) {
	    sprintf(out,"{\"result\":[%f],\"error\":null,\"id\":%s}\04",answer,json_dumps(id,JSON_ENCODE_ANY));
	    write(sockfd,out,strlen(out));
	  }
	  // Free all the JSON objects.
	  json_decref(m);
	  json_decref(p);
	  json_decref(id);
	  json_decref(c);
	  json_decref(n1);
	  json_decref(n2);
	  json_decref(root);
	} else {
	  printf("Request to evaluate something other than add\n");
	  exit(0);
	}
      } else { 
	printf("Invalid parameters for add\n");
	
	// Respond with an error message
        if(!json_is_null(id)) {
	  
	  sprintf(out,"{\"result\":null,\"error\":{\"message\":\"Bad parameters\"},\"id\":%s}\04",json_dumps(id,JSON_ENCODE_ANY));
	  write(sockfd,out,strlen(out));
	}
	json_decref(m);
	json_decref(p);
	json_decref(id);
	json_decref(c);
	json_decref(n1);
	json_decref(n2);
	json_decref(root);
      }
    } else {
      printf("Something other than evaluate received\n");
      exit(0);
    }
  }
}
