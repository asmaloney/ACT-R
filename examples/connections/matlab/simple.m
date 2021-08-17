% Read the ACT-R address and port config files

fid = fopen('~/act-r-address.txt','r');
address = fscanf(fid,'%s');
fclose(fid);

fid = fopen('~/act-r-port-num.txt','r');
port = fscanf(fid,'%d');
fclose(fid);

% Open a socket to connect to ACT-R 

socket = tcpip(address,port,'NetworkRole','client','Timeout',Inf);
fopen(socket);

% Set the name for this ACT-R connection

fwrite(socket,'{"method":"set-name","params":["Simple MATLAB"],"id":null}');
fwrite(socket,char(4));

% Evaluate the "act-r-version" command

fwrite(socket,'{"method":"evaluate","params":["act-r-version"],"id":1}');
fwrite(socket,char(4));

% Read characters from the stream until the
% end of message character is found.

result = '';
c = fread(socket,1);

while (c ~= char(4))
  result = strcat(result,c);
  c = fread(socket,1);
end

% Decode the JSON data from that string

response = jsondecode(result);

% Check to make sure there is a result matching the
% id provided (will always be the case here since 
% that's all the dispatcher will send)

if ~isempty(response.id)
  if response.id == 1
    if ~isempty(response.result)
      disp(strcat('ACT-R version is: ',response.result{1}));
    elseif ~isempty(response.error)
      disp('Error: ');
      disp(response.error.message);
    else
      disp('No result or error');
    end
  else
    disp('Invalid response id');
  end
else
  disp('No id value on result');
end

% Add a command called "matlab-add" which is referred to as "add" in the evaluate request

fwrite(socket,'{"method":"add","params":["matlab-add","add","Add the 2 values provided. Params: num1 num2"],"id":null}');
fwrite(socket,char(4));

% Loop forever reading messages from ACT-R
% and responding to the evaluate messages which
% require a result.

while 1
    
    result = '';
    c = fread(socket,1);

    while (c ~= char(4))
        result = strcat(result,c);
        c = fread(socket,1);
    end

    response = jsondecode(result);
  
    if ~isempty(response.method)
        if strcmp(response.method,'evaluate')
            if (~isempty(response.params) && length(response.params) == 4 && strcmp(response.params{1},'add'))
                % should also verify that they're numbers and protect the
                % addition for errors
                if ~isempty(response.id)
                    fwrite(socket,strcat('{"result":[',num2str(response.params{3}+response.params{4}),'],"error":null,"id":',jsonencode(response.id),'}',char(4)));
                end
            else
                disp('Invalid evaluate request');
            end
        else
            disp('Received method other than evaluate');
        end
    else
        disp('No method in message.');
    end    
end
