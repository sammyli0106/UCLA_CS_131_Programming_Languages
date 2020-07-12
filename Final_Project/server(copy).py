# import the needed libraries for the project
import asyncio
import argparse
import sys
import json
import time
import aiohttp

# Set up my API key (Replaced)
API_KEY = ''

# URL link BASE (Replaced)
LINK_BASE = ''

# Predefined IP address (Replaced)
IP = ''

# The communication relationship between the server
server_communicate = {
    'Hill': ['Jaquez', 'Smith'],
    'Singleton': ['Jaquez', 'Smith', 'Campbell'],
    'Smith': ['Campbell', 'Hill', 'Singleton'],
    'Jaquez': ['Hill', 'Singleton'],
    'Campbell': ['Singleton', 'Smith']
}

# set up the port number for each server
# Change the name
port_dict = {
    'Hill': 11425,
    'Singleton': 11426,
    'Smith': 11427,
    'Jaquez': 11428,
    'Campbell': 11429
}

# set up a return dict for verifying the parts of the input 
return_dict = {
    'good IAMAT': 1,
    'good WHATSAT': 2,
    'good UPDATEMSG': 3,
    'incorrect message': -1
}

# set up a message flag dictionary 
flag_dict = {
    'IAMAT': 1,
    'WHATSAT': 2
}

# set up the dictionary for client name
clients = {}

def clean_message(input_msg):
    # Remove the extra spaces, tabs, newline between the arguments and turn the input into a list
    return input_msg.strip().split()

def extract_msg(message, arrive_time):
    # output format :
    # AT Hill +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
    # [message_type, client_ID, coordinates, client sent time, server receive time, name of the received message server]
    return [message[0], message[1], message[2], message[3], str(arrive_time), sys.argv[1]]

def create_iamat_output_message(diff_in_time, out_message):
    output = ("AT {0} {1} {2}\n".format(sys.argv[1], diff_in_time, ' '.join(out_message[1:])))
    return output

def create_whatsat_output_message(client_content, diff_in_time):
    output = "AT {0} {1} {2} {3} {4}\n".format(client_content[5], diff_in_time, client_content[1], client_content[2],
                                               client_content[3])
    return output

def invalid_response(msg):
    # Return the original message with the ? symbol to the client 
    return "? {0}".format(msg)

def add_pos_sign(time_diff):
    output = "+" + str(time_diff)
    return output

def handle_iamat_time(correct_message, reach_time, iamat_content):
    # calculate the time difference between sent and received
    client_sent_time = float(correct_message[3])
    time_difference = reach_time - client_sent_time

    # store the iamat content in the client list according to the ClientID(correct_message[1])
    clients[correct_message[1]] = iamat_content

    # Check the time difference
    # if the time_difference is negative, it already come along with negative sign, no need to handle it
    # if the time_difference is positive, I need to explicitly add the positive sign
    if time_difference > 0:
        time_difference = add_pos_sign(time_difference)

    return time_difference

def flood_iamat_message(iamat_content):
    # flood message
    asyncio.ensure_future(flood('UPDATEMSG {0}\n'.format(' '.join(iamat_content[1:])), sys.argv[1]))

def handle_whatsat_time(client_info):
    # calculate the time difference between sent and received
    time_difference = float(client_info[4]) - float(client_info[3])

    # I dont need to handle negative time difference, because it comes along
    # I need to handle positive time difference by explicitly adding the sign
    if time_difference > 0:
        time_difference = add_pos_sign(time_difference)

    return time_difference

def extract_client_info(correct_message):
    return clients[correct_message[1]]

def format_location_info(client_place_in):
    return str(client_place_in[0]) + "," + str(client_place_in[1])

def extract_client_location(client_info):
    # verify the location first 
    client_place = verify_coordinate(client_info[2])
    # format my client location information 
    client_place = format_location_info(client_place)
    return client_place

async def format_output(message, reach_time):
    # This function is called to create the echo message that is send back to the client
    # We need to format the input before actually process it

    # Set up the output message
    output_message = ""
    # Remove the extra spaces between the arguments and turn the input into a list
    # Put the strip and split procedure into a function
    correct_message = clean_message(message)
    # Get the type of the message, 1, 2, 3, -1
    message_form = verify_input_message(correct_message)

    # Create different output message based on the type
    # This is a IAMAT message
    if message_form == flag_dict["IAMAT"]:

        print("Checking IAMAT message ...")

        # We have to check the content of IAMAT message
        check_iamat = verify_coordinate(correct_message[2])

        # If the coordinate is not correct 
        if check_iamat is None:
            # Set up the error message
            output_message = invalid_response(message)
        else:

            # This is a correct IAMAT message
            print("This is a valid IAMAT message...")            

            # get the iamat content processed
            iamat_content = extract_msg(correct_message, reach_time)

            # calculate the time difference 
            time_difference = handle_iamat_time(correct_message, reach_time, iamat_content)

            # define the output message
            output_message = create_iamat_output_message(time_difference, correct_message)

            # flood the output message to corresponding servers based on the relationships between them
            flood_iamat_message(iamat_content)

    # This is a WHATSAT message
    elif message_form == flag_dict["WHATSAT"]:

        print("Checking WHATSAT message ...")

        # Check if the ClientID is in our stored client list
        if correct_message[1] not in clients:
            # Set up the error message
            output_message = invalid_response(message)

        else:

            # This is a valid WHATSAT message
            print("This is a valid WHATSAT message ...")

            # extract the client information based from ClientID in the client list
            client_info = extract_client_info(correct_message)

            # extract the client coordinate location
            client_location = extract_client_location(client_info)

            # calculate the time difference between sent and received
            # I dont need to handle negative time difference, because it comes along
            # I need to handle positive time difference by explicitly adding the
            time_difference = handle_whatsat_time(client_info)

            output_message = create_whatsat_output_message(client_info, time_difference)

            # Extract the client radius
            # Could be float
            client_radius = float(correct_message[2]) * 1000

            # set up the url
            url_link = LINK_BASE + API_KEY + '&location=' + str(client_location) + '&radius=' + str(client_radius)

            # use aiohttp and json
            async with aiohttp.ClientSession() as session:
                async with session.get(url_link) as resp:
                # achieve the url link
                    reply = await resp.json()
                    # format the result 
                    reply['results'] = reply['results'][:int(correct_message[3])]
                    output_message += json.dumps(reply, indent=3)
                    output_message += "\n\n"

    else:
        # Set up the error message
        output_message = invalid_response(message)

    return output_message


def record_flood_start(serv):
    # Write a log message to indicate the beginning of the connection
    log_file.write("Start connection at port {0} with server {1} --- ".format(port_dict[serv], serv))

def record_flood_success():
    log_file.write("The connection is successful\n")

def record_flood_fail():
    log_file.write("Fail to start the connection\n")

async def flood(msg_input, server):

    # The function will send the AT message and flood to the correct servers from a specific server

    # Let's loop through the communication dictionary
    for serv in server_communicate[server]:
        # Write a log message to indicate the beginning of the connection
        record_flood_start(serv)

        try:
            # Add loop=loop gives me failure of flood, dont know why ??
            # reader, writer = await asyncio.open_connection(IP, port_dict[serv], loop=loop)
            reader, writer = await asyncio.open_connection(IP, port_dict[serv])
            # If there is no error or anything happen, connection is considered as successful
            # write to the log about the success of the flood 
            record_flood_success()
            # write a connection success message and related server names 
            print("Flood success...")
            print(serv)
            writer.write(msg_input.encode())
            await writer.drain()
            writer.close()
        except:
            # Here is fail to connect to the server
            # Record the failure to the log
            record_flood_fail()
            # Print out the flood fail message and server names for flag 
            print("Flood Fail...")
            print(serv)
            pass

def extract_sign(input_coord):
    output = []
    # extract all the signs from the input and check the number
    # Put this in a function
    for i in range(len(input_coord)):
        # add to the list when it is either + or -
        if input_coord[i] == "-" or input_coord[i] == "+":
            output.append(i)

    return output

def check_store_sign_len(store_sign_in):
    output = ""
    if len(store_sign_in) != 2:
        output = None

    return output

def check_first_sign(store_sign_in):
    output = ""
    if store_sign_in[0] != 0:
        output = None

    return output

def check_last_index(input_coordinate, store_sign_in):
    output = ""
    last_index = len(input_coordinate) - 1
    # Check the last symbol of the input
    if last_index == store_sign_in[1]:
        output = None

    return output


# check for coordinates 
def verify_coordinate(input_coord):
    # we need to check the input has correct number of + or -
    store_sign = []

    # This is the output that we will be returning at the end
    latitude = None

    # extract all the signs from the input and check the number
    store_sign = extract_sign(input_coord)

    # Check if there are more than two signs or less than two, which are not allowed 
    latitude = check_store_sign_len(store_sign)

    # Check the first symbol of the input, it need to be either + or -

    latitude = check_first_sign(store_sign)

    # # Check the last symbol of the input

    latitude = check_last_index(input_coord, store_sign)

    # Get our output ready
    try:
        # First slot is the first part of the coordinate, second slot is the second part of the coordinate
        latitude = float(input_coord[:store_sign[1]]), float(input_coord[store_sign[1]:])
    except:
        pass

    return latitude

def check_iamat_time(input_message):
    # if the coordinate is correct, then try extract the timestamp
    output = None
    try:
        output = float(input_message[3])
    except:
        pass

    return output


def verify_iamat(input_message):
    # I need to check client coordinates
    if verify_coordinate(input_message[2]) is None:
        # incorrect client coordinates
        return return_dict["incorrect message"]
    else:

        # if the coordinate is correct, then try extract the timestamp 
        timestamp = None
        timestamp = check_iamat_time(input_message)

        # check if the time is still None, then it is incorrect
        if timestamp is None:
            return return_dict["incorrect message"]

        # This is indeed a correct IAMAT message
        return return_dict["good IAMAT"]




def check_whatsat_radius(input_message):
    output = None 

    try:
        output = float(input_message[2])
    except:
        pass

    return output

def check_whatsat_items(input_message):

    output = None 

    try:
        output = int(input_message[3])
    except:
        pass

    return output


def verify_whatsat(input_message):

    # extract the client radius for later checking 
    radius = None
    radius = check_whatsat_radius(input_message)

    # Check the value extracted into radius
    # radius is at most 50 km and upper bound is 0
    if radius is None or radius > 50 or radius <= 0:
        # return -1
        return return_dict["incorrect message"]
    else:
        # The number of the requested result
        number_of_items = None
        number_of_items = check_whatsat_items(input_message)

        # Check the extracted value in number of items
        # number of results is at most 20 items and upper bound is 0
        if number_of_items is None or number_of_items > 20 or number_of_items <= 0:
            return return_dict["incorrect message"]
        else:
            # this is correct WHATSAT message
            return return_dict["good WHATSAT"]           

def verify_updatemsg(input_message):
    # check whether the length is exact 6, the UPDATEMSG has total of six fields 
    if len(input_message) == 6:
        # This is a correct AT message
        return return_dict["good UPDATEMSG"]
    else:
        # This is an incorrect AT message
        return return_dict["incorrect message"]


def verify_input_message(input_message):

    # We have four different type of messages which are IAMAT, WHATSAT, UPDATEMSG, incorrect
    """
    Return values:
        1 = correct IAMAT message
        2 = correct WHATSAT message
        3 = correct UPDATEMSG message
        -1 = incorrect message
    """

    # check if the input is empty
    if len(input_message) < 1:
        # return error code when the input is empty 
        return -1

    # Message type 
    input_message_type = input_message[0]

    # Message length 
    input_message_length = len(input_message)

    # check if the message is IAMAT
    if input_message_type == "IAMAT":
        # if the received message is IAMAT
        # format : IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
        # format : IAMAT ClientID Coordinates Timestamp

        # Check whether the length of the message is exact 4
        if input_message_length == 4:
            return verify_iamat(input_message)

        else:
            # incorrect IAMAT message
            return return_dict["incorrect message"]

    # check if the message is WHATSAT message
    elif input_message_type == "WHATSAT":
        # if the message is WHATSAT message
        # format : WHATSAT kiwi.cs.ucla.edu 10 5
        # format : WHATSAT ClientID Radius MaxNumOfResult

        # Check whether the length of the message is exact 4
        if input_message_length == 4:
            return verify_whatsat(input_message)

        else:
            # incorrect WHATSAT message
            return return_dict["incorrect message"]

    # Check if the message is AT message
    elif input_message_type == "UPDATEMSG":
        # if the message is AT message
        # format : AT Hill +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
        return verify_updatemsg(input_message)

    else:
        # None of the message from above, just return error code
        return return_dict["incorrect message"]

def pre_input_doc(input_msg):
    # Print to the server side about the successful received input 
    print("Received Input: {} ".format(input_msg))
    # Print to the log file about the successful received input 
    log_file.write("Received Input: " + input_msg)

def afterward_doc(input_msg):
    # This is what we are sending back to the client
    # Print to the server side abput the successful sending input 
    print("Sending message: {} ".format(input_msg))
    # Print to the log file about the successful sending input
    log_file.write("Sending message: " + input_msg)

def wait_flood(input_msg):
    # Store the most updated information about the client in the client dictionary 
    clients[input_msg[1]] = input_msg
    # The flood message that share between the servers based on server communication relationship 
    asyncio.ensure_future(flood('UPDATEMSG {0}\n'.format(' '.join(input_msg[1:])), sys.argv[1]))

def handle_UPDATE(input_msg):
    # First check to see if the client is an existing client in the dictionary 
    if input_msg[1] in clients:
        print("Existing Client ... ")
        # Client is one of the existing client, check to see if we have already receievd or outdated message
        info = input_msg[1]
        if clients[info][3] < input_msg[3]:
            print("Flood ... ")
            wait_flood(input_msg)
    else:
        # Client is a new client, store it immediately and share it with the rest of the related servers 
        print("New Client and Update ... ")
        wait_flood(input_msg)

def clean_up_writer(writer, output_message):
    # Send back message 
    # Record down in the log file 
    afterward_doc(output_message)
    # Decode the output message 
    writer.write(output_message.encode())


async def process_input(reader, writer):
    # Try to read the data from the buffer file
    # Cannot use readline, give me error, not sure why 
    data = await reader.read(10000)
    # Need to decode the input data 
    input_message = data.decode()
    # Write to the log file after reading everything and ends with next line
    pre_input_doc(input_message)

    # We need to format the input before actually process it
    # Remove the extra spaces between the arguments and turn the input into a list
    correct_message = clean_message(input_message)

    # UPDATEMSG message need to be handled separately
    # Check for flooding
    print("Check for flooding for the input message.")
    # If the message type is flooding and the rest of the fields are correct, move on to next step 
    if correct_message[0] == "UPDATEMSG" and verify_input_message(correct_message):

        print("Handle flooding here ...")
        log_file.write("Handle flooding here ....")

        # handle flooding 
        handle_UPDATE(correct_message)

    else:
        # Prepare the output message
        output_message = await format_output(input_message, time.time())

        # write to the log file of the sending message
        # this is the send back message
        clean_up_writer(writer, output_message)
        await writer.drain()

def check_command_serv():
    # check whether user provided the correct name of the server 
    if sys.argv[1] not in port_dict:
        print("Wrong server name. Please provide appropriate name of the server (Hill, Smith, Singleton, Jaquez, Campbell).")
        sys.exit(1)

class Server:
    def __init__(self, name, ip='127.0.0.1', port=8888, message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port
        self.message_max_length = int(message_max_length)

    async def handle_echo(self, reader, writer):
        '''
        on server side
        '''
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        print("{} received {} from {}".format(self.name, message, addr))

        sendback_message = message

        print("{} send: {}".format(self.name, sendback_message))
        writer.write(sendback_message.encode())
        await writer.drain()

        print("close the client socket")
        writer.close()

    def run_until_interrupted(self):

        # check whether user provided the correct name of the server 
        check_command_serv()

        # create a global log file to record down the connection in convenience of debugging
        global log_file
        log_file = open(sys.argv[1] + "Log.txt", "w+")

        loop = asyncio.get_event_loop()
        # Replace with my own process input function 
        coro = asyncio.start_server(process_input, '127.0.0.1', port_dict[sys.argv[1]], loop=loop)
        server = loop.run_until_complete(coro)

        # Serve requests until Ctrl+C is pressed
        print('serving on {}'.format(server.sockets[0].getsockname()))

        try:
            loop.run_forever()
        except KeyboardInterrupt:
            pass
        # Close the server
        server.close()
        loop.run_until_complete(server.wait_closed())
        loop.close()
        log_file.close()


if __name__ == '__main__':
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    print("Hello, welcome to server {}".format(args.server_name))

    server = Server(args.server_name)
    server.run_until_interrupted()









