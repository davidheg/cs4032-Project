# Module Project: Distributed File System

## Introduction
The assignment was to design and implement a distributed file system exhibiting a set of 7 features, 
which should be constructed using REST services, written in Haskell using the Servant library.

These features are:

    Distributed Transparent File Access
    Security Service
    Directory Service
    Replication
    Caching
    Transactions
    Lock Service


## Design
For my project, I started by planning out each of the features that I would be implementing from a 
high-level viewpoint e.g. how the user would request access to the server, and how the files would be 
transfered.

I then spent a while looking through the use-haskell and use-haskell-api templates, in order to understand
how the servant code worked, and to try and get a better grasp of haskell. This section of the process 
took longer than I had planned, and led to me starting actual coding later than i had hoped. Additionally,
I had trouble setting up the use-haskell-client so I decided to forgo it and create my own client-proxy.

### File Server
The first feature I began work on was the client-server arcchitecture that would serve as the core of the 
file system. I started this by buidling my own client, and ensuring that it could communicate with the
use-haskell server, and both store and obtain files from it, using its API. Once I had managed to do so, 
I moved onto to creating a custom datatype that would represent the files that would be sent and obtained.
Thus, I stored the use-haskell-api in a seperate project on my github, and redirected the server to pull
from that repo instead of the default one (see bellow for the link to that repository). Next, I added two
new methods to the API that would allow them to store and retrieve a file stored as one of these filetypes,
then implemented those in the server. I then intergrated functionaliuty to upload a file to the client,
send messages to the server to store or retrieve a file, and store a file that had been retrieved from the
server onto a user's computer. Once the basic upload/download functionalityh was working, I implemented a
polling service in the client that would regulairly check to see if a new version of a file had been implemented, 
and if it had to download it.

### Authentication
Next I began work on the authentication server. I used the default use-haskell template as the basis, then implemented
a new API for it, with methods to register and login, and datatypes to store user information. I then began to implement
a three key AES encryption service. I created datatypes that would represent tokens and tickets, which would contain the
datatypes that represented users and files, which would in turn be stored in datatypes representing encrypted requests and
responses, to-and-from the authentication server and client, and server and client respectively. When the client sent a
login request to the server, it would be encrypted using a key derived from their password. This would be decrypted by the 
AS using the password on record, and if the client information matched a token would be generated. The ticket within the token
would be encrypted using a server key that only the AS and file server possessed, and the token itself would be encrypted using
the password-derived key. This would be sent to and decrypted by the client, which would then use the token and ticet to encrypt 
and decrypt requests and responses from the server.

### Lock Server
Once I had authentication working, I began work on the lock server. Again, I used the base use-haskell server as a basis,
and created a new API for it that would handle users requesting the lock for a file, and returning a lock when it was complete.
If the lock was free, the server would return the ID for the lock and the lock would be set to be in use by said user.
The ID would be used by the client to obtain the file from the server. Said ID was generated by the server once a new file was uploaded, 
and sent to the lock server. Once a user was done with a lock, they would return the lock to the user to free it up for a new
user. 

## Implementation 
For my implementation of the datatypes that would be used to store information e.g. Files, while being sent between the servers and clients,
when sending more than one as a time I would create a datatype that would contain varaibles that would be of the types that I wanted to send
e.g. EncryptedRequest would contain a file type and a user type. However, this approach was unsuccesfull, as I was unable to create a method
to convert my custom datatypes to and from Bson. Thus, all custom types stored within other types are stored as strings, and are 
converted back into their actual datatype on arrival.

Concerning the encryption of the communication between the clients and servers, the actual JSON bodies of messages are not encrypted
but rather the contents of each datatype are encrypted as string, then converted into JSONS and added to messages. On arrival, they
are then converted back into their datatype, and the contents are decrypted back into their normal form. If these strings 
represent custom datatypes, they are converted into their actual datatypes. 

All information stored on a server are recorded in a MONGO database unique to each server.

##API
Please see https://github.com/davidheg/Cs4032-use-haskell-api for the servant APIs that was used for this project 
