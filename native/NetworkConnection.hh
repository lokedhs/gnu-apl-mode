/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright (C) 2014  Elias Mårtenson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef NETWORK_CONNECTION_HH
#define NETWORK_CONNECTION_HH

#include <pthread.h>
#include <string>
#include <vector>
#include <map>

#include "NetworkCommand.hh"

class NetworkConnection {
public:
    NetworkConnection( int socket_in );
    virtual ~NetworkConnection();
    void run( void );
    std::string read_line_from_fd( void );
    void write_string_to_fd( const std::string &s );
    std::vector<std::string> load_block( void );
    void send_reply( const std::string &message );
    void send_notification( const std::string &message );

private:
    int socket_fd;
    char buffer[1024];
    int buffer_pos;
    int buffer_length;
    std::map<std::string, NetworkCommand *> commands;
    pthread_mutex_t connection_lock;

    int process_command( const std::string &command );
    void show_si( void );
    void clear_si_stack( void );
    void send_function( const std::vector<std::string> &content );
    void show_function( const std::string &name );
};

class ConnectionError {
public:
    ConnectionError( const std::string &message_in ) : message( message_in ) {}
    virtual ~ConnectionError() {}
    std::string get_message( void ) { return message; }

protected:
    const std::string message;
};

class DisconnectedError : public ConnectionError {
public:
    DisconnectedError( const std::string &message ) : ConnectionError( message ) {};
};

class ProtocolError : public ConnectionError {
public:
    ProtocolError( const std::string &message ) : ConnectionError( message ) {};
};

#endif
