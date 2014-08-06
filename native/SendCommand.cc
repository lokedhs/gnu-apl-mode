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

#include "SendCommand.hh"
#include "NetworkConnection.hh"
#include "TempFileWrapper.hh"
#include "../Archive.hh"

#include "emacs.hh"
#include "util.hh"

#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

static void throw_with_error( const string &message )
{
    stringstream out;
    out << message << ": " << strerror( errno );
    throw ConnectionError( out.str() );
}

static void write_string_to_fd( const string &line, int fd )
{
    ssize_t ret = write( fd, line.c_str(), line.size() );
    if( ret == -1 ) {
        throw_with_error( "Error writing block" );
    }
    else if( ret != (ssize_t)line.size() ) {
        throw ConnectionError( "Incomplete write of buffer" );
    }
}

void SendCommand::run_command( NetworkConnection &conn, const vector<string> &args )
{
    vector<string> content = conn.load_block();

    if( args.size() > 3 ) {
        throw ConnectionError( "Illegal argument count" );
    }

    string name = "";
    int line = 0;
    if( args.size() > 1 ) {
        name = args[1];
        if( args.size() > 2 ) {
            line = strtol( args[2].c_str(), NULL, 10 );
            if( line < 0 ) {
                throw ConnectionError( "Illegal value for line" );
            }
        }
    }

    TempFileWrapper fd( "/tmp/apl_content" );
    if( line > 0 ) {
        stringstream buf;
        for( int i = 0 ; i < line ; i++ ) {
            buf << "\n";
        }
        write_string_to_fd( buf.str(), fd.get_fd() );
    }
    for( vector<string>::iterator i = content.begin() ; i != content.end() ; i++ ) {
        stringstream s;
        s << *i << "\n";
        write_string_to_fd( s.str(), fd.get_fd() );
    }
    fd.close();

    try {
        int dump_fd = -1;
        XML_Loading_Archive in( fd.get_name().c_str(), dump_fd );
        if( dump_fd == -1 ) {
            throw ConnectionError( "Failed to open temp file" );
        }

        Workspace::load_DUMP( COUT, UTF8_string( fd.get_name().c_str() ), dump_fd, true );
        if( !in.is_open() ) {
            throw ConnectionError( "Error loading dump file" );
        }

        in.read_vids();
        in.read_Workspace();

        stringstream out;
        out << "file loaded\n"
            << END_TAG << "\n";
        conn.write_string_to_fd( out.str() );
    }
    catch( Error &error ) {
        stringstream out;
        out << "error\n"
            << END_TAG << "\n";
        conn.write_string_to_fd( out.str() );
    }
}
