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

#include "emacs.hh"
#include "util.hh"
#include "../InputFile.hh"

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
    for( vector<string>::iterator i = content.begin() ; i != content.end() ; i++ ) {
        stringstream s;
        s << *i << "\n";
        write_string_to_fd( s.str(), fd.get_fd() );
    }
    fd.close();

    try {
        FILE *handle = fopen( fd.get_name().c_str(), "r" );
        if( handle == NULL ) {
            throw ConnectionError( "Unable to open generated temp file" );
        }
        const UTF8_string utfname( name.c_str() );
        InputFile fam( utfname, handle, false, false, true, false );
        fam.set_line_no( line );
        InputFile::files_todo.insert( InputFile::files_todo.begin(), fam );

        stringstream out;
        out << "content sent\n"
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
