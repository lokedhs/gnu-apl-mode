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

#include "TempFileWrapper.hh"

#include <stdlib.h>
#include "emacs.hh"

FileWrapper::FileWrapper( int fd_in )
    : fd( fd_in )
{
    if( fd == -1 ) {
        abort();
    }
}

FileWrapper::~FileWrapper()
{
    close( fd );
}

TempFileWrapper::TempFileWrapper( const std::string &prefix )
{
    stringstream name_buf;
    name_buf << prefix << "XXXXXX";
    const char *name_cstr = name_buf.str().c_str();
    DynArray( char, tmp_name, strlen( name_cstr ) + 1 );
    strcpy( tmp_name, name_cstr );

    fd = mkstemp( tmp_name );
    if( fd == -1 ) {
        abort();
    }

    name = tmp_name;
    closed = false;
}

TempFileWrapper::~TempFileWrapper()
{
    if( !closed ) {
        ::close( fd );
    }
    unlink( name.c_str() );
}

void TempFileWrapper::close()
{
    if( !closed ) {
        ::close( fd );
        closed = true;
    }
}
