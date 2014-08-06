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

#ifndef TEMP_FILE_WRAPPER_HH
#define TEMP_FILE_WRAPPER_HH

#include <vector>
#include <string>

class FileWrapper {
public:
    FileWrapper( int fd_in );
    ~FileWrapper();

private:
    int fd;
};

class TempFileWrapper {
public:
    TempFileWrapper( const std::string &prefix );
    ~TempFileWrapper();
    const std::string &get_name() { return name; }
    int get_fd() { return fd; }
    void close();

private:
    std::string name;
    int fd;
    bool closed;
};

#endif
