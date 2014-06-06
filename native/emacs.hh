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

#ifndef EMACS_HH
#define EMACS_HH

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunknown-pragmas"
#pragma GCC diagnostic ignored "-Wpragmas"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wreturn-type"
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wreorder"
#pragma GCC diagnostic ignored "-Wmismatched-tags"
#pragma GCC diagnostic ignored "-Woverloaded-virtual"
#include "../Native_interface.hh"
#pragma GCC diagnostic pop

void set_active( bool v );

#define PROTOCOL_VERSION "1.4"

#define END_TAG "APL_NATIVE_END_TAG"
#define NOTIFICATION_START_TAG "APL_NATIVE_NOTIFICATION_START"
#define NOTIFICATION_END_TAG "APL_NATIVE_NOTIFICATION_END"

class ActiveWrapper
{
public:
    ActiveWrapper() { set_active( true ); };
    virtual ~ActiveWrapper() { set_active( false ); };
};

const UCS_string ucs_string_from_string( const std::string &string );
Value_P make_string_cell( const std::string &string, const char *loc );

/// return a UTF8 encoded std:string
inline std::string to_string(const UCS_string & ucs)
{
    const UTF8_string utf(ucs);
    return string((const char *)(utf.get_items()), utf.size());
}

#endif
