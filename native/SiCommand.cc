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

#include "NetworkConnection.hh"
#include "SiCommand.hh"
#include "emacs.hh"

void SiCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    std::stringstream out;
    for( const StateIndicator *si = Workspace::SI_top() ; si ; si = si->get_parent() ) {
        out << si->function_name() << "\n";
    }
    out << END_TAG << "\n";

    conn.write_string_to_fd( out.str() );
}
