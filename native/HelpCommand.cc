/*
    This file is part of GNU APL, a free implementation of the
    ISO/IEC Standard 13751, "Programming Language APL, Extended"

    Copyright (C) 2018  Elias Mårtenson, Alexey Veretenniokv

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
#include "HelpCommand.hh"
#include "emacs.hh"
#include <sstream>
#include <assert.h>
#include <algorithm>

namespace {
// prepare string for sending:
// quotify, escape " and '\', convert newlines
inline std::string prepare_string(const string& string)
{
    std::string result;
    result.reserve(string.length()*1.25);
    result += "\"";
    for (string::size_type i = 0; i < string.length(); ++i) {
        switch (string[i]) {
        case '\n': 
            result += "\\n"; 
            break;
        case '"': // escape double quotes
        case '\\': // escape backslashes
            result += '\\';
            // Fall through.
        default:
            result += string[i];
        }
    }
    result += "\"";
    return result;
}

inline std::ostream & operator <<( std::ostream& stream, const HelpCommand::HelpEntry& entry) {
    // write HelpEntry as a S-expression to the stream
    stream << "(";
    stream << entry.arity << " ";
    stream << prepare_string(entry.symbol) << " ";
    stream << prepare_string(entry.name) << " ";
    stream << prepare_string(entry.short_desc) << " ";
    stream << prepare_string(entry.long_desc) << " ";
    stream << ")\n";
    return stream;
}

struct FindEntry {
    FindEntry(const std::string& prim) : symbol(prim) {}
    bool operator ()(const HelpCommand::HelpEntry& entry) {
        return entry.symbol == symbol;
    }
    std::string symbol;
};
    
} // anonymous namespace


HelpCommand::HelpCommand( std::string name_in ) : NetworkCommand( name_in ) {
#define help_def(ar, prim, name, title, descr)  \
    help_entries.push_back(HelpEntry(ar, prim, name, title, descr));
#include "../Help.def"
    
}
    

void HelpCommand::run_command( NetworkConnection &conn, const std::vector<std::string> &args )
{
    stringstream out;
    HelpEntries::iterator it = help_entries.begin(),
        end = help_entries.end();
    
    if (args.size() > 1) {
        FindEntry predicate(args[1]);
        bool found = false;
        while ((it = std::find_if(it, end, predicate)) != end) {
            out << *it++;
            found = true;
        }
        if (!found)
            out << "nil" << "\n";
    } else {
        for (; it != help_entries.end(); ++ it)
            out << *it;
    }
    out << END_TAG << "\n";
        
    conn.write_string_to_fd( out.str() );
}
