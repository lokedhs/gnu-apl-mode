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

#ifndef HELP_COMMAND_HH
#define HELP_COMMAND_HH

#include "NetworkCommand.hh"
#include "emacs.hh"
#include <vector>

// help command returns the entries from Help.def in
// form of S-expressions.
// It could either accept 1 argument - the APL symbol
// to describe, or no arguments - in this case all
// entries from Help.def will be returned
// If no entry found, "nil" is returned so it is
// safe to "read" on Emacs Lisp side.
class HelpCommand : public NetworkCommand {
public:
    struct HelpEntry
    {
        HelpEntry() {}
        HelpEntry(int ar,
                  const char* prim,
                  const char* arg_name,
                  const char* title,
                  const char* descr) :
            arity(ar),
            symbol(prim),
            name(arg_name),
            short_desc(title),
            long_desc(descr) {}
        int arity;
        std::string symbol;
        std::string name;
        std::string short_desc;
        std::string long_desc;
    };
    typedef std::vector<HelpEntry> HelpEntries;
public:
    HelpCommand( std::string name_in );
    virtual void run_command( NetworkConnection &conn, const std::vector<std::string> &args );
private: // variables
    HelpEntries help_entries;
};

#endif
