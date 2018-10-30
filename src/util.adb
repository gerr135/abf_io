--  Some common utility routines.
--  Author: George Shapovalov <gshapovalov@gmail.com>, (C) 2018
--  Copyright: See COPYING file that comes with this distribution

with Ada.Text_IO;

package body util is

    procedure DebugPrint(debug : Boolean; message : String; eol : Boolean := True) is
        use Ada.Text_IO;
    begin
        if not debug then return; end if;
        if eol then
            Put_Line(message);
        else
            Put(message);
        end if;
    end;


end util;
