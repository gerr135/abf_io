--
--  Some common utility routines.
--
--  Author: George Shapovalov <gshapovalov@gmail.com>, (C) 2018
--
--  Copyright: See COPYING file that comes with this distribution
--

with C;

package util is

    procedure DebugPrint(debug : Boolean; message : String; eol : Boolean := True);

    function CLong2String(int : C.long) return String;

--     ToDo : exception;

end util;
