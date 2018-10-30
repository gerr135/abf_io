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

    function CLong2String(int : C.long) return String is
        val, tmp : Integer;
        S : String(1..4) := "    ";
        use C;
    begin
        -- Strings are "fat arrays", so we cannot do unchecked_conversion,
        -- we need to convert byte by byte using Character'Val
        tmp := Integer(int);
        for i in 1 .. 4 loop
            val := tmp rem 256;
            tmp := tmp / 256;
            S(i) := Character'Val(val);
        end loop;
        return S;
    end;

    
end util;
