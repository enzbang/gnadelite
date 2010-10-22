------------------------------------------------------------------------------
--                                GnadeLite                                 --
--                                                                          --
--                            Copyright (C) 2008                            --
--                      Pascal Obry - Olivier Ramonat                       --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.       --
------------------------------------------------------------------------------

with Ada.Text_IO;

with DB.SQLite;

with Morzhol.Logs;

procedure T2 is

   use Ada;

   Handle : DB.SQLite.Handle;
begin
   Morzhol.Logs.Set (Morzhol.Logs.Information, False);
   Morzhol.Logs.Set (Morzhol.Logs.Warnings, False);
   Morzhol.Logs.Set (Morzhol.Logs.Error, False);


   Handle.Connect (DB.SQLite.In_Memory_Database);

   Handle.Execute
     ("create table t ('a' integer, 'b' integer);"
      & "insert into t (a, b) values (1, 2);"
      & "insert into t (a, b) values (3, 4);");

   declare
      Iter : DB.SQLite.Iterator;
      Line : DB.String_Vectors.Vector;
   begin
      Handle.Prepare_Select
        (Iter, "select * from t");

      while Iter.More loop
         Iter.Get_Line (Line);

         if DB.String_Vectors.Element (Line, 1) /= "" then
            Text_IO.Put (DB.String_Vectors.Element (Line, 1));
            Text_IO.Put (DB.String_Vectors.Element (Line, 2));
         end if;
         Line.Clear;
      end loop;
      Iter.End_Select;
   end;

   Handle.Close;

end T2;
