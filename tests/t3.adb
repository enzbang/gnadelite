------------------------------------------------------------------------------
--                                GnadeLite                                 --
--                                                                          --
--                            Copyright (C) 2010                            --
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
with Ada.Exceptions;

with DB.SQLite;

with Morzhol.Logs;

procedure T3 is
   use Ada;
   use Ada.Exceptions;

   Handle : DB.SQLite.Handle;
begin
   Morzhol.Logs.Set (Morzhol.Logs.Information, False);
   Morzhol.Logs.Set (Morzhol.Logs.Warnings, False);
   Morzhol.Logs.Set (Morzhol.Logs.Error, False);


   Handle.Connect (DB.SQLite.In_Memory_Database);

   Handle.Execute ("update user set toto='toto'");

   Handle.Close;

   Text_IO.Put_Line ("NOK");

exception
   when E : others =>
      if Exception_Message (E)
        = "DB_Error on Execute update user set toto='toto'"
      then
         Text_IO.Put_Line ("OK");
      else
         Text_IO.Put_Line ("NOK: " & Exception_Message (E));
      end if;
end T3;
