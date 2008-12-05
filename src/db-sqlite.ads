------------------------------------------------------------------------------
--                                GnadeLite                                 --
--                                                                          --
--                         Copyright (C) 2006-2008                          --
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

private with sqlite3_h;
private with Interfaces.C;

package DB.SQLite is

   DB_Error : exception renames DB.DB_Error;

   type Handle is new DB.Handle with private;

   In_Memory_Database : constant String := ":memory:";
   --  When the database name is ":memory:", a private in-memory
   --  database is created for the connection.  This in-memory
   --  database will vanish when the database connection is closed.

   overriding procedure Connect
     (DB       : in out Handle;
      Name     : in     String;
      User     : in     String := "";
      Password : in     String := "");
   --  Open the database named Name

   overriding procedure Close (DB : in out Handle);
   --  Close the current database

   --  Transaction

   overriding procedure Begin_Transaction (DB : in Handle);
   --  Start a new transaction, do not support embedded transactions

   overriding procedure Commit (DB : in Handle);
   --  Commit the current transaction

   overriding procedure Rollback (DB : in Handle);
   --  Rollback the current transaction

   --  Statement

   type Iterator is new DB.Iterator with private;

   overriding procedure Prepare_Select
     (DB   : in     Handle;
      Iter : in out Standard.DB.Iterator'Class;
      SQL  : in     String);
   --  Prepare a select statement (SQL must be a select command)

   overriding procedure Get_Line
     (Iter   : in out Iterator;
      Result :    out String_Vectors.Vector);
   --  Returns the current row and move to the next one

   overriding function More (Iter : in Iterator) return Boolean;
   --  Returns True if there is more data (row) to fetch

   overriding procedure End_Select (Iter : in out Iterator);

   overriding procedure Execute (DB : in Handle; SQL : in String);
   --  Execute SQL request into DB. Raise DB_Error in case of failure.

   overriding function Last_Insert_Rowid (DB : in Handle) return String;
   --  Returns the Id of the last inserted row id

private

   type Handle is new DB.Handle with record
      H : aliased sqlite3_h.Handle_Access;
   end record;

   type Iterator is new DB.Iterator with record
      H    : Handle;
      S    : aliased sqlite3_h.Statement_Access := null;
      Col  : Interfaces.C.int;
      More : Boolean;
   end record;

end DB.SQLite;
