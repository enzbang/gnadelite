------------------------------------------------------------------------------
--                                 GnadeLite                                --
--                                                                          --
--                           Copyright (C) 2006                             --
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

with Ada.Containers.Indefinite_Vectors;

package DB is

   use Ada;

   DB_Error : exception;
   --  Raised for all errors reported by the database

   type Handle is abstract tagged private;

   --  Open / Close

   procedure Connect
     (DB       : in out Handle;
      Name     : in     String;
      User     : in     String := "";
      Password : in     String := "") is abstract;
   --  Open the database named Name

   procedure Close (DB : in out Handle) is abstract;
   --  Close the current database

   --  Transaction

   procedure Begin_Transaction (DB : in Handle) is abstract;
   --  Start a new transaction, do not support embedded transactions

   procedure Commit (DB : in Handle) is abstract;
   --  Commit the current transaction

   procedure Rollback (DB : in Handle) is abstract;
   --  Rollback the current transaction

   --  Statement

   type Iterator is abstract tagged private;

   package String_Vectors is new Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => String);

   procedure Prepare_Select
     (DB   : in     Handle;
      Iter : in out Iterator'Class;
      SQL  : in     String) is abstract;
   --  Prepare a select statement (SQL must be a select command)

   procedure Get_Line
     (Iter   : in out Iterator;
      Result :    out String_Vectors.Vector) is abstract;
   --  Returns the current row and move to the next one

   function More (Iter : in Iterator) return Boolean is abstract;
   --  Returns True if there is more data (row) to fetch

   procedure End_Select (Iter : in out Iterator) is abstract;
   --  Finalize a select statement

   procedure Execute (DB : in Handle; SQL : in String) is abstract;
   --  Execute SQL request into DB

   function Last_Insert_Rowid (DB : in Handle) return String is abstract;
   --  Returns the Id of the last inserted row id

private

   type Handle is abstract tagged null record;

   type Iterator is abstract tagged null record;

end DB;
