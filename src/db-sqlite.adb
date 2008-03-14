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

with Ada.Unchecked_Deallocation;

with Morzhol.Logs;

package body DB.SQLite is

   use GNU.DB;
   use Morzhol;

   Module : constant Logs.Module_Name := "DB_SQLITE";

   Unique_Handle : SQLite3.Handle := null;
   --  Unique handle to use when we want to use in memory connection

   procedure Unchecked_Free is new Unchecked_Deallocation
     (Object => SQLite3.Object, Name => SQLite3.Handle);

   procedure Check_Result
     (Routine : in String;
      Result  : in SQLite3.Return_Value);
   pragma Inline (Check_Result);
   --  Check result, raises and exception if it is an error code

   procedure Step_Internal (Iter : in out Iterator);
   --  Advance to the next row and set Iter.More

   --  Thread safe access to the SQLite database

   protected SQLite_Safe is

      procedure Close
        (DB : in Handle; Result : out Sqlite3.Return_Value);
      --  Close the database

      procedure Exec
        (DB : in Handle; SQL : in String; Result : out Sqlite3.Return_Value);
      --  Execute an SQL statement

      procedure Open
        (DB : in Handle; Name : in String; Result : out Sqlite3.Return_Value);
      --  Open the database

      function Prepare_Select
        (DB   : in Handle;
         Iter : in Standard.DB.Iterator'Class;
         SQL  : in String) return Standard.DB.Iterator'Class;
      --  Prepare a select statement

   end SQLite_Safe;

   -----------------------
   -- Begin_Transaction --
   -----------------------

   overriding procedure Begin_Transaction (DB : in Handle) is
   begin
      Logs.Write (Module, "begin");
      Execute (DB, "begin");
   end Begin_Transaction;

   ------------------
   -- Check_Result --
   ------------------

   procedure Check_Result
     (Routine : in String;
      Result  : in SQLite3.Return_Value)
   is
      use type SQLite3.Return_Value;
   begin
      if Result /= SQLite3.SQLITE_OK then
         Logs.Write
           (Name    => Module,
            Kind    => Logs.Error,
            Content => Logs.NV
              ("Return_Value", SQLite3.Return_Value'Image (Result))
              & ", " & Logs.NV ("routine", Routine));
         raise DB_Error
           with "SQLite: Error " & SQLite3.Return_Value'Image (Result) &
             " in " & Routine;
      end if;
   end Check_Result;

   -----------
   -- Close --
   -----------

   overriding procedure Close (DB : in out Handle) is
     Result : Sqlite3.Return_Value;
   begin
      Logs.Write (Module, "close");
      SQLite_Safe.Close (DB, Result);
      Check_Result ("close", Result);
      Unchecked_Free (DB.H);
   end Close;

   ------------
   -- Commit --
   ------------

   overriding procedure Commit (DB : in Handle) is
   begin
      Logs.Write (Module, "commit");
      Execute (DB, "commit");
   end Commit;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (DB       : in out Handle;
      Name     : in     String;
      User     : in     String := "";
      Password : in     String := "")
   is
      pragma Unreferenced (User, Password);
      use type GNU.DB.SQLite3.Handle;

      Result : Sqlite3.Return_Value;
   begin
      Logs.Write (Module, "connect " & Logs.NV ("Name", Name));

      if Name = In_Memory_Database then
         if Unique_Handle = null then
            --  Open only one database connection !

            Unique_Handle := new GNU.DB.SQLite3.Object;
            DB.H := Unique_Handle;
            SQLite_Safe.Open (DB, Name, Result);
            Check_Result ("connect", Result);

         elsif DB.H = null then
            --  Get the open database connection
            DB.H := Unique_Handle;
         end if;

      else
         if DB.H = null then
            DB.H := new GNU.DB.SQLite3.Object;
         end if;

         SQLite_Safe.Open (DB, Name, Result);
         Check_Result ("connect", Result);
      end if;
   end Connect;

   ----------------
   -- End_Select --
   ----------------

   overriding procedure End_Select (Iter : in out Iterator) is
   begin
      Logs.Write (Module, "end_select");
      Check_Result ("end_select", SQLite3.finalize (Iter.S'Unchecked_Access));
   end End_Select;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (DB : in Handle; SQL : in String) is
     Result : SQLite3.Return_Value;
   begin
      Logs.Write (Module, "execute : " & Logs.NV ("SQL", SQL));
      SQLite_Safe.Exec (DB, SQL, Result);
      Check_Result ("execute", Result);
   exception
      when DB_Error =>
         raise DB_Error with "DB_Error on Execute " & SQL;
   end Execute;

   --------------
   -- Get_Line --
   --------------

   overriding procedure Get_Line
     (Iter   : in out Iterator;
      Result :    out String_Vectors.Vector)
   is
      use type SQLite3.Return_Value;
   begin
      for K in 0 .. Iter.Col - 1 loop
         String_Vectors.Append
           (Result, SQLite3.column_text (Iter.S'Unchecked_Access, K));
      end loop;

      Step_Internal (Iter);
   end Get_Line;

   -----------------------
   -- Last_Insert_Rowid --
   -----------------------

   overriding function Last_Insert_Rowid (DB : in Handle) return String is
     Rowid : constant String :=
                 SQLite3.uint64'Image (SQLite3.Last_Insert_Rowid (DB.H));
   begin
      --  Skip first whitespace returned by 'Image
      return Rowid (Rowid'First + 1 .. Rowid'Last);
   end Last_Insert_Rowid;

   ----------
   -- More --
   ----------

   overriding function More (Iter : in Iterator) return Boolean is
   begin
      return Iter.More;
   end More;

   --------------------
   -- Prepare_Select --
   --------------------

   overriding procedure Prepare_Select
     (DB   : in     Handle;
      Iter : in out Standard.DB.Iterator'Class;
      SQL  : in     String)
   is
      use type SQLite3.Statement_Reference;
   begin
      Iter := SQLite_Safe.Prepare_Select (DB, Iter, SQL);
   end Prepare_Select;

   --------------
   -- Rollback --
   --------------

   overriding procedure Rollback (DB : in Handle) is
   begin
      Logs.Write (Module, "rollback");
      Execute (DB, "rollback");
   end Rollback;

   -----------------
   -- SQLite_Safe --
   -----------------

   protected body SQLite_Safe is

      -----------
      -- Close --
      -----------

      procedure Close
        (DB : in Handle; Result : out Sqlite3.Return_Value) is
      begin
         Result := SQLite3.Close (DB.H);
      end Close;

      ----------
      -- Exec --
      ----------

      procedure Exec
        (DB : in Handle; SQL : in String; Result : out Sqlite3.Return_Value) is
      begin
         Result := SQLite3.Exec (DB.H, SQL);
      end Exec;

      ----------
      -- Open --
      ----------

      procedure Open
        (DB : in Handle; Name : in String; Result : out Sqlite3.Return_Value) is
      begin
         Result := SQLite3.Open (DB.H, Name);
      end Open;

      --------------------
      -- Prepare_Select --
      --------------------

      function Prepare_Select
        (DB   : in Handle;
         Iter : in Standard.DB.Iterator'Class;
         SQL  : in String) return Standard.DB.Iterator'Class
      is
         use type SQLite3.Statement_Reference;
         Select_Iter : Standard.DB.Iterator'Class := Iter;
      begin
         pragma Assert (Select_Iter in Iterator);
         Logs.Write
           (Module, "prepare select : " & Logs.NV ("SQL", SQL));

         Iterator (Select_Iter).H := DB;
         Iterator (Select_Iter).More := False;

         Check_Result
           ("prepare_select",
            SQLite3.prepare (DB.H, SQL, Iterator (Select_Iter).S'Unchecked_Access));

         Iterator (Select_Iter).Col :=
           SQLite3.column_count (Iterator (Select_Iter).S'Unchecked_Access);

         Step_Internal (Iterator (Select_Iter));
         return Select_Iter;
      end Prepare_Select;

   end SQLite_Safe;

   -------------------
   -- Step_Internal --
   -------------------

   procedure Step_Internal (Iter : in out Iterator) is
      use type SQLite3.Return_Value;
      R : SQLite3.Return_Value;
   begin
      R := SQLite3.step (Iter.S'Unchecked_Access);

      if R = SQLite3.SQLITE_DONE then
         Iter.More := False;

      elsif R = SQLite3.SQLITE_ROW then
         Iter.More := True;

      else
         Check_Result ("step_internal", R);
         Iter.More := False;
      end if;
   end Step_Internal;

end DB.SQLite;
