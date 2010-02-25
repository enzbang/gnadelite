------------------------------------------------------------------------------
--                                GnadeLite                                 --
--                                                                          --
--                         Copyright (C) 2006-2010                          --
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

with Interfaces.C.Strings;
with System;

with Morzhol.Logs;

package body DB.SQLite is

   use Morzhol;
   use Interfaces.C;
   use type sqlite3_h.Handle_Access;

   Module : constant Logs.Module_Name := "DB_SQLITE";

   Unique_Handle : sqlite3_h.Handle_Access := null;
   --  Unique handle to use when we want to use in memory connection

   procedure Check_Result
     (Routine   : in String;
      Result    : in int;
      Error_Msg : in Strings.chars_ptr := Strings.Null_Ptr);
   --  Check result, raises and exception if it is an error code

   procedure Step_Internal (Iter : in out Iterator);
   --  Advance to the next row and set Iter.More

   --  Thread safe access to the SQLite database

   protected SQLite_Safe is

      procedure Close
        (DB : in out Handle; Result : out int);
      --  Close the database
      pragma Precondition (DB.H /= null);

      procedure Exec
        (DB     : in Handle;
         SQL    : in String);
      --  Execute an SQL statement
      --  Raise DB_Error is case of failure
      pragma Precondition (SQL /= "");
      pragma Precondition (DB.H /= null);
      pragma Postcondition (DB.H /= null);

      procedure Open
        (DB     : in out Handle;
         Name   : in     String;
         Result :    out int);
      --  Open the database
      pragma Precondition (Name /= "");

      function Prepare_Select
        (DB   : in Handle;
         Iter : in Standard.DB.Iterator'Class;
         SQL  : in String) return Standard.DB.Iterator'Class;
      --  Prepare a select statement
      pragma Precondition (SQL /= "");
      pragma Precondition (DB.H /= null);
      pragma Postcondition (DB.H /= null);

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
     (Routine   : in String;
      Result    : in int;
      Error_Msg : in Strings.chars_ptr := Strings.Null_Ptr)
   is
      use type sqlite3_h.sqlite_result;

      DB_Result : sqlite3_h.sqlite_result;
      for DB_Result'Address use Result'Address;

      function Error_Message return String;
      --  Returns and free Error_Msg content if not null

      -------------------
      -- Error_Message --
      -------------------

      function Error_Message return String is
         use type Strings.chars_ptr;
      begin
         if Error_Msg = Strings.Null_Ptr then
            return "";

         else
            Free : declare
               V : constant String := Strings.Value (Error_Msg);
            begin
               sqlite3_h.sqlite3_free (Error_Msg'Address);
               return V;
            end Free;
         end if;
      end Error_Message;

   begin
      if not DB_Result'Valid then
         Logs.Write
           (Name    => Module,
            Kind    => Logs.Error,
            Content => "SQLite3 has return an unknown result in !" & Routine);
         raise DB_Error with "SQlite: Error (Unknown Error) in " & Routine;
      end if;

      if DB_Result /= sqlite3_h.SQLITE_OK then
         Logs.Write
           (Name    => Module,
            Kind    => Logs.Error,
            Content => Logs.NV
              ("Return_Value", sqlite3_h.sqlite_result'Image (DB_Result))
            & ", " & Logs.NV ("routine", Routine)
            & ", " & Logs.NV ("message", Error_Message));
         raise DB_Error
           with "SQLite: Error "
             & sqlite3_h.sqlite_result'Image (DB_Result) & " in " & Routine;
      end if;
   end Check_Result;

   -----------
   -- Close --
   -----------

   overriding procedure Close (DB : in out Handle) is
      Result : int;
   begin
      Logs.Write (Module, "close");
      SQLite_Safe.Close (DB, Result);
      Check_Result ("close", Result);
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

      Result : int;
   begin
      Logs.Write (Module, "connect " & Logs.NV ("Name", Name));
      SQLite_Safe.Open (DB, Name, Result);
      Check_Result ("connect", Result);
   end Connect;

   ----------------
   -- End_Select --
   ----------------

   overriding procedure End_Select (Iter : in out Iterator) is
   begin
      Logs.Write (Module, "end_select");
      Check_Result ("end_select_reset",
                    sqlite3_h.sqlite3_reset (Iter.S.all'Address));
      Check_Result ("end_select",
                    sqlite3_h.sqlite3_finalize (Iter.S.all'Address));
   end End_Select;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (DB : in Handle; SQL : in String) is
   begin
      Logs.Write (Module, "execute : " & Logs.NV ("SQL", SQL));
      SQLite_Safe.Exec (DB, SQL);
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
   begin
      Result.Clear;
      for K in 0 .. Iter.Col - 1 loop
         declare
            Text : constant Strings.chars_ptr :=
                     sqlite3_h.sqlite3_column_text (Iter.S.all'Address, K);
            use type Strings.chars_ptr;
         begin
            if Text /= Strings.Null_Ptr then
               String_Vectors.Append
                 (Result,
                  Interfaces.C.Strings.Value (Text));
            else
               String_Vectors.Append (Result, "");
            end if;
         end;
      end loop;

      Step_Internal (Iter);
   end Get_Line;

   -----------------------
   -- Last_Insert_Rowid --
   -----------------------

   overriding function Last_Insert_Rowid (DB : in Handle) return String is
      Rowid : constant String :=
                sqlite3_h.sqlite_int64'Image
                  (sqlite3_h.sqlite3_last_insert_rowid (DB.H.all'Address));
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
        (DB : in out Handle; Result : out int) is
      begin
         if DB.Ref_Count /= 0 then
            DB.Ref_Count := DB.Ref_Count - 1;
            Result := 0; --  SQLite3_OK;
         else
            Result := sqlite3_h.sqlite3_close (DB.H.all'Address);
         end if;
      end Close;

      ----------
      -- Exec --
      ----------

      procedure Exec
        (DB : in Handle; SQL : in String)
      is
         SQL_Stat  : Strings.chars_ptr := Strings.New_String (SQL);
         Result    : int;
         Error_Msg : Strings.chars_ptr := Strings.Null_Ptr;
      begin
         Result := sqlite3_h.sqlite3_exec_no_callback
           (DB.H.all'Address, SQL_Stat, System.Null_Address,
            System.Null_Address, Error_Msg'Address);

         --  Free

         Strings.Free (SQL_Stat);

         Check_Result ("Execute", Result, Error_Msg);
      end Exec;

      ----------
      -- Open --
      ----------

      procedure Open
        (DB     : in out Handle;
         Name   : in     String;
         Result :    out int) is

         procedure Open_Db;
         --  Open a database connection

         -------------
         -- Open_Db --
         -------------

         procedure Open_Db is
            SQL_Name : Strings.chars_ptr := Strings.New_String (Name);
         begin
            Result := sqlite3_h.sqlite3_open (SQL_Name, DB.H'Address);
            Strings.Free (SQL_Name);
         end Open_Db;

      begin
         if Name = In_Memory_Database then
            if Unique_Handle = null then
               --  Open only one database connection !
               Open_Db;
               Unique_Handle := DB.H;

            elsif DB.H = null then
               --  Get the open database connection
               DB.H   := Unique_Handle;
               Result := 0; --  SQLite_OK

               --  Increment the reference counter
               DB.Ref_Count := DB.Ref_Count + 1;

            else
               --  Nothing to do. Return OK
               Result := 0; -- SQLite_OK
            end if;

         else
            Open_Db;
         end if;
      end Open;

      --------------------
      -- Prepare_Select --
      --------------------

      function Prepare_Select
        (DB   : in Handle;
         Iter : in Standard.DB.Iterator'Class;
         SQL  : in String) return Standard.DB.Iterator'Class
      is
         Select_Iter : Standard.DB.Iterator'Class := Iter;
         zSql        : Strings.chars_ptr := Strings.New_String (SQL);
         Select_Res  : int;
      begin
         pragma Assert (Select_Iter in Iterator);

         Logs.Write
           (Module, "prepare select : " & Logs.NV ("SQL", SQL));

         Iterator (Select_Iter).H := DB;
         Iterator (Select_Iter).More := False;

         Select_Res := sqlite3_h.sqlite3_prepare_v2
              (db     => DB.H.all'Address,
               zSql   => zSql,
               nByte  => SQL'Length + 1,
               ppStmt => Iterator (Select_Iter).S'Address,
               pzTail => System.Null_Address);

         Strings.Free (zSql);

         Check_Result ("prepare_select", Select_Res);

         Column_Count : declare
            use type sqlite3_h.sqlite_result;

            DB_Result : sqlite3_h.sqlite_result;
            for DB_Result'Address use Select_Res'Address;
         begin
            if DB_Result = sqlite3_h.SQLITE_DONE then
               Iterator (Select_Iter).Col := 0;
            else
               Iterator (Select_Iter).Col :=
                 sqlite3_h.sqlite3_column_count
                   (Iterator (Select_Iter).S.all'Address);
               Step_Internal (Iterator (Select_Iter));
            end if;
         end Column_Count;

         return Select_Iter;
      end Prepare_Select;

   end SQLite_Safe;

   -------------------
   -- Step_Internal --
   -------------------

   procedure Step_Internal (Iter : in out Iterator) is
      R : int;
   begin
      R := sqlite3_h.sqlite3_step (Iter.S.all'Address);

      Analyse_Result : declare
         Result : sqlite3_h.sqlite_result;
         for Result'Address use R'Address;

         use type sqlite3_h.sqlite_result;
      begin
         if not Result'Valid then
            raise DB_Error with "Wrong result from sqlite3_step ?";
         else
            if Result = sqlite3_h.SQLITE_DONE then
               Iter.More := False;
            elsif Result = sqlite3_h.SQLITE_ROW then
               Iter.More := True;
            else
               Check_Result ("step_internal", R);
               Iter.More := False;
            end if;
         end if;
      end Analyse_Result;
   end Step_Internal;

begin
   --  sqlite3_initialize is present only in very recent SQLite3
   --  versions and it is safe to disable the call for now
   --  Check_Result ("initialize", sqlite3_h.sqlite3_initialize);
   null;
end DB.SQLite;
