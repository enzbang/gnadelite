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

with Ada.Directories;
with Ada.Exceptions;
with Ada.Task_Attributes;
with Ada.Text_IO;

with DB.SQLite;
with DB.Tools;

with Morzhol.Logs;

procedure T1 is

   use Ada;
   use Ada.Exceptions;

   Max_Insert : constant Positive := 10000;
   Verbose    : constant Boolean := False;

   task type Inserts is
      entry Start (Id : in Positive);
   end Inserts;

   task type Selects is
      entry Start (Id : in Positive);
   end Selects;

   type TLS_DBH is record
      Handle    : access DB.Handle'Class;
      Connected : Boolean;
   end record;

   type TLS_DBH_Access is access all TLS_DBH;

   Null_DBH : constant TLS_DBH :=
                TLS_DBH'(Handle => null, Connected => False);

   DB_Path : constant String := DB.SQLite.In_Memory_Database;

   package DBH_TLS is
     new Task_Attributes (Attribute => TLS_DBH, Initial_Value => Null_DBH);

   procedure Connect (DBH : in TLS_DBH_Access);
   --  Connects to the database if needed

   -------------
   -- Connect --
   -------------

   procedure Connect (DBH : in TLS_DBH_Access) is
   begin
      if not DBH.Connected then
         DBH.Handle := new DB.SQLite.Handle;
         DBH.Handle.Connect (DB_Path);
         DBH.Connected := True;
         DBH_TLS.Set_Value (DBH.all);
      end if;
   end Connect;

   -------------
   -- Inserts --
   -------------

   task body Inserts is
      DBH     : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Task_Id : Positive;
      Current : Positive := 1;
   begin
      Connect (DBH);

      accept Start (Id : in Positive) do
         Task_Id := Id;

         --  Insert a first element
         declare
            SQL : constant String :=
                    "insert into test (counter, tid) values ("
                      & Positive'Image (Current) & ", "
                      & Positive'Image (Task_Id) & ")";
         begin
            DBH.Handle.Execute (SQL);
         end;
         Current := Current + 1;
      end Start;

      loop
         declare
            SQL : constant String :=
                    "insert into test (counter, tid) values ("
                      & Positive'Image (Current) & ", "
                      & Positive'Image (Task_Id) & ")";
         begin
            DBH.Handle.Execute (SQL);
         end;

         exit when Current = Max_Insert;
         Current := Current + 1;
      end loop;

      DBH.Handle.Close;
   exception
      when E : others =>
         Text_IO.Put_Line
           ("Inserts" & Positive'Image (Task_Id) & "; "
            & Exception_Information (E));
   end Inserts;

   -------------
   -- Selects --
   -------------

   task body Selects is
      DBH     : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Task_Id : Positive;
      Last    : Natural := 1;
   begin
      accept Start (Id : in Positive) do
         Task_Id := Id;
         if Verbose then
            Text_IO.Put_Line ("Reader " & Positive'Image (Task_Id) & " start");
         end if;
         Connect (DBH);
      end Start;

      loop
         declare
            Iter : DB.SQLite.Iterator;
            Line : DB.String_Vectors.Vector;
         begin
            DBH.Handle.Prepare_Select
              (Iter, "select max(counter), tid from test");

            if Iter.More then
               Iter.Get_Line (Line);

               if DB.String_Vectors.Element (Line, 1) /= "" then
                  if Natural'Value
                    (DB.String_Vectors.Element (Line, 1)) = Max_Insert
                  then
                     Text_IO.Put_Line ("Reader "
                                       & Positive'Image (Task_Id)
                                       & " Stop successfully");
                     Line.Clear;
                     Iter.End_Select;
                     exit;
                  end if;

                  if Verbose and then Natural'Value
                    (DB.String_Vectors.Element (Line, 1)) /= Last
                  then
                     Last := Natural'Value
                       (DB.String_Vectors.Element (Line, 1));
                     Text_IO.Put_Line
                       ("Reader " & Positive'Image (Task_Id) & " see "
                        & DB.String_Vectors.Element (Line, 1) & " write by "
                        & DB.String_Vectors.Element (Line, 2));
                  end if;
               end if;
               Line.Clear;
            end if;
            Iter.End_Select;
         end;
      end loop;

      DBH.Handle.Close;
   exception
      when E : others =>
         Text_IO.Put_Line
           ("Selects" & Positive'Image (Task_Id) & "; "
            & Exception_Information (E));
   end Selects;

begin
   Morzhol.Logs.Set (Morzhol.Logs.Information, False);
   Morzhol.Logs.Set (Morzhol.Logs.Warnings, False);
   Morzhol.Logs.Set (Morzhol.Logs.Error, True); --  show errors

   Create_DB : declare
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
   begin
      Connect (DBH);
      DBH.Handle.Execute
        ("create table test ('Counter' boolean, 'tid' integer)");
   end Create_DB;

   declare
      Writers : array (1 .. 2) of Inserts;
      Readers : array (1 .. 10) of Selects;
   begin
      --  Start the readers

      for K in Readers'Range loop
         Readers (K).Start (K);
      end loop;

      --  Start the writer

      for K in Writers'Range loop
         Writers (K).Start (K);
      end loop;
   end;

end T1;
