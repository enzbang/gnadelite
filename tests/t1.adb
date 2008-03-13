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
      DBH : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Task_Id    : Positive;
      Current    : Positive := 1;
   begin
      Connect (DBH);

      accept Start (Id : in Positive) do
         Task_Id := Id;

         --  Insert a first element
         declare
            SQL : constant String := "insert into test (counter, tid) values ("
              & Positive'Image (Current) & ", " & Positive'Image (Task_Id) & ")";
         begin
            DBH.Handle.Execute (SQL);
         end;
         Current := Current + 1;
      end Start;

      loop
         declare
            SQL : constant String := "insert into test (counter, tid) values ("
              & Positive'Image (Current) & ", "
              & Positive'Image (Task_Id) & ")";
         begin
            DBH.Handle.Execute (SQL);
         end;

         exit when Current = Max_Insert;
         Current := Current + 1;
      end loop;
   exception
      when E : others => Text_IO.Put_Line (Exception_Information (E));
   end Inserts;

   -------------
   -- Selects --
   -------------

   task body Selects is
      DBH     : constant TLS_DBH_Access := TLS_DBH_Access (DBH_TLS.Reference);
      Task_Id : Positive;
      Last    : Positive := 1;
   begin
      accept Start (Id : in Positive) do
         Task_Id := Id;
         Text_IO.Put_Line ("Reader " & Positive'Image (Task_Id) & " start");
         Connect (DBH);
      end Start;
      loop
         declare
            Iter      : DB.SQLite.Iterator;
            Line      : DB.String_Vectors.Vector;
         begin
            DBH.Handle.Prepare_Select
              (Iter, "select max(counter), tid from test");
            if Iter.More then
               Iter.Get_Line (Line);
               if Positive'Value
                 (DB.String_Vectors.Element (Line, 1)) = Max_Insert then
                  Text_IO.Put_Line ("Reader "
                                    & Positive'Image (Task_Id)
                                    & " Stop successfully");
                  Line.Clear;
                  Iter.End_Select;
                  exit;
               end if;
               if Positive'Value
                 (DB.String_Vectors.Element (Line, 1)) /= Last then
                  Last := Positive'Value (DB.String_Vectors.Element (Line, 1));
                  Text_IO.Put_Line
                    ("Reader " & Positive'Image (Task_Id) & " see "
                     & DB.String_Vectors.Element (Line, 1) & " write by "
                     & DB.String_Vectors.Element (Line, 2));
               end if;
               Line.Clear;
            end if;
            Iter.End_Select;
         end;
      end loop;
   exception
      when E : others => Text_IO.Put_Line (Exception_Information (E));
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
      Writer_1 : Inserts;
      Writer_2 : Inserts;
      Writer_3 : Inserts;
      Reader_1 : Selects;
      Reader_2 : Selects;
   begin
      --  Start the writer

      Writer_1.Start (1);
      Writer_2.Start (2);
      Writer_3.Start (3);

      Reader_1.Start (1);
      Reader_2.Start (2);
   end;
end T1;
