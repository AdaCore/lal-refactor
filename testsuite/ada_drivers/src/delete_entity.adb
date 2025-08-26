------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with LAL_Refactor.Delete_Entity; use LAL_Refactor.Delete_Entity;
with LAL_Refactor.File_Edits;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

--  This procedure defines the Refactor Delete Entity Tool. Given the location
--  of an identifier in a source code file, and the project it belongs to, it
--  finds all references of the node's referenced declaration and checks if the
--  deletion will cause an issue.

--  Usage:
--  delete_entity -P <project_file> -S <source_code_file> -L <line_number>
--  -R <column_number>
--
--  -P, --project          Project file to use
--  -S, --source           Source code file of the identifier
--  -L, --line             Line number of the identifier
--  -R, --column           Column number of the identifier

procedure Delete_Entity is

   procedure Delete_Entity_App_Setup
     (Context : App_Context;
      Jobs : App_Job_Context_Array);
   --  This procedure is called right after command line options are parsed,
   --  the project is loaded (if present) and the list of files to process
   --  is computed.

   package Delete_Entity_App is new Libadalang.Helpers.App
     (Name             => "Delete_Entity",
      Description      => "Delete_Entity",
      App_setup        => Delete_Entity_App_Setup);

   package Args is

      package Source is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Delete_Entity_App.Args.Parser,
         Short       => "-S",
         Long        => "--source",
         Help        => "Source code file of the node",
         Arg_Type    => Unbounded_String,
         Convert     => To_Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Enabled     => True);

      package Line is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Delete_Entity_App.Args.Parser,
         Short       => "-L",
         Long        => "--line",
         Help        => "Line of the node",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

      package Column is new GNATCOLL.Opt_Parse.Parse_Option
        (Parser      => Delete_Entity_App.Args.Parser,
         Short       => "-R",
         Long        => "--column",
         Help        => "Column of the node",
         Arg_Type    => Natural,
         Convert     => Natural'Value,
         Default_Val => 1,
         Enabled     => True);

   end Args;

   -----------------------------
   -- Delete_Entity_App_Setup --
   -----------------------------

   procedure Delete_Entity_App_Setup
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);

      Source_File : constant String := To_String (Args.Source.Get);
      Unit        : constant Analysis_Unit :=
        Jobs (1).Analysis_Ctx.Get_From_File (Source_File);
      Sloc        : constant Source_Location :=
        (Line_Number (Args.Line.Get), Column_Number (Args.Column.Get));

      Edits : LAL_Refactor.Refactoring_Edits;

      function Units return Analysis_Unit_Array is ([Unit]);

   begin
      if Is_Delete_Entity_Available (Unit, Sloc) then
         Edits :=
           Create_Entity_Deleter (Unit, Sloc).Refactor (Units'Access);

         for Item of Edits.Diagnostics loop
            Ada.Text_IO.Put ("--  ");
            Ada.Text_IO.Put (Ada.Directories.Base_Name (Item.Filename));
            Ada.Text_IO.Put (':');
            Ada.Text_IO.Put (Image (Item.Location));
            Ada.Text_IO.Put (' ');
            Ada.Text_IO.Put_Line (Item.Info);
         end loop;

         LAL_Refactor.File_Edits.Apply_Edits (Edits.Text_Edits);
      end if;
   end Delete_Entity_App_Setup;

begin
   Delete_Entity_App.Run;
end Delete_Entity;
