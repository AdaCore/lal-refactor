--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  LAL_Refactor LAL_Refactor.Tools command line utilities

with Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.VFS;

with LAL_Refactor.Tools;

package LAL_Refactor.Command_Line is

   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "Refactor tools");

   package Help is new Parse_Flag
     (Parser   => Parser,
      Short    => "-h",
      Long     => "--help",
      Help     => "Help");

   package Tool is new Parse_Positional_Arg
     (Parser   => Parser,
      Name     => "tool",
      Help     => LAL_Refactor.Tools.Tool_List,
      Arg_Type => LAL_Refactor.Tools.Tool,
      Convert  => LAL_Refactor.Tools.Convert);

   package Verbose is new Parse_Flag
     (Parser   => Parser,
      Short    => "-v",
      Long     => "--verbose",
      Help     => "Print traces");

   package Pipe is new Parse_Flag
     (Parser   => Parser,
      Short    => "-p",
      Long     => "--pipe",
      Help     =>
         "Print the result to stdout instead of editing the files on disk");

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Sources is new Parse_Option_List
     (Parser      => Parser,
      Short       => "-S",
      Long        => "--sources",
      Help        => "Source files to refactor",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String);

   package Scenario_Variables is new Parse_Option_List
     (Parser      => Parser,
      Short       => "-X",
      Help        => "Specify an external reference for Project Files",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Accumulate  => True,
      Name        => "nm=val");

   function To_Virtual_File
     (File_Name : String)
      return GNATCOLL.VFS.Virtual_File
   is (GNATCOLL.VFS.Create (GNATCOLL.VFS."+" (File_Name)));

   package From_GNAT_Warnings is new Parse_Option
     (Parser      => Parser,
      Short       => "-fgw",
      Long        => "--from-gnat-warnings",
      Help        => "File with all obsolescent syntax GNAT warnings",
      Arg_Type    => GNATCOLL.VFS.Virtual_File,
      Convert     => To_Virtual_File,
      Default_Val => GNATCOLL.VFS.No_File,
      Name        => "<file_with_gnat_warnings>");

   package Remove_Indices is new Parse_Flag
     (Parser   => Parser,
      Short    => "-ri",
      Long     => "--remove-indices",
      Help     => "Removes the indices in empty and one element arrays");

end LAL_Refactor.Command_Line;
