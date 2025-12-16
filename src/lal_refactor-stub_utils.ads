--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains code generation tools which can be used
--  by refactoring tools. It only builds strings and does not know file paths,
--  so the refactoring tool must work out where to insert generated text.
--  Current capabilities: (classic as in concrete, not generic or abstract)
--    Function body generation from classic declaration
--    Procedure body generation from clasic declaration

private with Ada.Characters.Latin_1;

package LAL_Refactor.Stub_Utils is
   function Build_Subunit_Body
     (Name       : String;
      Signature  : String;
      Inner_Body : String;
      Offset     : Natural;
      Indent     : Natural) return Unbounded_String;
   --  Generate "[ident] [Name] is ... begin ... end [Name]" stub

   type Subp_Code_Generator is tagged private with First_Controlling_Parameter;

   function Create_Code_Generator (Decl : Subp_Decl) return Subp_Code_Generator
   with Pre => not (Decl.Is_Null or else Decl.Unit in No_Analysis_Unit);
   --  Extracts information from Spec node and builds generator

   function Generate_Subprogram_Body
     (Self : Subp_Code_Generator'Class) return Unbounded_String;
   --  Returns a subprogram body stub, prefilled with dummy null statement
   --  Indentation detected from context

private
   --  Keywords and helpful constants

   Default_Indent  : constant Natural := 3;
   Default_Nesting : constant Natural := 0;
   Begin_Keyword   : constant String := "begin";
   End_Keyword     : constant String := "end";
   Line_End        : constant String := "" & Ada.Characters.Latin_1.LF;
   Line_Term       : constant String := ";" & Line_End;
   Comment_Start   : constant String := "--  ";

   --  Formatting helpers

   function Padding
     (Offset : Natural; Indentation : Natural; Nesting : Natural)
      return Unbounded_String
   is ((Offset + Nesting * Indentation) * " ");

   function Fmt_Src
     (Line        : String;
      Offset      : Natural := Default_Indent;
      Indentation : Natural := Default_Indent;
      Nesting     : Natural := Default_Nesting;
      Stmt_Term   : Boolean := True) return Unbounded_String
   is (Padding (Offset, Indentation, Nesting)
       & Line
       & (if Stmt_Term then Line_Term else (Line_End)));
   --  Format code with appropriate indentation, newline,
   --  and terminating semicolon if specified

   type Subp_Code_Generator is tagged record
      Name : Unbounded_String;
      --  Decl node contains all necessary info including name, but it is
      --  convenient to store Name instead of querying Decl each time
      Decl : Subp_Decl;
      --  Could use Subp_Spec to build subprogram signature
      --  but Subp_Decl includes extra info such as overriding status
   end record;

end LAL_Refactor.Stub_Utils;
