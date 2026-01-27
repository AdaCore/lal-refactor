--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains code generation tools which can be used
--  by refactoring tools. It only builds strings and does not know file paths,
--  so the refactoring tool must work out where to insert generated text.
--
--  Current capabilities:
--    Function  body generation from concrete declaration
--    Procedure body generation from concrete declaration
--    Package   body generation from concrete or generic declaration

with VSS.Strings;        use VSS.Strings;
with VSS.String_Vectors; use VSS.String_Vectors;
with VSS.Strings.Conversions;

package LAL_Refactor.Stub_Utils is
   package Declaration_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, Basic_Subp_Decl);
   subtype Decl_Vector is Declaration_Vectors.Vector;

   subtype VSS_Vector is Virtual_String_Vector;

   function Build_Subunit_Body
     (Signature  : VSS_Vector;
      Inner_Body : String;
      End_Label  : String;
      Offset     : Natural;
      Indent     : Natural) return Virtual_String;
   --  Generate "[ident] [Name] is ... begin ... end [Name]" stub

   --------------------
   -- Code_Generator --
   --------------------

   type Code_Generator is abstract tagged private
   with First_Controlling_Parameter;
   --  Generator to analyse a LAL node and generate code stub strings

   function Generate_Body (Self : Code_Generator) return Unbounded_String
   is abstract;
   --  Generate code stub with formatting and indentation
   --  Note: Refactoring_Edits store text as Unbounded_String
   --  but code here mostly uses VSS

   -------------------------
   -- Subp_Code_Generator --
   -------------------------

   type Subp_Code_Generator is new Code_Generator with private;

   function Create_Code_Generator
     (Subprogram : Basic_Subp_Decl) return Subp_Code_Generator
   with Pre => not Subprogram.Is_Null;
   --  Extract information from LAL node to create subprogram generator

   overriding
   function Generate_Body (Self : Subp_Code_Generator) return Unbounded_String;
   --  Return a subprogram body string with context-matching indentation
   --
   --  """
   --  |   [Subprogram:Declaration]
   --  |   is
   --  |   begin
   --  |      [blank line]
   --  |   end [Subprogram:Name];
   --
   --  """

   ------------------------
   -- Pkg_Code_Generator --
   ------------------------

   type Pkg_Code_Generator is new Code_Generator with private;

   overriding
   function Generate_Body (Self : Pkg_Code_Generator) return Unbounded_String;
   --  Return a package body string indented like so
   --  (with | as column 0)
   --
   --  """
   --  |package body [Package:Name] is
   --  |
   --  |   [subprogram body]
   --  |   [...]
   --  |end [Package:Name];
   --  """

   function Create_Code_Generator
     (Spec : Base_Package_Decl; Subprograms : Decl_Vector)
      return Pkg_Code_Generator
   with Pre => not Spec.Is_Null and not Subprograms.Is_Empty;
   --  Extract information from LAL node to create package body generator

private
   --  Helpers and data for generating code --

   Default_Indent  : constant Natural := 3;
   Default_Nesting : constant Natural := 0;

   --  Default to lowercase for keywords
   Keyword_Is           : constant String := "is";
   Keyword_Begin        : constant String := "begin";
   Keyword_End          : constant String := "end";
   Keyword_Package_Body : constant String := "package body";

   function Padding
     (Offset : Natural; Indentation : Natural; Nesting : Natural)
      return Unbounded_String
   is ((Offset + Nesting * Indentation) * " ");
   --  Return leading indentation whitespace string

   --  VSS string conversion helpers --

   function To_VS (Item : String) return VSS.Strings.Virtual_String
   renames VSS.Strings.Conversions.To_Virtual_String;
   function To_VS (Item : Unbounded_String) return VSS.Strings.Virtual_String
   renames VSS.Strings.Conversions.To_Virtual_String;

   function From_VS (Item : VSS.Strings.Virtual_String'Class) return String
   renames VSS.Strings.Conversions.To_UTF_8_String;
   function From_VS
     (Item : VSS.Strings.Virtual_String'Class) return Unbounded_String
   renames VSS.Strings.Conversions.To_Unbounded_UTF_8_String;

   --  Code generation types --
   type Code_Generator is abstract tagged record
      Name : Unbounded_String;
   end record;

   --  Opening line of generated body
   function End_Name (Self : Code_Generator'Class) return String
   is (Keyword_End & " " & Self.Name.To_String & ";");
   --  Closing line of generated body

   type Subp_Code_Generator is new Code_Generator with record
      Decl : Basic_Subp_Decl;
   end record;

   type Pkg_Code_Generator is new Code_Generator with record
      Subprograms : Decl_Vector;
      --  Declared subprograms to generate in body
   end record;

end LAL_Refactor.Stub_Utils;
