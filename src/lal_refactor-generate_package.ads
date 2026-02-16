--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with Libadalang.Common;
with VSS;
with VSS.Strings;

private with Langkit_Support.Text;

use Libadalang.Common;
with LAL_Refactor.Stub_Utils;

package LAL_Refactor.Generate_Package is
   subtype Decl_Vector is LAL_Refactor.Stub_Utils.Decl_Vector;

   function To_Package_Decl (Node : Ada_Node) return Base_Package_Decl;
   --  If Node belongs to enclosing package declaration lines
   --  e.g. "package Spec is... " or "end Spec;"
   --  then return the package declaration node, else No_Base_Package_Decl

   function Is_Generate_Package_Available
     (Node : Ada_Node; Spec : out Base_Package_Decl) return Boolean;
   --  If Node is inside a package declaration
   --  check whether a matching package body exists.
   --  If yes but there are missing subprogram implementations,
   --  or no and the package includes subprogram declarations,
   --  Return true and write to Spec

   function Get_Body_Path (From_Spec : Base_Package_Decl) return String;
   --  Return path to existing body
   --  or create path for package body in same directory

   function Package_Body_Exists (For_Spec : Base_Package_Decl) return Boolean
   is (not For_Spec.P_Body_Part.Is_Null);
   --  Check package body exists in project

   --  These helpers are public so that Generate Subprogram
   --  may also use them
   procedure Add_New_Package_Edits
     (Edits      : out Refactoring_Edits;
      From_Spec  : Base_Package_Decl;
      With_Decls : Decl_Vector;
      To_Path    : Unbounded_String)
   with Pre => not (From_Spec.Is_Null or With_Decls.Is_Empty);
   --  Create edits to go into one new file

   procedure Update_Package_Edits
     (Edits      : out Refactoring_Edits;
      New_Decls  : Decl_Vector;
      To_Body    : Package_Body)
   with Pre => not (To_Body.Is_Null or New_Decls.Is_Empty);
   --  Text edits to modify an existing package body

   type Package_Generator is new Refactoring_Tool with private;

   function Build_Package_Generator
     (Spec : Base_Package_Decl) return Package_Generator
   with Pre => not (Spec.Is_Null or else Spec.F_Public_Part.Is_Null);
   --  Parse package spec and build generator object

   overriding
   function Refactor
     (Self           : Package_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Return edits for new or updated package body
   --  This includes deciding on the order in which
   --  the new subprogram body stubs will be inserted
   --  into the body file.
   --  Note that insertion points for text edits must be unique.
   --
   --  Generate Package Body:
   --    This action creates a new package body file.
   --    Currently, the subprogram stub order in this
   --    simply follows that of the package specification.
   --
   --  Update Package Body:
   --    This action inserts new subprogram body stubs
   --    into an existing package body file,
   --    which means some care must be taken with ordering.
   --
   --  Simple approach: insert stubs at the end of the package body.
   --  This means only one refactoring edit will be produced,
   --  containing generated text of all the stubs into one string.
   --
   --  More complicated: insert stubs in the same order as they
   --  are declared in the package specification.
   --  This would split one edit into multiple depending on
   --  where they are declared in the specification.

   -------------------------------
   --  LSP diagnostic reporting --

   type Generate_Package_Problem is new Refactoring_Diagnostic with private;

   overriding
   function Filename (Self : Generate_Package_Problem) return String;
   --  Filename where error occurred

   overriding
   function Location
     (Self : Generate_Package_Problem) return Source_Location_Range;
   --  Source location of problematic code

   overriding
   function Info (Self : Generate_Package_Problem) return String;
   --  Full error message

   function Problem
     (Msg, File : String; SLOC : Source_Location_Range)
      return Generate_Package_Problem;
private
   type Package_Generator is new Refactoring_Tool with record
      Spec        : Base_Package_Decl;
      Subprograms : Decl_Vector;
      Body_Path   : Unbounded_String;
   end record;

   type Generate_Package_Problem is new Refactoring_Diagnostic with record
      Info     : VSS.Strings.Virtual_String;
      Location : Source_Location_Range;
      Filename : VSS.Strings.Virtual_String;
   end record;

   function Is_Unimplemented_Subprogram (D : Ada_Node'Class) return Boolean
   is (D.Kind in Ada_Subp_Decl_Range | Ada_Generic_Subp_Internal_Range
       and then D.As_Subp_Decl.P_Body_Part_For_Decl.Is_Null);
   --  Helper to check for viable subprogram declarations

   function Get_Package_Name (Spec : Base_Package_Decl) return String
   is (Langkit_Support.Text.To_UTF8
         (Spec.F_Package_Name.P_Fully_Qualified_Name));
   --  Returns full namespaced and title-capitalised Dotted.Name

end LAL_Refactor.Generate_Package;
