--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with Ada.Containers.Indefinite_Vectors;

with VSS;
with VSS.Strings;

with Libadalang.Common; use Libadalang.Common;
private with Langkit_Support.Text;

package LAL_Refactor.Generate_Package is
   package Declaration_Vectors is new
     Ada.Containers.Indefinite_Vectors (Natural, Basic_Subp_Decl'Class);

   subtype Decl_Vector is Declaration_Vectors.Vector;

   function To_Package_Decl (Unit : Analysis_Unit) return Base_Package_Decl
   is (if Unit in No_Analysis_Unit
         or else Unit.Root.Is_Null
         or else Unit.Root.Kind not in Ada_Compilation_Unit_Range
         or else Unit.Root.As_Compilation_Unit.P_Decl.Is_Null
         or else Unit.Root.As_Compilation_Unit.P_Decl.Kind
                 not in Ada_Base_Package_Decl
       then No_Base_Package_Decl
       else Unit.Root.As_Compilation_Unit.P_Decl.As_Base_Package_Decl);
   --  Marshall a Unit node into a package specification node
   --  if it is one. Otherwise return No_Base_Package_Decl

   function Is_Generate_Package_Available
     (Unit : Analysis_Unit; Spec : out Base_Package_Decl) return Boolean;
   --  If Spec_Unit is a package specification with subprogram declarations,
   --  AND no implementation already exists either privately or in the body
   --  then write to Spec and return True.

   type Package_Generator is new Refactoring_Tool with private;

   function Build_Package_Generator
     (Spec : Base_Package_Decl) return Package_Generator
   with
     Pre =>
       not (Spec.Is_Null or else Spec.F_Public_Part.Is_Null);
   --  Parse package spec and build generator object

   function Get_Body_Path (From_Spec : Base_Package_Decl) return String;
      --  Return path to existing body
      --  or create path for package body in same directory

   function Package_Body_Exists (For_Spec : Base_Package_Decl) return Boolean
   is (not For_Spec.P_Body_Part.Is_Null);
   --  Check package body exists in project

   overriding
   function Refactor
     (Self           : Package_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Return edits for new or updated package body

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
   is (D.Kind in Ada_Basic_Subp_Decl
       and then D.As_Basic_Subp_Decl.P_Body_Part_For_Decl (False).Is_Null);
   --  Helper to check for viable subprogram declarations

   function Get_Package_Name (Spec : Base_Package_Decl) return String
   is (Langkit_Support.Text.To_UTF8
         (Spec.F_Package_Name.P_Fully_Qualified_Name));
   --  Returns full namespaced and title-capitalised Dotted.Name

end LAL_Refactor.Generate_Package;
