--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Containers.Ordered_Maps;

with VSS;
with VSS.Strings;

private with Langkit_Support.Text;

with Libadalang.Common; use Libadalang.Common;

with LAL_Refactor.Stub_Utils;

package LAL_Refactor.Generate_Package is
   subtype Subp_Set is LAL_Refactor.Stub_Utils.Subp_Set;

   package Subp_Decl_Maps is new Ada.Containers.Ordered_Maps
       (Key_Type     => Source_Location_Range,
        Element_Type => Subp_Set,
        "<"          => Langkit_Support.Slocs."<",
        "="          => LAL_Refactor.Stub_Utils.Subp_Sets."=");
   subtype Subp_Map is Subp_Decl_Maps.Map;

   function To_Package_Decl (Node : Ada_Node) return Base_Package_Decl;
   --  If Node belongs to enclosing package declaration lines
   --  e.g. "package Spec is... " or "end Spec;"
   --  then return the package declaration node, else No_Base_Package_Decl

   function Can_Generate (Spec : Base_Package_Decl) return Boolean;
   --  Once found, check package spec is a viable candidate

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

   type Package_Mode is (Create_New, Update_Existing);

   type Package_Generator (Action : Package_Mode) is
     new Refactoring_Tool with private;

   function Create_Package_Generator
     (Spec : Base_Package_Decl) return Package_Generator
   with Pre => Can_Generate (Spec);
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
   --  type Package_Mode is (Create_New, Update_Existing);

   type Package_Generator (Action : Package_Mode) is new Refactoring_Tool
   with record
      Spec      : Base_Package_Decl;
      Body_Path : Unbounded_String;
      case Action is
         when Create_New =>
            All_Subprograms : Subp_Set;
            --  One ordered list of subprogram declarations
         when Update_Existing =>
            New_Subprograms : Subp_Map;
            --  Map of subprogram declarations split around
            --  implemented subprograms,
            --  using insertion point as key
      end case;
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

   function Has_Unimplemented_Subprograms
     (Decl_Block : Declarative_Part'Class) return Boolean
   is (not Decl_Block.Is_Null
       and then
         (for some Decl of Decl_Block.F_Decls =>
            Is_Unimplemented_Subprogram (Decl)));

   function Can_Generate (Spec : Base_Package_Decl) return Boolean
   is (not Spec.Is_Null
       and then
         (Has_Unimplemented_Subprograms (Spec.F_Public_Part)
          or Has_Unimplemented_Subprograms (Spec.F_Private_Part)));

   function Get_Package_Name (Spec : Base_Package_Decl) return String
   is (Langkit_Support.Text.To_UTF8
         (Spec.F_Package_Name.P_Fully_Qualified_Name));
   --  Returns full namespaced and title-capitalised Dotted.Name

end LAL_Refactor.Generate_Package;
