--
--
--  Copyright (C) 2025-2026, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools to generate subprogram body stubs
--  from declarations.
--  The tool checks for subprogram declarations without an implementation.
--  If the subprogram is locally declared in a declarative part such as the
--  declaration part of a parent subprogram body or declare block,
--  the resulting subprogram body stub is generated in the same scope.
--
--  Top-level subprograms declared in package specifications will be
--  inserted into the corresponding package body file if it exists.
--  Otherwise, a new package body file will be generated
--  in the same directory, containing only the generated subprogram body.
--
--  Abstract and generic subprogram declarations are not supported,
--  although subprograms declared in generic packages are supported.
--
--  See Generate Package to complete package bodies with multiple subprograms.

with VSS;
with VSS.Strings;

with Libadalang.Common; use Libadalang.Common;

package LAL_Refactor.Generate_Subprogram is

   function Is_Generate_Subprogram_Available
     (Unit        : Analysis_Unit;
      Start_Loc   : Source_Location;
      Target_Subp : out Subp_Decl) return Boolean
   with Pre => not (Unit in No_Analysis_Unit or else Unit.Root.Is_Null);
   --  Check whether Start_Loc is inside a subprogram declaration.
   --  See Valid_Subp_Decl for permitted subprogram decl types
   --
   --  If so, check whether a matching subprogram body exists in the
   --  expected scope (local or package body file).
   --
   --  Refactoring is available if no subprogram body exists.

   function Get_Subp_Decl (Node : Ada_Node'Class) return Subp_Decl;
   --  Navigate from a child node to parent subprogram declaration
   --  Return No_Subp_Decl if no such parent exists

   function Valid_Subp_Decl (N : Ada_Node'Class) return Boolean
   is (N.Kind in Ada_Subp_Decl_Range
       and then not N.P_Parent_Basic_Decl.Is_Null
       and then
         ((N.P_Parent_Basic_Decl.Kind
           in Ada_Base_Package_Decl | Ada_Subp_Body_Range)
          or else
            (N.P_Parent_Basic_Decl.Kind in Ada_Generic_Package_Decl_Range
             and then not N.P_Semantic_Parent.Is_Null
             and then
               N.P_Semantic_Parent.Kind
               in Ada_Generic_Package_Internal_Range)));
   --  This check filters for subprograms declared in:
   --    a subprogram body declarative part
   --    a subprogram body declare block
   --    a concrete package specification
   --    a generic package internal specification
   --  Abstract and generic subprogram declarations are excluded.

   -----------------------------
   --  Subprogram generation --

   type Subprogram_Generator is new Refactoring_Tool with private;

   function Create_Subprogram_Generator
     (Node : Ada_Node'Class) return Subprogram_Generator
   with Pre => not Node.Is_Null and then Valid_Subp_Decl (Node);
   --  Creates a Subprogram_Generator to generate a body for Target_Subp
   --  Fails if Target_Subp cannot be marshalled into a Subp_Decl
   --  or if it is an unsupported subprogram type, e.g. abstract

   overriding
   function Refactor
     (Self           : Subprogram_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Driver for Generate action

   -------------------------------
   --  LSP diagnostic reporting --

   type Generate_Subprogram_Problem is new Refactoring_Diagnostic with private;

   overriding
   function Filename (Self : Generate_Subprogram_Problem) return String;
   --  Filename where error occurred

   overriding
   function Location
     (Self : Generate_Subprogram_Problem) return Source_Location_Range;
   --  Source location of problematic code

   overriding
   function Info (Self : Generate_Subprogram_Problem) return String;
   --  Full error message

   function Report_Error
     (Msg : String; Node : Ada_Node'Class) return Generate_Subprogram_Problem;
   --  Use for LSP error reporting if Node info available

   function Report_Error
     (Msg, File : String; SLOC : Source_Location_Range)
      return Generate_Subprogram_Problem;
   --  Used for LSP error propagation when Node.Is_Null
private

   type Generate_Mode is (Local, Add_To_Pkg_Body, New_Pkg_Body);
   --  @Local
   --    Non-visible declaration, nested in parent subprogram
   --    Scope may be declare block or subprogram declarative part
   --    Stub location: insert into same scope
   --
   --  @Add_To_Pkg_Body
   --    Visible declaration, top-level package declaration
   --    Scope is package specification, package body exists
   --    Stub location: insert into package body
   --
   --  @New_Pkg_Body
   --    Visible declaration, top-level package declaration
   --    Scope is package specification, no package body exists
   --    Stub location: create new package body for stub

   type Subprogram_Generator is new Refactoring_Tool with record
      Target_Subp : Subp_Decl;
      --  LAL node containing contextual information
      Action      : Generate_Mode;
      --  Indicates the type of refactoring edit required
      --  File creation or modification
   end record;

   type Generate_Subprogram_Problem is new Refactoring_Diagnostic with record
      Info     : VSS.Strings.Virtual_String;
      Location : Source_Location_Range;
      Filename : VSS.Strings.Virtual_String;
   end record;

   --  LAL node traversal helpers for package declarations
   function Get_Parent_Package_Spec (D : Subp_Decl) return Base_Package_Decl
   with Pre => not D.Is_Null;
end LAL_Refactor.Generate_Subprogram;
