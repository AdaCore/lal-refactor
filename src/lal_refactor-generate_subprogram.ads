--
--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools to generate subprogram body stubs
--  from declarations.
--  At present, the tool only checks for a subprogram body in the same file,
--  and generates body stubs on the line directly under the declaration.
--  TODO : handle subprogram declarations in package specifications.

with VSS;
with VSS.Strings;

package LAL_Refactor.Generate_Subprogram is

   function Is_Generate_Subprogram_Available
     (Unit        : Analysis_Unit;
      Start_Loc   : Source_Location;
      Target_Subp : out Subp_Decl) return Boolean
   with Pre => not (Unit in No_Analysis_Unit or else Unit.Root.Is_Null);
   --  Check whether Start_Loc is inside a subprogram declaration
   --  e.g.
   --
   --    overriding procedure F (N : Integer) with Pre => N > 10;
   --
   --  Anywhere in this lin (including start indentation) all the way
   --  up to and including the final parenthesis will count as inside
   --  the subprogram declaration. The semicolon will not count.
   --
   --  If a subprogram is found, set Target_Subp to the Subp_Decl node.
   --  Check if a body already exists in the same declarative scope
   --  e.g. a declare block or subprogram declarative part (see TODO)
   --  Offer refactoring if no subprogram body found.

   function Get_Subp_Decl (Node : Ada_Node'Class) return Subp_Decl;
   --  Only use this when Node is already inside a Subp_Decl

   -----------------------------
   --  Subprogram generation --

   type Subprogram_Generator is new Refactoring_Tool with private;

   function Generate (Self : Subprogram_Generator) return Refactoring_Edits
   with Post => not Generate'Result.Has_Failed;
   --  Generates a subprogram body from the declaration

   overriding
   function Refactor
     (Self           : Subprogram_Generator;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Driver for Generate action

   function Create_Subprogram_Generator
     (Target_Subp : Ada_Node'Class; Dest_Filename : String)
      return Subprogram_Generator
   with
     Pre =>
       not ((Target_Subp.Is_Null or else Target_Subp.Unit in No_Analysis_Unit)
            or Dest_Filename'Length = 0)
       and then not Get_Subp_Decl (Target_Subp).Is_Null;
   --  Creates a Subprogram_Generator to generate a body for Target_Subp
   --  Fails if Target_Subp cannot be marshalled into a Subp_Decl

   function Get_Insertion_Point
     (Self : Subprogram_Generator) return Source_Location_Range;
   --  Calculate suitable text insertion point
   --  TODO currently defaults to insertion directly below declaration
   --  (as refactoring only available for non-package decls)

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

   type Subprogram_Generator is new Refactoring_Tool with record
      Target_Subp   : Subp_Decl;
      --  Use parent Subp_Decl node instead of child Subp_Spec node
      --  as Decl includes overriding status
      Dest_Filename : Unbounded_String;
      --  At present this defaults to Target_Subp source file
      --  but this will support subprogram generation in packages
   end record;

   type Generate_Subprogram_Problem is new Refactoring_Diagnostic with record
      Info     : VSS.Strings.Virtual_String;
      Location : Source_Location_Range;
      Filename : VSS.Strings.Virtual_String;
   end record;

end LAL_Refactor.Generate_Subprogram;
