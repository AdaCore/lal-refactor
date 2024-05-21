--
--  Copyright (C) 2022-2024, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains refactoring tools that allow sorting a compilation
--  unit prelude in alphabetical order.

with Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Text; use Langkit_Support.Text;

package LAL_Refactor.Sort_Dependencies is

   function Is_Sort_Dependencies_Available
     (Unit             : Analysis_Unit;
      Sloc             : Source_Location)
      return Boolean;
   --  Returns True if Sloc is inside a compilation unit prelude

   type Dependencies_Sorter is new Refactoring_Tool with private;

   function Create_Dependencies_Sorter
     (Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
      No_Separator     : Boolean := True;
      Where            : Source_Location_Range := No_Source_Location_Range)
     return Dependencies_Sorter;
   --  Dependencies_Sorter constructor.
   --
   --  Compilation_Unit is the unit that Dependencies_Sorter will use to
   --  format the prelude.
   --
   --  If No_Separator is True, Dependencies_Sorter will not separate with/use
   --  clauses groups by a new line.
   --
   --  In this context, a group is composed by with/use clauses that have the
   --  same parent package. For instance, if `No_Seperator = True`, the
   --  following prelude:
   --
   --  ```
   --  with Foo;
   --  with Bar;
   --  with Foo.Bar;
   --  with Baz;
   --  with Garply;
   --  ```
   --
   --  Will be sorted like:
   --
   --  ```
   --  with Bar;
   --
   --  with Baz;
   --
   --  with Foo;
   --  with Foo.Bar
   --
   --  with Garply;
   --  ```

   overriding
   function Refactor
     (Self           : Dependencies_Sorter;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Sorts the Compilation_Unit's preludes passed to Dependencies_Sorter's
   --  constructor Create_Dependencies_Sorter.
   --  The prelude is sorted alphabetically.

private

   type Dependencies_Sorter is new Refactoring_Tool with
      record
         Compilation_Unit : Libadalang.Analysis.Compilation_Unit;
         No_Separator     : Boolean;
         --  If True, do not add an empty line between with/use clauses groups

         Where            : Source_Location_Range := No_Source_Location_Range;
         --  TODO: doc
      end record;

   type Clause_Type is abstract tagged
     record
        Node              : Libadalang.Analysis.Ada_Node;
        Leading_Comments  : Unbounded_Text_Type;
        Trailing_Comments : Unbounded_Text_Type;
     end record;

   type Pragma_Clause_Type is tagged;

   type With_Clause_Type is tagged;

   type Use_Package_Clause_Type is tagged;

   type Clause_Visitor is interface;
   --  Normal visitor pattern for Clause_Type.
   --  The implementations that need to be visited are With_Clause_Type and
   --  Use_Package_Clause_Type.

   procedure Visit
     (Self   : in out Clause_Visitor;
      Clause : With_Clause_Type'Class)
   is abstract;

   procedure Visit
     (Self   : in out Clause_Visitor;
      Clause : Use_Package_Clause_Type'Class)
   is abstract;

   type Pragma_Clause_Type is new Clause_Type with null record;

   function To_Ada_Source
     (Self : Pragma_Clause_Type)
      return Unbounded_Text_Type;

   package Pragma_Clause_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Pragma_Clause_Type);

   subtype Pragma_Clause_Vector is Pragma_Clause_Vectors.Vector;

   function To_Ada_Source
     (Self : Pragma_Clause_Vector) return Unbounded_Text_Type;
   --  Converts Self into source code

   package Unbounded_Text_Vectors is new
     Ada.Containers.Vectors
       (Index_Type          => Positive,
        Element_Type        => Unbounded_Text_Type);

   subtype Unbounded_Text_Vector is Unbounded_Text_Vectors.Vector;

   type Packages_Clause_Type is abstract new Clause_Type with
     record
       Is_Dotted_Name           : Boolean;
       Packages                 : Unbounded_Text_Vector;
       First_Package_First_Name : Unbounded_Text_Type;
       Associated_Pragmas       : Pragma_Clause_Vector;
     end record;

   procedure Accept_Visitor
     (Self : Packages_Clause_Type; Visitor : in out Clause_Visitor'Class)
   is abstract;

   function "<" (Left, Right : Packages_Clause_Type) return Boolean;
   --  Less function used to sort `Clause_Type` objects where `Packages`
   --  are compared sequentially.

   type Packages_Clause_Access is access Packages_Clause_Type'Class;

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Packages_Clause_Type'Class, Packages_Clause_Access);

   type Use_Package_Clause_Type is new Packages_Clause_Type with null record;

   overriding
   procedure Accept_Visitor
     (Self : Use_Package_Clause_Type; Visitor : in out Clause_Visitor'Class);
   --  Calls Visitor.Visit on Self

   function To_Ada_Source
     (Self : Use_Package_Clause_Type)
      return Unbounded_Text_Type;
   --  Converts Self into source code

   package Use_Package_Clause_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Use_Package_Clause_Type);

   subtype Use_Package_Clause_Vector is Use_Package_Clause_Vectors.Vector;

   function To_Ada_Source
     (Self : Use_Package_Clause_Vector) return Unbounded_Text_Type;
   --  Converts Self into source code

   package Use_Package_Clause_Vectors_Sorting is new
     Use_Package_Clause_Vectors.Generic_Sorting ("<");

   type With_Clause_Type is new Packages_Clause_Type with
     record
       Associated_Use_Package_Clauses : Use_Package_Clause_Vector;
     end record;

   function To_Ada_Source
     (Self : With_Clause_Type)
      return Unbounded_Text_Type;
   --  Converts Self into source code

   overriding
   procedure Accept_Visitor
     (Self : With_Clause_Type; Visitor : in out Clause_Visitor'Class);
   --  Calls Visitor.Visit on Self

   package With_Clause_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => With_Clause_Type);

   subtype With_Clause_Vector is With_Clause_Vectors.Vector;

   package With_Clause_Vectors_Sorting is new
     With_Clause_Vectors.Generic_Sorting ("<");

   package Unbounded_Text_Type_Hashed_Sets is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => Unbounded_Text_Type,
        Hash                => Hash,
        Equivalent_Elements => "=",
        "="                 => "=");

   subtype Unbounded_Text_Type_Hashed_Set is
     Unbounded_Text_Type_Hashed_Sets.Set;

   type Prelude_Section_Type is
     record
       Clauses  : With_Clause_Vector;
       Packages : Unbounded_Text_Type_Hashed_Set;
     end record;

   procedure Add_With_Clause
     (Self        : in out Prelude_Section_Type;
      With_Clause : With_Clause_Type'Class);
   --  Adds Clause to Self

   procedure Associate_Use_Package_Clauses
     (Self                : in out Prelude_Section_Type;
      Use_Package_Clauses : in out Use_Package_Clause_Vector);
   --  Associates Use_Package_Clauses to Self.Clause

   function Contains
     (Self : Prelude_Section_Type; Package_Name : Unbounded_Text_Type)
      return Boolean;
   --  True if Self has a `WithCluase` that contains Package_Name

   function To_Ada_Source
     (Self : Prelude_Section_Type; No_Separator : Boolean := True)
      return Unbounded_Text_Type;
   --  Converts Self into source code.
   --  If No_Separator is False, then adds blank lines to separate groups of
   --  clauses.

   procedure Sort (Self : in out Prelude_Section_Type);
   --  Sorts Self.Clauses

   type Prelude_Type is new Clause_Visitor with
     record
        Public_Section          : Prelude_Section_Type;
        Private_Section         : Prelude_Section_Type;
        Limited_Section         : Prelude_Section_Type;
        Limited_Private_Section : Prelude_Section_Type;
        Leftover_Use_Section    : Use_Package_Clause_Vector;
     end record;

   procedure Associate_Use_Package_Clauses (Self : in out Prelude_Type);
   --  Associates Use_Package_Clauses to With_Clauses

   procedure Sort (Self : in out Prelude_Type);
   --  Sorts all sections

   function To_Ada_Source
     (Self : Prelude_Type; No_Separator : Boolean := True)
      return Unbounded_Text_Type;
   --  Converts Self into source code by concatenating each section.
   --  If No_Separator is False, then adds blank lines to separate groups of
   --  clauses.

   overriding
   procedure Visit
     (Self : in out Prelude_Type; Clause : Use_Package_Clause_Type'Class);
   --  Adds Clause to Self.Leftover_Use_Section

   overriding
   procedure Visit
     (Self : in out Prelude_Type; Clause : With_Clause_Type'Class);
   --  Adds Clause to the correct Self's prelude section

end LAL_Refactor.Sort_Dependencies;
