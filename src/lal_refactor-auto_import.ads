--
--  Copyright (C) 2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Langkit_Support.Text;
with Libadalang.Helpers;

--  This package contains the Refactor Auto Imports Tool utilities
package LAL_Refactor.Auto_Import is

   use Libadalang.Helpers;

   subtype Analysis_Unit_Vector is Unit_Vectors.Vector;

   type Import_Type is record
      Import    : Langkit_Support.Text.Unbounded_Text_Type;
      Qualifier : Langkit_Support.Text.Unbounded_Text_Type;
   end record;

   function "<" (Left, Right : Import_Type) return Boolean;
   --  Compares Left.Import with Right.Import and if equal, compares
   --  Left.Qualifier with Right.Qualifier.

   function Image (Object : Import_Type) return String;
   --  Returns a String with the following format:
   --  "Import: ${Object.Import}; Qualifier: ${Object.Qualifier}".

   package Import_Type_Ordered_Sets is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Import_Type);

   subtype Import_Type_Ordered_Set is Import_Type_Ordered_Sets.Set;

   function Is_Auto_Import_Available
     (Unit              : Analysis_Unit;
      Location          : Source_Location;
      Units             : not null access function return Analysis_Unit_Array;
      Name              : out Libadalang.Analysis.Name;
      Available_Imports : out Import_Type_Ordered_Set)
      return Boolean;
   --  Checks if Location in Unit is a Name which does not resolve. If so,
   --  fills Available_Imports with all the suggestions that would make Name
   --  resolve. Suggestions that have the component Qualifier equal to an
   --  empty string means that the name does not need to be qualifier, only
   --  imported.t

   type Auto_Importer is new Refactoring_Tool with private;

   function Create_Auto_Importer
     (Unit     : Analysis_Unit;
      Location : Source_Location;
      Import   : Import_Type)
      return Auto_Importer;
   --  Auto_Importer constructor.
   --  Location in Unit must resolve to a Name where Import can be applied.
   --  Use Is_Auto_Import_Available to confirm that Unit and Location can
   --  safely be used on this constuctor.

   function Create_Auto_Importer
     (Name   : Libadalang.Analysis.Name;
      Import : Import_Type)
      return Auto_Importer;
   --  Auto_Importer constructor.
   --  Name must be a Name where Import can be applied.
   --  Use the out parameter of Is_Auto_Import_Available to confirm that Name
   --  can safely be used on this constuctor.

   overriding
   function Refactor
     (Self           : Auto_Importer;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits;
   --  Adds the import and qualifies the name

private

   type Auto_Importer is new Refactoring_Tool with record
      Name   : Libadalang.Analysis.Name;
      --  Name that cannot be resolved

      Import : Import_Type;
      --  The with clause and qualifier that make Name resolvable
   end record;

end LAL_Refactor.Auto_Import;
