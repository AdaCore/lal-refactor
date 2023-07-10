--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  LAL_Refactor suppress_dead_params tool

with Ada.Strings.Unbounded;
with Libadalang.Analysis;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with VSS.Text_Streams;

package LAL_Refactor.Tools.Suppress_Dead_Params_Tool is
   package LAL renames Libadalang.Analysis;
   package ReFac renames LAL_Refactor;
   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "Suppress Dead Params");

   function "<" (L, R : LAL.Defining_Name) return Boolean;

   function "<" (L, R : LAL.Subp_Spec) return Boolean
      is (L.F_Subp_Name < R.F_Subp_Name);

   type Value (Fixed : Boolean := False) is record
      case Fixed is
         when True  => V : LAL.Enum_Literal_Decl;
         when False => null;
      end case;
   end record;

   function Hash (N : LAL.Defining_Name) return Ada.Containers.Hash_Type is
     (N.As_Ada_Node.Hash);

   function Hash (N : LAL.Subp_Spec) return Ada.Containers.Hash_Type is
     (N.As_Ada_Node.Hash);

   package Values is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Defining_Name,
      Element_Type    => Value,
      Hash            => Hash,
      Equivalent_Keys => LAL."=");
   use Values;
   --  Global mapping from parameters to their fixed enumeration value if
   --  any, or Any_Value if it has been detected that there is no such fixed
   --  enumeration value.

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Source is new Parse_Option
     (Parser      => Parser,
      Short       => "-S",
      Long        => "--source",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   package Defining_Name_Ordered_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => LAL.Defining_Name,
      "<"          => "<",
      "="          => LAL."=");

   package Subp_Spec_To_Edit_Text is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type            => LAL.Subp_Spec,
        Element_Type        => ReFac.Text_Edit_Map,
        "<"                 => "<",
        "="                 => ReFac.Text_Edit_Ordered_Maps."=");

   package Subp_Spec_To_Defining_Names_Set is
      new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => LAL.Subp_Spec,
        Element_Type    => Defining_Name_Ordered_Sets.Set,
        Hash            => Hash,
        Equivalent_Keys => LAL."=",
        "="             => Defining_Name_Ordered_Sets."=");

   type Edit_Infos is record
      Text_Info        : Subp_Spec_To_Edit_Text.Map;
      Removable_Params : Subp_Spec_To_Defining_Names_Set.Map;
   end record;

   function Find_Dead_Param (Unit_Array : LAL.Analysis_Unit_Array)
                                      return Edit_Infos;

   procedure Run
     (Unit_Array : LAL.Analysis_Unit_Array;
      Stream     : in out VSS.Text_Streams.Output_Text_Stream'Class);
   --  Suppress_Params_Tool main procedure

end LAL_Refactor.Tools.Suppress_Dead_Params_Tool;
