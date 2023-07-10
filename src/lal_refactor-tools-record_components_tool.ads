--
--  Copyright (C) 2022-2023, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  LAL_Refactor record components tool

with Ada.Strings.Unbounded;
with Libadalang.Analysis;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Hashed_Maps;
with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with VSS.Text_Streams;

package LAL_Refactor.Tools.Record_Components_Tool is
   package LAL renames Libadalang.Analysis;
   package ReFac renames LAL_Refactor;
   package Slocs renames Langkit_Support.Slocs;

   function "<" (L, R : LAL.Record_Def) return Boolean;

   function "<" (L, R : LAL.Defining_Name) return Boolean is
     (Slocs.Start_Sloc (LAL.Sloc_Range (L)) <
                      Slocs.Start_Sloc (LAL.Sloc_Range (R)));

   function Defining_Name_Hash (Element : LAL.Defining_Name)
                                return Ada.Containers.Hash_Type is
   (LAL.Hash (Element.As_Ada_Node));

   package Defining_Name_Ordered_Sets is new
   Ada.Containers.Indefinite_Ordered_Sets
    (Element_Type        => LAL.Defining_Name,
     "<"                 => "<",
     "="                 => LAL."=");

   package Record_Def_To_Text_Edit_Map_Ordered_Maps is new
   Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type            => LAL.Record_Def,
        Element_Type        => ReFac.Text_Edit_Map,
        "<"                 => "<",
        "="                 => ReFac.Text_Edit_Ordered_Maps."="
       );

   subtype Record_Text_Edits is Record_Def_To_Text_Edit_Map_Ordered_Maps.Map;

   package Defining_Name_To_Defining_Name_Ordered_Set_Ordered_Maps is new
   Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type            => LAL.Defining_Name,
        Element_Type        => Defining_Name_Ordered_Sets.Set,
        Hash                => Defining_Name_Hash,
        Equivalent_Keys     => LAL."=",
        "="                 => Defining_Name_Ordered_Sets."="
       );

   subtype Deletable_Record_Componenets is
     Defining_Name_To_Defining_Name_Ordered_Set_Ordered_Maps.Map;

   Parser : Argument_Parser :=
     Create_Argument_Parser (Help => "Record Components");

   package Project is new Parse_Option
     (Parser      => Parser,
      Short       => "-P",
      Long        => "--project",
      Help        => "Project",
      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Convert     => Ada.Strings.Unbounded.To_Unbounded_String,
      Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String);

   type Delete_Infos is record
      Deletable_Names : Deletable_Record_Componenets;
      Texts_Edit : Record_Text_Edits;
   end record;

   --  this delete_info give the two hash map with record node as keys
   --  Record_Deletable_Names gives the components names set in the record
   --  Texts_Edit gives the text_edit related to the record

   function Get_Record_Name (Node : LAL.Record_Def'Class)
                            return LAL.Defining_Name;
   --  Return the defining name of the given record node.

   function Find_Unused_Components (Unit_Array : LAL.Analysis_Unit_Array)
                                    return Delete_Infos;
   --  this procedure finds all the unused components in records in
   --  all project files, gives the delete infomation as output.

   procedure Run (Unit_Array : LAL.Analysis_Unit_Array;
                  Stream     : in out
                               VSS.Text_Streams.Output_Text_Stream'Class);
   --  Record_Components_Tool main procedure

end LAL_Refactor.Tools.Record_Components_Tool;
