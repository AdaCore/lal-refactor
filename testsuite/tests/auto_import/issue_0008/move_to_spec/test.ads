with Ada.Text_IO;

package Test is
   procedure Bar;
   procedure Foo (File : Ada.Text_IO.File_Type; U : Ada.Strings.Unbounded.Unbounded_String) renames Put_Line;
end Test;
