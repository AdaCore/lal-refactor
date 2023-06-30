package Main_Package is

   type Parent is tagged null record;

   procedure Foo (A : Parent) is null;

   type Child is new Parent with null record;

   procedure Foo (A : Child) is null;

end Main_Package;
