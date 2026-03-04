package Single_Priv is
   type Critter is (Cuttlefish, Manta_Ray, Nautilus);
private
   procedure Render (Friend : Critter);

   function Drown (Creature : Critter) return String is
   ("glub glub glub");
end Single_Priv;
--  Test package generation of only one private subprogram
--  Ignore privately implemented subprogram