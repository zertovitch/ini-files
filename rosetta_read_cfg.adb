--  https://rosettacode.org/wiki/Read_a_configuration_file#Ada

with Config; use Config;
with Ada.Text_IO; use Ada.Text_IO;

procedure Rosetta_Read_Cfg is
  cfg: Configuration:= Init("rosetta_read.cfg", Variable_Terminator => ' ');
  fullname       : String  := Value_Of(cfg, "*", "FULLNAME");
  favouritefruit : String  := Value_Of(cfg, "*", "FAVOURITEFRUIT");
  needspeeling   : Boolean :=   Is_Set(cfg, "*", "NEEDSPEELING");
  seedsremoved   : Boolean :=   Is_Set(cfg, "*", "SEEDSREMOVED");
  otherfamily    : String  := Value_Of(cfg, "*", "OTHERFAMILY");
begin
  Put_Line("fullname = "       & fullname);
  Put_Line("favouritefruit = " & favouritefruit);
  Put_Line("needspeeling = "   & Boolean'Image(needspeeling));
  Put_Line("seedsremoved = "   & Boolean'Image(seedsremoved));
  Put_Line("otherfamily = "    & otherfamily);
end;
