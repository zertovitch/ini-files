--  https://rosettacode.org/wiki/Read_a_configuration_file#Ada

with Config; use Config;
with Ada.Text_IO; use Ada.Text_IO;

procedure Rosetta_Cfg is
  cfg: Configuration:= Init("rosetta.cfg", Variable_Terminator => ' ');
  fullname       : String  := Value_Of(cfg, "*", "FULLNAME");
  favouritefruit : String  := Value_Of(cfg, "*", "FAVOURITEFRUIT");
  needspeeling   : Boolean := Value_Of(cfg, "*", "NEEDSPEELING", Default => True);
  seedsremoved   : Boolean := Value_Of(cfg, "*", "SEEDSREMOVED");
  otherfamily    : String  := Value_Of(cfg, "*", "OTHERFAMILY");
begin
  Put_Line("fullname = "       & fullname);
  Put_Line("favouritefruit = " & favouritefruit);
  Put_Line("needspeeling = "   & Boolean'Image(needspeeling));
  Put_Line("seedsremoved = "   & Boolean'Image(seedsremoved));
  Put_Line("otherfamily = "    & otherfamily);
end;
