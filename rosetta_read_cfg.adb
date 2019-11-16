--  https://rosettacode.org/wiki/Read_a_configuration_file#Ada

with Config; use Config;
with Ada.Text_IO; use Ada.Text_IO;

procedure Rosetta_Read_Cfg is
  cfg : constant Configuration := Init ("rosetta_read.cfg", Case_Sensitive => False, Variable_Terminator => ' ');
  fullname       : constant String  := cfg.Value_Of ("*", "fullname");
  favouritefruit : constant String  := cfg.Value_Of ("*", "favouritefruit");
  needspeeling   : constant Boolean :=   cfg.Is_Set ("*", "needspeeling");
  seedsremoved   : constant Boolean :=   cfg.Is_Set ("*", "seedsremoved");
  otherfamily    : constant String  := cfg.Value_Of ("*", "otherfamily");
begin
  Put_Line ("fullname = "       & fullname);
  Put_Line ("favouritefruit = " & favouritefruit);
  Put_Line ("needspeeling = "   & Boolean'Image (needspeeling));
  Put_Line ("seedsremoved = "   & Boolean'Image (seedsremoved));
  Put_Line ("otherfamily = "    & otherfamily);
end Rosetta_Read_Cfg;
