--  https://rosettacode.org/wiki/Update_a_configuration_file#Ada

with Config; use Config;

procedure Rosetta_Updt_Cfg is
  cfg : constant Configuration := Init ("rosetta_updt.cfg", Case_Sensitive => False, Variable_Terminator => ' ');
begin
  cfg.Disable ("*", "needspeeling");
  --  TBD: Enable the seedsremoved option by removing the semicolon and any leading whitespace
  cfg.Replace_Value ("*", "numberofbananas", "1024");
  --  TBD: Enable (or create if it does not exist in the file) a parameter for numberofstrawberries with a value of 62000
end Rosetta_Updt_Cfg;
