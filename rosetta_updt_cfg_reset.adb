--  Create (reset) the configuration file, then call the Rosetta_Updt_Cfg demo.
--  The config file to be modified will be changed only if there is any command-line argument.

with Rosetta_Updt_Cfg;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Rosetta_Updt_Cfg_Reset is

  procedure Write_Cfg_file(name: String) is
    c: File_Type;
  begin
    Create(c, Out_File, name);
    Put_Line(c, "# This is a configuration file in standard configuration file format");
    Put_Line(c, "#");
    Put_Line(c, "# Lines begininning with a hash or a semicolon are ignored by the application");
    Put_Line(c, "# program. Blank lines are also ignored by the application program.");
    Put_Line(c, "");
    Put_Line(c, "# The first word on each non comment line is the configuration option.");
    Put_Line(c, "# Remaining words or numbers on the line are configuration parameter");
    Put_Line(c, "# data fields.");
    Put_Line(c, "");
    Put_Line(c, "# Note that configuration option names are not case sensitive. However,");
    Put_Line(c, "# configuration parameter data is case sensitive and the lettercase must");
    Put_Line(c, "# be preserved.");
    Put_Line(c, "");
    Put_Line(c, "# This is a favourite fruit");
    Put_Line(c, "FAVOURITEFRUIT banana");
    Put_Line(c, "");
    Put_Line(c, "# This is a boolean that should be set");
    Put_Line(c, "NEEDSPEELING");
    Put_Line(c, "");
    Put_Line(c, "# This boolean is commented out");
    Put_Line(c, "; SEEDSREMOVED");
    Put_Line(c, "");
    Put_Line(c, "# How many bananas we have");
    Put_Line(c, "NUMBEROFBANANAS 48");
    Close(c);
  end;

begin
  Write_Cfg_file("rosetta_BEFORE_updt.cfg");
  Write_Cfg_file("rosetta_updt.cfg");
  if Argument_Count > 0 then
    Rosetta_Updt_Cfg;
  end if;
end;
