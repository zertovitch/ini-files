with Config; use Config;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

procedure Test_Config is

  -- In a real application, where the current directory might change,
  -- you may want to refer to the directory where the application has started:

  function Config_name return String is
    full: constant String:= Ada.Command_Line.Command_Name;
    last: Natural:= full'First-1;
  begin
    for i in full'Range loop
      if full(i)='\' or full(i)='/' then
        last:= i;
      end if;
    end loop;
    return full(full'First..last) & "test_config.ini";
  end Config_name;

  procedure Create_ini_file is
     f: File_Type;
  begin
     Create(f, Out_File, Config_name);
     Put_Line(f, "; Nice comment");
     Put_Line(f, "# Nice comment 2");
     New_Line(f);
     Put_Line(f, "[Profile 1]");
     Put_Line(f, "MyString = abcd # a comment here...");
     Put_Line(f, "ShowScheme =-1  ; another comment");
     Put_Line(f, "MyFloat = +123.456");
     New_Line(f);
     Put_Line(f, "[Profile phantom] ; This section will be replaced during the test");
     Put_Line(f, "MyString = abcd...");
     Put_Line(f, "ShowScheme =-1 ");
     Put_Line(f, "MyFloat = +123.456");
     New_Line(f);
     Put_Line(f, "[Profile containing errors]");
     Put_Line(f, "MyString");
     Put_Line(f, "ShowScheme = invalid integer! ");
     Put_Line(f, "MyFloat = invalid float!");
     Close(f);
  end Create_ini_file;

  c: Configuration;

   procedure Test_reading_ini(sec: String) is
      use Ada.Exceptions;
  begin
     Put_Line( "Read test in section: """ & sec & '"');

     Put_Line( " A string: {" & Value_Of(c, sec, "MyString") & '}');

     begin
        Put_Line( " An integer: " & Integer'Image(Value_Of(c, sec, "ShowScheme")) );
     exception
     when E : Data_Error => Put_Line (Exception_Information(E));
     end;

     begin
        Put_Line( " A floating-point value: " & Long_Float'Image(Value_Of(c, sec, "MyFloat")) );
     exception
     when E : Data_Error => Put_Line (Exception_Information(E));
     end;
  end Test_reading_ini;

   procedure Test_read_sections is
      cfg: Configuration:= Init(Config_name);
      sl: Section_List := cfg.Read_Sections;
   begin
      Put_Line("Config file has following sections:");
      for s of sl loop
         Put_Line("  - " & s);
      end loop;
   end;

begin
  Put_Line("Test for Ini file manager.");
  Put_Line("Project currently hosted at: " & web);
  Create_ini_file;
  Test_read_sections;
  for a in reverse Type_Mismatch_Action loop
     New_Line;
     Put_Line("*********** Expected behaviour on bad input is: " & Type_Mismatch_Action'Image(a) & " *****");
     --
     c:= Init(Config_name, Case_Sensitive => True, On_Type_Mismatch => a);
     --
     Replace_Value(c, "Profile containing errors", "MyString", Type_Mismatch_Action'Image(a));
     Replace_Section(c, "Profile phantom", "blabla=1" & LF & "blabla_final=2" & LF);
     Test_reading_ini("Profile 1");
     Test_reading_ini("Profile containing errors");
  end loop;
end Test_Config;
