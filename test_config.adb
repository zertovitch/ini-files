with Ada.Text_IO; use Ada.Text_IO;
with Config; use Config;

procedure Test_Config is

  name: constant String:= "test_config.ini";

  procedure Create_ini is
     f: File_Type;
  begin
     Create(f, Out_File, name);
     Put_Line(f, "; Nice comment");
     Put_Line(f, "# Nice comment 2");
     New_Line(f);
     Put_Line(f, "[Profile 1]");
     Put_Line(f, "MyString = abcd # a comment here...");
     Put_Line(f, "ShowScheme =-1  ; another comment");
     Put_Line(f, "MyFloat = +123.456");
     New_Line(f);
     Put_Line(f, "[Profile phantom] ; will be replaced");
     Put_Line(f, "MyString = abcd...");
     Put_Line(f, "ShowScheme =-1 ");
     Put_Line(f, "MyFloat = +123.456");
     New_Line(f);
     Put_Line(f, "[Profile with errors]");
     Put_Line(f, "MyString");
     Put_Line(f, "ShowScheme = invalid integer! ");
     Put_Line(f, "MyFloat = invalid float!");
     Close(f);
  end;

  c: Configuration;

  procedure Test(sec: String) is
  begin
     Put_Line( '{' & Value_Of(c, sec, "MyString") & '}');
     Put_Line( Integer'Image(Value_Of(c, sec, "ShowScheme")) );
     Put_Line( Float'Image(Value_Of(c, sec, "MyFloat")) );
  end Test;

begin
  Create_ini;
  for a in reverse Type_Mismatch_Action loop
     Put_Line("*********** Behaviour: " & Type_Mismatch_Action'Image(a));
     -- In Ada 2005+ can be written as "c.Init(name)", and so on.
     Init(c, name, Case_Sensitive => True, On_Type_Mismatch => a);
     Replace_Value(c, "Profile with errors", "MyString", Type_Mismatch_Action'Image(a));
     Replace_Section(c, "Profile phantom", "blabla=1" & LF & "blabla_final=2" & LF);
     Test("Profile 1");
     Test("Profile with errors");
  end loop;
end;


