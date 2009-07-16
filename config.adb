-- Created On      : Fri Apr 26 08:13:44 1996

with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

package body Config is

   procedure Free is new Ada.Unchecked_Deallocation(String, Str_Ptr);

   procedure Init(Cfg              : out Configuration;
                  File_Name        :  in String;
                  Case_Sensitive   :  in Boolean := True;
                  On_Type_Mismatch :  in Type_Mismatch_Action := Raise_Data_Error
                  )
   is
   begin
      Free(Cfg.Config_File);
      Cfg.Config_File := new String'(File_Name);
      Cfg.Case_Sensitive:= Case_Sensitive;
      Cfg.On_Type_Mismatch:= On_Type_Mismatch;
   end Init;

   function Is_number_start(c: Character) return Boolean is
   begin
      case c is
         when '0'..'9' | '+' | '-' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_number_start;

   -- Internal
   --
   procedure Get_Value(Cfg         :  in Configuration;
                       Section     :  in String;
                       Mark        :  in String;
                       Line        : out String;
                       Value_Start : out Natural;
                       Value_End   : out Natural;
                       Found_Line  : out Natural
                      )
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Strings;
      use Ada.Characters.Handling;

      File              : File_Type;

      Line_End          : Natural                       := 0;
      Line_Count        : Natural                       := 0;

      Sect_End          : Natural;
      Comment_Ind       : Natural;
      Equal_Ind         : Natural;

      Found_Section_End : Natural;
      Found_Mark_Start  : Natural;
      Found_Mark_End    : Natural                       := 0;
      In_Found_Section  : Boolean                       := False;

      Value_Start_Try   : Natural;

   begin -- Get_Value
      Value_Start  := Line'First;
      Value_End    := Line'First - 1;
      Found_Line   := 0;
      Open(File, In_File, Cfg.Config_File.all);
      Read_File:
      while not End_Of_File(File) loop
         Get_Line(File, Line, Line_End); -- error if line end > line'Last
         Line_Count:= Line_Count + 1;
         if Line_End > 1 then
            case Line(Line'First) is
               when '[' =>
                  Sect_End := Index(Source  => Line(Line'First .. Line_End),
                                    Pattern => "]");
                  -- error if ext_end = 0
                  Found_Section_End := Sect_End - 1;
                  -- pragma Debug(Put_Line("Config: found_section => " &
                  -- Line(2..Found_Section_End)));
                  if Cfg.Case_Sensitive then
                     In_Found_Section :=(Section = Line(Line'First+1..Found_Section_End));
                  else
                     In_Found_Section :=
                       (To_Lower(Section) = To_Lower(Line(Line'First+1..Found_Section_End)));
                  end if;
               when ';' | '#' =>
                  null; -- This is a full-line comment
               when others =>
                  if Section = "*" then
                     In_Found_Section := True;
                  end if;
                  if In_Found_Section then
                     Comment_Ind := Index(Source => Line(Line'First .. Line_End),
                                          Set    => To_Set("#;"));
                     if Comment_Ind >= Line'First then
                        Line_End := Comment_Ind - 1;
                     end if;
                     Equal_Ind := Index(Source  => Line(Line'First .. Line_End),
                                        Pattern => "=");
                     if Equal_Ind >= Line'First then
                        Found_Mark_Start :=
                          Index_Non_Blank(Line(Line'First .. Equal_Ind-1), Forward);
                        Found_Mark_End :=
                          Index_Non_Blank(Line(Line'First .. Equal_Ind-1), Backward);
                     else
                        Found_Mark_Start :=
                          Index_Non_Blank(Line(Line'First .. Line_End), Forward);
                        Found_Mark_End :=
                          Index_Non_Blank(Line(Line'First .. Line_End), Backward);
                     end if;
                     -- pragma Debug(Put_Line("Config: found_mark    => " &
                     -- Line(Found_Mark_start..Found_Mark_End)));
                     if Found_Mark_Start > 0 and then
                       Found_Mark_End > 0
                     then
                        if (Cfg.Case_Sensitive and then
                            (Line(Found_Mark_Start..Found_Mark_End) = Mark))
                          or else (not Cfg.Case_Sensitive and then
                                   (To_Lower(Line(Found_Mark_Start..
                                                  Found_Mark_End))
                                    = To_Lower(Mark)))
                        then
                           Found_Line := Line_Count;
                           if Equal_Ind >= Line'First then
                              Value_Start_Try :=
                                Index_Non_Blank(Line(Equal_Ind+1..Line_End),
                                                Forward);
                              if Value_Start_Try >= Line'First then
                                 Value_End :=
                                   Index_Non_Blank(Line(Value_Start_Try..Line_End),
                                                   Backward);
                                 Value_Start  := Value_Start_Try;
                              end if;
                           end if;
                           exit Read_File;
                        end if;
                     end if;
                  end if;
            end case;
         end if;
      end loop Read_File;
      Close(File);
   end Get_Value;

   Max_Line_Length: constant:= 1000;

   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in String := "")
                    return String
   is
      Line              : String(1 .. Max_Line_Length);
      Value_Start       : Natural;
      Value_End         : Natural;
      Found_Line        : Natural;
   begin
      Get_Value(Cfg, Section, Mark, Line, Value_Start, Value_End, Found_Line);
      if Line(Value_Start .. Value_End) = "" then
         return Default;
      else
         return Line(Value_Start .. Value_End);
      end if;
   end Value_Of;

   procedure Type_Error(Cfg: in Configuration; Val, Desc: String) is
      use Ada.Text_IO;
   begin
      case Cfg.On_Type_Mismatch is
         when Raise_Data_Error =>
            raise Ada.Text_IO.Data_Error;

         when Print_Warning    =>
            Put_Line(
               Standard_Error,
               "Config: warning: `" & val & "' is not " & desc
            );

         when Be_Quiet         =>
            null;
      end case;
   end Type_Error;

   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in Integer := 0)
                    return Integer
   is
      Value_As_String : constant String := Value_Of(Cfg, Section, Mark);
   begin
      if Value_As_String'Length > 2 and then
        Value_As_String(Value_As_String'First..Value_As_String'First+1) = "0x"
      then
         return Integer'Value("16#" &
                              Value_As_String(Value_As_String'First+2 ..
                                              Value_As_String'Last) &
                              "#");
      elsif Value_As_String'Length > 0  and then
         Is_number_start(Value_As_String(Value_As_String'First))
      then
         return Integer'Value(Value_As_String);
      else
         Type_Error(Cfg, Value_As_String, "an integer number");
         return Default;
      end if;

   exception
      when others =>
         Type_Error(Cfg, Value_As_String, "an integer number");
         return Default;
   end Value_Of;


   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in Float := 0.0)
                    return Float
   is
      Value_As_String : constant String := Value_Of(Cfg, Section, Mark);
      Val  : Float;
      Last : Positive;
   begin
      if Value_As_String'Length > 0 and then
         Is_number_start(Value_As_String(Value_As_String'First))
      then
         -- Val := Float'Value(Value_As_String);
         -- ^ an old compiler doesn't like some floats repr. through 'Value
         Ada.Float_Text_IO.Get(Value_As_String, Val, Last);
         return Val;
      else
         Type_Error(Cfg, Value_As_String, "a floating-point number");
         return Default;
      end if;
   exception
      when others =>
         Type_Error(Cfg, Value_As_String, "a floating-point number");
         return Default;
   end Value_Of;

   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in Boolean := False) return Boolean
   is
   begin
      return Boolean'Value(Value_Of(Cfg, Section, Mark, Boolean'Image(Default)));
   end Value_Of;

   -- Return True if one of the following conditions is met:
   --  o the Mark is within the Section, but no equal sign is in that line,
   --  o the Mark is set to either 1, True or Yes.
   -- All other cases return False.
   function Is_Set(Cfg     : in Configuration;
                   Section : in String;
                   Mark    : in String)
                  return Boolean is
      use Ada.Characters.Handling;
      Line              : String(1 .. Max_Line_Length);
      Value_Start       : Natural;
      Value_End         : Natural;
      Found_Line        : Natural;
   begin
      Get_Value(Cfg, Section, Mark, Line, Value_Start, Value_End, Found_Line);
      declare
        Value : constant String := To_Lower(Line(Value_Start .. Value_End));
      begin
         return Found_Line > 0 and then
           (Value = ""     or else Value = "1" or else
            Value = "true" or else Value = "yes");
      end;
   end Is_Set;

   function File_Name(Cfg: Configuration) return String is
   begin
     return Cfg.Config_File.all;
   end File_Name;

   procedure Replace_Value(Cfg     : in Configuration;
                           Section : in String;
                           Mark    : in String;
                           New_Value: in String)
   is
      Line              : String(1 .. Max_Line_Length);
      Value_Start       : Natural;
      Value_End         : Natural;
      Found_Line        : Natural;
      Equal_Ind         : Natural;
      Line_End          : Natural    := 0;
      Line_Count        : Natural    := 0;
      use Ada.Text_IO;
      File_1, File_2    : File_Type;
      use Ada.Strings.Fixed;
   begin
      Get_Value(Cfg, Section, Mark, Line, Value_Start, Value_End, Found_Line);
      if Found_Line = 0 then
         raise Location_Not_found;
      end if;
      Open(File_1, In_File, Cfg.Config_File.all);
      Create(File_2, Out_File, ""); -- temp file
      Read_File:
      while not End_Of_File(File_1) loop
         Get_Line(File_1, Line, Line_End);
         Line_Count:= Line_Count + 1;
         if Line_Count = Found_Line then -- Change this line
            Equal_Ind := Index(Source  => Line(Line'First .. Line_End),
                               Pattern => "=");
            if Equal_Ind < Line'First then -- No '=' yet, will change...
              Put(File_2, Line(Line'First .. Line_End) & '=');
            else
              Put(File_2, Line(Line'First .. Equal_Ind));
            end if;
            Put_Line(File_2, New_Value);
         else -- any other line: just copy
            Put_Line(File_2, Line(Line'First .. Line_End));
         end if;
      end loop Read_File;
      Close(File_1);
      -- To stick with pure Ada 95, there is no "rename", so
      -- we need to do copy & delete !
      declare
         Temp_name: constant String:= Name(File_2);
      begin
         Close(File_2);
         Open(File_1, In_File, Temp_name);
      end;
      Create(File_2, Out_File, Cfg.Config_File.all);
      Read_File_Again:
      while not End_Of_File(File_1) loop
         Get_Line(File_1, Line, Line_End);
         Put_Line(File_2, Line(Line'First .. Line_End));
      end loop Read_File_Again;
      Delete(File_1);
      Close(File_2);
   end Replace_Value;

end Config;
