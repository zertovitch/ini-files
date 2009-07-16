-- Created On      : Fri Apr 26 08:13:44 1996


with Ada.Text_IO;
with Ada.Float_Text_IO; -- needed for alsys buglet
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with Util.Sfile;

package body Config is



   type Str_Ptr is access String;
   Config_File : Str_Ptr;
   procedure Free is new Ada.Unchecked_Deallocation(String, Str_Ptr);


   procedure Init(Filename : String) is
   begin
      if Config_File /= null then
         Free(Config_File);
      end if;

      Config_File := new String'(Filename);
   end Init;


   Global_Found : Boolean := False;

   function Value_Of(Section : in String;
                     Mark    : in String;
                     Default : in String := "")
                    return String
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;
      use Ada.Strings;
      use Ada.Characters.Handling;

      File              : File_Type;
      Filename          : constant String := Config_File.all;

      Max_Line_Length   : constant                      := 1000;
      Line_Start        : Natural                       := 1;
      Line_End          : Natural                       := 0;
      Line              : String(1 .. Max_Line_Length);

      Sect_End          : Natural;
      Comment_Ind       : Natural;
      Equal_Ind         : Natural;

      Found_Section_End : Natural;
      Found_Mark_Start  : Natural;
      Found_Mark_End    : Natural                       := 0;
      Value_Start       : Natural                       := 1;
      Value_End         : Natural                       := 0;
      In_Found_Section  : Boolean                       := False;
   begin -- Value_Of
      Global_Found := False;
      Util.Sfile.Open(File, In_File, Filename); -- may raise error
  Read_File:
      while not End_Of_File(File) loop
         Get_Line(File, Line, Line_End); -- error line_end > max_line_length
         if Line_End > 1 then
            if Line(1) = '[' then
               Sect_End := Index(Source  => Line(1 .. Line_End),
                                 Pattern => "]");
               -- error if ext_end = 0
               Found_Section_End := Sect_End - 1;
               -- pragma Debug(Put_Line("Config: found_section => " &
               -- Line(2..Found_Section_End)));
               if Case_Sensitive then
                  In_Found_Section :=(Section = Line(2..Found_Section_End));
               else
                  In_Found_Section :=
                    (To_Lower(Section) = To_Lower(Line(2..Found_Section_End)));
               end if;
            else
               if Section = "*" then
                  In_Found_Section := True;
               end if;
               if In_Found_Section then
                  Comment_Ind := Index(Source  => Line(1 .. Line_End),
                                       Pattern => "#");
                  if Comment_Ind > 0 then
                     Line_End := Comment_Ind - 1;
                  end if;
                  Equal_Ind := Index(Source  => Line(1 .. Line_End),
                                     Pattern => "=");
                  if Equal_Ind > 0 then
                     Found_Mark_Start :=
                       Index_Non_Blank(Line(1 .. Equal_Ind-1), Forward);
                     Found_Mark_End :=
                       Index_Non_Blank(Line(1 .. Equal_Ind-1), Backward);
                  else
                     Found_Mark_Start :=
                       Index_Non_Blank(Line(1 .. Line_End), Forward);
                     Found_Mark_End :=
                       Index_Non_Blank(Line(1 .. Line_End), Backward);
                  end if;
                  -- pragma Debug(Put_Line("Config: found_mark    => " &
                  -- Line(Found_Mark_start..Found_Mark_End)));
                  if Found_Mark_Start > 0 and then
                    Found_Mark_End > 0
                  then
                     if (Case_Sensitive and then
                         (Line(Found_Mark_Start..Found_Mark_End) = Mark))
                       or else (not Case_Sensitive and then
                                (To_Lower(Line(Found_Mark_Start..
                                               Found_Mark_End))
                                 = To_Lower(Mark)))
                     then
                        Global_Found := True;
                        if Equal_Ind > 0 then
                           Value_Start :=
                             Index_Non_Blank(Line(Equal_Ind+1..Line_End),
                                             Forward);
                           Value_End :=
                             Index_Non_Blank(Line(Value_Start..Line_End),
                                             Backward);
                        else
                           Value_Start  := 1;
                           Value_End    := 0;
                        end if;
                        exit Read_File;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end loop Read_File;
      Util.Sfile.Close(File);
      return Line(Value_Start .. Value_End);
   end Value_Of;


   function Value_Of(Section : in String;
                     Mark    : in String;
                     Default : in Integer := 0)
                    return Integer
   is
      use Ada.Text_IO;
      Value_As_String : constant String := Value_Of(Section, Mark);
   begin -- Value_Of
      if Value_As_String'Length > 2 and then
        Value_As_String(Value_As_String'First..Value_As_String'First+1) = "0x"
      then
         return Integer'Value("16#" &
                              Value_As_String(Value_As_String'First+2 ..
                                              Value_As_String'Last) &
                              "#");
      elsif Value_As_String'Length > 0 then
         return Integer'Value(Value_As_String);
      else
         case On_Type_Mismatch is
            when Raise_Data_Error =>
               raise Ada.Text_IO.Data_Error;

            when Print_Warning    =>
               Put(Standard_Error, "Config: warning `");
               Put(Standard_Error, "' is not an integer number");
               New_Line(Standard_Error);
               return Default;

            when Be_Quiet         =>
               return Default;
         end case;
         -- return Default;
      end if;

   exception
      when others =>
         case On_Type_Mismatch is
            when Raise_Data_Error =>
               raise Ada.Text_IO.Data_Error;

            when Print_Warning    =>
               Put(Standard_Error, "Config: warning `");
               Put(Standard_Error, Value_As_String);
               Put(Standard_Error, "' is not an integer number");
               New_Line(Standard_Error);
               return Default;

            when Be_Quiet         =>
               return Default;

         end case;
         -- return 0;
   end Value_Of;


   function Value_Of(Section : in String;
                     Mark    : in String;
                     Default : in Float := 0.0)
                    return Float
   is
      use Ada.Text_IO;
      Value_As_String : constant String := Value_Of(Section, Mark);
   begin -- Value_Of
      if Value_As_String'Length > 0 then
         declare
            Val : Float;
            Last : Positive;
         begin
            -- Val := Float'Value(Value_As_String);
            Ada.Float_Text_IO.Get(Value_As_String, Val, Last);
            return Val;
         end;
      else
         Put(Standard_Error, "Config: warning `");
         Put(Standard_Error, "' is not a floating point number");
         New_Line(Standard_Error);
         return Default;
      end if;
   exception
      when others =>
         Put(Standard_Error, "Config: warning `");
         Put(Standard_Error, Value_As_String);
         Put(Standard_Error, "' is not a floating point number");
         New_Line(Standard_Error);
         return Default;
   end Value_Of;


   -- Return True if one of the following conditions is met:
   --  o the Mark is within the Section, but no equal sign is in that line,
   --  o the Mark is set to either 1, True or Yes.
   -- All other cases return False.
   function Is_Set(Section : in String;
                   Mark    : in String)
                  return Boolean is
      use Ada.Characters.Handling;
      Value : constant String := To_Lower(Value_Of(Section, Mark));
   begin
      return Global_Found and then
        (Value = ""     or else Value = "1" or else
         Value = "true" or else Value = "yes");
   end Is_Set;


end Config;
