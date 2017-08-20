-- Created On      : Fri Apr 26 08:13:44 1996

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

package body Config is

   procedure Free is new Ada.Unchecked_Deallocation(String, Str_Ptr);

   procedure Init(Cfg                 : out Configuration;
                  File_Name           :     String;
                  Case_Sensitive      :     Boolean := True;
                  On_Type_Mismatch    :     Type_Mismatch_Action := Raise_Data_Error;
                  Variable_Terminator :     Character := '='
                  )
   is
   begin
      Free(Cfg.Config_File);
      Cfg.Config_File := new String'(File_Name);
      Cfg.Case_Sensitive:= Case_Sensitive;
      Cfg.On_Type_Mismatch:= On_Type_Mismatch;
      Cfg.Variable_Terminator:= Variable_Terminator;
   end Init;

   function Init(File_Name           :  String;
                 Case_Sensitive      :  Boolean := True;
                 On_Type_Mismatch    :  Type_Mismatch_Action := Raise_Data_Error;
                 Variable_Terminator :  Character := '='
                 )
   return Configuration
   is
      Cfg: Configuration;
   begin
      Init(Cfg, File_Name, Case_Sensitive, On_Type_Mismatch, Variable_Terminator);
      return Cfg;
   end Init;


   function Is_Number_Start (c: Character) return Boolean is
   begin
      case c is
         when '0'..'9' | '+' | '-' =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Number_Start;


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
                     In_Found_Section := Section = Line(Line'First+1..Found_Section_End);
                  else
                     In_Found_Section :=
                       To_Lower(Section) = To_Lower(Line(Line'First+1..Found_Section_End));
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
                                        Pattern => (1 => Cfg.Variable_Terminator));
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
                     --   Put_Line("Config: found_mark    => [" &
                     --   Line(Found_Mark_start..Found_Mark_End) & "] equal ind=" & Equal_Ind'Img &
                     --   " termi=[" & Cfg.Variable_Terminator & ']');
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

   function Value_Of(Cfg     : Configuration;
                     Section : String;
                     Mark    : String;
                     Default : String := "")
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

   procedure Type_Error(Cfg: Configuration; Val, Desc: String) is
      use Ada.Text_IO;
      Err_Msg : constant String := "'" & Val & "' is not " & Desc;
   begin
      case Cfg.On_Type_Mismatch is
         when Raise_Data_Error =>
            raise Ada.Text_IO.Data_Error with Err_Msg;

         when Print_Warning    =>
            Put_Line(Standard_Error, "Config: warning: " & Err_Msg);

         when Be_Quiet         =>
            null;
      end case;
   end Type_Error;

   function Value_Of(Cfg     : Configuration;
                     Section : String;
                     Mark    : String;
                     Default : Integer := 0)
   return Integer
   is
      Line              : String(1 .. Max_Line_Length);
      Value_Start       : Natural;
      Value_End         : Natural;
      Found_Line        : Natural;
   begin
      Get_Value(Cfg, Section, Mark, Line, Value_Start, Value_End, Found_Line);

      if Found_Line = 0 then
         return Default;
      end if;

      declare
         Value_As_String : constant String := Line (Value_Start .. Value_End);
      begin
         if Value_As_String'Length > 2 and then
           Line (Value_Start .. Value_Start+1) = "0x"
         then
            return Integer'Value("16#" &
                                   Line(Value_Start+2 .. Value_End) &
                                   "#");
         elsif Value_As_String'Length > 0  and then
           Is_Number_Start(Line(Value_Start))
         then
            return Integer'Value(Value_As_String);
         else
            Type_Error(Cfg, Value_As_String, "an integer number in line"&Found_Line'Img);
            return Default;
         end if;
      end;
   exception
      when others =>
         Type_Error(Cfg, Line (Value_Start..Value_End), "an integer number in line"&Found_Line'Img);
         return Default;
   end Value_Of;

   function Value_Of(Cfg     : Configuration;
                     Section : String;
                     Mark    : String;
                     Default : Long_Float := 0.0)
   return Long_Float
   is
      Value_As_String : constant String := Value_Of(Cfg, Section, Mark);
      Val  : Long_Float;
      Last : Positive;
      package LFIO is new Ada.Text_IO.Float_IO(Long_FLoat);
   begin
      if Value_As_String'Length > 0 and then
         Is_Number_Start(Value_As_String(Value_As_String'First))
      then
         -- Val := Long_Float'Value(Value_As_String);
         -- ^ an old compiler doesn't like some floats repr. through 'Value
         LFIO.Get(Value_As_String, Val, Last);
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

   function Value_Of(Cfg     : Configuration;
                     Section : String;
                     Mark    : String;
                     Default : Boolean := False)
   return Boolean
   is
   begin
      return Boolean'Value(Value_Of(Cfg, Section, Mark, Boolean'Image(Default)));
   end Value_Of;

   -- Return True if one of the following conditions is met:
   --  o the Mark is within the Section, but no equal sign is in that line,
   --  o the Mark is set to either 1, True or Yes.
   -- All other cases return False.
   function Is_Set(Cfg     : Configuration;
                   Section : String;
                   Mark    : String)
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

   -- List of strings, for memorizing a config file.

   type Ini_Line;
   type Ini_Line_Ptr is access Ini_Line;
   type Ini_Line is record
     next: Ini_Line_Ptr:= null;
     line: Str_Ptr;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation(Ini_Line, Ini_Line_Ptr);

   procedure Write_and_Free(Cfg         : in     Configuration;
                            new_contents: in out Ini_Line_Ptr)
   is
      curr, to_free: Ini_Line_Ptr:= null;
      use Ada.Text_IO;
      File              : File_Type;
   begin
      Create(File, Out_File, Cfg.Config_File.all);
      curr:= new_contents;
      while curr /= null loop
         Put_Line(File, curr.line.all);
         to_free:= curr;
         curr:= curr.next;
         Free(to_free.line);
         Free(to_free);
      end loop;
      Close(File);
      new_contents:= null;
   end Write_and_Free;

   procedure Replace_Value(Cfg      : Configuration;
                           Section  : String;
                           Mark     : String;
                           New_Value: String)
   is
      Line              : String(1 .. Max_Line_Length);
      Value_Start       : Natural;
      Value_End         : Natural;
      Found_Line        : Natural;
      Equal_Ind         : Natural;
      Line_End          : Natural    := 0;
      Line_Count        : Natural    := 0;
      use Ada.Text_IO;
      File              : File_Type;
      use Ada.Strings.Fixed;
      --
      root, curr, new_ini_line: Ini_Line_Ptr:= null;
   begin
      Get_Value(Cfg, Section, Mark, Line, Value_Start, Value_End, Found_Line);
      if Found_Line = 0 then
         raise Location_Not_Found;
      end if;
      Open(File, In_File, Cfg.Config_File.all);
      Read_File:
      while not End_Of_File(File) loop
         Get_Line(File, Line, Line_End);
         Line_Count:= Line_Count + 1;
         --
         new_ini_line:= new Ini_Line;
         if root = null then
           root:= new_ini_line;
         else
           curr.next:= new_ini_line;
         end if;
         curr:= new_ini_line;
         --
         if Line_Count = Found_Line then -- Change this line
            Equal_Ind := Index(Source  => Line(1 .. Line_End),
                               Pattern => (1 => Cfg.Variable_Terminator));
            if Equal_Ind < 1 then -- No '=' or so yet, will change...
              curr.line:= new String'(Line(1 .. Line_End) & Cfg.Variable_Terminator & New_Value);
            else
              curr.line:= new String'(Line(1 .. Equal_Ind) & New_Value);
            end if;
         else -- any other line: just copy
            curr.line:= new String'(Line(1 .. Line_End));
         end if;
      end loop Read_File;
      Close(File);
      -- Now, write the new file
      Write_and_Free(Cfg, root);
   end Replace_Value;

   procedure Replace_Section(Cfg         : Configuration;
                             Section     : String;
                             New_Contents: String)
   is
      Line              : String(1 .. Max_Line_Length);
      Line_End          : Natural    := 0;
      Line_Count        : Natural    := 0;
      use Ada.Text_IO;
      File              : File_Type;
      use Ada.Strings.Fixed;
      --
      root, curr, new_ini_line: Ini_Line_Ptr:= null;
      --
      procedure List_progress is
      begin
         new_ini_line:= new Ini_Line;
         if root = null then
           root:= new_ini_line;
         else
           curr.next:= new_ini_line;
         end if;
         curr:= new_ini_line;
      end;
      --
      Matched_section, Found_section: Boolean:= False;
      I: Natural:= New_Contents'First;
      use Ada.Characters.Handling;
   begin
      Open(File, In_File, Cfg.Config_File.all);
      Read_File:
      while not End_Of_File(File) loop
         Get_Line(File, Line, Line_End);
         Line_Count:= Line_Count + 1;
         if Line_End > 0 and then
            Line(1)= '['
         then                     -- It is a section header.
            Matched_section:=
               Line_End >= 2 + Section'Length and then
                 (
                      (Cfg.Case_Sensitive and then
                       Line(2..2 + Section'Length) = Section & ']'
                      )
                    or else
                      ((not Cfg.Case_Sensitive) and then
                        To_Lower(Line(2..2 + Section'Length)) = To_Lower(Section) & ']'
                      )
                 );
            List_progress;
            curr.line:= new String'(Line(1 .. Line_End));
            if Matched_section then
               Found_section:= True;
               for J in New_Contents'Range loop -- copy new contents
                  if New_Contents(J)= LF then
                     List_progress;
                     curr.line:= new String'(New_Contents(I .. J-1));
                     I:= J+1;
                  end if;
                  if J = New_Contents'Last then
                     List_progress;
                     curr.line:= new String'(New_Contents(I .. J));
                  end if;
                  -- NB: we can have have a LF at the end, hence both "if"-s
               end loop;
            end if;
         elsif Matched_section then
            null; -- don't copy old contents
         else
            List_progress;
            curr.line:= new String'(Line(1 .. Line_End));
         end if;
      end loop Read_File;
      Close(File);
      -- Now, write the new file
      Write_and_Free(Cfg, root);
      if not Found_section then
         raise Section_Not_Found;
      end if;
   end Replace_Section;


   --  Disable the Mark using a semicolon prefix
   --
   procedure Disable(Cfg      : Configuration;
                     Section  : String;
                     Mark     : String)
   is
      Line              : String(1 .. Max_Line_Length);
      Value_Start       : Natural;
      Value_End         : Natural;
      Found_Line        : Natural;
      Line_End          : Natural    := 0;
      Line_Count        : Natural    := 0;
      use Ada.Text_IO;
      File              : File_Type;
      use Ada.Strings.Fixed;
      --
      root, curr, new_ini_line: Ini_Line_Ptr:= null;
   begin
      Get_Value(Cfg, Section, Mark, Line, Value_Start, Value_End, Found_Line);
      if Found_Line = 0 then
         raise Location_Not_Found;
      end if;
      Open(File, In_File, Cfg.Config_File.all);
      Read_File:
      while not End_Of_File(File) loop
         Get_Line(File, Line, Line_End);
         Line_Count:= Line_Count + 1;
         --
         new_ini_line:= new Ini_Line;
         if root = null then
           root:= new_ini_line;
         else
           curr.next:= new_ini_line;
         end if;
         curr:= new_ini_line;
         --
         if Line_Count = Found_Line then -- Comment out this line
            curr.line:= new String'("; " & Line(1 .. Line_End));
         else -- any other line: just copy
            curr.line:= new String'(Line(1 .. Line_End));
         end if;
      end loop Read_File;
      Close(File);
      -- Now, write the new file
      Write_and_Free(Cfg, root);
   end Disable;


   function Read_Sections (Cfg : Configuration) return Section_List
   is
      use Ada.Text_IO;
      use Ada.Strings.Fixed;

      File          : File_Type;
      Line          : String (1..1000);
      Line_End      : Natural     := 0;
      Sect_End      : Natural;

      Section_Names : Section_List;
   begin
      Open(File, In_File, Cfg.Config_File.all);

      Read_File:
      while not End_Of_File(File) loop
         Get_Line(File, Line, Line_End); -- error if line end > line'Last
         if Line_End > 1 then
            if Line(Line'First) = '[' then
               --  Line'first = 1 as defined above
               Sect_End := Index (Line(1 .. Line_End), "]") - 1;
               String_Vector.Append (Section_Names, Line(2 .. Sect_End));
            end if;
         end if;
      end loop Read_File;
      Close(File);
      return Section_Names;
   end Read_Sections;

end Config;
