-- Created On      : Fri Apr 26 08:09:14 1996


package Config is

   -- initialize the package by providing the absolute path to the
   -- file where the configuration parameters are stored.  The
   -- routine can be called multiple times to read several files.
   procedure Init(Filename : String);


   -- Read the config file in a case sensitive way if True, i.e. Section
   -- and Mark must match exactly the upper and lower case spelling.  If
   -- set False, case is irrelevant for Section and Mark.  The return value
   -- still contains the exact case as in the config file.
   Case_Sensitive : Boolean := True;

   -- what to do in case the found mark does not match the expected type
   -- (e.g. the program wants an integer, but the config file contains a
   -- string)
   type Type_Mismatch_Action is(Raise_Data_Error,
                                Print_Warning,
                                Be_Quiet);

   On_Type_Mismatch : Type_Mismatch_Action := Print_Warning;

   -- We intentionally use the built in types Integer and Float to keep
   -- this package as portable as possible and to avoid unnecessary project
   -- dependancies.  Clients of this package generally can directly convert
   -- to the target types.
   function Value_Of(Section : in String;
                     Mark    : in String;
                     Default : in String := "") return String;

   function Value_Of(Section : in String;
                     Mark    : in String;
                     Default : in Integer := 0) return Integer;

   function Value_Of(Section : in String;
                     Mark    : in String;
                     Default : in Float := 0.0) return Float;


   -- Return True if one of the following conditions is met:
   --  o the Mark is within the Section, but no equal sign is in that line,
   --  o the Mark is set to either 1, True or Yes.
   -- All other cases return False.
   function Is_Set(Section : in String;
                   Mark    : in String) return Boolean;


end Config;
