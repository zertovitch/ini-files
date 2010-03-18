-----------------------------------------------------------------------------
--  Config - A simple package for parsing and modifying configuration files
--           (also known as .ini, .inf, .cfg, ... files)
--
--  Copyright (c) Rolf Ebert 1996..2009
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2007 on the site
-- http://www.opensource.org/licenses/mit-license.php

-----------------------------------------------------------------------------

-- Change log
--
-- 20-Jul-2009:  GdM: - Added Replace_Section
-- 17-Jul-2009:  GdM: - Improved Replace_Value
-- 15-Jul-2009:  GdM: - Added type Configuration to wrap name and options
--                    - Added Replace_Value
--                    - A few fixes
-- 26-Apr-1996:   RE: Created
--

-- Created On      : Fri Apr 26 08:09:14 1996


package Config is

   web: constant String:= "http://sourceforge.net/projects/ini-files/";
   -- ^-- hopefully the latest version is there

   type Configuration is tagged private;

   type Type_Mismatch_Action is ( Raise_Data_Error,
                                  Print_Warning,
                                  Be_Quiet );

   -- Initialize the package by providing the absolute path to the
   -- file where the configuration parameters are stored.  The
   -- routine can be called multiple times to read several files.
   --
   procedure Init(Cfg              : out Configuration;
                  File_Name        :  in String;
                  --
                  -- Read the config file in a case sensitive way if True, i.e. Section
                  -- and Mark must match exactly the upper and lower case spelling.  If
                  -- set False, case is irrelevant for Section and Mark.  The return value
                  -- still contains the exact case as in the config file.
                  Case_Sensitive   :  in Boolean := True;
                  --
                  -- What to do in case the found mark does not match the expected type
                  -- (e.g. the program wants an integer, but the config file contains a
                  -- string)
                  On_Type_Mismatch :  in Type_Mismatch_Action := Raise_Data_Error
                  --
                  );


   -- We intentionally use the built-in types Integer and Long_Float to keep
   -- this package as portable as possible and to avoid unnecessary project
   -- dependencies.  Clients of this package generally can directly convert
   -- to the target types.
   --
   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in String := "") return String;

   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in Integer := 0) return Integer;

   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in Long_Float := 0.0) return Long_Float;

   function Value_Of(Cfg     : in Configuration;
                     Section : in String;
                     Mark    : in String;
                     Default : in Boolean := False) return Boolean;

   -- Return True if one of the following conditions is met:
   --  o the Mark is within the Section, but no equal sign is in that line,
   --  o the Mark is set to either 1, True or Yes.
   -- All other cases return False.
   --
   function Is_Set(Cfg     : in Configuration;
                   Section : in String;
                   Mark    : in String) return Boolean;

   -- Get the file name, e.g. to rewrite a fresh new config file.
   --
   function File_Name(Cfg: Configuration) return String;

   -- Replace a single value in a configuration file.
   -- The whole .ini file is rewritten each time, so it's rather a
   -- solution for making sporadic changes.
   -- For changing many values, it might be better to rewrite
   -- the whole file in one go, or at least use Replace_Section.
   --
   procedure Replace_Value(Cfg      : in Configuration;
                           Section  : in String;
                           Mark     : in String;
                           New_Value: in String);

   Location_Not_Found: exception;

   -- Replace the full contents of a section with new contents.
   -- Line breaks are obtained by inserting the LF character
   -- (defined below) in the New_Contents string.
   -- Replace_Section is especially useful for config files
   -- shared by several programs, with not all sections in common.
   --
   procedure Replace_Section(Cfg         : in Configuration;
                             Section     : in String;
                             New_Contents: in String);

   LF: constant Character:= Character'Val(10);

   Section_Not_Found: exception;

private

   type Str_Ptr is access String;

   type Configuration is tagged record
      Config_File      : Str_Ptr:= null;
      Case_Sensitive   : Boolean := True;
      On_Type_Mismatch : Type_Mismatch_Action := Raise_Data_Error;
   end record;

end Config;
