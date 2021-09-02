Welcome to the Ini file manager
===============================

The Ini file manager consists of a single package, Config, which
can read and modify informations from various configuration
files known as "ini" files. They are called "ini" files because
they have often the ".ini" extension, but some also have
".inf", ".cfg" extensions. They are actually text files which
look like the following:

     ; Comment line
     
     [Section 1]
     a_string = abcd     # a comment here...
     a_number = +123.456 ; another comment
     
     [Section 2]
     a_string = efgh

Intro to the Ini file manager
=============================

For more examples in this format, just search files with
the .ini extension on your computer!
The Config package is typically used as in
the following demo procedure:

        with Config; use Config;
        with Ada.Text_IO; use Ada.Text_IO;
        procedure Small_demo is
          c: Configuration;
        begin
          Init(c, "config.ini");
          Put_Line(Value_Of(c, "Section 2", "a_string"));
        end;

Contents
========
  - config.ads            : package specification
  - config.adb            : package body
  - tests/test_config.adb : test/demo procedure
  - ini_files.gpr         : project file for the GNAT compiler
  - ini_files_oa.prj      : project file for the ObjectAda compiler
  - README.md             : this file

Warning & legal
===============
There is NO WARRANTY in this software. Read copyright notice in config.ads.
  
How to build and test the Ini file manager
==========================================
For building with GNAT/GCC:
      - type "gprbuild -p -P ini_files.gpr" in the command line
  or
      - use the project file, ini_files.gpr, from the
        GNAT Programming Studio (GPS); press F4
  or
      - copy the two files config.ads and config.adb to your project
  or
      - open test_config.adb with AdaGIDE, press F3
  or
      - your way...

For building with ObjectAda, you can use the ini_files_oa.prj project file.

For other compilers, it should be simple as well: have
config.ads, config.adb, test_config.adb at the same place
and build test_config as the main program.
      
As a result there is a test_config[.exe] executable.

How to use the Ini file manager
===============================
Chances are that the only usage you'll make of Config will be to
read values from a configuration file. This is done with the
function Value_Of as in the example above.

If you need to update the configuration file, you can replace single
values (Replace_Value) or whole sections (Replace_Section).
If you need to rewrite the entire configuration file, it is simpler
to do it in one go, with Put_Line's.

Look at test_config.adb for a complete example, especially when it
comes to handle numerical errors contained in a configuration file.

Enjoy!

Gautier de Montmollin & Rolf Ebert
