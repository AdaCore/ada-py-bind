------------------------------------------------------------------------------
--                              ada-py-bind                                 --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Py_Bind.Py_Value;
with Py_Bind.Py_Module;

package Py_Bind.Py_Functions is

   generic
      Name     : String;
      with function Func (Args : Py_Args) return PyObject;
      with package Class is new Py_Bind.Py_Value (<>);

      Profile  : Py_Fn_Profile := Empty_Profile;
      Doc      : String := "";
   package Raw_Method is
      function Method return PyMethodDef;
   end Raw_Method;

   generic
      Name     : String;
      with function Func (Args : Py_Args) return PyObject;
      with package Module is new Py_Bind.Py_Module (<>);
      Profile  : Py_Fn_Profile;
      Doc      : String := "";
   package Raw_Function is
      function Method return PyMethodDef;
   end Raw_Function;

end Py_Bind.Py_Functions;
