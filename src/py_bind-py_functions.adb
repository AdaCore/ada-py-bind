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

package body Py_Bind.Py_Functions is

   ----------------
   -- Raw_Method --
   ----------------

   package body Raw_Method is
      function Raw_Method
        (Dummy : PyObject; Args : PyObject; KwArgs : PyObject) return PyObject
        with Convention => C;

      ----------------
      -- Raw_Method --
      ----------------

      function Raw_Method
        (Dummy : PyObject; Args : PyObject; KwArgs : PyObject) return PyObject
      is
      begin
         return Func (Create (Args, KwArgs, Profile));
      exception
         when E : others => return Handle_Error (E);
      end Raw_Method;

      M : constant PyMethodDef
        := Create_Method_Def (Name, Raw_Method'Unrestricted_Access, Doc);

      function Method return PyMethodDef is (M);

   begin
      Add_Method (Class.Py_Type, M, null, Class.Module.Desc.Module);
   end Raw_Method;

   ------------------
   -- Raw_Function --
   ------------------

   package body Raw_Function is

      function Raw_Method
        (Dummy : PyObject; Args : PyObject; KwArgs : PyObject) return PyObject
        with Convention => C;

      ----------------
      -- Raw_Method --
      ----------------

      function Raw_Method
        (Dummy : PyObject; Args : PyObject; KwArgs : PyObject) return PyObject
      is
      begin
         return Func (Create (Args, KwArgs, Profile));
      exception
         when E : others => return Handle_Error (E);
      end Raw_Method;

      M : constant PyMethodDef
        := Create_Method_Def (Name, Raw_Method'Unrestricted_Access, Doc);

      function Method return PyMethodDef is (M);

   begin
      Add_Function (Module.Desc.Module, M, null);
   end Raw_Function;

end Py_Bind.Py_Functions;
