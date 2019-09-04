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

with Py_Bind.Py_Type_Descriptor;
with Py_Bind.Py_Value;
with System;

generic
   with package Self_Val is new Py_Bind.Py_Value (<>);
package Py_Bind.Py_Property is

   generic
      with package Val_Desc is new Py_Bind.Py_Type_Descriptor (<>);
      Property_Name : String;
      with function Getter
        (Self : Self_Val.Val_Desc.Ada_T) return Val_Desc.Ada_Type;
      with procedure Setter (Self : in out Self_Val.Val_Desc.Ada_T;
                             Val  : Val_Desc.Ada_Type);
   package Byval is
      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
        with Convention => C;

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
        with Convention => C;
   end Byval;

   generic
      with package Val_Desc is new Py_Bind.Py_Type_Descriptor (<>);
      Property_Name : String;
      with function Getter
        (Self : Self_Val.Access_Desc.Ada_T) return Val_Desc.Ada_Type;
      with procedure Setter (Self : Self_Val.Access_Desc.Ada_T;
                             Val  : Val_Desc.Ada_Type);
   package Byref is
      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
        with Convention => C;

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
        with Convention => C;
   end Byref;

end Py_Bind.Py_Property;
