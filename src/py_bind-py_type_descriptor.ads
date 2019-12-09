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

--  This package describes a Python type. While it could be instantiated
--  directly by a client, most use cases are covered either by pre-bound types
--  in ``Py_Bind.Py_Types``, either by helpers in the same package, such as
--  ``Simple_Discrete_Binding``, ``Simple_Enum_Binding``, or finally, via
--  ``Py_Bind.Py_Value`` for record types.
--
--  From the user's standpoint, this module is useful to access properties
--  of a type, as well as helpers, when writing manual bindings for
--  functions/properties.

generic
   type Ada_Type (<>) is private;

   with function Py_Type return PyObject is <>;

   with function To_Python
     (Self : Ada_Type) return Py_Object'Class is <>;

   with function To_Ada (Self : PyObject) return Ada_Type is <>;

package Py_Bind.Py_Type_Descriptor is

   subtype Ada_T is Ada_Type;

   function P_To_Python (Self : Ada_T) return Py_Object_Ref;

   function P_To_Python_Unsafe
     (Self : Ada_T) return Py_Object'Class renames To_Python;

   function P_To_Ada (Self : PyObject) return Ada_T renames To_Ada;
   function P_Py_Type return PyObject renames Py_Type;

   function Get_Arg (Args : Py_Args; Index : Positive) return Ada_Type;
   --  Helper, given a ``Py_Args`` instance, will return the argument at
   --  ``Index`` as ``Ada_Type``.

end Py_Bind.Py_Type_Descriptor;
