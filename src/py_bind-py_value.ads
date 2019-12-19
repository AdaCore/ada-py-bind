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
with Py_Bind.Py_Module;

generic
   type Val (<>) is private;

   with package Module is new Py_Bind.Py_Module (<>);

   Name : String;

   with procedure Destroy (Self : in out Val) is null;

   with function Create return Val is <>;
   --  Constructor function for Val. Returns an initialized version, suitable
   --  to initialize the memory of objects created from Python.

package Py_Bind.Py_Value is

   type Val_Access is access all Val;

   type T is new Py_Object with record
      Owns_Data : Boolean;
   end record;

   overriding procedure Destroy (Self : in out T);

   function To_Python (Self : Val) return Py_Object'Class;
   --  Return a new python object wrapping ``Self``. Note that an allocated
   --  copy of ``Self`` will be wrapped.

   function To_Python (Self : Val_Access) return Py_Object'Class;
   --  Return a new python object wrapping ``Self``. Note that Self will be
   --  directly wrapped, so the users needs to make sure that the life-time
   --  of Self is respected from the Python side.

   function To_Ada (Self : PyObject) return Val_Access;
   function To_Ada (Self : PyObject) return Val;

   function Py_Type return PyObject;

   package Access_Desc is new Py_Type_Descriptor (Val_Access);
   package Val_Desc is new Py_Type_Descriptor (Val);

   No_T : constant T := (Py_Object with False);

end Py_Bind.Py_Value;
