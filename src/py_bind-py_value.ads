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
   type Rec is private;
   with package Module is new Py_Bind.Py_Module (<>);
   Name : String;
   with procedure Destroy (Self : in out Rec) is null;
package Py_Bind.Py_Value is

   type Rec_Access is access all Rec;

   type T is new Py_Object with record
      Owns_Data : Boolean;
   end record;

   overriding procedure Destroy (Self : in out T);

   function To_Python (Self : Rec) return Py_Object'Class;
   function To_Python (Self : Rec_Access) return Py_Object'Class;

   function To_Ada (Self : PyObject) return Rec_Access;
   function To_Ada (Self : PyObject) return Rec;

   function Py_Type return PyObject;

   package Access_Desc is new Py_Type_Descriptor (Rec_Access);
   package Val_Desc is new Py_Type_Descriptor (Rec);

   No_T : constant T := (Py_Object with False);

end Py_Bind.Py_Value;
