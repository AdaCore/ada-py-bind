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

with System.Aux_DEC; use System.Aux_DEC;

with Py_Bind.Py_Type_Descriptor;

--  This package exposes:
--
--  1. Utilities from going from Ada common types to Python types, and vice
--  versa. You have functions that you can use directly, but that you shouldn't
--  have to use most of the time with Py_Bind APIs, and type descriptors
--  packages, that will give you a higher level way of interacting with
--  the types.
--
--  2. Generics that will allow the creation of new functions and type
--  descriptors for Ada basic types such as custom Integers/Scalars/Enums.

package Py_Bind.Types is

   function Py_No_Type return PyObject is (Py_None);
   --  Return the absence of type in Python (eg. None)

   function Py_String_Type return PyObject;
   --  Return Python's string type

   function Py_Int_Type return PyObject;
   --  Return Python's int type

   function Py_Float_Type return PyObject;
   --  Return Python's int type

   type Unit_Type is null record;
   --  Type representing the absence of type

   -----------------------------------------
   --  Ada -> Python Conversion functions --
   -----------------------------------------

   --  Functions used to convert a Python type to an Ada type. Used in type
   --  descriptors instanciations below.

   function To_Ada (Dummy : PyObject) return Unit_Type;
   function To_Ada (Self : PyObject) return String;
   function To_Ada (Self : PyObject) return Integer;
   function To_Ada (Self : PyObject) return Float;

   -----------------------------------------
   --  Ada -> Python Conversion functions --
   -----------------------------------------

   --  Functions used to convert an Ada type to a python type. Used in type
   --  descriptors instanciations below.

   function To_Python (Dummy : Unit_Type) return Py_Object'Class;
   function To_Python (Self : String) return Py_Object'Class;
   function To_Python (Self : Integer) return Py_Object'Class;
   function To_Python (Self : Float) return Py_Object'Class;

   -----------------------------
   -- Standard types bindings --
   -----------------------------

   --  Set of type descriptors meant to be used to bind Ada to Python. You can
   --  pass the type descriptor directly when one is needed as a parameter, or
   --  use the functions.

   package No_Type is new Py_Bind.Py_Type_Descriptor
     (Unit_Type, Py_No_Type);

   package String_Type is new Py_Bind.Py_Type_Descriptor
     (String, Py_String_Type);

   package Int_Type is new Py_Bind.Py_Type_Descriptor (Integer, Py_Int_Type);

   package Float_Type is new Py_Bind.Py_Type_Descriptor (Float, Py_Float_Type);

   -------------------------------------------
   -- Generic type descriptors constructors --
   -------------------------------------------

   --  Set of generic type descriptor constructors. Useful if you want to:
   --
   --  - Bind non standard discrete/scalar types.
   --
   --  - Do lightweight bindings of enum types.

   generic
      type T is (<>);
   package Simple_Discrete_Binding is
      function To_Python (Self : T) return Py_Object'Class;
      function To_Ada (Self : PyObject) return T;

      package Type_Desc is new Py_Bind.Py_Type_Descriptor (T, Py_Int_Type);
   end Simple_Discrete_Binding;
   --  Create a type descriptor to do a simple Ada discrete type binding. This
   --  is useful for any discrete type that is not handled by the predefined
   --  type descriptors above.

   generic
      type T is (<>);
      --  Enum type to bind.
   package Simple_Enum_Binding is

      pragma Compile_Time_Error
        (T'Type_Class /= Type_Class_Enumeration,
         "T has to be an enumeration type");
      --  Check that the type passed as parameter is an enum. Emit a compile
      --  error if not.

      function To_Python (Self : T) return Py_Object'Class;
      function To_Ada (Self : PyObject) return T;

      package Type_Desc is new Py_Bind.Py_Type_Descriptor (T, Py_String_Type);
   end Simple_Enum_Binding;
   --  Create a type descriptor to do a simple Ada enum <-> Python string
   --  binding. This is most useful for lightweight binding of enum fields,
   --  where you don't want to create a dedicated python type for the enum
   --  type.

   generic
      type T is digits <>;
   package Simple_Real_Binding is
      function To_Python (Self : T) return Py_Object'Class;
      function To_Ada (Self : PyObject) return T;

      package Type_Desc is new Py_Bind.Py_Type_Descriptor (T, Py_Float_Type);
   end Simple_Real_Binding;
   --  Create a type descriptor to do a simple Ada floating point type binding.
   --  This is useful for any discrete type that is not handled by the
   --  predefined type descriptors above.

end Py_Bind.Types;
