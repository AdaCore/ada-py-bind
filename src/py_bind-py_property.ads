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
with System.Aux_DEC; use System.Aux_DEC;

--  Container package to generate properties for a given Python bound type,
--  represented by ``Self_Val``.

generic
   with package Self_Val is new Py_Bind.Py_Value (<>);
   --  Type for ``self``, for generated properties.
package Py_Bind.Py_Property is

   --  Bind a property, given a value descriptor, a getter and a setter. Using
   --  this package, the getter and the setter will take direct instances of
   --  the type descriptor.
   --
   --  WARNING: If the type descriptor describes a composite type *value*, then
   --  this should not be used, but ``From_Py_Value`` should be used.

   generic
      with package Val_Desc is new Py_Bind.Py_Type_Descriptor (<>);
      --  Type descriptor for the value of the property.

      Property_Name : String;
      --  Name of the resulting property.

      with function Getter
        (Self : Self_Val.Val_Desc.Ada_T) return Val_Desc.Ada_Type;
      --  Getter for the property

      with procedure Setter
        (Self : in out Self_Val.Val_Desc.Ada_T; Val : Val_Desc.Ada_Type);
      --  Setter for the property

      Override_Composite : Boolean := False;
      --  Whether to override the error that will be raised when you pass a
      --  composite type in Val_Desc.
   package From_Descriptor is private

      pragma Compile_Time_Error
        (not Override_Composite
         and then
           (Val_Desc.Ada_T'Type_Class
            in Type_Class_Record | Type_Class_Array | Type_Class_Task),
         "T has to be a non-composite type. Use From_Py_Value "
          & "or set Override_Composite");
      --  Check that the type passed as parameter is an enum. Emit a compile
      --  error if not.

      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
        with Convention => C;

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
        with Convention => C;
   end From_Descriptor;

   --  Bind a property, given a ``Py_Value``, a getter and a setter. By
   --  default, if you instantiated a ``Py_Value`` package to bind some
   --  Ada data structure, this should be the go-to option, because it
   --  will correctly handle by-reference semantics for you.
   --
   --  NOTE: The getter and setter will use access types, both for the receiver
   --  value and for the property value. This is necessary in order to properly
   --  implement by-reference semantics. If you don't need that, consider using
   --  ``From_Descriptor``.

   generic
      with package Val_Desc is new Py_Bind.Py_Value (<>);
      --  Type descriptor for the value of the property.

      Property_Name : String;
      --  Name of the resulting property.

      with function Getter
        (Self : Self_Val.Access_Desc.Ada_T) return Val_Desc.Access_Desc.Ada_T;
      --  Getter function for the property.

      with procedure Setter
        (Self : Self_Val.Access_Desc.Ada_T;
         Val  : Val_Desc.Access_Desc.Ada_T);
      --  Setter procedure for the property.

   package From_Py_Value is private
      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
        with Convention => C;

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
        with Convention => C;
   end From_Py_Value;

   --  Bind a read-only property, given a value descriptor and a getter. The
   --  getter passes ``self`` by value.

   generic
      with package Val_Desc is new Py_Bind.Py_Type_Descriptor (<>);
      --  Type descriptor for the value of the property.

      Property_Name : String;
      --  Name of the resulting property.

      with function Getter
        (Self : Self_Val.Val_Desc.Ada_T) return Val_Desc.Ada_Type;
      --  Getter function for the property.

   package Read_Only is
   end Read_Only;

end Py_Bind.Py_Property;
