generic
   type Ada_Type (<>) is private;

   with function Py_Type return PyObject is <>;

   with function To_Python_Unsafe
     (Self : Ada_Type) return Py_Object'Class is <>;

   with function To_Ada (Self : PyObject) return Ada_Type is <>;
package Py_Bind.Py_Type_Descriptor is

   subtype Ada_T is Ada_Type;

   function To_Python (Self : Ada_T) return Py_Object_Ref;

   function P_To_Python_Unsafe
     (Self : Ada_T) return Py_Object'Class renames To_Python_Unsafe;

   function P_To_Ada (Self : PyObject) return Ada_T renames To_Ada;
   function P_Py_Type return PyObject renames Py_Type;

   function Get_Arg (Args : Py_Args; Index : Positive) return Ada_Type;
end Py_Bind.Py_Type_Descriptor;
