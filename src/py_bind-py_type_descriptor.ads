generic
   type Ada_Type (<>) is private;
   type Py_Object_Type is new Py_Object with private;
   
   with function Py_Type return PyObject is <>;
   
   with function To_Python (Self : Ada_Type) return Py_Object_Type is <>;
   with function To_Ada (Self : PyObject) return Ada_Type is <>;
   
   with procedure Destroy (Self : in out Ada_Type) is null;
package Py_Bind.Py_Type_Descriptor is
   
   subtype Ada_T is Ada_Type;
   subtype Py_Object_T is Py_Object_Type;
   
   function P_To_Python (Self : Ada_T) return Py_Object_T renames To_Python;
   function P_To_Ada (Self : PyObject) return Ada_T renames To_Ada;
   procedure P_Destroy (Self : in out Ada_T) renames Destroy;
   function P_Py_Type return PyObject renames Py_Type;
end Py_Bind.Py_Type_Descriptor;  

 
