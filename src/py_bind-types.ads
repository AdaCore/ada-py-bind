with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Py_Bind.Py_Type_Descriptor;

package Py_Bind.Types is
   function To_Python
     (Self : String) return Unmanaged_Py_Object
   is
     (Unmanaged_Py_Object'(Py_Data => PyString_FromString (Self)));
   
   function To_Ada
     (Self : PyObject) return String
   is 
     (PyString_AsString (Self));
   
   function Py_Type return PyObject is (Py_None);
   
   package String_Type is new Py_Bind.Py_Type_Descriptor
     (String, Unmanaged_Py_Object);
    
end Py_Bind.Types; 
