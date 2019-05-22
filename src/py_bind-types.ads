with Py_Bind.Py_Type_Descriptor;
with Interfaces.C; use Interfaces.C;

package Py_Bind.Types is
   function To_Python
     (Self : String) return Unmanaged_Py_Object
   is
     (Unmanaged_Py_Object'(Py_Data => PyString_FromString (Self)));

   function To_Ada (Self : PyObject) return String
   is (PyString_AsString (Self));

   function Py_Type return PyObject is (Py_None);

   package String_Type is new Py_Bind.Py_Type_Descriptor
     (String, Unmanaged_Py_Object);

   function To_Python
     (Self : Integer) return Unmanaged_Py_Object
   is
     (Unmanaged_Py_Object'(Py_Data => PyInt_FromLong (long (Self))));

   function To_Ada (Self : PyObject) return Integer
   is (Integer (PyInt_AsLong (Self)));

   package Int_Type is new Py_Bind.Py_Type_Descriptor
     (Integer, Unmanaged_Py_Object);

end Py_Bind.Types;
