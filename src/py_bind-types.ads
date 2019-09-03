with Py_Bind.Py_Type_Descriptor;
with Interfaces.C; use Interfaces.C;

package Py_Bind.Types is

   function To_Python_Unsafe
     (Self : String) return Py_Object'Class
   is
     (Py_Object'(Py_Data => PyString_FromString (Self)));

   function To_Ada (Self : PyObject) return String;
   function Py_No_Type return PyObject is (Py_None);
   function Py_String_Type return PyObject;
   function Py_Int_Type return PyObject;

   function To_Python_Unsafe
     (Self : Integer) return Py_Object'Class
   is
     (Py_Object'(Py_Data => PyInt_FromLong (long (Self))));

   function To_Ada (Self : PyObject) return Integer
   is (Integer (PyInt_AsLong (Self)));

   type Dummy_Type is null record;
   function To_Ada (Dummy : PyObject) return Dummy_Type is (null record);

   function To_Python_Unsafe (Dummy : Dummy_Type) return Py_Object'Class
   is (Py_Object'(Py_Data => Py_None));

   package No_Type is new Py_Bind.Py_Type_Descriptor
     (Dummy_Type, Py_No_Type);
   package String_Type is new Py_Bind.Py_Type_Descriptor
     (String, Py_String_Type);
   package Int_Type is new Py_Bind.Py_Type_Descriptor (Integer, Py_Int_Type);

end Py_Bind.Types;
