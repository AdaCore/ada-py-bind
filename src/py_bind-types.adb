with Interfaces.C; use Interfaces.C;

package body Py_Bind.Types is
   PyString_Type : aliased Dummy;
   pragma Import (C, PyString_Type, "PyString_Type");

   PyInt_Type : aliased Dummy;
   pragma Import (C, PyInt_Type, "PyInt_Type");

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return String
   is
   begin
      if PyString_Check (Self) then
         return PyString_AsString (Self);
      else
         Type_Error ("Expected string, got " & Image (Self));
         raise Python_Type_Error;
      end if;
   end To_Ada;

   --------------------
   -- Py_String_Type --
   --------------------

   function Py_String_Type return PyObject is
   begin
      return PyString_Type'Unrestricted_Access;
   end Py_String_Type;

   -----------------
   -- Py_Int_Type --
   -----------------

   function Py_Int_Type return PyObject is
   begin
      return PyInt_Type'Unrestricted_Access;
   end Py_Int_Type;

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Dummy : Unit_Type) return Py_Object'Class
   is (Py_Object'(Py_Data => Py_None));

   ---------------
   -- To_Python --
   ---------------

   function To_Python
     (Self : String) return Py_Object'Class
   is
     (Py_Object'(Py_Data => PyString_FromString (Self)));

   ---------------
   -- To_Python --
   ---------------

   function To_Python
     (Self : Integer) return Py_Object'Class
   is
     (Py_Object'(Py_Data => PyInt_FromLong (long (Self))));

   function To_Ada (Dummy : PyObject) return Unit_Type is (null record);

   function To_Ada (Self : PyObject) return Integer
   is (Integer (PyInt_AsLong (Self)));

end Py_Bind.Types;
