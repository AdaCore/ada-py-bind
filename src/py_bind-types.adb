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

end Py_Bind.Types;
