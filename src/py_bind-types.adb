package body Py_Bind.Types is

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

end Py_Bind.Types;
