package body Py_Bind.Py_Type_Descriptor is

   -------------
   -- Get_Arg --
   -------------

   function Get_Arg (Args : Py_Args; Index : Positive) return Ada_Type
   is
      Dummy : PyObject;
   begin
      declare
         P_Obj : constant PyObject := Get_Item (Args, Index);
         A_Obj : constant Ada_Type := To_Ada (P_Obj);
      begin
         Py_DECREF (P_Obj);
         return A_Obj;
      end;
   exception
      when E : others =>
         Dummy := Handle_Error (E);
         raise;
   end Get_Arg;

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Self : Ada_T) return Py_Object_Ref is
      Ret : Py_Object_Ref;
   begin
      Py_Object_Smart_Ptr.Set (Ret, To_Python_Unsafe (Self));
      return Ret;
   end To_Python;

end Py_Bind.Py_Type_Descriptor;
