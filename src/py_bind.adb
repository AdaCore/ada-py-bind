package body Py_Bind is
   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : in out Unmanaged_Py_Object)
   is
   begin
      if Self.Py_Data /= Py_None then
         Py_DECREF (Self.Py_Data);
         Self.Py_Data := Py_None;
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : in out Managed_Py_Object) is
   begin
      if Self.Py_Data /= Py_None then
         Py_DECREF (Self.Py_Data);
         Self.Py_Data := Py_None;
      end if;
   end Destroy;

   -----------
   -- Image --
   -----------

   function Image (Obj : PyObject) return String is
      Py_Repr : constant PyObject := PyObject_Repr (Obj);
   begin
      return S : constant String := PyString_AsString (Py_Repr) do
         Py_DECREF (Py_Repr);
      end return;
   end Image;

end Py_Bind;
