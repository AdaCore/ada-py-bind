package body Py_Bind.Py_Property is

   procedure Init;

   ----------------
   -- Raw_Getter --
   ----------------

   function Raw_Getter
     (Obj : PyObject; Dummy : System.Address) return PyObject
   is
      Self : constant Self_Val.Val_Desc.Ada_T :=
        Self_Val.Val_Desc.P_To_Ada (Obj);
      P : constant Py_Object'Class := Val_Desc.To_Python (Getter (Self));
   begin
      return P.Get_PyObject;
   end Raw_Getter;

   ----------------
   -- Raw_Setter --
   ----------------

   function Raw_Setter
     (Obj     : PyObject;
      Prop    : PyObject;
      Dummy   : System.Address) return Integer
   is
      Self : Self_Val.Val_Desc.Ada_T := Self_Val.Val_Desc.P_To_Ada (Obj);
      Val : constant Val_Desc.Ada_Type := Val_Desc.To_Ada (Prop);
   begin
      Setter (Self, Val);
      return 0;
   exception
      when others =>
         return 1;
   end Raw_Setter;

   procedure Init is
      Dummy : Boolean := PyDescr_NewGetSet
        (Typ     => Self_Val.Val_Desc.P_Py_Type,
         Name    => Property_Name,
         Setter  => Raw_Setter'Unrestricted_Access,
         Getter  => Raw_Getter'Unrestricted_Access,
         Doc     => "");
   begin
      null;
   end Init;

begin
   Self_Val.Module.Init_Fns.Append (Init'Unrestricted_Access);
end Py_Bind.Py_Property;
