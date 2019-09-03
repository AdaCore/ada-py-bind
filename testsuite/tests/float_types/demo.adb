package body Demo is
   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module is
   begin
      Module.Initialize_Module;
   end Initialize_Module;

   ------------
   -- Py_Add --
   ------------

   function Py_Add_Float (Args : Py_Args) return PyObject is
      X : constant Float := Float_Type.P_To_Ada (Args.Get_Item (1));
      Y : constant Float := Float_Type.P_To_Ada (Args.Get_Item (2));
   begin
      return Float_Type.P_To_Python_Unsafe (X + Y).Py_Data;
   end Py_Add_Float;

end Demo;
