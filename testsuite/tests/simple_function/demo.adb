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

   function Py_Add (Args : Py_Args) return PyObject is
      X : constant Integer := Int_Type.P_To_Ada (Args.Get_Item (1));
      Y : constant Integer := Int_Type.P_To_Ada (Args.Get_Item (2));
   begin
      return Int_Type.P_To_Python_Unsafe (X + Y).Py_Data;
   end Py_Add;

end Demo;
