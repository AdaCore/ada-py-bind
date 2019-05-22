package body Py_Bind.Py_Module is
   procedure Initialize_Module is
   begin
      Desc.Module := Py_InitModule (Module_Name);
   end Initialize_Module;
begin
   Initialize_Module;
end Py_Bind.Py_Module;
