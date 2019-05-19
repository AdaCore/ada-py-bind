package body Py_Bind.Py_Module is
   procedure Initialize_Module is
   begin
      Desc.Module := Py_InitModule (Module_Name);

      for Init_Fn of Desc.Init_Fns loop
         Init_Fn.all;
      end loop;
   end Initialize_Module;

end Py_Bind.Py_Module;
