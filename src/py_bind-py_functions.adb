package body Py_Bind.Py_Functions is

   ----------------
   -- Raw_Method --
   ----------------

   package body Raw_Function is

      Args_Spec : constant Py_Args_Spec := Create (Args);

      function Raw_Method
        (Dummy : PyObject; Args : PyObject; KwArgs : PyObject) return PyObject
        with Convention => C;

      ----------------
      -- Raw_Method --
      ----------------

      function Raw_Method
        (Dummy : PyObject; Args : PyObject; KwArgs : PyObject) return PyObject
      is
      begin
         declare
            PA : constant Py_Args := Create (Args, KwArgs, Args_Spec);
         begin
            return Func (PA);
         end;
      exception
         when E : others => return Handle_Error (E);
      end Raw_Method;

      M : constant PyMethodDef
        := Create_Method_Def (Name, Raw_Method'Unrestricted_Access, Doc);

      function Method return PyMethodDef is (M);

   begin
      if Class.Py_Type = Py_None then
         Add_Function (Module, M, null);
      else
         Add_Method (Class.Py_Type, M, null, Module);
      end if;

   end Raw_Function;

end Py_Bind.Py_Functions;
