package body Py_Bind.Py_Functions is

   ----------------
   -- Raw_Method --
   ----------------

   package body Raw_Method is
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
            PA : constant Py_Args := Create (Args, KwArgs, Profile);
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
      Add_Method (Class.Py_Type, M, null, Class.Module.Desc.Module);
   end Raw_Method;

   ------------------
   -- Raw_Function --
   ------------------

   package body Raw_Function is

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
            PA : constant Py_Args := Create (Args, KwArgs, Profile);
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
      Add_Function (Module.Desc.Module, M, null);
   end Raw_Function;

end Py_Bind.Py_Functions;
