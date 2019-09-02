with Py_Bind.Py_Value;
with Py_Bind.Py_Module;

package Py_Bind.Py_Functions is

   generic
      Name     : String;
      with function Func (Args : Py_Args) return PyObject;
      with package Class is new Py_Bind.Py_Value (<>);

      Profile  : Py_Fn_Profile := Empty_Profile;
      Doc      : String := "";
   package Raw_Method is
      function Method return PyMethodDef;
   end Raw_Method;

   generic
      Name     : String;
      with function Func (Args : Py_Args) return PyObject;
      with package Module is new Py_Bind.Py_Module (<>);
      Profile  : Py_Fn_Profile;
      Doc      : String := "";
   package Raw_Function is
      function Method return PyMethodDef;
   end Raw_Function;

end Py_Bind.Py_Functions;
