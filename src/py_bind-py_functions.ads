with Py_Bind.Py_Value;

package Py_Bind.Py_Functions is

   generic
      Name     : String;
      with function Func (Args : Py_Args) return PyObject;
      with package Class is new Py_Bind.Py_Value (<>);
      Module : PyObject := Class.Module.Desc.Module;
      Args_Spec : Py_Args_Spec := Empty_Args_Spec;
      Doc    : String := "";
   package Raw_Function is
      function Method return PyMethodDef;
   end Raw_Function;

end Py_Bind.Py_Functions;
