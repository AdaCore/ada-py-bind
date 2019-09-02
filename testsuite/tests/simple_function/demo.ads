with GNATCOLL.Python; use GNATCOLL.Python;

with Py_Bind; use Py_Bind;
with Py_Bind.Py_Module;
with Py_Bind.Types; use Py_Bind.Types;
with Py_Bind.Py_Functions;

pragma Warnings (Off, "unrecognized pragma ""Py_Bind");

package Demo is
   package Module is new Py_Module ("gen");

   function Py_Add (Args : Py_Args) return PyObject;

   package Py_Add_Fn is new Py_Bind.Py_Functions.Raw_Function
     ("add", Py_Add, Module,
      Doc => "Add two numbers",
      Profile => Create_Profile
        ((Arg_Spec ("x", Int_Type.P_Py_Type),
          Arg_Spec ("y", Int_Type.P_Py_Type)), Int_Type.P_Py_Type));

   procedure Initialize_Module;
   pragma Export (C, Initialize_Module, "initgen");
end Demo;
