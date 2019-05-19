with GNATCOLL.Python; use GNATCOLL.Python;

generic
   Module_Name : String;
package Py_Bind.Py_Module is
   procedure Initialize_Module;
   
   Desc : Module_Descriptor;
end Py_Bind.Py_Module;  
