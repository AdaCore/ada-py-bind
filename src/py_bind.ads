with GNATCOLL.Python; use GNATCOLL.Python;
with GNATCOLL.Traces;
with GNATCOLL.Refcount;
with Ada.Containers.Vectors;

package Py_Bind is
   type Py_Object is interface;

   function Get_PyObject (Self : Py_Object) return PyObject is abstract;
   procedure Destroy (Self : in out Py_Object) is null;

   use GNATCOLL;
   use GNATCOLL.Traces;

   type Managed_Py_Object is new Refcount.Refcounted and Py_Object with record
      Py_Data : PyObject := Py_None;
   end record;

   type Unmanaged_Py_Object is new Py_Object with record
      Py_Data : PyObject := Py_None;
   end record;

   overriding function Get_PyObject
     (Self : Managed_Py_Object) return PyObject
   is
     (Self.Py_Data);

   overriding function Get_PyObject
     (Self : Unmanaged_Py_Object) return PyObject
   is
     (Self.Py_Data);

   overriding procedure Destroy
     (Self : in out Unmanaged_Py_Object);
   overriding procedure Destroy
     (Self : in out Managed_Py_Object);

   Me : Trace_Handle := Create ("Py_Bind", From_Config);

   type Init_Fn is access procedure;

   package Init_Fn_Vectors is new Ada.Containers.Vectors (Positive, Init_Fn);
   subtype Init_Fn_Vector is Init_Fn_Vectors.Vector;

   type Module_Descriptor is record
      Init_Fns : Init_Fn_Vector;
      Module : PyObject;
   end record;

   No_Module_Descriptor : Module_Descriptor :=
     (Init_Fn_Vectors.Empty_Vector, Py_None);
end Py_Bind;
