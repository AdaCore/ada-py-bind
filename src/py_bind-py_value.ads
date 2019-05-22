with Py_Bind.Py_Type_Descriptor;
with Py_Bind.Py_Module;

generic
   type Rec is private;
   with package Module is new Py_Bind.Py_Module (<>);
   Name : String;
   with procedure Destroy (Self : in out Rec) is null;
package Py_Bind.Py_Value is

   type Rec_Access is access all Rec;

   type T is new Managed_Py_Object with record
      Owns_Data : Boolean;
   end record;

   overriding procedure Free (Self : in out T);

   function To_Python (Self : Rec) return T;
   function To_Python (Self : Rec_Access) return T;

   function To_Ada (Self : PyObject) return Rec_Access;
   function To_Ada (Self : PyObject) return Rec;

   function Py_Type return PyObject;

   package Access_Desc is new Py_Type_Descriptor (Rec_Access, T);
   package Val_Desc is new Py_Type_Descriptor (Rec, T);

   No_T : constant T := (Managed_Py_Object with False);

end Py_Bind.Py_Value;
