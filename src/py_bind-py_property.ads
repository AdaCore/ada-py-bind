with Py_Bind.Py_Type_Descriptor;
with Py_Bind.Py_Value;
with System;

generic
   with package Self_Val is new Py_Bind.Py_Value (<>);
package Py_Bind.Py_Property is

   generic
      with package Val_Desc is new Py_Bind.Py_Type_Descriptor (<>);
      Property_Name : String;
      with function Getter
        (Self : Self_Val.Val_Desc.Ada_T) return Val_Desc.Ada_Type;
      with procedure Setter (Self : in out Self_Val.Val_Desc.Ada_T;
                             Val  : Val_Desc.Ada_Type);
--        Parent_Owns_Result : Boolean := False;
   package Byval is
      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
        with Convention => C;

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
        with Convention => C;
   end Byval;

   generic
      with package Val_Desc is new Py_Bind.Py_Type_Descriptor (<>);
      Property_Name : String;
      with function Getter
        (Self : Self_Val.Access_Desc.Ada_T) return Val_Desc.Ada_Type;
      with procedure Setter (Self : Self_Val.Access_Desc.Ada_T;
                             Val  : Val_Desc.Ada_Type);
--        Parent_Owns_Result : Boolean := False;
   package Byref is
      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
        with Convention => C;

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
        with Convention => C;
   end Byref;

end Py_Bind.Py_Property;
