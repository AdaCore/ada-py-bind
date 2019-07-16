with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Interfaces.C;
with System;

package body Py_Bind.Py_Value is

   type T_Access is access all T;

   procedure Init;
   procedure Set_UD (Self : Rec_Access; Obj : PyObject);

   function To_Python (Self : Rec_Access; Owns_Ptr : Boolean := True) return T;

   function Default_Constructor
     (Data : PyObject; Args : PyObject) return PyObject with Convention => C;

   function Default_Destructor
     (Data : PyObject; Args : PyObject) return PyObject with Convention => C;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Rec,
      Name   => Rec_Access);

   Class   : PyObject;

   function Py_Type return PyObject is (Class);

   ------------
   -- Set_UD --
   ------------

   procedure Set_UD (Self : Rec_Access; Obj : PyObject) is
      S : constant PyObject := PyCObject_FromVoidPtr (Self.all'Address);
   begin
      PyObject_SetAttrString (Obj, "__userdata", S);
   end Set_UD;

   ----------
   -- Free --
   ----------

   overriding procedure Destroy (Self : in out T) is
   begin
      if Self.Owns_Data then
         declare
            R : Rec_Access := To_Ada (Self.Py_Data);
         begin
            Destroy (R.all);
            Free (R);
         end;
      end if;
      Destroy (Self);
   end Destroy;

   ---------------
   -- To_Python --
   ---------------

   function To_Python_Unsafe (Self : Rec_Access) return Py_Object'Class is
   begin
      return To_Python (Self, False);
   end To_Python_Unsafe;

   ---------------
   -- To_Python --
   ---------------

   function To_Python_Unsafe (Self : Rec) return Py_Object'Class is
   begin
      return To_Python (new Rec'(Self), True);
   end To_Python_Unsafe;

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Self : Rec_Access; Owns_Ptr : Boolean := True) return T
   is
      Obj  : PyObject;
      Args : PyObject;
      New_Method : constant PyObject :=
        PyObject_GetAttrString (Class, "__new__");
      Init_Method : constant PyObject :=
        PyObject_GetAttrString (Class, "__init__");
      T_Inst : T_Access;
   begin

      --  Creating a new instance is equivalent to calling its metaclass. This
      --  is true for both new-style classes and old-style classes (for which
      --  the tp_call slot is set to PyInstance_New.

      --  Here, we are in fact calling  Class.__new__ (cls, *args, **kwargs).
      --  After allocating memory, this in turns automatically tp_init in the
      --  type definition, which in the case of GNATCOLL cases is often set to
      --  slot_tp_init. The latter in turn calls __init__

      Args := PyTuple_New (1);
      PyTuple_SetItem (Args, 0, Class);

      Obj := PyObject_Call
        (Object => New_Method,
         Args   => Args,
         Kw     => null);   --  NOT: Py_None, which is not a valid dictionary
      Py_DECREF (New_Method);

      if Obj = null then
         if Active (Me) then
            Trace (Me, "Could not create instance");
            PyErr_Print;    --  debugging only
         end if;
         return No_T;
      end if;

      if Active (Me) then
         Assert
           (Me, Get_Refcount (Obj) = 1,
            "Object's refcount should be 1, got "
            & Get_Refcount (Obj)'Img,
            Raise_Exception => False);
      end if;

      --  The PyObject should have a single reference in the end, owned by
      --  the class instance itself.

      --  Set Self as user data on python object
      Set_UD (Self, Obj);

      PyTuple_SetItem (Args, 0, Obj);

      declare
         Dummy : PyObject := PyObject_Call
           (Object => Init_Method,
            Args   => Args,
            Kw     => null);
      begin
         Py_DECREF (Args);
         Py_DECREF (Init_Method);
      end;

      T_Inst := new T'(Py_Data   => Obj,
                       Owns_Data => Owns_Ptr);

      return T_Inst.all;
   end To_Python;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Rec_Access is
      UD          : PyObject;
   begin
      if PyObject_HasAttrString (Self, "__userdata") then
         UD := PyObject_GetAttrString (Self, "__userdata");

         --  Get the PyCapsule object embedded in the __userdata field

         declare
            A : constant System.Address := PyCObject_AsVoidPtr (UD);
            R : Rec_Access with Import;
            for R'Address use A'Address;
         begin
            return R;
         end;
      end if;
      return null;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Rec is
   begin
      return To_Ada (Self).all;
   end To_Ada;

   -------------------------
   -- Default_Constructor --
   -------------------------

   function Default_Constructor
     (Data : PyObject; Args : PyObject) return PyObject
   is
      pragma Unreferenced (Data);
      R : Rec_Access;
      Self : PyObject;
   begin
      if Args /= null then
         Self := PyObject_GetItem (Args, 0);
         if Self /= null then
            R := To_Ada (Self);
            if R = null then
               R := new Rec;
               Set_UD (R, Self);
            end if;
         end if;
      end if;

      return Py_None;
   end Default_Constructor;

   ------------------------
   -- Default_Destructor --
   ------------------------

   function Default_Destructor
     (Data : PyObject; Args : PyObject) return PyObject
   is
      pragma Unreferenced (Args, Data);
   begin
      return Py_None;
   end Default_Destructor;

   procedure Init is

      use Interfaces.C.Strings;

      Dict        : constant PyDictObject := PyDict_New;
      Ignored     : Integer;
      S           : chars_ptr;
      Constructor, Destructor : PyMethodDef;
   begin
      PyDict_SetItemString
        (Dict, "__module__",
         PyObject_GetAttrString (Module.Desc.Module, "__name__"));

      Class := Type_New
        (Name  => Name,
         Bases => null,
         Dict  => Dict);

      if Class = null then
         PyErr_Print;
         raise Program_Error
           with "Could not register class " & Name;
      end if;

      Constructor :=
        Create_Method_Def
          ("__init__", Default_Constructor'Unrestricted_Access);

      Destructor :=
        Create_Method_Def
          ("__del__", Default_Destructor'Unrestricted_Access);

      Add_Method (Class, Constructor, Module => Module.Desc.Module);
      Add_Method (Class, Destructor, Module => Module.Desc.Module);

      S := New_String (Name);
      Ignored := PyModule_AddObject (Module.Desc.Module, S, Class);
      Free (S);
   end Init;

begin
   Init;
end Py_Bind.Py_Value;
