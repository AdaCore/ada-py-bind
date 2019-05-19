with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with Interfaces.C;

package body Py_Bind.Py_Value is

   function To_Python (Self : Rec_Access; Owns_Ptr : Boolean := True) return T;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Rec,
      Name   => Rec_Access);

   Class   : PyObject;

   function Py_Type return PyObject is (Class);

   overriding procedure Free (Self : in out T) is
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
   end Free;

   function To_Python (Self : Rec_Access) return T is
   begin
      return To_Python (Self, False);
   end To_Python;

   function To_Python (Self : Rec) return T is
   begin
      return To_Python (new Rec'(Self), True);
   end To_Python;

   function To_Python (Self : Rec_Access; Owns_Ptr : Boolean := True) return T
   is
      Obj  : PyObject;
      Args : PyObject;
   begin

      --  Creating a new instance is equivalent to calling its metaclass. This
      --  is true for both new-style classes and old-style classes (for which
      --  the tp_call slot is set to PyInstance_New.
      --  Here, we are in fact calling  Class.__new__ (cls, *args, **kwargs).
      --  After allocating memory, this in turns automatically tp_init in the
      --  type definition, which in the case of GNATCOLL cases is often set to
      --  slot_tp_init. The latter in turn calls __init__
      --
      --  ??? This API does not permit passing extra parameters to the call

      Args := PyTuple_New (0);
      Obj := PyObject_Call
        (Object => Class,
         Args   => Args,
         Kw     => null);   --  NOT: Py_None, which is not a valid dictionary
      Py_DECREF (Args);

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

      if Active (Me) then
         Assert
           (Me,
            Get_Refcount (Obj) = 1,
            "New_Instance should own a single refcount of PyObject, got "
            & Get_Refcount (Obj)'Image);
      end if;

      --  Set Self as __userdata field on the PyObject
      declare
         S : PyObject with Import;
         for S'Address use Self'Address;
      begin
         PyObject_SetAttrString (Obj, "__userdata", S);
      end;

      return T'(Refcount.Refcounted
                with Py_Data   => Obj,
                Owns_Data => True);
   end To_Python;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Rec_Access is
      use Interfaces.C.Strings;
      UD_Accessor : chars_ptr := New_String ("__userdata");
      UD          : PyObject :=
        PyObject_GetAttrString (Self, UD_Accessor);
   begin
      Free (UD_Accessor);

      declare
         R : Rec_Access with Import;
         for R'Address use UD'Address;
      begin
         return R;
      end;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Rec is
   begin
      return To_Ada (Self).all;
   end To_Ada;

   procedure Init is

      use Interfaces.C.Strings;

      Dict    : constant PyDictObject := PyDict_New;
      Ignored : Integer;
      S       : chars_ptr;
   begin
      PyDict_SetItemString
        (Dict, "__module__",
         PyObject_GetAttrString (Module.Module, "__name__"));

      Class := Type_New
        (Name  => Name,
         Bases => null,
         Dict  => Dict);
      if Class = null then
         PyErr_Print;
         raise Program_Error
           with "Could not register class " & Name;
      end if;

      S := New_String (Name);
      Ignored := PyModule_AddObject (Module.Module, S, Class);
      Free (S);
   end Init;

begin
   Module.Init_Fns.Append (Init'Unrestricted_Access);
end Py_Bind.Py_Value;
