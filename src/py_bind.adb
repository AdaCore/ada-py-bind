with Ada.Text_IO; use Ada.Text_IO;

package body Py_Bind is

   function Get_Item (Dict : PyObject; Key : String) return PyObject;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : in out Py_Object)
   is
   begin
      if Self.Py_Data /= Py_None then
         Py_DECREF (Self.Py_Data);
         Self.Py_Data := Py_None;
      end if;
   end Destroy;

   -------------
   -- Release --
   -------------

   procedure Release (Self : in out Py_Object'Class) is
   begin
      Self.Destroy;
   end Release;

   -----------
   -- Image --
   -----------

   function Image (Obj : PyObject) return String is
      Py_Repr : constant PyObject := PyObject_Repr (Obj);
   begin
      return S : constant String := PyString_AsString (Py_Repr) do
         Py_DECREF (Py_Repr);
      end return;
   end Image;

   ----------------
   -- Type_Error --
   ----------------

   procedure Type_Error (Msg : String) is
      PyExc_TypeError : PyObject;
      pragma Import (C, PyExc_TypeError, "PyExc_TypeError");
   begin
      PyErr_SetString (PyExc_TypeError, Msg);
   end Type_Error;

   -----------------
   -- Index_Error --
   -----------------

   procedure Index_Error (Msg : String) is
      PyExc_TypeError : PyObject;
      pragma Import (C, PyExc_TypeError, "PyExc_IndexError");
   begin
      PyErr_SetString (PyExc_TypeError, Msg);
   end Index_Error;

   --------------
   -- Min_Args --
   --------------

   function Min_Args (Args : Py_Args_Spec) return Natural
   is
      Ret : Natural := 0;
   begin
      for Arg of Args loop
         if not Arg.Is_Kw then
            Ret := Ret + 1;
         end if;
      end loop;
      return Ret;
   end Min_Args;

   --------------
   -- Max_Args --
   --------------

   function Max_Args (Args : Py_Args_Spec) return Natural
   is
     (Args'Length);

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Py_Args) is
   begin
      Py_Object (Self).Destroy;
      if Self.KwArgs /= Py_None then
         Py_DECREF (Self.KwArgs);
         Self.KwArgs := Py_None;
      end if;
   end Destroy;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (Args : Py_Args; Index : Positive) return PyObject is
   begin
      return Args.Matched_Args (Index);
   end Get_Item;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (Dict : PyObject; Key : String) return PyObject
   is
      P_Key : constant PyObject := PyString_FromString (Key);
   begin
      if Dict /= null and then PyDict_Contains (Dict, P_Key) then
         Py_DECREF (P_Key);
         return PyDict_GetItemString (Dict, Key);
      end if;
      return Py_None;
   end Get_Item;

   ------------------
   -- Handle_Error --
   ------------------

   function Handle_Error (E : Exception_Occurrence) return PyObject is
      E_Id : constant Exception_Id := Exception_Identity (E);
   begin
      Put_Line ("IN HANDLE ERROR");
      if E_Id = Python_Bounds_Error'Identity then
         Index_Error (Exception_Message (E));
      elsif E_Id = Python_Type_Error'Identity then
         Type_Error (Exception_Message (E));
      else
         Reraise_Occurrence (E);
      end if;

      return Py_None;
   end Handle_Error;

   ------------
   -- Create --
   ------------

   function Create
     (Args, KwArgs : PyObject;
      Args_Spec    : Py_Args_Spec;
      Valid_Kws    : access Name_Sets.Set) return Py_Args
   is
      Ret : Py_Args :=
        Py_Args'
          (Args_Spec'Last, Args, KwArgs, Args_Spec, (others => Py_None));
      Nb_Args : constant Positive := PyObject_Size (Args);

   begin
      --  Match arguments
      for J in Ret.Args_Spec'Range loop
         if J <= Nb_Args then
            --  First, get argument positionally.
            Ret.Matched_Args (J) := PyObject_GetItem (Args, J - 1);
         else
            --  If we're past positional arguments, try to get argument via kw.
            Ret.Matched_Args (J) :=
              Get_Item (Ret.KwArgs, To_String (Ret.Args_Spec (J).Name));
         end if;
      end loop;

      --  Check arguments
      for J in Ret.Matched_Args'Range loop
         declare
            M_Arg : PyObject renames Ret.Matched_Args (J);
            Spec  : Py_Arg_Spec renames Ret.Args_Spec (J);
         begin
            --  If any mandatory argument hasn't been matched, then raise an
            --  error.
            if M_Arg = Py_None and then not Spec.Is_Kw then
               raise Python_Type_Error with
                 "Missing argument " & To_String (Spec.Name);
            end if;

            --  If an argument has been matched but is of the wrong type, raise
            --  an error.
            if not PyObject_IsInstance (M_Arg, Spec.Py_Type) then
               raise Python_Type_Error with
                 "Wrong type for argument " & To_String (Spec.Name)
                 & ": expected " & Image (Spec.Py_Type)
                 & ", got " & Image (GetTypeObject (M_Arg));
            end if;
         end;
      end loop;

      --  Check that there aren't extra kw arguments.
      if KwArgs /= null then
         declare
            Pos      : Integer := 0;
            Key, Val : PyObject;
         begin
            loop
               PyDict_Next (KwArgs, Pos, Key, Val);
               if not Valid_Kws.Contains (PyString_AsString (Key)) then
                  raise Python_Type_Error with
                    "Unexpected kw argument: " & PyString_AsString (Key);
               end if;
               exit when Pos = -1;
            end loop;
         end;
      end if;

      return Ret;
   end Create;

end Py_Bind;
