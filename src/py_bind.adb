with Ada.Text_IO; use Ada.Text_IO;

package body Py_Bind is

   function Get_Item (Dict : PyObject; Key : String) return PyObject;

   function Stripped_Image (Self : Integer) return String;
   --  Return the image of ``Self`` without a leading whitespace if Self is a
   --  natural number.

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

   -------------------
   -- Runtime_Error --
   -------------------

   procedure Runtime_Error (Msg : String) is
      PyExc_RuntimeError : PyObject;
      pragma Import (C, PyExc_RuntimeError, "PyExc_RuntimeError");
   begin
      PyErr_SetString (PyExc_RuntimeError, Msg);
   end Runtime_Error;

   -----------------
   -- Index_Error --
   -----------------

   procedure Index_Error (Msg : String) is
      PyExc_TypeError : PyObject;
      pragma Import (C, PyExc_TypeError, "PyExc_IndexError");
   begin
      PyErr_SetString (PyExc_TypeError, Msg);
   end Index_Error;

   ------------
   -- Create --
   ------------

   function Create_Profile
     (Args     : Py_Arg_Spec_Array;
      Ret_Type : PyObject := null) return Py_Fn_Profile is
   begin
      return Args_Spec : Py_Fn_Profile (Args'Length) do
         Args_Spec.Args := Args;
         Args_Spec.Ret_Type := Ret_Type;
         for Arg_Spec of Args_Spec.Args loop
            Args_Spec.Valid_Kws.Include (To_String (Arg_Spec.Name));
         end loop;
      end return;
   end Create_Profile;

   --------------
   -- Min_Args --
   --------------

   function Min_Args (Args : Py_Fn_Profile) return Natural
   is
      Ret : Natural := 0;
   begin
      for Arg of Args.Args loop
         if not Arg.Is_Kw then
            Ret := Ret + 1;
         end if;
      end loop;
      return Ret;
   end Min_Args;

   --------------
   -- Max_Args --
   --------------

   function Max_Args (Args : Py_Fn_Profile) return Natural
   is
     (Args.Nb_Args);

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
      if E_Id = Python_Bounds_Error'Identity then
         Index_Error (Exception_Message (E));
      elsif E_Id = Python_Type_Error'Identity then
         Type_Error (Exception_Message (E));
      else
         Reraise_Occurrence (E);
      end if;

      return null;
   end Handle_Error;

   --------------------
   -- Stripped_Image --
   --------------------

   function Stripped_Image (Self : Integer) return String is
      Img : constant String := Self'Image;
   begin
      return (if Img (1) = '-' then Img else Img (2 .. Img'Last));
   end Stripped_Image;

   ------------
   -- Create --
   ------------

   function Create
     (Args, KwArgs : PyObject;
      Args_Spec    : Py_Fn_Profile) return Py_Args
   is
      Ret         : Py_Args :=
        Py_Args'
          (Args_Spec.Nb_Args, Args, KwArgs,
           Args_Spec'Unrestricted_Access,
           (others => Py_None));
      Nb_Pos_Args : constant Natural := PyObject_Size (Args);
      Nb_Args     : constant Natural :=
        (if KwArgs = null then 0 else PyObject_Size (KwArgs)) + Nb_Pos_Args;
   begin

      --  Check number of arguments
      if Nb_Args > Max_Args (Args_Spec) then
         raise Python_Type_Error
           with "Too many arguments. Expected max "
           & Stripped_Image (Max_Args (Args_Spec))
           & ", got " & Stripped_Image (Positive (Nb_Args));
      end if;

      if Nb_Args > Max_Args (Args_Spec) then
         raise Python_Type_Error
           with "Too few arguments. Expected at least "
           & Stripped_Image (Min_Args (Args_Spec))
           & ", got " & Stripped_Image (Nb_Args);
      end if;

      --  Match arguments
      for J in Ret.Args_Spec.Args'Range loop
         if J <= Nb_Pos_Args then
            --  First, get argument positionally.
            Ret.Matched_Args (J) := PyObject_GetItem (Args, J - 1);
         else
            --  If we're past positional arguments, try to get argument via kw.
            Ret.Matched_Args (J) :=
              Get_Item (Ret.KwArgs, To_String (Ret.Args_Spec.Args (J).Name));
         end if;
      end loop;

      --  Check arguments
      for J in Ret.Matched_Args'Range loop
         declare
            M_Arg : PyObject renames Ret.Matched_Args (J);
            Spec  : Py_Arg_Spec renames Ret.Args_Spec.Args (J);
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
               if not Args_Spec.Valid_Kws.Contains (PyString_AsString (Key))
               then
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
