------------------------------------------------------------------------------
--                              ada-py-bind                                 --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with Py_Bind.Types;           use Py_Bind.Types;

package body Py_Bind is

   Python_Type_Error    : exception;
   Python_Bounds_Error  : exception;
   Python_Runtime_Error : exception;
   --  Internal exception types. Used by *_Error procedures, and by
   --  ``Handle_Error``.

   Python_Error_Message : Unbounded_String;
   --  Global python error message. Used to work around the fact that Ada's
   --  exception messages are 256 chars max.

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
   begin
      Python_Error_Message := To_Unbounded_String (Msg);
      raise Python_Type_Error;
   end Type_Error;

   -------------------
   -- Runtime_Error --
   -------------------

   procedure Runtime_Error (Msg : String) is
   begin
      Python_Error_Message := To_Unbounded_String (Msg);
      raise Python_Runtime_Error;
   end Runtime_Error;

   -----------------
   -- Index_Error --
   -----------------

   procedure Index_Error (Msg : String) is
   begin
      Python_Error_Message := To_Unbounded_String (Msg);
      raise Python_Bounds_Error;
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
      Err_Msg : constant String := To_String (Python_Error_Message);
   begin
      if E_Id = Python_Bounds_Error'Identity then
         Set_Error (Py_Index_Error, Err_Msg);
      elsif E_Id = Python_Type_Error'Identity then
         Set_Error (Py_Type_Error, Err_Msg);
      elsif E_Id = Python_Runtime_Error'Identity then
         Set_Error (Py_Runtime_Error, Err_Msg);
      elsif E_Id = Python_Bubble_Up'Identity then
         --  In that case, we just raised to interrupt the Ada control flow and
         --  raise the current python error. Do nothing.
         null;
      else
         --  In the rest of cases, we had an unexpected Ada exception.
         --  Transform it into a Python exception.
         Set_Error
           (Py_Runtime_Error,
            "Native Ada exception: "
            & ASCII.LF
            & Exception_Message (E)
            & ASCII.LF
            & "Traceback: "
            & Symbolic_Traceback (E));
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
      Profile    : Py_Fn_Profile) return Py_Args
   is
      Ret         : Py_Args :=
        Py_Args'
          (Profile.Nb_Args, Args, KwArgs,
           Profile'Unrestricted_Access,
           (others => Py_None));
      Nb_Pos_Args : constant Natural := PyObject_Size (Args);
      Nb_Args     : constant Natural :=
        (if KwArgs = null then 0 else PyObject_Size (KwArgs)) + Nb_Pos_Args;
   begin

      --  Check number of arguments
      if Nb_Args > Max_Args (Profile) then
         Type_Error
           ("Too many arguments. Expected max "
            & Stripped_Image (Max_Args (Profile))
            & ", got " & Stripped_Image (Positive (Nb_Args)));
      end if;

      if Nb_Args > Max_Args (Profile) then
         Type_Error
           ("Too few arguments. Expected at least "
            & Stripped_Image (Min_Args (Profile))
            & ", got " & Stripped_Image (Nb_Args));
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
               Type_Error
                 ("Missing argument " & To_String (Spec.Name));
            end if;

            --  If an argument has been matched but is of the wrong type, raise
            --  an error.
            Type_Check
              (M_Arg, Spec.Py_Type,
               "argument " & To_String (Spec.Name));
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
               if not Profile.Valid_Kws.Contains (PyString_AsString (Key))
               then
                  Type_Error
                    ("Unexpected kw argument: " & PyString_AsString (Key));
               end if;
               exit when Pos = -1;
            end loop;
         end;
      end if;

      return Ret;
   end Create;

   ----------------
   -- Type_Check --
   ----------------

   procedure Type_Check
     (Val, Expected_Type : PyObject;
      Target_Desc        : String)
   is
      Res : constant Boolean :=
        PyObject_IsInstance (Val, Expected_Type);
   begin
      --  PyObject_IsInstance can set an error if the Py_Type is
      --  invalid. In that case, raise immediately.
      if PyErr_Occurred /= null then
         raise Python_Bubble_Up;
      end if;

      if not Res then
         Type_Error
           ("Wrong type for " & Target_Desc
            & ": expected " & Image (Expected_Type)
            & ", got " & Image (To_PyObject (GetTypeObject (Val))));
      end if;

   end Type_Check;

   --------------
   -- Arg_Spec --
   --------------

   function Arg_Spec
     (Name    : String;
      Py_Type : PyObject;
      Is_Kw   : Boolean := False;
      Doc     : String := "") return Py_Arg_Spec
   is
     (Py_Arg_Spec'(To_Unbounded_String (Name), Py_Type, Is_Kw,
                   To_Unbounded_String (Doc)));

   --------------
   -- Min_Args --
   --------------

   function Min_Args (Args : Py_Args) return Natural
   is (Min_Args (Args.Args_Spec.all));

   --------------
   -- Max_Args --
   --------------

   function Max_Args (Args : Py_Args) return Natural
   is (Max_Args (Args.Args_Spec.all));

end Py_Bind;
