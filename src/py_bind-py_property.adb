package body Py_Bind.Py_Property is

   package body Byval is

      procedure Init;

      ----------------
      -- Raw_Getter --
      ----------------

      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
      is
      begin
         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            P    : constant Py_Object'Class :=
              Val_Desc.To_Python (Getter (Self.all));
         begin
            return P.Py_Data;
         end;
      end Raw_Getter;

      ----------------
      -- Raw_Setter --
      ----------------

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
      is
         Ignore : PyObject;
      begin
         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            Val  : constant Val_Desc.Ada_Type := Val_Desc.To_Ada (Prop);
         begin
            Setter (Self.all, Val);
            return 0;
         end;
      exception
         when E : others =>
            Ignore := Handle_Error (E);
            return 1;
      end Raw_Setter;

      ----------
      -- Init --
      ----------

      procedure Init is
         Dummy : Boolean :=
           PyDescr_NewGetSet
             (Typ     => Self_Val.Val_Desc.P_Py_Type,
              Name    => Property_Name,
              Setter  => Raw_Setter'Unrestricted_Access,
              Getter  => Raw_Getter'Unrestricted_Access,
              Doc     => "");
      begin
         null;
      end Init;

   begin
      Init;
   end Byval;

   package body Byref is

      procedure Init;

      ----------------
      -- Raw_Getter --
      ----------------

      function Raw_Getter
        (Obj : PyObject; Dummy : System.Address) return PyObject
      is
      begin
         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            P    : constant Py_Object'Class :=
              Val_Desc.To_Python (Getter (Self));
         begin
            return P.Py_Data;
         end;
      exception
         when E : others =>
            return Handle_Error (E);
      end Raw_Getter;

      ----------------
      -- Raw_Setter --
      ----------------

      function Raw_Setter
        (Obj     : PyObject;
         Prop    : PyObject;
         Dummy   : System.Address) return Integer
      is
         Ignore : PyObject;
      begin
         --  We use a declare block to ensure that exception in To_Ada
         --  functions are caught.
         declare
            Self : constant Self_Val.Access_Desc.Ada_T :=
              Self_Val.Access_Desc.P_To_Ada (Obj);
            Val  : constant Val_Desc.Ada_Type := Val_Desc.To_Ada (Prop);
         begin
            Setter (Self, Val);
            return 0;
         end;
      exception
         when E : others =>
            Ignore := Handle_Error (E);
            return 1;
      end Raw_Setter;

      ----------
      -- Init --
      ----------

      procedure Init is
         Dummy : Boolean := PyDescr_NewGetSet
           (Typ     => Self_Val.Val_Desc.P_Py_Type,
            Name    => Property_Name,
            Setter  => Raw_Setter'Unrestricted_Access,
            Getter  => Raw_Getter'Unrestricted_Access,
            Doc     => "");
      begin
         null;
      end Init;

   begin
      Init;
   end Byref;

end Py_Bind.Py_Property;
