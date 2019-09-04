with Interfaces.C; use Interfaces.C;

package body Py_Bind.Types is
   PyString_Type : aliased Dummy;
   pragma Import (C, PyString_Type, "PyString_Type");

   PyInt_Type : aliased Dummy;
   pragma Import (C, PyInt_Type, "PyInt_Type");

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return String
   is
   begin
      if PyString_Check (Self) then
         return PyString_AsString (Self);
      else
         Type_Error ("Expected string, got " & Image (Self));
      end if;
   end To_Ada;

   --------------------
   -- Py_String_Type --
   --------------------

   function Py_String_Type return PyObject is
   begin
      return PyString_Type'Unrestricted_Access;
   end Py_String_Type;

   -----------------
   -- Py_Int_Type --
   -----------------

   function Py_Int_Type return PyObject is
   begin
      return PyInt_Type'Unrestricted_Access;
   end Py_Int_Type;

   -------------------
   -- Py_Float_Type --
   -------------------

   function Py_Float_Type return PyObject is
      PyFloat_Type : aliased Dummy;
      pragma Import (C, PyFloat_Type, "PyFloat_Type");
   begin
      return PyFloat_Type'Unrestricted_Access;
   end Py_Float_Type;

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Dummy : Unit_Type) return Py_Object'Class
   is (Py_Object'(Py_Data => Py_None));

   ---------------
   -- To_Python --
   ---------------

   function To_Python
     (Self : String) return Py_Object'Class
   is
     (Py_Object'(Py_Data => PyString_FromString (Self)));

   ---------------
   -- To_Python --
   ---------------

   function To_Python
     (Self : Integer) return Py_Object'Class
   is
     (Py_Object'(Py_Data => PyInt_FromLong (long (Self))));

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Dummy : PyObject) return Unit_Type is (null record);

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Integer
   is (Integer (PyInt_AsLong (Self)));

   ------------
   -- To_Ada --
   ------------

   function To_Ada (Self : PyObject) return Float
   is
     (Float (PyFloat_AsDouble (Self)));

   ---------------
   -- To_Python --
   ---------------

   function To_Python (Self : Float) return Py_Object'Class
   is
     (Py_Object'(Py_Data => PyFloat_FromDouble (Interfaces.C.double (Self))));

   -----------------------------
   -- Simple_Discrete_Binding --
   -----------------------------

   package body Simple_Discrete_Binding is

      function To_Python (Self : T) return Py_Object'Class is
        (Py_Object'
           (Py_Data => PyInt_FromLong (Interfaces.C.long (T'Pos (Self)))));

      function To_Ada (Self : PyObject) return T
      is (T'Val (PyInt_AsLong (Self)));

   end Simple_Discrete_Binding;

   -------------------------
   -- Simple_Enum_Binding --
   -------------------------

   package body Simple_Enum_Binding is

      function To_Python (Self : T) return Py_Object'Class is
        (Py_Object'
           (Py_Data => PyString_FromString (T'Image (Self))));

      function All_Values (Current : T := T'First) return String is
        ("""" & T'Image (Current) & """" &
         (if Current /= T'Last
          then ", " & All_Values (T'Succ (Current))
          else ""));

      function To_Ada (Self : PyObject) return T
      is
         Str : constant String := PyString_AsString (Self);
      begin
         begin
            return (T'Value (Str));
         exception
            when Constraint_Error =>
               Type_Error
                 ("Wrong value for enum: """ & Str & """"
                  & (if T'Pos (T'Last) - T'Pos (T'First) > 100
                    then ""
                    else
                       "." & ASCII.LF & "  Possible values: " & All_Values));
         end;
      end To_Ada;

   end Simple_Enum_Binding;

end Py_Bind.Types;
