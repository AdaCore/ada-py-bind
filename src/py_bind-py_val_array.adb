package body Py_Bind.Py_Val_Array is

   --------------
   -- Set_Item --
   --------------

   function Set_Item (Args : Py_Args) return PyObject is
      Self : constant Py_Container.Val_Access :=
        Py_Container.Access_Desc.Get_Arg (Args, 1);

      Key  : constant Index_Desc.Ada_T :=
        Index_Desc.To_Ada (Args.Get_Item (2));
      --  Get the Ada value of the index.

      Item : constant Element_Val.Access_Desc.Ada_T :=
        Element_Val.Access_Desc.P_To_Ada (Args.Get_Item (3));
      --  Get the Ada value for the element.
   begin
      --  Set element
      Self (Key) := Item.all;
      return Py_None;
   end Set_Item;

   --------------
   -- Get_Item --
   --------------

   function Get_Item (Args : Py_Args) return PyObject is
      Self : constant Py_Container.Val_Access :=
        Py_Container.Access_Desc.Get_Arg (Args, 1);

      Key  : constant Index_Desc.Ada_T :=
        Index_Desc.To_Ada (Args.Get_Item (2));
      --  Get the Ada value for the index.
   begin
      --  Convert the element at index to Python and return it
      return Element_Val.Access_Desc.P_To_Python_Unsafe
        (Self (Key)'Unrestricted_Access).Py_Data;
   end Get_Item;

end Py_Bind.Py_Val_Array;
