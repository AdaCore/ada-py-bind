with Py_Bind.Py_Type_Descriptor;
with Py_Bind.Py_Module;
with Py_Bind.Py_Functions;
with Py_Bind.Py_Value;

--  This package is a helper that will allow clients to create bindings to
--  definite bounded arrays, using a ``Py_Type_Descriptor`` as the way to bind
--  the array's element and index.
--
--  NOTE: The bounds of the array will be *exactly* the same as in Ada. If they
--  start at ``1`` in Ada, they'll start at 1 in Python. If the indexing type
--  is an enum, then the indexing type in python will be different depending
--  on what mechanism is used to bind the enum.

generic
   type Index_Type is (<>);
   --  Index type for the array.

   with package Index_Desc
     is new Py_Type_Descriptor (Index_Type, others => <>);
   --  Index descriptor for the index type.

   type Element_Type is private;
   --  Element type for the array.

   with package Element_Desc
     is new Py_Type_Descriptor (Element_Type, others => <>);
   --  Element descriptor for the element type.

   type Array_Type is array (Index_Type) of Element_Type;
   --  Ada array type to bind.

   with package Module is new Py_Bind.Py_Module (<>);
   --  Module in which the Python type will be added.

   Name : String;
   --  Name of the python type.
package Py_Bind.Desc_Array is

   package Py_Container
   is new Py_Bind.Py_Value (Array_Type, Module, Name);

private

   --  We use the __getitem__/__setitem__ special
   --  methods to allow indexing of the bound type - see
   --  https://docs.python.org/2/reference/datamodel.html#object.__getitem__
   --
   --  TODO: Implement __iter__/__next__ to allow iterating on arrays.

   function Set_Item (Args : Py_Args) return PyObject;
   function Get_Item (Args : Py_Args) return PyObject;

   package Get_Item_Method is new Py_Bind.Py_Functions.Raw_Method
     ("__getitem__",
      Get_Item,
      Py_Container,
      Create_Profile
        ((Arg_Spec ("self", Py_Container.Py_Type),
          Arg_Spec ("key", Index_Desc.P_Py_Type)),
         Element_Desc.P_Py_Type));

   package Set_Item_Method is new Py_Bind.Py_Functions.Raw_Method
     ("__setitem__",
      Set_Item,
      Py_Container,
      Create_Profile
        ((Arg_Spec ("self", Py_Container.Py_Type),
          Arg_Spec ("key", Index_Desc.P_Py_Type),
          Arg_Spec ("value", Element_Desc.P_Py_Type))));

end Py_Bind.Desc_Array;
