FUNCTION-POOL zabaplint_ui.

TABLES: zabaplint_glob_data.

TYPES: BEGIN OF ty_node.
    INCLUDE STRUCTURE treev_node.
TYPES: text TYPE text50,
       END OF ty_node.

TYPES: ty_nodes TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY.

DATA: gv_read_only TYPE abap_bool,
      gv_ok_code   LIKE sy-ucomm,
      go_container TYPE REF TO cl_gui_custom_container,
      go_editor    TYPE REF TO cl_gui_textedit,
      go_splitter  TYPE REF TO cl_gui_easy_splitter_container,
      go_tree      TYPE REF TO cl_gui_simple_tree.
