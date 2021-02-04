*&---------------------------------------------------------------------*
*& Report  Z518375_ALV
*&
*&---------------------------------------------------------------------*
REPORT z518375_alv.
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  Define the Local class inheriting from the CL_SALV_MODEL_LIST
*  to get an access of the model, controller and adapter which inturn
*  provides the Grid Object
*----------------------------------------------------------------------*
CLASS lcl_salv_model DEFINITION INHERITING FROM cl_salv_model_list.
  PUBLIC SECTION.
    DATA: o_control TYPE REF TO cl_salv_controller_model,
          o_adapter TYPE REF TO cl_salv_adapter.
    METHODS:
      grabe_model
        IMPORTING
          io_model TYPE REF TO cl_salv_model,
       grabe_controller,
       grabe_adapter.
  PRIVATE SECTION.
    DATA: lo_model TYPE REF TO cl_salv_model.
ENDCLASS.                    "LCL_SALV_MODEL DEFINITION
*----------------------------------------------------------------------*
* Event handler for the added buttons
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
* Local Report class - Definition
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_y_employees,
            status TYPE char4,
*            field1 TYPE char30,
            pernr TYPE pernr_d,
            subty TYPE pa2001-subty,
            begda TYPE pa2001-begda,
            endda TYPE pa2001-endda,
            react TYPE pa0768-react,  " Estado reporte
            conty TYPE pa0768-conty,  " Contingencia
            fecon TYPE pa0768-fecon,  " Fecha de baja
            fatep TYPE pa0768-fatep,  " Fecha AT y EP
           END OF ty_y_employees.
    TYPES: ty_t_employees TYPE STANDARD TABLE OF ty_y_employees. "pa2001.
    DATA: t_data TYPE ty_t_employees.
    DATA: o_salv       TYPE REF TO cl_salv_table.
    DATA: o_salv_model TYPE REF TO lcl_salv_model.
    METHODS:
      create_lines,
      get_data,
      set_editable,
      generate_output.
ENDCLASS.                    "lcl_report DEFINITION
*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: lo_report TYPE REF TO lcl_report.

TABLES: pa2001.
*----------------------------------------------------------------------*
* At selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 500 TITLE title
                                     AS WINDOW.

PARAMETERS s_lines TYPE char4 DEFAULT 10.
*SELECT-OPTIONS s_pernr FOR pa2001-pernr DEFAULT '04000111'.
*SELECT-OPTIONS s_dates FOR pa2001-begda.
*PARAMETERS s_subty TYPE pa2001-subty DEFAULT '7735'.
SELECTION-SCREEN END OF SCREEN 500.

title = 'Número de líneas necesarias'.

CALL SELECTION-SCREEN '0500' STARTING AT 10 10.
*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CREATE OBJECT lo_report.
  lo_report->create_lines( ).
*  lo_report->get_data( ).
  lo_report->generate_output( ).
*----------------------------------------------------------------------*
* Local Report class - Implementation
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.
  METHOD set_editable.

  ENDMETHOD.                    "set_editable
  METHOD create_lines.
    DATA: lh_data LIKE LINE OF me->t_data.
    lh_data-status = icon_yellow_light.
    ADD 1 TO s_lines.
    WHILE sy-index < s_lines.
      APPEND lh_data TO me->t_data.
*      APPEND INITIAL LINE TO me->t_data.
    ENDWHILE.
  ENDMETHOD.                    "create_lines
  METHOD get_data.
*   test data
*    SELECT pernr subty begda endda FROM pa2001 "* FROM pa2001
*           INTO TABLE me->t_data
*              WHERE pernr IN s_pernr
*              AND   begda >= s_dates-low
*              AND   endda <= s_dates-high
*              AND   subty EQ s_subty.
*           UP TO 30 ROWS.
  ENDMETHOD.                    "get_data
  METHOD generate_output.
*...New ALV Instance ...............................................
    TRY.
        cl_salv_table=>factory(
           EXPORTING
             list_display = abap_false
           IMPORTING
             r_salv_table = o_salv
           CHANGING
             t_table      = t_data ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.
*...PF Status.......................................................
*   Add MYFUNCTION from the report SALV_DEMO_TABLE_EVENTS
    o_salv->set_screen_status(
      pfstatus      =  'SALV_HENAR'
      report        =  sy-cprog "'SALV_DEMO_TABLE_EVENTS'
      set_functions = o_salv->c_functions_all ).
*...Event handler for the button.....................................
    DATA: lo_events TYPE REF TO cl_salv_events_table,
          lo_event_h TYPE REF TO lcl_event_handler.
* event object
    lo_events = o_salv->get_event( ).
* event handler
    CREATE OBJECT lo_event_h.
* setting up the event handler
    SET HANDLER lo_event_h->on_user_command FOR lo_events.
*...Get Model Object ...............................................
    DATA: lo_alv_mod TYPE REF TO cl_salv_model.
*   Narrow casting
    lo_alv_mod ?= o_salv.
*   object for the local inherited class from the CL_SALV_MODEL_LIST
    CREATE OBJECT o_salv_model.
*   grabe model to use it later
    CALL METHOD o_salv_model->grabe_model
      EXPORTING
        io_model = lo_alv_mod.
*...Generate ALV output ...............................................
    o_salv->display( ).
  ENDMETHOD.                    "generate_output
ENDCLASS.                    "lcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
* LCL_SALV_MODEL implementation
*----------------------------------------------------------------------*
CLASS lcl_salv_model IMPLEMENTATION.
  METHOD grabe_model.
*   save the model
    lo_model = io_model.
  ENDMETHOD.                    "grabe_model
  METHOD grabe_controller.
*   save the controller
    o_control = lo_model->r_controller.
  ENDMETHOD.                    "grabe_controller
  METHOD grabe_adapter.
*   save the adapter from controller
    o_adapter ?= lo_model->r_controller->r_adapter.
  ENDMETHOD.                    "grabe_adapter
ENDCLASS.                    "LCL_SALV_MODEL IMPLEMENTATION
*----------------------------------------------------------------------*
* Event Handler for the SALV
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_user_command.
    DATA: lo_grid TYPE REF TO cl_gui_alv_grid,
    lo_full_adap TYPE REF TO cl_salv_fullscreen_adapter.
    DATA: ls_layout TYPE lvc_s_layo.
    DATA: lt_p2001 TYPE TABLE OF pa2001.
    DATA l_data LIKE LINE OF lo_report->t_data.

    CASE e_salv_function.
*     Make ALV as Editable ALV
      WHEN 'MYFUNCTION'.
*       Contorller
        CALL METHOD lo_report->o_salv_model->grabe_controller.
*       Adapter
        CALL METHOD lo_report->o_salv_model->grabe_adapter.
*       Fullscreen Adapter (Down Casting)
        lo_full_adap ?= lo_report->o_salv_model->o_adapter.
*       Get the Grid
        lo_grid = lo_full_adap->get_grid( ).
*       Got the Grid .. ?
        IF lo_grid IS BOUND.
*         Editable ALV
          ls_layout-edit = 'X'.
*         Set the front layout of ALV
          CALL METHOD lo_grid->set_frontend_layout
            EXPORTING
              is_layout = ls_layout.
*         refresh the table
          CALL METHOD lo_grid->refresh_table_display.
        ENDIF.
*     Make ALV as Editable ALV
      WHEN 'LOAD_DATA'.
        lo_report->get_data( ).
      WHEN 'SAVE_CHANG'.
        LOOP AT lo_report->t_data INTO l_data.
          SELECT SINGLE a~pernr a~subty a~begda a~endda i~react i~conty i~fecon i~fatep
            INTO CORRESPONDING FIELDS OF l_data " lo_report->t_data
             FROM pa2001 AS a
              INNER JOIN pa0768 AS i
                  ON  a~pernr = i~pernr
                  AND a~begda = i~begda
                    WHERE a~pernr = l_data-pernr
                    AND   a~subty = l_data-subty
                                  .
            IF sy-subrc is INITIAL.
              l_data-status = icon_green_light.
            ENDIF.

        ENDLOOP.

        IF sy-subrc <> 0.
* Implement suitable error handling here
        ELSE.
          CLEAR  lo_report->t_data[].
          APPEND l_data TO lo_report->t_data.
          TRY.
              CALL METHOD lo_report->o_salv->refresh
                EXPORTING
                  refresh_mode = if_salv_c_refresh=>soft.
            CATCH cx_salv_msg.                          "#EC NO_HANDLER
          ENDTRY.
        ENDIF.

    ENDCASE.
  ENDMETHOD.                    "on_user_command
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION